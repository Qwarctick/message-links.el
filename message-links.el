;;; message-links.el --- Manage reference links in text -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2022 Philippe Noel

;; Author: Philippe Noel <philippe.noel@loria.fr>

;; URL: https://github.com/PhilippeNoel1/message-links.el
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Manage reference links.
;;

;;; Usage:

;; M-x message-links-add-link          - Add a new link.
;; M-x message-links-convert-link-at-point - Convert URL at point to a reference.
;; M-x message-links-convert-links-all - Convert all URLs in buffer/region.
;; M-x message-links-renumber-all      - Re-number links sequentially.

;;; Code:


(defgroup message-links nil
  "Manage reference links in text."
  :group 'message)

(defcustom message-links-link-header "--- links ---"
  "Header used to separate links from the original text.
If set to nil, no header will be used."
  :type '(choice (const nil) string))

(defcustom message-links-index-start 1
  "Index of the first link inserted."
  :type 'integer)

(defcustom message-links-enable-link-header t
  "OBSOLETE: use `message-links-link-header' instead.

Use the link header to separate original text from links."
  :type 'boolean)


(defcustom message-links-sep-footnotes-link '("[" . "]: ")
  "The text to use for links in the footnotes.
If the default is used, links in footnotes looks like `[1]: '."
  :type '(cons string string))

(defcustom message-links-sep-text-link '("[" . "]")
  "The text to use for number link in the text.
If the default is used, links in text looks like `[1]'."
  :type '(cons string string))

(defcustom message-links-match-link-at-point-functions
  '(message-links-match-link-at-point-url
    message-links-match-link-at-point-sha1)
  "List of functions to match a link at point.
Each function is called with no arguments until one returns non-nil."
  :type '(repeat function))

(defcustom message-links-match-sha1-min-length 8
  "Minimum length for SHA1 hash matching."
  :type 'integer)

(defcustom message-links-match-link-forward-fn
  #'message-links-match-link-forward-default
  "Function that finds the next link, taking a single LIMIT argument."
  :type 'function)

(defcustom message-links-limit-range-fn #'message-links-limit-range-default
  "Function to return a cons cell with minimum & maximum range.
By default this returns ((point-min) . (point-max)).

Use to limit the region considered for links and link creation.
This can be useful for adding links to commit-logs which often
show status which is ignored at the end of the commit message."
  :type 'function)

;;; Implementations of link matching functions.

(require 'thingatpt)

(defun message-links-match-link-at-point-url ()
  "Return the bounds of the URL at point."
  (bounds-of-thing-at-point 'url))

(defun message-links-match-link-at-point-sha1 ()
  "Return the bounds of a Git SHA1 hash at point.
Matches hexadecimal strings of at least `message-links-match-sha1-min-length' characters."
  (when (thing-at-point-looking-at
         (format "\\b[0-9a-fA-F]\\{%d,\\}\\b"
                 message-links-match-sha1-min-length))
    (cons (match-beginning 0) (match-end 0))))

(defun message-links-match-link-forward-default (limit)
  "Return the bounds of a link up to LIMIT, scanning forwards as needed."
  (let ((bounds nil))
    (save-excursion
      (while (and (null bounds) (< (point) limit))
        (unless (setq bounds (message-links--match-link-at-point))
          ;; No match, skip over non-blank, then blank space to the next
          ;; possible candidate for a URL.
          (skip-chars-forward "^[:blank:]\n" limit)
          (skip-chars-forward "[:blank:]\n" limit))))
    bounds))

(defun message-links-limit-range-default ()
  "Return the minimum/maximum ranges."
  (cons (point-min) (point-max)))

;;; Private Implementations.

(defun message-links--match-link-at-point ()
  "Return the first match of `message-links-match-link-at-point-functions'."
  (run-hook-with-args-until-success
   'message-links-match-link-at-point-functions))

(defmacro message-links--with-range-limit (&rest body)
  "Execute BODY with `message-links-limit-range-fn' applied.
By convention only use in public, interactive functions.
While harmless, there is no need to wrap blocks of code multiple times."
  (declare (indent 0))
  `(let ((limit-range (funcall message-links-limit-range-fn)))
     (save-restriction
       (narrow-to-region (car limit-range) (cdr limit-range))
       ,@body)))

(defun message-links--goto-last-non-blank-eol ()
  "Move the cursor to the line ending of the last non-blank line."
  (goto-char (point-max))
  (skip-chars-backward "[:blank:]\n" (point-min)))

(defun message-links--add-link-impl (link)
  "Add LINK, leaving the point where the link reference ends."
  (let ((short-link-index
         (number-to-string (1+ (message-links--get-max-footnote-link))))
        (pos-init (point)))

    (cond
     (message-links-link-header
      ;; Ensure message-links-link-header present in the message (when non-nil).
      (message-links--goto-last-non-blank-eol)
      (unless (save-match-data
                (save-excursion
                  (re-search-backward
                   (message-links--gen-link-header-search-regex)
                   nil t)))
        (insert "\n\n" message-links-link-header)))
     ;; No link header expected.
     (t
      (message-links--goto-last-non-blank-eol)

      ;; When inserting the first link without an explicit header:
      ;; add a blank line separating links from non-link text
      ;; (not essential but reads better).
      (when (string-equal
             (number-to-string message-links-index-start) short-link-index)
        (insert "\n"))))

    (insert "\n" (message-links--gen-footnotes-link short-link-index) link)

    ;; Leave the cursor after the link text (as if the user had typed it in).
    (goto-char pos-init)
    (insert (message-links--gen-text-link short-link-index))))

(defun message-links--convert-link-from-bounds (bounds)
  "Convert text at BOUNDS to a footnote link."
  (let ((link (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (delete-region (car bounds) (cdr bounds))
    (save-excursion
      (goto-char (car bounds))
      (message-links--add-link-impl link)
      (point))))

(defun message-links--convert-links-all-in-region (region-min region-max)
  "Convert all links in REGION-MIN, REGION-MAX range.
Return the number of links converted."
  (let ((bounds nil)
        (count 0)
        (pos-last -1)
        (footnote-regex (message-links--footnote-link-regex)))
    (save-excursion
      (goto-char region-min)
      (while (and (setq bounds
                        (funcall message-links-match-link-forward-fn
                                 region-max))
                  ;; Ensure a misbehaving forward function never enters
                  ;; an eternal loop by scanning backwards.
                  (< pos-last (car bounds))

                  ;; Finally check this link is not itself
                  ;; part of a referenced link.
                  (save-excursion
                    (goto-char (car bounds))
                    (beginning-of-line)
                    (not (looking-at footnote-regex t))))
        ;; Step to the end of the newly added link reference.
        (setq pos-last (message-links--convert-link-from-bounds bounds))
        (goto-char pos-last)
        (setq count (1+ count))))
    count))

(defun message-links--extract-footnote-links ()
  "Extract the footnotes links in the buffer.
Return a list of strings or nil."
  (let ((footnote-links '()))
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward (message-links--footnote-link-regex) nil t)
          (push (match-string-no-properties 0) footnote-links))))
    footnote-links))

(defun message-links--get-max-footnote-link ()
  "Get the maximum index of the footnote links in the buffer.
Return the maximum value if links can be found in the buffer.
Else, return `message-links-index-start' minus 1."
  (let* ((footnote-links (message-links--extract-footnote-links))
         (footnotes-indexes
          (mapcar
           (lambda (x)
             (save-match-data
               (and (string-match "\\([0-9]+\\)" x)
                    (string-to-number (match-string 1 x)))))
           footnote-links)))
    (cond
     (footnotes-indexes
      (apply #'max footnotes-indexes))
     (t
      (1- message-links-index-start)))))

(defun message-links--footnote-link-regex ()
  "Generate the regex used to extract the footnote links.
Match group 1 isolates the number."
  (concat
   "^"
   (regexp-quote (car message-links-sep-footnotes-link))
   "\\([0-9]+\\)"
   (regexp-quote (cdr message-links-sep-footnotes-link))))

(defun message-links--text-link-regex ()
  "Generate the regex used to extract the text link.
Match group 1 isolates the number."
  (concat
   (regexp-quote (car message-links-sep-text-link))
   "\\([0-9]+\\)"
   (regexp-quote (cdr message-links-sep-text-link))))

(defun message-links--gen-footnotes-link (index)
  "Generate the link prefix from INDEX.
To be inserted at the bottom (footnote) of the buffer."
  (concat
   (car message-links-sep-footnotes-link)
   index
   (cdr message-links-sep-footnotes-link)))

(defun message-links--gen-text-link (index)
  "Generate the in-line link text from INDEX.
To be inserted in the body text."
  (concat
   (car message-links-sep-text-link) index (cdr message-links-sep-text-link)))

(defun message-links--gen-link-header-search-regex ()
  "Generate the regex used to search for the header."
  (concat
   "^[[:blank:]]*" (regexp-quote message-links-link-header) "[[:blank:]]*$"))

(defun message-links--renumber-all-impl ()
  "Re-number all links according to their appearance in the document."
  (let ((footnote-regex (message-links--footnote-link-regex))
        ;; The line beginning position of the link-header
        ;; or where the first footnote occurs.
        ;; Use for search limiting to prevent footnote matches
        ;; also being used as link-text.
        (footnote-beg nil)
        ;; A list of (key bol eol num-beg num-end) for each footnote,
        ;; in the order they appear in the buffer.
        (footnote-list (list))
        ;; Map old link numbers to new ones.
        (index-map (make-hash-table :test 'eql))
        (index message-links-index-start)
        ;; A list of (beg end text) replacements, from last to first.
        (edit-list (list))

        ;; Count changes (for reporting).
        (count-edits 0)
        (count-missing-links 0))
    (save-excursion
      ;; Scan footnotes for link destinations.
      (goto-char (point-min))
      (when message-links-link-header
        (when (re-search-forward
               (message-links--gen-link-header-search-regex)
               nil t)
          (setq footnote-beg (line-beginning-position))))
      (while (re-search-forward footnote-regex nil t)
        (let ((key (string-to-number (match-string-no-properties 1))))
          (when (assoc key footnote-list)
            (user-error "Renumber: link %d is defined twice" key))
          (unless footnote-beg
            (setq footnote-beg (line-beginning-position)))
          (push (list
                 key
                 (line-beginning-position)
                 (line-end-position)
                 (match-beginning 1)
                 (match-end 1))
                footnote-list)))
      (setq footnote-list (nreverse footnote-list))
      ;; Set to avoid errors (no footnotes were found).
      (unless footnote-beg
        (setq footnote-beg (point-max)))

      ;; Scan the body text for link-text,
      ;; numbering links in the order they're first referenced.
      (goto-char (point-min))
      (let ((regex (message-links--text-link-regex)))
        (while (re-search-forward regex footnote-beg t)
          (let ((key (string-to-number (match-string-no-properties 1))))
            (cond
             ((assoc key footnote-list)
              (unless (gethash key index-map)
                (puthash key index index-map)
                (setq index (1+ index)))
              (push (list
                     (match-beginning 1)
                     (match-end 1)
                     (number-to-string (gethash key index-map)))
                    edit-list))
             (t
              (setq count-missing-links (1+ count-missing-links)))))))

      ;; Footnotes which are never referenced go last.
      (dolist (footnote footnote-list)
        (unless (gethash (car footnote) index-map)
          (puthash (car footnote) index index-map)
          (setq index (1+ index))))

      ;; Re-order the footnote lines to match the order they're referenced,
      ;; text between footnotes is left in place.
      (let ((line-list
             (sort (mapcar
                    (lambda (footnote)
                      (pcase-let ((`(,key ,bol ,eol ,num-beg ,num-end) footnote))
                        (let ((new-index (gethash key index-map)))
                          (cons
                           new-index
                           (concat
                            (buffer-substring-no-properties bol num-beg)
                            (number-to-string new-index)
                            (buffer-substring-no-properties num-end eol))))))
                    footnote-list)
                   (lambda (a b) (< (car a) (car b))))))
        (dolist (footnote footnote-list)
          (push (list (nth 1 footnote) (nth 2 footnote) (cdr (pop line-list)))
                edit-list)))

      ;; Apply edit-list from last to first
      ;; (so number-width doesn't invalidate bounds).
      (pcase-dolist (`(,beg ,end ,text) edit-list)
        (unless (string-equal text (buffer-substring-no-properties beg end))
          (setq count-edits (1+ count-edits))
          (goto-char beg)
          (delete-region beg end)
          (insert text))))

    ;; Report results.
    (let ((report-edits-made
           (cond
            ((zerop count-edits)
             "not required")
            (t
             (format "%d edit(s)" count-edits))))
          (report-links-found
           (format " (%d link(s) found)" (length footnote-list)))
          (report-links-missing
           (cond
            ((zerop count-missing-links)
             "")
            (t
             (format ", (%d link(s) missing)" count-missing-links)))))
      (message "Renumber: %s"
               (concat
                report-edits-made report-links-found report-links-missing)))))

(defun message-links--convert-links-all-impl (beg end bounds-description)
  "Convert all links between BEG and END, reporting with BOUNDS-DESCRIPTION."
  (let ((count (message-links--convert-links-all-in-region beg end)))
    (cond
     ((zerop count)
      (message "No links found in %s" bounds-description))
     (t
      (message "Added %d link(s) in %s" count bounds-description)))))

;;; Public Functions (Auto-Loaded).

;;;###autoload
(defun message-links-add-link (link)
  "Insert the LINK under the text.
The LINK will be added after the `message-links-link-header' if it is not
already present or added to the link list."
  (interactive "sLink to insert: ")
  (message-links--with-range-limit
    (message-links--add-link-impl link)))

;;;###autoload (defalias 'message-links-add #'message-links-add-link)

;;;###autoload
(defun message-links-convert-link-at-point ()
  "Convert the link at the cursor to a footnote link."
  (interactive)
  (message-links--with-range-limit
    (let ((bounds (message-links--match-link-at-point)))
      (cond
       (bounds
        (goto-char (message-links--convert-link-from-bounds bounds)))
       (t
        (message "No link at point"))))))

;;;###autoload
(defun message-links-convert-links-all ()
  "Convert all links in the buffer (or active-region) to footnotes."
  (interactive)
  (message-links--with-range-limit
    (cond
     ((region-active-p)
      (message-links--convert-links-all-impl
       (region-beginning) (region-end) "region"))
     (t
      (message-links--convert-links-all-impl
       (point-min) (point-max) "buffer")))))

;;;###autoload
(defun message-links-renumber-all ()
  "Re-number all links according to their appearance in the document."
  (interactive)
  (message-links--with-range-limit
    (message-links--renumber-all-impl)))


(provide 'message-links)
;;; message-links.el ends here
