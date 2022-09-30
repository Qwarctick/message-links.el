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

;;; Usage

;; TODO.

;;; Code:


(defgroup message-links nil
  "Manage reference links into text."
  :group 'message)

(defcustom message-links-link-header
  "\n\n---links---\n"
  "Header used to separate links from the original text.
If set to nil, no header will be used."
  :type 'string
  :group 'message-links)

(defcustom message-links-index-start
  1
  "Index of the first link inserted."
  :type 'integer
  :group 'message-links)

(defcustom message-links-enable-link-header
  t
  "OBSOLETE: use `message-links-link-header' instead.

Use the link header to separate original text from links."
  :type 'boolean
  :group 'message-links)

(defcustom message-links-sep-footnotes-link
  '("[" . "] : ")
  "The text to use for links in the footnotes
If the default is used, links in footnotes looks like '[1] : '"
  :type 'alist
  :group 'message-links)

(defcustom message-links-sep-text-link
  '("[" . "]")
  "The text to use for number link in the text.
If the default is used, links in text looks like '[1]'"
  :type 'alist
  :group 'message-links)

(defcustom message-links-match-link-at-point-fn
  'message-links-match-link-at-point-default
  "Function that matches a link at the point."
  :type 'function
  :group 'message-links)

(defcustom message-links-match-link-forward-fn
  'message-links-match-link-forward-default
  "Function that finds the next link, taking a single LIMIT argument."
  :type 'function
  :group 'message-links)

;;; Implementations of link matching functions.

(require 'thingatpt)

(defun message-links-match-link-at-point-default ()
  "Return the bounds of the link at point."
  (bounds-of-thing-at-point 'url))

(defun message-links-match-link-forward-default (limit)
  "Return the bounds of the link before LIMIT, scanning forwards as needed."
  (let ((bounds nil))
    (save-excursion
      (while (and (null bounds) (< (point) limit))
        (unless (setq bounds (funcall message-links-match-link-at-point-fn))
          ;; No match, skip over non-blank, then blank space to the next
          ;; possible candidate for a URL.
          (skip-chars-forward "^[:blank:]\n" limit)
          (skip-chars-forward "[:blank:]\n" limit))))
    bounds))

(defun message-links--add-link-impl (link)
  "Add LINK, returning the point where the link reference ends."
  (save-excursion
    (let ((short-link-index (number-to-string
                             (1+ (message--links-get-max-footnote-link))))
          (pos-result nil))
      (insert (message-links--gen-text-link short-link-index))
      (setq pos-result (point))

      (cond
       ;; Insert link after the link header if used.
       (message-links-link-header
        (cond
         ;; No message-links-link-header present in the message.
         ((not (search-forward message-links-link-header nil t))
          (goto-char (point-max))
          (insert message-links-link-header)
          (insert (message-links--gen-footnotes-link short-link-index) link))
         ;; Message found in the compose message.
         (t
          (goto-char (point-max))
          (insert (concat "\n"
                          (message-links--gen-footnotes-link short-link-index)
                          link)))))
       ;; Insert links without the link header.
       (t
        (goto-char (point-max))
        (insert (concat "\n"
                        (message-links--gen-footnotes-link short-link-index)
                        link))))
      pos-result)))

(defun message-links-add-link (link)
  "Insert the LINK under the text.
The LINK will be added after the `message-links-link-header' if it is not
already present or added to the link list."
  (interactive "sLink to insert: ")
  (message-links--add-link-impl link))

(defun message-links--convert-link-from-bounds (bounds)
  "Add BOUNDS as a link."
  (let ((link (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (delete-region (car bounds) (cdr bounds))
    (save-excursion
      (goto-char (car bounds))
      (message-links--add-link-impl link))))

(defun message-links-convert-link-at-point ()
  "Convert the link at the cursor to a footnote link."
  (interactive)
  (let ((bounds (funcall message-links-match-link-at-point-fn)))
    (cond
     (bounds
      (message-links--convert-link-from-bounds bounds))
     (t
      (message "No link at point")))))

(defun message-links-convert-links-all ()
  "Convert all links in the buffer (or active-region) to footnotes."
  (interactive)
  (let ((region-min (point-min))
        (region-max (point-max))
        (has-region nil)
        (bounds nil)
        (count 0)
        (pos-last -1)
        (footnote-regex (message-links--footnote-link-regex)))

    ;; Isolate to a region when found.
    (when (region-active-p)
      (setq region-min (region-beginning) )
      (setq region-max (region-end))
      (setq has-region t))

    (save-excursion
      (goto-char region-min)
      (while (and (setq bounds (funcall message-links-match-link-forward-fn region-max))
                  ;; Ensure a misbehaving forward function never enters
                  ;; an eternal loop by scanning backwards.
                  (< pos-last  (car bounds))

                  ;; Finally check this link is not it's self
                  ;; part of a referenced link.
                  (save-excursion
                    (goto-char (car bounds))
                    (beginning-of-line)
                    (not (looking-at footnote-regex t))))
        ;; Step to the end of the newly added link reference.
        (setq pos-last (message-links--convert-link-from-bounds bounds))
        (goto-char pos-last)
        (setq count (1+ count))))

    (cond
     ((zerop count)
      (message "No links found in %s"
               (if has-region "region" "buffer")))
     (t
      (message "Added %d link(s) in %s"
               count
               (if has-region "region" "buffer"))))))

(defun message-links--extract-footnote-links ()
  "Extract the footnotes links in the buffer.
Return a list of string or nil"
  (let ((footnote-links '()))
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward (message-links--footnote-link-regex) nil t)
          (push (match-string-no-properties 0) footnote-links))))
    footnote-links))

(defun message--links-get-max-footnote-link ()
  "Get the maximum index of the footnote links in the buffer.
Return the maximum value if links can be found in the buffer.
Else, return `message-links-index-start' minus 1."
  (let* ((footnote-links (message-links--extract-footnote-links))
         (footnotes-indexes
          (mapcar (lambda (x)
                    (save-match-data
                      (and (string-match "\\([0-9]+\\)" x)
                           (string-to-number (match-string 1 x)))))
                  footnote-links)))
    (if footnotes-indexes
        (apply #'max footnotes-indexes)
      (1- message-links-index-start))))

(defun message-links--footnote-link-regex ()
  "Generate the regex used to extract the footnote links."
  (concat
   "^"
   (regexp-quote (car message-links-sep-footnotes-link))
   "[0-9]+"
   (regexp-quote (cdr message-links-sep-footnotes-link))))

(defun message-links--gen-footnotes-link (index)
  "Generate the link to insert at the bottom (footnote) of the buffer"
  (concat
   (car message-links-sep-footnotes-link)
   index
   (cdr message-links-sep-footnotes-link)))

(defun message-links--gen-text-link (index)
  "Generate the link to insert in the text."
  (concat
   (car message-links-sep-text-link)
   index
   (cdr message-links-sep-text-link)))

(defalias 'message-links-add 'message-links-add-link)

;;; message-links.el ends here
