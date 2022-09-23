;;; message-links.el -*- lexical-binding: t; -*-

(defgroup message-links nil
  "Manage reference links into text"
  :group 'message)

(defcustom message-links-link-header
  "\n\n---links---\n"
  "Header used to separate links from the original text"
  :type 'string
  :group 'message-links)

(defcustom message-links-index-start
  1
  "Index of the first link inserted"
  :type 'integer
  :group 'message-links)

(defcustom message-links-enable-link-header
  t
  "Use the link header to separate original text from links"
  :type 'boolean
  :group 'message-links)

(defcustom message-links-separator
  " : "
  "The text to place between the number and the link"
  :type 'string
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

(defun message-links-add-link (link)
  "Insert the LINK under the text.
The LINK will be added after the `message-links-link-header' if it is not
already present or added to the link list."
  (interactive "sLink to insert: ")
  (save-excursion
    (let ((short-link-index (number-to-string (1+ (message--links-get-max-short-link)))))
      (insert (concat "[" short-link-index "]"))
      (if message-links-enable-link-header
          (progn ; Insert link after the link header
            (if (not (search-forward message-links-link-header nil t))
                (progn ;; No message-links-link-header present in the message
                  (goto-char (point-max))
                  (insert message-links-link-header)
                  (insert (concat "[" short-link-index "]"
                                  message-links-separator link)))
              (progn ;; Message found in the compose message
                (goto-char (point-max))
                (insert (concat "\n[" short-link-index "]"
                                message-links-separator link)))))
        (progn  ; Insert links without the link header
          (goto-char (point-max))
          (insert (concat "\n[" short-link-index "]"
                          message-links-separator link)))))))

(defun message--links-get-max-short-link ()
  "Get the maximum index of the links in the buffer.
Return the maximum value if links can be found in the buffer.
Else, return `message-links-index-start' minus 1.

Be careful, due to the regex used to find links, if a line of the
original text starts with '[0-9]*', this will be considered as a link"
  (let ((short-links '()))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              (concat "^\\[\\([0-9]*\\)]"
                      (regexp-quote message-links-separator))
              nil t)
        (push (string-to-number (match-string-no-properties 1)) short-links)))
    (if short-links
      (apply #'max short-links)
      (1- message-links-index-start))))

(defalias 'message-links-add 'message-links-add-link)

;;;###autoload
(define-minor-mode message-links-mode
  "Toggle message-links-mode

Call `message-links-add' to add a link into you message buffer.
"
  :lighter " message-links")
;;;###autoload

(provide 'message-links-mode)
