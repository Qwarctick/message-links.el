;;; message-links.el -*- lexical-binding: t; -*-

(defgroup message-links nil
  "Manage reference links into text"
  :group 'message)

(defcustom message-links-link-header
  "\n\n---links---\n"
  "Header used to separate links from the original text"
  :type 'string
  :group 'message-links)


(defun message-links-add-link (link)
  "Insert the link into the under the correct part of the message defined by `message-links-link-header'"
  (interactive "sLink to insert: ")
  (save-excursion
    (let ((short-link-index (number-to-string (1+ (message-links-get-max-short-link)))))
      (insert (concat "[" short-link-index "]"))
      (if (not (search-forward message-links-link-header nil t))
          (progn ;; No message-links-link-header present in the message
            (goto-char (point-max))
            (insert message-links-link-header)
            (insert (concat "[" short-link-index "] : " link)))
        (progn ;; Message found in the compose message
          (goto-char (point-max))
          (insert (concat "\n[" short-link-index "] : " link)))))))

(defun message-links-get-max-short-link ()
  "Get the maximum index after `message-links-link-header'.

Return the maximum value. Return 0 otherwise."
  (let ((short-links '()))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward message-links-link-header nil t)  ; Move point at the end of the header link
          (progn
            (while (search-forward-regexp "[\\([0-9]*\\)]" nil t)
              (push (string-to-number (match-string-no-properties 1)) short-links))
            (message "%s" short-links)
            (apply #'max short-links))
        0))))

(defalias 'message-links-add 'message-links-add-link)

;;;autoload
(define-minor-mode message-links-mode
  "Toggle message-links-mode

Call `message-links-add-link' to add a link into you message buffer.
"
  :lighter " message-links")
;;;###autoload

(provide 'message-links-mode)
