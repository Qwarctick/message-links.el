;;; message-links-test.el --- Link utility test -*- lexical-binding: t -*-

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

;;
;; To test this file run:
;;
;; emacs -batch -l tests/message-links-test.el -f ert-run-tests-batch-and-exit
;;

;;; Code:

(require 'ert)

;;; Setup Environment

(setq message-links-basedir (concat (file-name-directory load-file-name) ".."))
(add-to-list 'load-path message-links-basedir)
(require 'message-links)

;;; Test Utilities

(defun message-links-test--do-convert-all-links ()
  "Run a test on the current buffer using CHAR-ODD & CHAR-EVEN."
  (message-links-convert-links-all)
  (buffer-substring-no-properties (point-min) (point-max)))

(defmacro message-links-test--convert-all-links-from-before-after (before after)
  `(let ((buf (generate-new-buffer "message-links-test.txt")))
     (let ((before-var ,before)
           (after-var ,after))
       (with-current-buffer buf
         (apply 'insert before-var)
         (let ((code-str-expect (apply 'concat after-var))
               (code-str-result (message-links-test--do-convert-all-links)))
           (should (equal code-str-expect code-str-result)))))))

;;; Tests

(ert-deftest link-nop ()
  "Do Nothing."
  (message-links-test--convert-all-links-from-before-after
   (list "do nothing\n")
   (list "do nothing\n")))

(ert-deftest link-single ()
  "Convert a single link."
  (message-links-test--convert-all-links-from-before-after
   (list "Link to https://www.gnu.org page.\n")
   (list
    "Link to [1] page.\n"
    "\n"
    "---links---\n"
    "[1] : https://www.gnu.org\n")))

(provide 'message-links-test)
;;; message-links-test.el ends here
