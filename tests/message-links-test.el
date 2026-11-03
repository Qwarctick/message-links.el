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

(add-to-list 'load-path (concat (file-name-directory load-file-name) ".."))
(require 'message-links)

;;; Test Utilities

(defun message-links-test--convert-all-links-from-before-after (before after)
  "Insert BEFORE, convert all links and check the result matches AFTER.
Both are lists of strings."
  (with-temp-buffer
    (apply #'insert before)
    (message-links-convert-links-all)
    (should
     (equal
      (apply #'concat after)
      (buffer-substring-no-properties (point-min) (point-max))))))

(defun message-links-test--renumber-all-links-from-before-after (before after)
  "Insert BEFORE, renumber all links and check the result matches AFTER.
Both are lists of strings."
  (declare (indent 0))
  (with-temp-buffer
    (apply #'insert before)
    (message-links-renumber-all)
    (should
     (equal
      (apply #'concat after)
      (buffer-substring-no-properties (point-min) (point-max))))))

;;; Tests: Convert All Links

(ert-deftest link-nop ()
  "Do Nothing."
  (message-links-test--convert-all-links-from-before-after
   (list "do nothing\n") (list "do nothing\n")))

(ert-deftest link-single ()
  "Convert a single link."
  (message-links-test--convert-all-links-from-before-after
   (list "Link to https://www.gnu.org page.\n")
   ;; format-next-line: off
   (list
    "Link to [1] page.\n"
    "\n"
    "--- links ---\n"
    "[1]: https://www.gnu.org\n")))

(ert-deftest link-single-no-header ()
  "Convert a single link (without a header)."
  (let ((message-links-link-header nil))
    (message-links-test--convert-all-links-from-before-after
     (list "Link to https://www.gnu.org page.\n")
     ;; format-next-line: off
     (list
      "Link to [1] page.\n"
      "\n"
      "[1]: https://www.gnu.org\n"))))

(ert-deftest link-multi ()
  "Converts multiple links at once."
  (message-links-test--convert-all-links-from-before-after
   (list
    "Link to https://www.gnu.org page.\n"
    "\n"
    "Another link to WIKIPEDIA: https://en.wikipedia.org page.\n"
    "Two links on the same https://www.test.org line https://www.site.org/\n")
   (list
    "Link to [1] page.\n"
    "\n"
    "Another link to WIKIPEDIA: [2] page.\n"
    "Two links on the same [3] line [4]\n"
    "\n"
    "--- links ---\n"
    "[1]: https://www.gnu.org\n"
    "[2]: https://en.wikipedia.org\n"
    "[3]: https://www.test.org\n"
    "[4]: https://www.site.org/\n")))

(ert-deftest link-with-indented-header ()
  "Convert with an indented link header (to ensure it's supported)."
  (let ((message-links-link-header nil))
    (message-links-test--convert-all-links-from-before-after
     (list
      "Link to [1] page, another link to https://www.test.org page.\n"
      "\n"
      "    --- links ---\n"
      "[1]: https://www.gnu.org\n")
     (list
      "Link to [1] page, another link to [2] page.\n"
      "\n"
      "    --- links ---\n"
      "[1]: https://www.gnu.org\n"
      "[2]: https://www.test.org\n"))))

(ert-deftest link-with-limit-range ()
  "Convert with a limited range (using commit-message like formatting)."
  (let ((message-links-limit-range-fn
         ;; Skip the subject line and comment at the end.
         ;; Adding links before the comment.
         (lambda ()
           (let ((min (point-min))
                 (max (point-max)))
             (save-excursion
               (goto-char min)
               (forward-line 1)
               (setq min (point))
               (when (re-search-forward (concat
                                         "^"
                                         (regexp-quote
                                          "# Ignore lines after this."))
                                        nil t)
                 (setq max (line-beginning-position))))
             (cons min max)))))
    (message-links-test--convert-all-links-from-before-after
     (list
      "Subject Line https://www.example.org\n"
      "\n"
      "Link to https://www.gnu.org page, another link to https://www.test.org page.\n"
      "\n"
      "# Ignore lines after this.\n"
      "#\n"
      "# Changes to be committed:\n"
      "# modified: some_file.txt\n")
     (list
      "Subject Line https://www.example.org\n"
      "\n"
      "Link to [1] page, another link to [2] page.\n"
      "\n"
      "--- links ---\n"
      "[1]: https://www.gnu.org\n"
      "[2]: https://www.test.org\n"
      "\n"
      "# Ignore lines after this.\n"
      "#\n"
      "# Changes to be committed:\n"
      "# modified: some_file.txt\n"))))

;;; Tests: Renumber All

(ert-deftest renumber-nop-single ()
  "Renumber simple (no work is needed)."
  (message-links-test--renumber-all-links-from-before-after
    (list "Link to [1] page.\n" "\n" "[1]: https://www.gnu.org\n")
    (list "Link to [1] page.\n" "\n" "[1]: https://www.gnu.org\n")))

(ert-deftest renumber-nop-complex ()
  "Renumber complex (no work is needed)."
  (message-links-test--renumber-all-links-from-before-after
    (list
     "Link to [1] page.\n"
     "\n"
     "Another link to WIKIPEDIA: [2] page.\n"
     "Two links on the same [3] line [4]\n"
     "\n"
     "--- links ---\n"
     "[1]: https://www.gnu.org\n"
     "[2]: https://en.wikipedia.org\n"
     "[3]: https://www.test.org\n"
     "[4]: https://www.site.org/\n")
    (list
     "Link to [1] page.\n"
     "\n"
     "Another link to WIKIPEDIA: [2] page.\n"
     "Two links on the same [3] line [4]\n"
     "\n"
     "--- links ---\n"
     "[1]: https://www.gnu.org\n"
     "[2]: https://en.wikipedia.org\n"
     "[3]: https://www.test.org\n"
     "[4]: https://www.site.org/\n")))

(ert-deftest renumber-single ()
  "Renumber a single link."
  (message-links-test--renumber-all-links-from-before-after
    (list "Link to [123] page.\n" "\n" "[123]: https://www.gnu.org\n")
    (list "Link to [1] page.\n" "\n" "[1]: https://www.gnu.org\n")))

(ert-deftest renumber-complex ()
  "Renumber more complex links."
  (message-links-test--renumber-all-links-from-before-after
    (list
     "Link to [65535] page.\n"
     "\n"
     "Another link to WIKIPEDIA: [0] page.\n"
     "Two links on the same [16777216] line [4]\n"
     "\n"
     "--- links ---\n"
     "[65535]: https://www.gnu.org\n"
     "[0]: https://en.wikipedia.org\n"
     "[16777216]: https://www.test.org\n"
     "[4]: https://www.site.org/\n")
    (list
     "Link to [1] page.\n"
     "\n"
     "Another link to WIKIPEDIA: [2] page.\n"
     "Two links on the same [3] line [4]\n"
     "\n"
     "--- links ---\n"
     "[1]: https://www.gnu.org\n"
     "[2]: https://en.wikipedia.org\n"
     "[3]: https://www.test.org\n"
     "[4]: https://www.site.org/\n")))

(ert-deftest renumber-complex-no-header ()
  "Renumber more complex links (without a header)."
  (let ((message-links-link-header nil))
    (message-links-test--renumber-all-links-from-before-after
      (list
       "Link to [65535] page.\n"
       "\n"
       "Another link to WIKIPEDIA: [0] page.\n"
       "Two links on the same [16777216] line [4]\n"
       "\n"
       "[65535]: https://www.gnu.org\n"
       "[0]: https://en.wikipedia.org\n"
       "[16777216]: https://www.test.org\n"
       "[4]: https://www.site.org/\n")
      (list
       "Link to [1] page.\n"
       "\n"
       "Another link to WIKIPEDIA: [2] page.\n"
       "Two links on the same [3] line [4]\n"
       "\n"
       "[1]: https://www.gnu.org\n"
       "[2]: https://en.wikipedia.org\n"
       "[3]: https://www.test.org\n"
       "[4]: https://www.site.org/\n"))))

(ert-deftest renumber-complex-and-sort ()
  "Renumber a complex links that require sorting."
  (message-links-test--renumber-all-links-from-before-after
    (list
     "Link to [65535] page.\n"
     "\n"
     "Another link to WIKIPEDIA: [0] page.\n"
     "Two links on the same [16777216] line [4]\n"
     "\n"
     "--- links ---\n"
     "[4]: https://www.site.org/\n"
     "[16777216]: https://www.test.org\n"
     "[0]: https://en.wikipedia.org\n"
     "[65535]: https://www.gnu.org\n")
    (list
     "Link to [1] page.\n"
     "\n"
     "Another link to WIKIPEDIA: [2] page.\n"
     "Two links on the same [3] line [4]\n"
     "\n"
     "--- links ---\n"
     "[1]: https://www.gnu.org\n"
     "[2]: https://en.wikipedia.org\n"
     "[3]: https://www.test.org\n"
     "[4]: https://www.site.org/\n")))

(ert-deftest renumber-complex-and-sort-fragmented ()
  "Renumber a complex links & sort, with contents between links."
  (message-links-test--renumber-all-links-from-before-after
    (list
     "Link to [65535] page.\n"
     "\n"
     "Another link to WIKIPEDIA: [0] page.\n"
     "Two links on the same [16777216] line [4]\n"
     "\n"
     "--- links ---\n"
     "[4]: https://www.site.org/\n"
     "\n"
     "[16777216]: https://www.test.org\n"
     "Some text\n"
     "[0]: https://en.wikipedia.org\n"
     "Some more text\n"
     "\n"
     "\n"
     "[65535]: https://www.gnu.org\n")
    (list
     "Link to [1] page.\n"
     "\n"
     "Another link to WIKIPEDIA: [2] page.\n"
     "Two links on the same [3] line [4]\n"
     "\n"
     "--- links ---\n"
     "[1]: https://www.gnu.org\n"
     "\n"
     "[2]: https://en.wikipedia.org\n"
     "Some text\n"
     "[3]: https://www.test.org\n"
     "Some more text\n"
     "\n"
     "\n"
     "[4]: https://www.site.org/\n")))

(ert-deftest renumber-duplicate-reference ()
  "Renumber a link referenced more than once."
  (message-links-test--renumber-all-links-from-before-after
    (list
     "See [2] and again [2], then [1].\n"
     "\n"
     "--- links ---\n"
     "[1]: https://a.org\n"
     "[2]: https://b.org\n")
    (list
     "See [1] and again [1], then [2].\n"
     "\n"
     "--- links ---\n"
     "[1]: https://b.org\n"
     "[2]: https://a.org\n")))

(ert-deftest renumber-unreferenced-footnote ()
  "Renumber with a footnote that isn't referenced, it goes last."
  (message-links-test--renumber-all-links-from-before-after
    (list
     "Text [2].\n"
     "\n"
     "--- links ---\n"
     "[1]: https://a.org\n"
     "[2]: https://b.org\n")
    (list
     "Text [1].\n"
     "\n"
     "--- links ---\n"
     "[1]: https://b.org\n"
     "[2]: https://a.org\n")))

(ert-deftest renumber-missing-link ()
  "Renumber skipping bracketed numbers which aren't links."
  (message-links-test--renumber-all-links-from-before-after
    (list
     "Use a[0] then [5].\n"
     "\n"
     "--- links ---\n"
     "[5]: https://a.org\n")
    (list
     "Use a[0] then [1].\n"
     "\n"
     "--- links ---\n"
     "[1]: https://a.org\n")))

(provide 'message-links-test)
;;; message-links-test.el ends here
