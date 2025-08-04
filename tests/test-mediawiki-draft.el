;;; test-mediawiki-draft.el --- Unit tests for mediawiki-draft.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el contributors

;; This file is part of mediawiki.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for mediawiki-draft.el module.

;;; Code:

(require 'ert)

;;; Test Interactive Draft Functions

(ert-deftest test-mediawiki-aaa-draft-view-draft ()
  "Test mediawiki-draft-view-draft function."
  (should (functionp 'mediawiki-draft-view-draft))

  (should (commandp 'mediawiki-draft-view-draft)))

(ert-deftest test-mediawiki-aaa-draft ()
  "Test mediawiki-draft function."
  (should (functionp 'mediawiki-draft))

  (should (commandp 'mediawiki-draft)))

(ert-deftest test-mediawiki-aaa-draft-page ()
  "Test mediawiki-draft-page function."
  (should (functionp 'mediawiki-draft-page))

  (should (commandp 'mediawiki-draft-page)))

(ert-deftest test-mediawiki-aaa-draft-region ()
  "Test mediawiki-draft-region function."
  (should (functionp 'mediawiki-draft-region))

  (should (commandp 'mediawiki-draft-region)))

(ert-deftest test-mediawiki-aaa-draft-buffer ()
  "Test mediawiki-draft-buffer function."
  (should (functionp 'mediawiki-draft-buffer))

  (should (commandp 'mediawiki-draft-buffer)))

(ert-deftest test-mediawiki-aaa-draft-clipboard ()
  "Test mediawiki-draft-clipboard function."
  (should (functionp 'mediawiki-draft-clipboard))

  (should (commandp 'mediawiki-draft-clipboard)))

;;; Test Register Operations

(ert-deftest test-mediawiki-aaa-draft-register-functions ()
  "Test draft register operation functions."
  (should (functionp 'mediawiki-draft-copy-page-to-register))

  (should (commandp 'mediawiki-draft-copy-page-to-register))

  (should (functionp 'mediawiki-draft-yank-page-to-register))

  (should (commandp 'mediawiki-draft-yank-page-to-register))

  (should (functionp 'mediawiki-draft-send))

  (should (commandp 'mediawiki-draft-send)))

;;; Test Reply Functionality

(ert-deftest test-mediawiki-aaa-draft-reply ()
  "Test mediawiki-draft-reply function."
  (should (functionp 'mediawiki-draft-reply))

  (should (commandp 'mediawiki-draft-reply)))

;;; Test Customization Variables

(ert-deftest test-mediawiki-draft-customization ()
  "Test that draft customization variables are defined."
  (should (boundp 'mediawiki-draft-mode-hook))
  (should (listp mediawiki-draft-mode-hook))

  (should (boundp 'mediawiki-draft-register))
  (should (characterp mediawiki-draft-register))

  (should (boundp 'mediawiki-draft-filter-functions))
  (should (listp mediawiki-draft-filter-functions))

  (should (boundp 'mediawiki-draft-handler-functions))
  (should (listp mediawiki-draft-handler-functions))

  (should (boundp 'mediawiki-draft-data-file))
  (should (stringp mediawiki-draft-data-file))

  (should (boundp 'mediawiki-draft-leader-text))
  (should (stringp mediawiki-draft-leader-text)))

;;; Test Draft Variables

(ert-deftest test-mediawiki-draft-variables ()
  "Test that draft variables are properly defined."
  (should (boundp 'mediawiki-draft-buffer))
  (should (stringp mediawiki-draft-buffer))

  (should (boundp 'mediawiki-draft-send-archive))
  (should (boundp 'mediawiki-draft-mode-map)))

;;; Test Utility Functions

(ert-deftest test-mediawiki-draft-time-to-seconds ()
  "Test mediawiki-draft-time-to-seconds function."
  ;; Test with mock time value
  (let ((test-time '(12345 67890 123456)))
    (let ((result (mediawiki-draft-time-to-seconds test-time)))
      (should (numberp result))
      (should (> result 0)))))

(ert-deftest test-mediawiki-draft-mail-date ()
  "Test mediawiki-draft-mail-date function."
  ;; Test default format
  (let ((date (mediawiki-draft-mail-date)))
    (should (stringp date))
    (should (> (length date) 0)))

  ;; Test RFC822 format
  (let ((rfc-date (mediawiki-draft-mail-date t)))
    (should (stringp rfc-date))
    (should (> (length rfc-date) 0))
    ;; RFC822 format should contain timezone info
    (should (string-match-p "[+-][0-9]\\{4\\}" rfc-date))))

(ert-deftest test-mediawiki-draft-buffer-desc ()
  "Test mediawiki-draft-buffer-desc function."
  (with-temp-buffer
    ;; Test with short content
    (insert "Short line")
    (let ((desc (mediawiki-draft-buffer-desc)))
      (should (string= "Short line" desc)))

    ;; Test with long content (should be truncated)
    (erase-buffer)
    (insert (make-string 100 ?x))
    (let ((desc (mediawiki-draft-buffer-desc)))
      (should (stringp desc))
      (should (<= (length desc) 60)))))

;;; Test Draft File Management

(ert-deftest test-mediawiki-draft-append-to-file-structure ()
  "Test mediawiki-draft-append-to-file function structure."
  (should (functionp 'mediawiki-draft-append-to-file))

  ;; We can't test actual file operations without side effects,
  ;; but we can test the function exists and has the right structure
  (with-temp-buffer
    (insert "Test draft content")

    ;; Mock read-string and file operations
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt) "Test Subject"))
              ((symbol-function 'append-to-file)
               (lambda (start end filename) nil))
              ((symbol-function 'find-buffer-visiting)
               (lambda (filename) nil)))

      ;; Should not error
      (should-not (condition-case nil
                      (mediawiki-draft-append-to-file)
                    (error t))))))

;;; Test Interactive Draft Functions

(ert-deftest test-mediawiki-draft-view-draft ()
  "Test mediawiki-draft-view-draft function."
  (should (functionp 'mediawiki-draft-view-draft))
  (should (get 'mediawiki-draft-view-draft 'autoload))
  (should (commandp 'mediawiki-draft-view-draft)))

(ert-deftest test-mediawiki-draft ()
  "Test mediawiki-draft function."
  (should (functionp 'mediawiki-draft))
  (should (get 'mediawiki-draft 'autoload))
  (should (commandp 'mediawiki-draft)))

(ert-deftest test-mediawiki-draft-page ()
  "Test mediawiki-draft-page function."
  (should (functionp 'mediawiki-draft-page))
  (should (get 'mediawiki-draft-page 'autoload))
  (should (commandp 'mediawiki-draft-page)))

(ert-deftest test-mediawiki-draft-region ()
  "Test mediawiki-draft-region function."
  (should (functionp 'mediawiki-draft-region))
  (should (get 'mediawiki-draft-region 'autoload))
  (should (commandp 'mediawiki-draft-region)))

(ert-deftest test-mediawiki-draft-buffer ()
  "Test mediawiki-draft-buffer function."
  (should (functionp 'mediawiki-draft-buffer))
  (should (get 'mediawiki-draft-buffer 'autoload))
  (should (commandp 'mediawiki-draft-buffer)))

(ert-deftest test-mediawiki-draft-clipboard ()
  "Test mediawiki-draft-clipboard function."
  (should (functionp 'mediawiki-draft-clipboard))
  (should (get 'mediawiki-draft-clipboard 'autoload))
  (should (commandp 'mediawiki-draft-clipboard)))

;;; Test Register Operations

(ert-deftest test-mediawiki-draft-register-functions ()
  "Test draft register operation functions."
  (should (functionp 'mediawiki-draft-copy-page-to-register))
  (should (get 'mediawiki-draft-copy-page-to-register 'autoload))
  (should (commandp 'mediawiki-draft-copy-page-to-register))

  (should (functionp 'mediawiki-draft-yank-page-to-register))
  (should (get 'mediawiki-draft-yank-page-to-register 'autoload))
  (should (commandp 'mediawiki-draft-yank-page-to-register))

  (should (functionp 'mediawiki-draft-send))
  (should (get 'mediawiki-draft-send 'autoload))
  (should (commandp 'mediawiki-draft-send)))

;;; Test Reply Functionality

(ert-deftest test-mediawiki-draft-reply ()
  "Test mediawiki-draft-reply function."
  (should (functionp 'mediawiki-draft-reply))
  (should (get 'mediawiki-draft-reply 'autoload))
  (should (commandp 'mediawiki-draft-reply)))

;;; Test Draft Mode

(ert-deftest test-mediawiki-draft-mode ()
  "Test mediawiki-draft-mode definition."
  (should (functionp 'mediawiki-draft-mode))

  ;; Test that it's a derived mode
  (should (get 'mediawiki-draft-mode 'derived-mode-parent))

  ;; Test mode in a buffer
  (with-temp-buffer
    (mediawiki-draft-mode)
    (should (eq major-mode 'mediawiki-draft-mode))
    (should (string-match-p "MW-Draft" mode-name))))

;;; Test Draft Mode Keymap

(ert-deftest test-mediawiki-draft-mode-keymap ()
  "Test mediawiki-draft-mode keymap."
  (should (keymapp mediawiki-draft-mode-map))

  ;; Test that key bindings are set
  (with-temp-buffer
    (mediawiki-draft-mode)
    ;; The keymap should be active
    (should (current-local-map))))

;;; Test Draft Handler Functions

(ert-deftest test-mediawiki-draft-handler-functions ()
  "Test that draft handler functions are properly configured."
  ;; Test that default handler is included
  (should (member 'mediawiki-draft-append-to-file
                  mediawiki-draft-handler-functions))

  ;; Test that it's a list of functions
  (dolist (handler mediawiki-draft-handler-functions)
    (should (functionp handler))))

;;; Test Draft Region Processing

(ert-deftest test-mediawiki-draft-region-structure ()
  "Test mediawiki-draft-region function structure."
  (with-temp-buffer
    (insert "Test content for drafting")
    (mark-whole-buffer)

    ;; Mock the handler functions to avoid side effects
    (let ((mediawiki-draft-handler-functions
           (list (lambda () t)))) ; Mock handler that returns t

      ;; Should not error
      (should-not (condition-case nil
                      (mediawiki-draft-region (point-min) (point-max))
                    (error t))))))

;;; Test Customization Groups

(ert-deftest test-mediawiki-draft-customization-group ()
  "Test that mediawiki-draft customization group is defined."
  (should (get 'mediawiki-draft 'group-documentation)))

(provide 'test-mediawiki-draft)

;;; test-mediawiki-draft.el ends here
