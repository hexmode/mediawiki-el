;;; test-mediawiki-font-lock.el --- Unit tests for mediawiki-font-lock.el -*- lexical-binding: t; -*-

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

;; Unit tests for mediawiki-font-lock.el module.

;;; Code:

(require 'ert)
(require 'mediawiki-font-lock)

;;; Test Tag Definitions

(ert-deftest test-mediawiki-simple-tags ()
  "Test that mediawiki-simple-tags is properly defined."
  (should (listp mediawiki-simple-tags))
  (should (> (length mediawiki-simple-tags) 0))

  ;; Test that expected tags are present
  (should (member "b" mediawiki-simple-tags))
  (should (member "i" mediawiki-simple-tags))
  (should (member "code" mediawiki-simple-tags))
  (should (member "nowiki" mediawiki-simple-tags))
  (should (member "math" mediawiki-simple-tags))

  ;; Test that all elements are strings
  (dolist (tag mediawiki-simple-tags)
    (should (stringp tag))))

(ert-deftest test-mediawiki-complex-tags ()
  "Test that mediawiki-complex-tags is properly defined."
  (should (listp mediawiki-complex-tags))
  (should (> (length mediawiki-complex-tags) 0))

  ;; Test that expected tags are present
  (should (member "a" mediawiki-complex-tags))
  (should (member "div" mediawiki-complex-tags))
  (should (member "table" mediawiki-complex-tags))
  (should (member "td" mediawiki-complex-tags))
  (should (member "tr" mediawiki-complex-tags))

  ;; Test that all elements are strings
  (dolist (tag mediawiki-complex-tags)
    (should (stringp tag))))

;;; Test URL Protocol Definitions

(ert-deftest test-mediawiki-url-protocols ()
  "Test that mediawiki-url-protocols is properly defined."
  (should (listp mediawiki-url-protocols))
  (should (> (length mediawiki-url-protocols) 0))

  ;; Test that expected protocols are present
  (should (member "http" mediawiki-url-protocols))
  (should (member "https" mediawiki-url-protocols))
  (should (member "ftp" mediawiki-url-protocols))
  (should (member "mailto" mediawiki-url-protocols))

  ;; Test that all elements are strings
  (dolist (protocol mediawiki-url-protocols)
    (should (stringp protocol))))

;;; Test Font-Lock Keywords

(ert-deftest test-mediawiki-font-lock-keywords ()
  "Test that mediawiki-font-lock-keywords is properly defined."
  (should (listp mediawiki-font-lock-keywords))
  (should (> (length mediawiki-font-lock-keywords) 0))

  ;; Test that each keyword is either a cons cell or a list
  (dolist (keyword mediawiki-font-lock-keywords)
    (should (or (consp keyword) (listp keyword)))))

;;; Test Font-Lock Pattern Matching

(ert-deftest test-mediawiki-font-lock-bold-pattern ()
  "Test that bold pattern matches correctly."
  (let ((test-cases '(("'''bold text'''" . t)
                      ("'''bold'''" . t)
                      ("''not bold''" . nil)
                      ("''''bold and italic''''" . nil) ; This should match the 4-apostrophe pattern instead
                      ("regular text" . nil))))

    (dolist (case test-cases)
      (let ((text (car case))
            (should-match (cdr case)))
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (if should-match
              (should (re-search-forward "'''\\([^']\\|[^']'\\)*?\\('''\\|\n\n\\)" nil t))
            (should-not (re-search-forward "'''\\([^']\\|[^']'\\)*?\\('''\\|\n\n\\)" nil t))))))))

(ert-deftest test-mediawiki-font-lock-italic-pattern ()
  "Test that italic pattern matches correctly."
  (let ((test-cases '(("''italic text''" . t)
                      ("''italic''" . t)
                      ("'''not italic'''" . nil)
                      ("regular text" . nil))))

    (dolist (case test-cases)
      (let ((text (car case))
            (should-match (cdr case)))
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (if should-match
              (should (re-search-forward "''\\([^']\\|[^']'\\)*?\\(''\\|\n\n\\)" nil t))
            (should-not (re-search-forward "''\\([^']\\|[^']'\\)*?\\(''\\|\n\n\\)" nil t))))))))

(ert-deftest test-mediawiki-font-lock-header-pattern ()
  "Test that header pattern matches correctly."
  (let ((test-cases '(("== Header ==" . t)
                      ("=== Subheader ===" . t)
                      ("==== Level 4 ====" . t)
                      ("= Single =" . t)
                      ("== Unbalanced =" . nil)
                      ("regular text" . nil))))

    (dolist (case test-cases)
      (let ((text (car case))
            (should-match (cdr case)))
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (if should-match
              (should (re-search-forward "^\\(==+\\)\\(.*\\)\\(\\1\\)" nil t))
            (should-not (re-search-forward "^\\(==+\\)\\(.*\\)\\(\\1\\)" nil t))))))))

(ert-deftest test-mediawiki-font-lock-link-pattern ()
  "Test that wiki link pattern matches correctly."
  (let ((test-cases '(("[[Main Page]]" . t)
                      ("[[User:Example|Example]]" . t)
                      ("[[Category:Test]]" . t)
                      ("[not a wiki link]" . nil)
                      ("regular text" . nil))))

    (dolist (case test-cases)
      (let ((text (car case))
            (should-match (cdr case)))
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (if should-match
              (should (re-search-forward "\\(\\[\\[\\)\\([^]\n|]*\\)\\(|?\\)\\([^]\n]*\\)\\(\\]\\]\\)" nil t))
            (should-not (re-search-forward "\\(\\[\\[\\)\\([^]\n|]*\\)\\(|?\\)\\([^]\n]*\\)\\(\\]\\]\\)" nil t))))))))

;;; Test Font-Lock Integration

(ert-deftest test-mediawiki-font-lock-keywords-structure ()
  "Test the structure of font-lock keywords."
  (dolist (keyword mediawiki-font-lock-keywords)
    (cond
     ;; Simple cons cell (REGEXP . FACE)
     ((and (consp keyword) (not (listp (cdr keyword))))
      (should (stringp (car keyword)))
      (should (symbolp (cdr keyword))))

     ;; List form (REGEXP FACE) or (REGEXP (MATCH FACE) ...)
     ((listp keyword)
      (should (stringp (car keyword)))
      (should (or (symbolp (cadr keyword))
                  (listp (cadr keyword))))))))

;;; Test Regular Expression Validity

(ert-deftest test-mediawiki-font-lock-regexp-validity ()
  "Test that all regular expressions in font-lock keywords are valid."
  (dolist (keyword mediawiki-font-lock-keywords)
    (let ((regexp (if (listp keyword) (car keyword) (car keyword))))
      (should (stringp regexp))
      ;; Test that regexp doesn't cause errors
      (should-not (condition-case nil
                      (string-match regexp "test")
                    (error t))))))

(provide 'test-mediawiki-font-lock)

;;; test-mediawiki-font-lock.el ends here
