;;; test-mediawiki-simple.el --- Simple functional tests for mediawiki.el -*- lexical-binding: t; -*-

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

;; Simple functional tests for mediawiki.el that focus on core functionality
;; without testing implementation details like autoload declarations.

;;; Code:

(require 'ert)
(require 'mediawiki)

;;; Core Functionality Tests

(ert-deftest test-mediawiki-simple-core-functions ()
  "Test that core functions exist and work."
  ;; Test core utility functions
  (should (functionp 'mediawiki-translate-pagename))
  (should (string= "Main Page" (mediawiki-translate-pagename "Main_Page")))

  ;; Test site functions
  (should (functionp 'mediawiki-site-url))
  (should (stringp (mediawiki-site-url "Wikipedia")))

  ;; Test that all modules are loaded
  (should (featurep 'mediawiki-core))
  (should (featurep 'mediawiki-utils))
  (should (featurep 'mediawiki-faces))
  (should (featurep 'mediawiki-font-lock))
  (should (featurep 'mediawiki-http))
  (should (featurep 'mediawiki-api))
  (should (featurep 'mediawiki-auth))
  (should (featurep 'mediawiki-site))
  (should (featurep 'mediawiki-page))
  (should (featurep 'mediawiki-draft))
  (should (featurep 'mediawiki-mode)))

(ert-deftest test-mediawiki-simple-mode ()
  "Test that mediawiki-mode works."
  (with-temp-buffer
    (mediawiki-mode)
    (should (eq major-mode 'mediawiki-mode))
    (should (string= mode-name "MW"))

    ;; Test basic text insertion
    (mediawiki-insert-bold)
    (should (string-match-p "'''" (buffer-string)))

    (erase-buffer)
    (mediawiki-insert-italics)
    (should (string-match-p "''" (buffer-string)))

    (erase-buffer)
    (mediawiki-insert-link)
    (should (string-match-p "\\[\\[.*\\]\\]" (buffer-string)))))

(ert-deftest test-mediawiki-simple-font-lock ()
  "Test that font-lock keywords are defined."
  (should (boundp 'mediawiki-font-lock-keywords))
  (should (listp mediawiki-font-lock-keywords))
  (should (> (length mediawiki-font-lock-keywords) 0)))

(ert-deftest test-mediawiki-simple-faces ()
  "Test that faces are defined."
  (should (facep 'font-mediawiki-bold-face))
  (should (facep 'font-mediawiki-italic-face))
  (should (facep 'font-mediawiki-math-face)))

(ert-deftest test-mediawiki-simple-api ()
  "Test API parameter formatting."
  (should (functionp 'mediawiki-api-param))
  (should (string= "123" (mediawiki-api-param 123)))
  (should (string= "test" (mediawiki-api-param "test")))
  (should (string= "a|b|c" (mediawiki-api-param '("a" "b" "c")))))

(ert-deftest test-mediawiki-simple-site-management ()
  "Test site management functions."
  (should (functionp 'mediawiki-site-list))
  (should (listp (mediawiki-site-list)))
  (should (member "Wikipedia" (mediawiki-site-list)))

  (should (functionp 'mediawiki-site-valid-p))
  (should (mediawiki-site-valid-p "Wikipedia"))
  (should-not (mediawiki-site-valid-p "NonExistentSite")))

(ert-deftest test-mediawiki-simple-debug ()
  "Test debug functionality."
  (should (boundp 'mediawiki-debug))
  (should (functionp 'mediawiki-debug-line))

  ;; Test debug line with debugging enabled
  (let ((mediawiki-debug t))
    (when (get-buffer mediawiki-debug-buffer)
      (kill-buffer mediawiki-debug-buffer))
    (mediawiki-debug-line "Test message")
    (should (get-buffer mediawiki-debug-buffer))
    (kill-buffer mediawiki-debug-buffer)))

(ert-deftest test-mediawiki-simple-backward-compatibility ()
  "Test that key functions from the original monolithic file are available."
  ;; Core functions
  (should (functionp 'mediawiki-translate-pagename))
  (should (functionp 'mediawiki-make-url))

  ;; Site functions
  (should (functionp 'mediawiki-site))
  (should (functionp 'mediawiki-site-url))

  ;; Authentication functions
  (should (functionp 'mediawiki-do-login))
  (should (functionp 'mediawiki-do-logout))
  (should (functionp 'mediawiki-logged-in-p))

  ;; Mode functions
  (should (functionp 'mediawiki-mode))
  (should (functionp 'mediawiki-insert-bold))
  (should (functionp 'mediawiki-insert-italics))

  ;; Draft functions
  (should (functionp 'mediawiki-draft))
  (should (functionp 'mediawiki-draft-buffer)))

(provide 'test-mediawiki-simple)

;;; test-mediawiki-simple.el ends here
