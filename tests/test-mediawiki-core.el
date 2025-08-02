;;; test-mediawiki-core.el --- Unit tests for mediawiki-core.el -*- lexical-binding: t; -*-

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

;; Unit tests for mediawiki-core.el module.

;;; Code:

(require 'ert)
(require 'mediawiki-core)

;;; Test Constants and Variables

(ert-deftest test-mediawiki-version ()
  "Test that mediawiki-version is defined and is a string."
  (should (stringp mediawiki-version))
  (should (string-match-p "^[0-9]+\\.[0-9]+\\.[0-9]+$" mediawiki-version)))

(ert-deftest test-mediawiki-core-variables ()
  "Test that core variables are properly defined."
  (should (stringp mediawiki-permission-denied))
  (should (stringp mediawiki-view-source))
  (should (stringp mediawiki-site-default))
  (should (stringp mediawiki-argument-pattern))
  (should (stringp mediawiki-URI-pattern)))

(ert-deftest test-mediawiki-customization-groups ()
  "Test that customization groups are defined."
  (should (get 'mediawiki 'group-documentation)))

;;; Test Utility Functions

(ert-deftest test-mediawiki-translate-pagename ()
  "Test mediawiki-translate-pagename function."
  ;; Test basic underscore replacement
  (should (string= "Main Page" (mediawiki-translate-pagename "Main_Page")))
  (should (string= "User talk:Example" (mediawiki-translate-pagename "User_talk:Example")))

  ;; Test with multiple underscores
  (should (string= "This is a test page" (mediawiki-translate-pagename "This_is_a_test_page")))

  ;; Test with no underscores
  (should (string= "MainPage" (mediawiki-translate-pagename "MainPage")))

  ;; Test with nil input
  (should (null (mediawiki-translate-pagename nil)))

  ;; Test with empty string
  (should (string= "" (mediawiki-translate-pagename ""))))

(ert-deftest test-mediawiki-make-url ()
  "Test mediawiki-make-url function."
  ;; Mock mediawiki-site-url function
  (cl-letf (((symbol-function 'mediawiki-site-url)
             (lambda (site) "https://en.wikipedia.org/w/")))

    ;; Test basic URL construction with action
    ;; Note: mm-url-form-encode-xwfu uses + for spaces, not %20
    (should (string-match-p
             "https://en.wikipedia.org/w/\\?title=Main\\+Page&action=edit"
             (mediawiki-make-url "Main Page" "edit")))

    ;; Test URL construction without action
    (should (string-match-p
             "https://en.wikipedia.org/w/\\?title=Main\\+Page$"
             (mediawiki-make-url "Main Page" nil)))

    ;; Test with underscore in title (should be converted to space then encoded)
    (should (string-match-p
             "https://en.wikipedia.org/w/\\?title=Main\\+Page&action=view"
             (mediawiki-make-url "Main_Page" "view")))

    ;; Test with special characters that need encoding
    (should (string-match-p
             "https://en.wikipedia.org/w/\\?title=Test%26Example&action=edit"
             (mediawiki-make-url "Test&Example" "edit")))))

;;; Test Pattern Matching

(ert-deftest test-mediawiki-uri-pattern ()
  "Test that mediawiki-URI-pattern matches expected URLs."
  (let ((pattern mediawiki-URI-pattern))
    ;; Test basic HTTP URL
    (should (string-match pattern "http://example.com/"))

    ;; Test HTTPS URL
    (should (string-match pattern "https://example.com/"))

    ;; Test URL with port
    (should (string-match pattern "https://example.com:8080/"))

    ;; Test URL with subdomain
    (should (string-match pattern "https://en.wikipedia.org/"))

    ;; Test that it doesn't match invalid URLs
    (should-not (string-match pattern "ftp://example.com/"))
    (should-not (string-match pattern "example.com"))))

;;; Test Variable Initialization

(ert-deftest test-mediawiki-global-variables-initialization ()
  "Test that global variables are properly initialized."
  (should (null mediawiki-page-uri))
  (should (null mediawiki-site))
  (should (listp mediawiki-page-history))
  (should (null mediawiki-site-info)))

;;; Test Argument Pattern

(ert-deftest test-mediawiki-argument-pattern ()
  "Test that argument pattern works correctly."
  (let ((result (format mediawiki-argument-pattern "Main Page" "edit")))
    (should (string= "?title=Main Page&action=edit" result))))

(provide 'test-mediawiki-core)

;;; test-mediawiki-core.el ends here
