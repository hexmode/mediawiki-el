;;; test-mediawiki-utils.el --- Unit tests for mediawiki-utils.el -*- lexical-binding: t; -*-

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

;; Unit tests for mediawiki-utils.el module.

;;; Code:

(require 'ert)
(require 'mediawiki-utils)

;;; Test Debug Variables

(ert-deftest test-mediawiki-debug-variables ()
  "Test that debug variables are properly defined."
  (should (boundp 'mediawiki-debug))
  (should (boundp 'mediawiki-debug-buffer))
  (should (stringp mediawiki-debug-buffer)))

;;; Test Debug Functions

(ert-deftest test-mediawiki-debug-line ()
  "Test mediawiki-debug-line function."
  (let ((mediawiki-debug t)
        (test-line "Test debug message"))

    ;; Clear debug buffer if it exists
    (when (get-buffer mediawiki-debug-buffer)
      (kill-buffer mediawiki-debug-buffer))

    ;; Test debug line insertion
    (mediawiki-debug-line test-line)

    ;; Check that debug buffer was created and contains the message
    (should (get-buffer mediawiki-debug-buffer))
    (with-current-buffer mediawiki-debug-buffer
      (should (string-match-p test-line (buffer-string))))

    ;; Clean up
    (kill-buffer mediawiki-debug-buffer)))

(ert-deftest test-mediawiki-debug-line-disabled ()
  "Test mediawiki-debug-line when debugging is disabled."
  (let ((mediawiki-debug nil))

    ;; Clear debug buffer if it exists
    (when (get-buffer mediawiki-debug-buffer)
      (kill-buffer mediawiki-debug-buffer))

    ;; Test that no debug buffer is created when debugging is off
    (mediawiki-debug-line "Test message")
    (should-not (get-buffer mediawiki-debug-buffer))))

(ert-deftest test-mediawiki-debug ()
  "Test mediawiki-debug function."
  (let ((mediawiki-debug t)
        (test-buffer (get-buffer-create "*test-buffer*"))
        (test-function "test-function"))

    ;; Clear debug buffer if it exists
    (when (get-buffer mediawiki-debug-buffer)
      (kill-buffer mediawiki-debug-buffer))

    ;; Add some content to test buffer
    (with-current-buffer test-buffer
      (insert "Test buffer content"))

    ;; Test debug function
    (mediawiki-debug test-buffer test-function)

    ;; Check that debug buffer contains function name and buffer content
    (should (get-buffer mediawiki-debug-buffer))
    (with-current-buffer mediawiki-debug-buffer
      (let ((content (buffer-string)))
        (should (string-match-p test-function content))
        (should (string-match-p "Test buffer content" content))))

    ;; Check that test buffer was killed
    (should-not (buffer-live-p test-buffer))

    ;; Clean up
    (kill-buffer mediawiki-debug-buffer)))

(ert-deftest test-mediawiki-debug-disabled ()
  "Test mediawiki-debug when debugging is disabled."
  (let ((mediawiki-debug nil)
        (test-buffer (get-buffer-create "*test-buffer*")))

    ;; Add some content to test buffer
    (with-current-buffer test-buffer
      (insert "Test buffer content"))

    ;; Clear debug buffer if it exists
    (when (get-buffer mediawiki-debug-buffer)
      (kill-buffer mediawiki-debug-buffer))

    ;; Test debug function with debugging disabled
    (mediawiki-debug test-buffer "test-function")

    ;; Check that no debug buffer was created
    (should-not (get-buffer mediawiki-debug-buffer))

    ;; Check that test buffer was still killed
    (should-not (buffer-live-p test-buffer))))

;;; Test Utility Functions

(ert-deftest test-mediawiki-page-get-metadata ()
  "Test mediawiki-page-get-metadata function."
  ;; Create a mock page structure
  (let ((mock-page '(page
                     ((title . "Test Page")
                      (edittoken . "test-token")
                      (starttimestamp . "2025-01-01T00:00:00Z"))
                     (revisions . "test-revisions"))))

    ;; Test extracting different metadata items
    (should (string= "Test Page"
                     (mediawiki-page-get-metadata mock-page 'title)))
    (should (string= "test-token"
                     (mediawiki-page-get-metadata mock-page 'edittoken)))
    (should (string= "2025-01-01T00:00:00Z"
                     (mediawiki-page-get-metadata mock-page 'starttimestamp)))

    ;; Test extracting non-existent metadata
    (should (null (mediawiki-page-get-metadata mock-page 'nonexistent)))))

;;; Test Customization

(ert-deftest test-mediawiki-debug-customization ()
  "Test that mediawiki-debug is properly customizable."
  (should (get 'mediawiki-debug 'custom-type))
  (should (eq 'boolean (get 'mediawiki-debug 'custom-type))))

(provide 'test-mediawiki-utils)

;;; test-mediawiki-utils.el ends here
