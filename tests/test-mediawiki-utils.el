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
  ;; Create a mock page structure (JSON alist)
  (let ((mock-page '((title . "Test Page")
                     (edittoken . "test-token")
                     (starttimestamp . "2025-01-01T00:00:00Z"))))

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

;;; Test Timestamp Formatting

(ert-deftest test-mediawiki-format-timestamp-just-now ()
  "Test `mediawiki-format-timestamp' returns \"just now\" for <60s."
  (let* ((ts (format-time-string
              "%Y-%m-%dT%TZ"
              (time-subtract (current-time) 30) t)))
    (should (string= "just now" (mediawiki-format-timestamp ts)))))

(ert-deftest test-mediawiki-format-timestamp-minutes ()
  "Test `mediawiki-format-timestamp' returns proper minute strings."
  (let* ((ts (format-time-string
              "%Y-%m-%dT%TZ"
              (time-subtract (current-time) 120) t)))
    (should (string= "2 minutes ago" (mediawiki-format-timestamp ts)))))

(ert-deftest test-mediawiki-format-timestamp-hours ()
  "Test `mediawiki-format-timestamp' returns proper hour strings."
  (let* ((ts (format-time-string
              "%Y-%m-%dT%TZ"
              (time-subtract (current-time) 8000) t)))
    (should (string= "2 hours ago" (mediawiki-format-timestamp ts)))))

(ert-deftest test-mediawiki-format-timestamp-days ()
  "Test `mediawiki-format-timestamp' returns proper day strings."
  (let* ((ts (format-time-string
              "%Y-%m-%dT%TZ"
              (time-subtract (current-time) 345600) t)))
    (should (string= "4 days ago" (mediawiki-format-timestamp ts)))))

;;; Test Size Change Formatting

(ert-deftest test-mediawiki-format-size-change-positive ()
  "Test `mediawiki-format-size-change' with positive numbers."
  (let ((result (mediawiki-format-size-change 42)))
    (should (string= "+42" result))
    (should (eq 'font-lock-type-face
                (get-text-property 0 'face result)))))

(ert-deftest test-mediawiki-format-size-change-negative ()
  "Test `mediawiki-format-size-change' with negative numbers."
  (let ((result (mediawiki-format-size-change -15)))
    (should (string= "-15" result))
    (should (eq 'font-lock-warning-face
                (get-text-property 0 'face result)))))

(ert-deftest test-mediawiki-format-size-change-zero ()
  "Test `mediawiki-format-size-change' returns \" 0\" for zero."
  (let ((result (mediawiki-format-size-change 0)))
    (should (string= " 0" result))
    (should-not (get-text-property 0 'face result))))

(ert-deftest test-mediawiki-format-size-change-nil ()
  "Test `mediawiki-format-size-change' returns \" 0\" for nil."
  (let ((result (mediawiki-format-size-change nil)))
    (should (string= " 0" result))
    (should-not (get-text-property 0 'face result))))

;;; Test URL Builders

(ert-deftest test-mediawiki-make-page-url ()
  "Test `mediawiki-make-page-url' returns proper URL."
  (cl-letf (((symbol-function 'mediawiki-site-url)
             (lambda (_site) "https://en.wikipedia.org/w/")))
    (let ((result (mediawiki-make-page-url "enwiki" "Main Page")))
      (should (string-match-p
               "^https://en\\.wikipedia\\.org/w/index\\.php\\?title="
               result))
      (should (string-match-p "Main" result))
      (should (string-match-p "Page" result)))))

(ert-deftest test-mediawiki-make-revision-url ()
  "Test `mediawiki-make-revision-url' includes &oldid=N."
  (cl-letf (((symbol-function 'mediawiki-site-url)
             (lambda (_site) "https://en.wikipedia.org/w/")))
    (let ((result (mediawiki-make-revision-url "enwiki" "Test" 12345)))
      (should (string-match-p "&oldid=12345" result))
      (should (string-match-p "title=Test" result)))))

(ert-deftest test-mediawiki-make-user-page-url ()
  "Test `mediawiki-make-user-page-url' includes User: prefix."
  (cl-letf (((symbol-function 'mediawiki-site-url)
             (lambda (_site) "https://en.wikipedia.org/w/")))
    (let ((result (mediawiki-make-user-page-url "enwiki" "JohnDoe")))
      (should (string-match-p "User" result))
      (should (string-match-p "JohnDoe" result)))))

;;; Test URL Title Extraction

(ert-deftest test-mediawiki-extract-title-from-url-query ()
  "Test `mediawiki-extract-title-from-url' with ?title= pattern.
MediaWiki URLs use underscores for spaces."
  (should (string= "Main_Page"
                   (mediawiki-extract-title-from-url
                    "https://en.wikipedia.org/w/index.php?title=Main_Page"))))

(ert-deftest test-mediawiki-extract-title-from-url-ampersand ()
  "Test `mediawiki-extract-title-from-url' with &title= pattern."
  (should (string= "Foo"
                   (mediawiki-extract-title-from-url
                    "https://en.wikipedia.org/w/index.php?title=Foo&oldid=123"))))

(ert-deftest test-mediawiki-extract-title-from-url-no-title ()
  "Test `mediawiki-extract-title-from-url' returns nil for no title."
  (should (null (mediawiki-extract-title-from-url
                 "https://en.wikipedia.org/wiki/Main_Page"))))

(provide 'test-mediawiki-utils)

;;; test-mediawiki-utils.el ends here
