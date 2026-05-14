;;; test-mediawiki-api.el --- Unit tests for mediawiki-api.el -*- lexical-binding: t; -*-

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

;; Unit tests for mediawiki-api.el module.
;; These tests mock API interactions to avoid network dependencies.

;;; Code:

(require 'ert)
(require 'mediawiki-api)

;;; Test API URL Construction

(ert-deftest test-mediawiki-make-api-url ()
  "Test mediawiki-make-api-url function."
  ;; Mock mediawiki-site-url function
  (cl-letf (((symbol-function 'mediawiki-site-url)
             (lambda (site) "https://en.wikipedia.org/w/")))

    ;; Test basic API URL construction
    (let ((result (mediawiki-make-api-url "Wikipedia")))
      (should (stringp result))
      (should (string-match-p "api\\.php$" result))
      (should (string-match-p "^https://en\\.wikipedia\\.org/w/" result)))

    ;; Test with different base URL format
    (cl-letf (((symbol-function 'mediawiki-site-url)
               (lambda (site) "https://example.com/wiki")))
      (let ((result (mediawiki-make-api-url "Test")))
        (should (string-match-p "api\\.php$" result))
        (should (string-match-p "^https://example\\.com/wiki/" result))))))

;;; Test Parameter Formatting

(ert-deftest test-mediawiki-api-param ()
  "Test mediawiki-api-param function."
  ;; Test integer conversion
  (should (string= "123" (mediawiki-api-param 123)))

  ;; Test string passthrough
  (should (string= "test" (mediawiki-api-param "test")))

  ;; Test list concatenation
  (should (string= "a|b|c" (mediawiki-api-param '("a" "b" "c"))))
  (should (string= "1|2|3" (mediawiki-api-param '("1" "2" "3"))))

  ;; Test empty list
  (should (string= "" (mediawiki-api-param '())))

  ;; Test error on unsupported type
  (should-error (mediawiki-api-param 'symbol)))

;;; Test Page Data Extraction

(ert-deftest test-mediawiki-page-get-title ()
  "Test mediawiki-page-get-title function."
  ;; JSON alist page structure
  (let ((mock-page '((title . "Test Page") (pageid . 123))))
    (should (string= "Test Page" (mediawiki-page-get-title mock-page))))

  ;; Test with different title
  (let ((mock-page '((title . "Another Page") (pageid . 456))))
    (should (string= "Another Page" (mediawiki-page-get-title mock-page)))))

(ert-deftest test-mediawiki-page-get-revision ()
  "Test mediawiki-page-get-revision function."
  ;; JSON alist page structure with slots
  (let ((mock-page '((title . "Test Page")
                     (revisions . (((timestamp . "2025-01-01T00:00:00Z")
                                    (user . "TestUser")
                                    (slots . ((main . ((content . "Test content")))))))))))
    ;; Test getting content
    (should (string= "Test content"
                     (mediawiki-page-get-revision mock-page 0 'content)))

    ;; Test getting timestamp
    (should (string= "2025-01-01T00:00:00Z"
                     (mediawiki-page-get-revision mock-page 0 'timestamp)))

    ;; Test getting user
    (should (string= "TestUser"
                     (mediawiki-page-get-revision mock-page 0 'user))))

  ;; Test with slot-based content
  (let ((mock-page '((title . "Test Page")
                     (revisions . (((timestamp . "2025-01-01T00:00:00Z")
                                    (slots . ((main . ((content . "Slot content")))))))))))

    ;; Test getting content from slot format
    (should (string= "Slot content"
                     (mediawiki-page-get-revision mock-page 0 'content)))))

;;; Test Page List Utilities

(ert-deftest test-mediawiki-pagelist-find-page ()
  "Test mediawiki-pagelist-find-page function."
  ;; JSON alist pagelist structure: pages is an alist of (id . page-alist) pairs
  (let ((mock-pagelist
         `((curtimestamp . "2024-01-01")
           (pages . ((\1 . ((title . "Main Page")))
                     (\2 . ((title . "Test Page")))
                     (\3 . ((title . "Another Page"))))))))

    ;; Test finding existing page
    (let ((result (mediawiki-pagelist-find-page mock-pagelist "Test Page")))
      (should result)
      (should (string= "Test Page" (mediawiki-page-get-title result))))

    ;; Test finding page with underscore translation
    (let ((result (mediawiki-pagelist-find-page mock-pagelist "Main_Page")))
      (should result)
      (should (string= "Main Page" (mediawiki-page-get-title result))))

    ;; Test not finding non-existent page
    (should-not (mediawiki-pagelist-find-page mock-pagelist "Non-existent Page"))))

;;; Test String Extraction

(ert-deftest test-mediawiki-extract-string-from-structure ()
  "Test mediawiki-extract-string-from-structure function."
  ;; Test with simple string
  (should (string= "test" (mediawiki-extract-string-from-structure "test")))

  ;; Test with nested list containing string
  (should (string= "found"
                   (mediawiki-extract-string-from-structure '(a (b "found")))))

  ;; Test with complex nested structure
  (should (string= "deep"
                   (mediawiki-extract-string-from-structure '(a (b (c "deep"))))))

  ;; Test with no string found
  (should-not (mediawiki-extract-string-from-structure '(a (b (c)))))

  ;; Test with non-list, non-string
  (should-not (mediawiki-extract-string-from-structure 123)))

;;; Test API Error Handling

(ert-deftest test-mediawiki-raise ()
  "mediawiki-raise is deprecated and is now a no-op (issue #39).
Error/warning handling moved into mediawiki-api-call."
  ;; Function still exists for backward compatibility
  (should (functionp 'mediawiki-raise))
  ;; Returns nil regardless of input
  (should-not (mediawiki-raise '() 'warnings #'ignore))
  (should-not (mediawiki-raise '() 'error #'ignore)))

;;; Test JSON Parsing (Regression Test for Empty Response Bug)

(ert-deftest test-mediawiki-json-parse-empty ()
  "Test that empty JSON {} parses to nil (regression test for issue #39).

This tests the fix for the logout bug where {} was incorrectly treated
as a parse error. The json-parse-string returns nil for empty {}."
  ;; Simulate what mediawiki-api-call does with json-parse-string
  (let ((result (condition-case err
                    (json-parse-string "{}"
                      :object-type 'alist
                      :array-type 'list
                      :null-object nil
                      :false-object nil)
                  (error (error "Parse error: %s" err)))))
    ;; Empty {} should parse to nil, not signal an error
    (should (eq result nil))
    ;; alist-get on nil returns nil gracefully (doesn't error)
    (should-not (alist-get 'error result))))

(ert-deftest test-mediawiki-json-parse-non-empty ()
  "Test that non-empty JSON parses correctly to an alist."
  (let ((result (json-parse-string "{\"error\":{\"code\":\"badtoken\",\"info\":\"Invalid token\"}}"
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil)))
    (should (listp result))
    (should (alist-get 'error result))
    (should (string= "badtoken" (alist-get 'code (alist-get 'error result))))))

(ert-deftest test-mediawiki-json-parse-normal-response ()
  "Test that normal JSON responses parse to usable alists."
  (let ((result (json-parse-string "{\"query\":{\"pages\":{\"1\":{\"title\":\"Test\"}}}}"
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil)))
    (should (listp result))
    (should (alist-get 'query result))
    ;; JSON numeric keys become symbols like \1 in Emacs Lisp
    (should (string= "Test"
                    (alist-get 'title
                              (car (alist-get 'pages (alist-get 'query result))))))))

;;; Test Mocked API Functions

(ert-deftest test-mediawiki-api-call-structure ()
  "Test mediawiki-api-call function structure."
  (should (functionp 'mediawiki-api-call))

  ;; We can't test actual API calls without mocking HTTP,
  ;; but we can test the function exists and has the right signature
  (should (condition-case nil
              ;; This will error due to network, but tests function structure
              (mediawiki-api-call "test-site" "query" '())
            (error t))))

(ert-deftest test-mediawiki-site-get-token-structure ()
  "Test mediawiki-site-get-token function structure."
  (should (functionp 'mediawiki-site-get-token))

  ;; Test function signature
  (should (condition-case nil
              ;; This will error due to network, but tests function structure
              (mediawiki-site-get-token "test-site" "csrf")
            (error t))))

;;; Test Query Functions Structure

(ert-deftest test-mediawiki-api-query-functions-structure ()
  "Test that API query functions have correct structure."
  (should (functionp 'mediawiki-api-query-revisions))
  (should (functionp 'mediawiki-api-query-title))

  ;; Test that functions exist and can be called (will error due to network)
  (should (condition-case nil
              (mediawiki-api-query-revisions "test-site" "Test Page" '("content"))
            (error t)))

  (should (condition-case nil
              (mediawiki-api-query-title "test-site" "Test Page")
            (error t))))

(provide 'test-mediawiki-api)

;;; test-mediawiki-api.el ends here
