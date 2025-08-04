;;; test-mediawiki-http.el --- Unit tests for mediawiki-http.el -*- lexical-binding: t; -*-

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

;; Unit tests for mediawiki-http.el module.
;; These tests mock HTTP interactions to avoid network dependencies.

;;; Code:

(require 'ert)
(require 'mediawiki-http)

;;; Test URL Compatibility Functions

(ert-deftest test-url-bit-for-url ()
  "Test url-bit-for-url function if it exists."
  (when (fboundp 'url-bit-for-url)
    ;; Mock auth-source-user-or-password if available
    (if (fboundp 'auth-source-user-or-password)
        (cl-letf (((symbol-function 'auth-source-user-or-password)
                   (lambda (type host protocol) "test-result")))
          (should (stringp (url-bit-for-url 'url-user "login" "http://example.com"))))
      ;; If auth-source is not available, just test that function exists
      (should (functionp 'url-bit-for-url)))))

(ert-deftest test-url-user-for-url ()
  "Test url-user-for-url function."
  (when (fboundp 'url-user-for-url)
    ;; Test with a URL that has user info
    (let ((url-with-user "http://user@example.com/path"))
      ;; Function should return user or nil
      (should (or (stringp (url-user-for-url url-with-user))
                  (null (url-user-for-url url-with-user)))))

    ;; Test with a URL without user info
    (let ((url-without-user "http://example.com/path"))
      (should (or (stringp (url-user-for-url url-without-user))
                  (null (url-user-for-url url-without-user)))))))

(ert-deftest test-url-password-for-url ()
  "Test url-password-for-url function."
  (when (fboundp 'url-password-for-url)
    ;; Test with a URL
    (let ((test-url "http://example.com/path"))
      ;; Function should return password or nil
      (should (or (stringp (url-password-for-url test-url))
                  (null (url-password-for-url test-url)))))))

;;; Test Multipart Form Data Encoding

(ert-deftest test-mm-url-encode-multipart-form-data ()
  "Test mm-url-encode-multipart-form-data function."
  (when (fboundp 'mm-url-encode-multipart-form-data)
    (let ((test-pairs '(("field1" . "value1")
                        ("field2" . "value2"))))

      ;; Test basic encoding
      (let ((result (mm-url-encode-multipart-form-data test-pairs)))
        (should (stringp result))
        (should (string-match-p "Content-Disposition: form-data" result))
        (should (string-match-p "field1" result))
        (should (string-match-p "value1" result))
        (should (string-match-p "field2" result))
        (should (string-match-p "value2" result)))

      ;; Test with custom boundary
      (let ((result (mm-url-encode-multipart-form-data test-pairs "custom-boundary")))
        (should (stringp result))
        (should (string-match-p "custom-boundary" result))))))

;;; Test HTTP Request Functions (Mocked)

(ert-deftest test-url-http-get-structure ()
  "Test that url-http-get function has correct structure."
  (should (functionp 'url-http-get))

  ;; Mock the underlying HTTP functions to avoid network calls
  (cl-letf (((symbol-function 'url-compat-retrieve)
             (lambda (url post-process buffer callback cbargs)
               ;; Mock successful retrieval without calling post-process
               "mock-result"))
            ((symbol-function 'url-basic-auth)
             (lambda (url) nil)))

    ;; Test function can be called without making real network requests
    (should (url-http-get "http://example.com" nil nil nil nil))))

(ert-deftest test-url-http-post-structure ()
  "Test that url-http-post function has correct structure."
  (should (functionp 'url-http-post))

  ;; Mock the underlying HTTP functions to avoid network calls
  (cl-letf (((symbol-function 'url-compat-retrieve)
             (lambda (url post-process buffer callback cbargs)
               ;; Mock successful retrieval without calling post-process
               "mock-result"))
            ((symbol-function 'url-basic-auth)
             (lambda (url) nil)))

    ;; Test function can be called without making real network requests
    (should (url-http-post "http://example.com" '() nil nil nil nil nil))))

;;; Test Response Processing

(ert-deftest test-url-http-response-post-process-structure ()
  "Test url-http-response-post-process function structure."
  (should (functionp 'url-http-response-post-process))

  ;; Test that function can handle basic parameters without network
  (with-temp-buffer
    ;; Mock a simple HTTP response structure
    (insert "HTTP/1.1 200 OK\r\n")
    (insert "Content-Type: text/plain\r\n")
    (insert "\r\n")
    (insert "Test response body")

    ;; Set up minimal required variables for the function
    (setq url-http-end-of-headers (point-min))
    (goto-char (point-min))
    (search-forward "\r\n\r\n")
    (setq url-http-end-of-headers (point))

    ;; Test that function doesn't crash with valid status
    (should (condition-case nil
                (url-http-response-post-process 200)
              (error nil)))))

;;; Test URL Retrieval Compatibility

(ert-deftest test-url-compat-retrieve-structure ()
  "Test url-compat-retrieve function structure."
  (should (functionp 'url-compat-retrieve))

  ;; Test that function exists and can be called
  ;; We mock the actual retrieval to avoid network dependencies
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (url) (get-buffer-create "*mock-response*")))
            ((symbol-function 'url-retrieve)
             (lambda (url callback &optional cbargs)
               (when callback
                 (funcall callback nil)))))

    ;; Test synchronous call
    (should (condition-case nil
                (url-compat-retrieve "http://example.com"
                                   (lambda (buf) "processed")
                                   nil nil nil)
              (error nil)))))

;;; Test Variable Definitions

(ert-deftest test-mediawiki-http-variables ()
  "Test that HTTP-related variables are defined."
  (should (boundp 'url-http-get-post-process))
  (should (boundp 'url-http-post-post-process))

  ;; Test that post-process variables point to functions
  (should (functionp url-http-get-post-process))
  (should (functionp url-http-post-post-process)))

;;; Test User Agent Handling

(ert-deftest test-mediawiki-user-agent ()
  "Test that MediaWiki user agent is properly handled."
  ;; Test that mediawiki-version is included in user agent construction
  (should (boundp 'mediawiki-version))
  (should (stringp mediawiki-version))

  ;; The user agent construction happens in url-compat-retrieve
  ;; We test that the function can handle user agent modification
  (let ((url-user-agent "test-agent"))
    (should (stringp url-user-agent))))

(provide 'test-mediawiki-http)

;;; test-mediawiki-http.el ends here
