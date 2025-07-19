;;; test-json-api-framework.el --- Test JSON API communication framework -*- lexical-binding: t; -*-

;; Test script for the JSON-based MediaWiki API communication framework

(require 'mediawiki-api)
(require 'mediawiki-core)

;; Simple test assertion function
(defun test-assert (condition message)
  "Simple assertion for testing."
  (if condition
      (message "✓ %s" message)
    (error "✗ %s" message)))

;; Test data structures
(defun test-api-response-creation ()
  "Test creating API response structures."
  (let ((response (make-mediawiki-api-response
                   :success t
                   :data '((query . ((pages . ((12345 . ((title . "Test Page"))))))))
                   :warnings nil
                   :errors nil)))
    (test-assert (mediawiki-api-response-success response) "API response success flag")
    (test-assert (mediawiki-api-response-data response) "API response has data")
    (message "✓ API response creation test passed")))

;; Test error creation
(defun test-error-creation ()
  "Test MediaWiki API error creation."
  (let ((error (mediawiki-api-create-error "badtoken" "Invalid token")))
    (test-assert (string= (plist-get error :code) "badtoken") "Error code")
    (test-assert (string= (plist-get error :info) "Invalid token") "Error info")
    (message "✓ Error creation test passed")))

;; Test parameter building
(defun test-parameter-building ()
  "Test API parameter building."
  (let ((params (mediawiki-api-build-params
                 "action" "query"
                 "titles" "Main Page"
                 "prop" "info")))
    (test-assert (= (length params) 3) "Parameter count")
    (test-assert (string= (cdr (assoc "action" params)) "query") "Action parameter")
    (test-assert (string= (cdr (assoc "titles" params)) "Main Page") "Titles parameter")
    (message "✓ Parameter building test passed")))

;; Test version comparison
(defun test-version-comparison ()
  "Test MediaWiki version comparison."
  (test-assert (= (mediawiki-api-version-compare '(1 35 0) '(1 35 0)) 0) "Equal versions")
  (test-assert (= (mediawiki-api-version-compare '(1 35 0) '(1 34 0)) 1) "Newer version")
  (test-assert (= (mediawiki-api-version-compare '(1 34 0) '(1 35 0)) -1) "Older version")
  (message "✓ Version comparison test passed"))

;; Test error classification
(defun test-error-classification ()
  "Test error code classification."
  (test-assert (eq (mediawiki-api-classify-error-code "badtoken") 'authentication) "Auth error classification")
  (test-assert (eq (mediawiki-api-classify-error-code "ratelimited") 'rate-limit) "Rate limit classification")
  (test-assert (eq (mediawiki-api-classify-error-code "badtitle") 'validation) "Validation error classification")
  (test-assert (eq (mediawiki-api-classify-error-code "editconflict") 'conflict) "Conflict error classification")
  (message "✓ Error classification test passed"))

;; Test response validation
(defun test-response-validation ()
  "Test API response validation."
  (let ((valid-response (make-mediawiki-api-response
                         :success t
                         :data '((query . ((pages . nil))))
                         :warnings nil
                         :errors nil)))
    (test-assert (null (mediawiki-api-validate-response valid-response)) "Valid response validation")
    (message "✓ Response validation test passed")))

;; Run all tests
(defun run-json-api-tests ()
  "Run all JSON API framework tests."
  (message "Running JSON API framework tests...")
  (condition-case err
      (progn
        (test-api-response-creation)
        (test-error-creation)
        (test-parameter-building)
        (test-version-comparison)
        (test-error-classification)
        (test-response-validation)
        (message "✓ All JSON API framework tests passed!"))
    (error
     (message "✗ Test failed: %s" (error-message-string err)))))

;; Run tests when file is loaded
(run-json-api-tests)

;;; test-json-api-framework.el ends here