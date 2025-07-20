;;; test-response-processing.el --- Test HTTP response processing utilities -*- lexical-binding: t; -*-

;; Test script for the HTTP response processing utilities

;; Load dependencies
(load-file "mediawiki-core.el")
(load-file "mediawiki-http.el")

;; Enable debugging for testing
(setq mediawiki-debug t)
(setq mediawiki-debug-verbose t)

(defun test-response-validation ()
  "Test response validation function."
  (message "Testing response validation...")
  
  ;; Test valid response
  (let ((valid-response '(:success t :status-code 200 :headers (("content-type" . "application/json")) :body "{}")))
    (if (mediawiki-http-validate-response valid-response)
        (message "✓ Valid response validation passed")
      (message "✗ Valid response validation failed")))
  
  ;; Test invalid response (missing success field)
  (let ((invalid-response '(:status-code 200 :headers nil :body "{}")))
    (if (not (mediawiki-http-validate-response invalid-response))
        (message "✓ Invalid response validation correctly failed")
      (message "✗ Invalid response validation incorrectly passed"))))

(defun test-json-parsing ()
  "Test JSON response parsing."
  (message "Testing JSON parsing...")
  
  ;; Test valid JSON
  (let ((json-response '(:body "{\"test\": \"value\"}")))
    (let ((parsed (mediawiki-http-parse-json-response json-response)))
      (if (and parsed (equal (plist-get parsed :test) "value"))
          (message "✓ JSON parsing passed")
        (message "✗ JSON parsing failed: expected 'value', got %s" (plist-get parsed :test)))))
  
  ;; Test invalid JSON
  (let ((invalid-json-response '(:body "{invalid json")))
    (let ((parsed (mediawiki-http-parse-json-response invalid-json-response)))
      (if (not parsed)
          (message "✓ Invalid JSON correctly returned nil")
        (message "✗ Invalid JSON incorrectly parsed")))))

(defun test-status-code-utilities ()
  "Test status code utility functions."
  (message "Testing status code utilities...")
  
  ;; Test success status
  (if (mediawiki-http-is-success-status 200)
      (message "✓ Success status detection passed")
    (message "✗ Success status detection failed"))
  
  ;; Test client error status
  (if (mediawiki-http-is-client-error 404)
      (message "✓ Client error status detection passed")
    (message "✗ Client error status detection failed"))
  
  ;; Test server error status
  (if (mediawiki-http-is-server-error 500)
      (message "✓ Server error status detection passed")
    (message "✗ Server error status detection failed"))
  
  ;; Test status description
  (let ((desc (mediawiki-http-get-status-description 404)))
    (if (string= desc "Not Found")
        (message "✓ Status description passed")
      (message "✗ Status description failed: %s" desc))))

(defun test-response-health-check ()
  "Test response health checking."
  (message "Testing response health check...")
  
  ;; Test healthy response
  (let ((healthy-response '(:success t :status-code 200 
                           :headers (("content-type" . "application/json"))
                           :body "{\"result\": \"ok\"}")))
    (let ((health (mediawiki-http-check-response-health healthy-response)))
      (if (eq (plist-get health :status) :healthy)
          (message "✓ Healthy response check passed")
        (message "✗ Healthy response check failed"))))
  
  ;; Test response with warnings
  (let ((warning-response '(:success t :status-code 200 
                           :headers nil  ; Missing content-type
                           :body "{\"result\": \"ok\"}")))
    (let ((health (mediawiki-http-check-response-health warning-response)))
      (if (> (plist-get health :warning-count) 0)
          (message "✓ Response warning detection passed")
        (message "✗ Response warning detection failed")))))

(defun test-error-classification ()
  "Test error classification."
  (message "Testing error classification...")
  
  ;; Test auth error
  (let ((error-type (mediawiki-http-classify-error 401)))
    (if (eq error-type 'auth-error)
        (message "✓ Auth error classification passed")
      (message "✗ Auth error classification failed: %s" error-type)))
  
  ;; Test rate limit error
  (let ((error-type (mediawiki-http-classify-error 429)))
    (if (eq error-type 'rate-limit-error)
        (message "✓ Rate limit error classification passed")
      (message "✗ Rate limit error classification failed: %s" error-type))))

(defun run-all-tests ()
  "Run all response processing tests."
  (message "=== Running HTTP Response Processing Tests ===")
  (test-response-validation)
  (test-json-parsing)
  (test-status-code-utilities)
  (test-response-health-check)
  (test-error-classification)
  (message "=== Tests Complete ===")
  
  ;; Show debug buffer contents if it exists
  (when (get-buffer mediawiki-debug-buffer)
    (message "=== Debug Buffer Contents ===")
    (with-current-buffer mediawiki-debug-buffer
      (message "%s" (buffer-string)))))

;; Run the tests
(run-all-tests)

;;; test-response-processing.el ends here