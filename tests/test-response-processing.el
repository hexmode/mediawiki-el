;;; test-response-processing.el --- Test HTTP response processing utilities -*- lexical-binding: t; -*-

;; Test script for the HTTP response processing utilities

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-http)

;; Enable debugging for testing
(setq mediawiki-debug t)
(setq mediawiki-debug-verbose t)

(ert-deftest test-response-validation ()
  "Test response validation function."
  ;; Test valid response
  (let ((valid-response '(:success t :status-code 200 :headers (("content-type" . "application/json")) :body "{}")))
    (should (mediawiki-http-validate-response valid-response)))
  
  ;; Test invalid response (missing success field)
  (let ((invalid-response '(:status-code 200 :headers nil :body "{}")))
    (should-not (mediawiki-http-validate-response invalid-response))))

(ert-deftest test-json-parsing ()
  "Test JSON response parsing."
  ;; Test valid JSON
  (let ((json-response '(:body "{\"test\": \"value\"}")))
    (let ((parsed (mediawiki-http-parse-json-response json-response)))
      (should parsed)
      (should (equal (plist-get parsed :test) "value"))))
  
  ;; Test invalid JSON
  (let ((invalid-json-response '(:body "{invalid json")))
    (let ((parsed (mediawiki-http-parse-json-response invalid-json-response)))
      (should-not parsed))))

(ert-deftest test-status-code-utilities ()
  "Test status code utility functions."
  ;; Test success status
  (should (mediawiki-http-is-success-status 200))
  
  ;; Test client error status
  (should (mediawiki-http-is-client-error 404))
  
  ;; Test server error status
  (should (mediawiki-http-is-server-error 500))
  
  ;; Test status description
  (let ((desc (mediawiki-http-get-status-description 404)))
    (should (string= desc "Not Found"))))

(ert-deftest test-response-health-check ()
  "Test response health checking."
  ;; Test healthy response
  (let ((healthy-response '(:success t :status-code 200 
                           :headers (("content-type" . "application/json"))
                           :body "{\"result\": \"ok\"}")))
    (let ((health (mediawiki-http-check-response-health healthy-response)))
      (should (eq (plist-get health :status) :healthy))))
  
  ;; Test response with warnings
  (let ((warning-response '(:success t :status-code 200 
                           :headers nil  ; Missing content-type
                           :body "{\"result\": \"ok\"}")))
    (let ((health (mediawiki-http-check-response-health warning-response)))
      (should (> (plist-get health :warning-count) 0)))))

(ert-deftest test-error-classification ()
  "Test error classification."
  ;; Test auth error
  (let ((error-type (mediawiki-http-classify-error 401)))
    (should (eq error-type 'auth-error)))
  
  ;; Test rate limit error
  (let ((error-type (mediawiki-http-classify-error 429)))
    (should (eq error-type 'rate-limit-error))))

(provide 'test-response-processing)

;;; test-response-processing.el ends here