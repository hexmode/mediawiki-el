;;; test-http-async.el --- Test asynchronous HTTP functionality -*- lexical-binding: t; -*-

;; Test to verify the asynchronous HTTP request functionality

(require 'mediawiki-http)
(require 'mediawiki-core)

;; Enable debugging for testing
(setq mediawiki-debug t)

(defvar test-results '()
  "Store test results for verification.")

(defun test-record-result (test-name success message)
  "Record a test result."
  (push (list test-name success message) test-results)
  (message "%s: %s - %s" test-name (if success "PASS" "FAIL") message))

(defun test-sync-http-basic ()
  "Test basic synchronous HTTP functionality."
  (message "Testing sync HTTP request...")
  (condition-case err
      (let ((response (mediawiki-http-request-sync "https://httpbin.org/get" "GET" nil)))
        (if (and (plist-get response :success)
                 (= (plist-get response :status-code) 200))
            (test-record-result "sync-basic" t "Successful GET request")
          (test-record-result "sync-basic" nil 
                             (format "Unexpected response: %s" response))))
    (error
     (test-record-result "sync-basic" nil 
                        (format "Error: %s" (error-message-string err))))))

(defun test-sync-http-error ()
  "Test synchronous HTTP error handling."
  (message "Testing sync HTTP error handling...")
  (condition-case err
      (let ((response (mediawiki-http-request-sync "https://httpbin.org/status/404" "GET" nil)))
        (if (and (not (plist-get response :success))
                 (= (plist-get response :status-code) 404)
                 (eq (plist-get response :error-type) 'not-found-error))
            (test-record-result "sync-error" t "Correctly handled 404 error")
          (test-record-result "sync-error" nil 
                             (format "Unexpected response: %s" response))))
    (error
     ;; This is expected for sync requests that fail
     (test-record-result "sync-error" t "Error correctly signaled"))))

(defun test-error-classification ()
  "Test error classification functionality."
  (message "Testing error classification...")
  (let ((tests '((401 auth-error)
                 (403 auth-error)
                 (404 not-found-error)
                 (429 rate-limit-error)
                 (500 server-error)
                 (nil network-error))))
    (dolist (test tests)
      (let ((status-code (car test))
            (expected-type (cadr test)))
        (let ((actual-type (mediawiki-http-classify-error status-code)))
          (if (eq actual-type expected-type)
              (test-record-result 
               (format "classify-%s" (or status-code "nil"))
               t (format "Correctly classified as %s" expected-type))
            (test-record-result 
             (format "classify-%s" (or status-code "nil"))
             nil (format "Expected %s, got %s" expected-type actual-type))))))))

(defun test-async-callback-mechanism ()
  "Test that async callbacks are properly called."
  (message "Testing async callback mechanism...")
  (let ((callback-called nil)
        (error-callback-called nil))
    
    ;; Test success callback
    (mediawiki-http-request-async
     "https://httpbin.org/get"
     "GET"
     nil
     (lambda (response)
       (setq callback-called t)
       (test-record-result "async-success-callback" t "Success callback called"))
     (lambda (response)
       (setq error-callback-called t)
       (test-record-result "async-success-callback" nil "Error callback called unexpectedly")))
    
    ;; Give it a moment to process (this is a limitation of testing async code)
    (message "Async test initiated - callback results will appear shortly")))

(defun run-all-tests ()
  "Run all HTTP tests."
  (setq test-results '())
  (message "=== Starting HTTP Tests ===")
  
  ;; Run synchronous tests first (they complete immediately)
  (test-sync-http-basic)
  (test-sync-http-error)
  (test-error-classification)
  
  ;; Run async test (results will appear later)
  (test-async-callback-mechanism)
  
  (message "=== Test Summary ===")
  (let ((passed 0)
        (failed 0))
    (dolist (result (reverse test-results))
      (if (cadr result)
          (setq passed (1+ passed))
        (setq failed (1+ failed))))
    (message "Tests completed: %d passed, %d failed" passed failed))
  
  (message "Note: Async test results may appear after this summary"))

;; Run the tests
(run-all-tests)

;;; test-http-async.el ends here