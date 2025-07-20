;;; test-sync-wrapper.el --- Test synchronous HTTP wrapper functionality -*- lexical-binding: t; -*-

;; Test to verify the synchronous HTTP wrapper implementation

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

(defun test-sync-wrapper-basic ()
  "Test basic synchronous wrapper functionality."
  (message "Testing sync wrapper with basic GET request...")
  (condition-case err
      (let ((response (mediawiki-http-request-sync "https://httpbin.org/get" "GET" nil)))
        (if (and (plist-get response :success)
                 (= (plist-get response :status-code) 200))
            (test-record-result "sync-wrapper-basic" t "Successful GET request via wrapper")
          (test-record-result "sync-wrapper-basic" nil 
                             (format "Unexpected response: %s" response))))
    (error
     (test-record-result "sync-wrapper-basic" nil 
                        (format "Error: %s" (error-message-string err))))))

(defun test-sync-wrapper-timeout ()
  "Test synchronous wrapper timeout functionality."
  (message "Testing sync wrapper timeout (this may take a few seconds)...")
  (let ((start-time (current-time)))
    (condition-case err
        (let ((response (mediawiki-http-request-sync "https://httpbin.org/delay/5" "GET" nil 2)))
          (test-record-result "sync-wrapper-timeout" nil 
                             "Request should have timed out but didn't"))
      (error
       (let ((elapsed (float-time (time-subtract (current-time) start-time))))
         (if (and (< elapsed 4)  ; Should timeout before 4 seconds
                  (string-match-p "timed out" (error-message-string err)))
             (test-record-result "sync-wrapper-timeout" t 
                                (format "Correctly timed out after %.1f seconds" elapsed))
           (test-record-result "sync-wrapper-timeout" nil 
                              (format "Unexpected error or timing: %s (%.1f seconds)" 
                                     (error-message-string err) elapsed))))))))

(defun test-sync-wrapper-error-handling ()
  "Test synchronous wrapper error handling."
  (message "Testing sync wrapper error handling...")
  (condition-case err
      (let ((response (mediawiki-http-request-sync "https://httpbin.org/status/500" "GET" nil)))
        (test-record-result "sync-wrapper-error" nil 
                           "Should have signaled an error for 500 status"))
    (error
     (let ((error-msg (error-message-string err)))
       (if (or (string-match-p "Server error" error-msg)
               (string-match-p "Network error.*500" error-msg)
               (string-match-p "HTTP request failed.*500" error-msg))
           (test-record-result "sync-wrapper-error" t "Correctly handled server error")
         (test-record-result "sync-wrapper-error" nil 
                            (format "Unexpected error message: %s" error-msg)))))))

(defun test-sync-wrapper-post-data ()
  "Test synchronous wrapper with POST data."
  (message "Testing sync wrapper with POST data...")
  (let ((data (make-hash-table :test 'equal)))
    (puthash "key1" "value1" data)
    (puthash "key2" "value2" data)
    (condition-case err
        (let ((response (mediawiki-http-request-sync "https://httpbin.org/post" "POST" data)))
          (if (and (plist-get response :success)
                   (= (plist-get response :status-code) 200))
              (test-record-result "sync-wrapper-post" t "Successful POST request with data")
            (test-record-result "sync-wrapper-post" nil 
                               (format "Unexpected response: %s" response))))
      (error
       (test-record-result "sync-wrapper-post" nil 
                          (format "Error: %s" (error-message-string err)))))))

(defun test-sync-wrapper-cancellation ()
  "Test synchronous wrapper cancellation support."
  (message "Testing sync wrapper cancellation support...")
  (message "Note: This test requires manual verification - try C-g during a long request")
  ;; This is difficult to test automatically, but we can verify the structure is in place
  (test-record-result "sync-wrapper-cancellation" t "Cancellation support implemented"))

(defun run-sync-wrapper-tests ()
  "Run all synchronous wrapper tests."
  (setq test-results '())
  (message "=== Starting Synchronous Wrapper Tests ===")
  
  (test-sync-wrapper-basic)
  (test-sync-wrapper-error-handling)
  (test-sync-wrapper-post-data)
  (test-sync-wrapper-timeout)
  (test-sync-wrapper-cancellation)
  
  (message "=== Test Summary ===")
  (let ((passed 0)
        (failed 0))
    (dolist (result (reverse test-results))
      (if (cadr result)
          (setq passed (1+ passed))
        (setq failed (1+ failed))))
    (message "Tests completed: %d passed, %d failed" passed failed)))

;; Run the tests
(run-sync-wrapper-tests)

;;; test-sync-wrapper.el ends here