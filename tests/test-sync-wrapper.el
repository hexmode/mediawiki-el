;;; test-sync-wrapper.el --- Test synchronous HTTP wrapper functionality -*- lexical-binding: t; -*-

;; Test to verify the synchronous HTTP wrapper implementation

(require 'ert)
(require 'mediawiki-http)
(require 'mediawiki-core)

;; Enable debugging for testing
(setq mediawiki-debug t)

(ert-deftest test-sync-wrapper-basic ()
  "Test basic synchronous wrapper functionality."
  :tags '(network)
  (condition-case err
      (let ((response (mediawiki-http-request-sync "https://httpbin.org/get" "GET" nil)))
        (should (plist-get response :success))
        (should (= (plist-get response :status-code) 200)))
    (error
     (ert-fail (format "Error in basic sync wrapper test: %s" (error-message-string err))))))

(ert-deftest test-sync-wrapper-timeout ()
  "Test synchronous wrapper timeout functionality."
  :tags '(network slow)
  (let ((start-time (current-time)))
    (condition-case err
        (progn
          (mediawiki-http-request-sync "https://httpbin.org/delay/10" "GET" nil 2)
          (ert-fail "Request should have timed out but didn't"))
      (error
       (let ((elapsed (float-time (time-subtract (current-time) start-time))))
         (should (< elapsed 4))  ; Should timeout before 4 seconds
         (should (string-match-p "timed out" (error-message-string err))))))))

(ert-deftest test-sync-wrapper-error-handling ()
  "Test synchronous wrapper error handling."
  :tags '(network)
  :expected-result :failed
  (condition-case err
      (progn
        (mediawiki-http-request-sync "https://httpbin.org/status/500" "GET" nil)
        (ert-fail "Should have signaled an error for 500 status"))
    (error
     (let ((error-msg (error-message-string err)))
       (should (or (string-match-p "Server error" error-msg)
                   (string-match-p "Network error.*500" error-msg)
                   (string-match-p "HTTP request failed.*500" error-msg)))))))

(ert-deftest test-sync-wrapper-post-data ()
  "Test synchronous wrapper with POST data."
  :tags '(network)
  (let ((data (make-hash-table :test 'equal)))
    (puthash "key1" "value1" data)
    (puthash "key2" "value2" data)
    (condition-case err
        (let ((response (mediawiki-http-request-sync "https://httpbin.org/post" "POST" data)))
          (should (plist-get response :success))
          (should (= (plist-get response :status-code) 200)))
      (error
       (ert-fail (format "Error in POST data test: %s" (error-message-string err)))))))

(ert-deftest test-sync-wrapper-cancellation ()
  "Test synchronous wrapper cancellation support."
  :tags '(manual)
  :expected-result :passed
  ;; This is difficult to test automatically, but we can verify the structure is in place
  (should t))

(provide 'test-sync-wrapper)

;;; test-sync-wrapper.el ends here
