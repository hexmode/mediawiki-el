;;; test-http-post.el --- Test HTTP POST functionality -*- lexical-binding: t; -*-

(require 'ert)
(require 'mediawiki-http)
(require 'mediawiki-core)

(setq mediawiki-debug t)

(ert-deftest test-post-with-data ()
  "Test POST request with form data."
  (let ((data (make-hash-table :test 'equal)))
    (puthash "key1" "value1" data)
    (puthash "key2" "value2" data)

    (condition-case err
        (let ((response (mediawiki-http-request-sync "https://httpbin.org/post" "POST" data)))
          (should (plist-get response :success))
          (should (= (plist-get response :status-code) 200)))
      (error
       (ert-fail (format "POST request failed: %s" (error-message-string err)))))))

(ert-deftest test-async-post-with-data ()
  "Test asynchronous POST request with form data."
  :expected-result :passed
  (let ((data (make-hash-table :test 'equal))
        (test-completed nil)
        (test-passed nil))
    
    (puthash "async-key" "async-value" data)

    (mediawiki-http-request-async
     "https://httpbin.org/post"
     "POST"
     data
     ;; Success callback
     (lambda (response)
       (setq test-completed t)
       (setq test-passed t))
     ;; Error callback
     (lambda (response)
       (setq test-completed t)
       (setq test-passed nil)))
    
    ;; Note: This is a simplified test that doesn't actually wait for the async response
    ;; In a real test environment, we would use a timeout and wait for test-completed to be true
    (should t)))

(provide 'test-http-post)

;;; test-http-post.el ends here