;;; test-http-post.el --- Test HTTP POST functionality -*- lexical-binding: t; -*-

(require 'mediawiki-http)
(require 'mediawiki-core)

(setq mediawiki-debug t)

(defun test-post-with-data ()
  "Test POST request with form data."
  (message "Testing POST request with data...")

  (let ((data (make-hash-table :test 'equal)))
    (puthash "key1" "value1" data)
    (puthash "key2" "value2" data)

    (condition-case err
        (let ((response (mediawiki-http-request-sync "https://httpbin.org/post" "POST" data)))
          (if (and (plist-get response :success)
                   (= (plist-get response :status-code) 200))
              (message "POST test: PASS - Successful POST request")
            (message "POST test: FAIL - Unexpected response: %s" response)))
      (error
       (message "POST test: FAIL - Error: %s" (error-message-string err))))))

(defun test-async-post-with-data ()
  "Test asynchronous POST request with form data."
  (message "Testing async POST request with data...")

  (let ((data (make-hash-table :test 'equal)))
    (puthash "async-key" "async-value" data)

    (mediawiki-http-request-async
     "https://httpbin.org/post"
     "POST"
     data
     ;; Success callback
     (lambda (response)
       (message "Async POST test: PASS - Successful async POST request"))
     ;; Error callback
     (lambda (response)
       (message "Async POST test: FAIL - Error: %s" (plist-get response :error))))))

;; Run tests
(test-post-with-data)
(test-async-post-with-data)
(message "POST tests completed")

;;; test-http-post.el ends here
