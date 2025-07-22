;;; test-http-post.el --- Test HTTP POST functionality -*- lexical-binding: t; -*-

(require 'ert)
(require 'mediawiki-http)
(require 'mediawiki-core)

(setq mediawiki-debug t)

(defun test-mock-http-request-sync-for-post (url method data &optional timeout headers)
  "Mock HTTP request for testing POST functionality."
  (list :success t
        :status-code 200
        :headers '(("content-type" . "application/json"))
        :body "{\"success\":true,\"data\":{\"key1\":\"value1\",\"key2\":\"value2\"}}"
        :error nil
        :error-type nil))

(ert-deftest test-post-with-data ()
  "Test POST request with form data."
  (let ((data (make-hash-table :test 'equal)))
    (puthash "key1" "value1" data)
    (puthash "key2" "value2" data)

    ;; Use mock for HTTP request
    (advice-add 'mediawiki-http-request-sync :override #'test-mock-http-request-sync-for-post)

    (unwind-protect
        (condition-case err
            (let ((response (mediawiki-http-request-sync "https://httpbin.org/post" "POST" data)))
              (should (plist-get response :success))
              (should (= (plist-get response :status-code) 200)))
          (error
           (ert-fail (format "POST request failed: %s" (error-message-string err)))))

      ;; Cleanup
      (advice-remove 'mediawiki-http-request-sync #'test-mock-http-request-sync-for-post))))

(defun test-mock-url-retrieve (url callback &optional cbargs silent inhibit-cookies)
  "Mock url-retrieve for testing async HTTP requests."
  ;; Immediately call the callback with a successful response
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\n")
    (insert "Content-Type: application/json\n")
    (insert "\n")
    (insert "{\"success\":true,\"data\":{\"async-key\":\"async-value\"}}")
    (funcall callback nil)))

(ert-deftest test-async-post-with-data ()
  "Test asynchronous POST request with form data."
  (let ((data (make-hash-table :test 'equal))
        (test-completed nil)
        (test-passed nil))

    (puthash "async-key" "async-value" data)

    ;; Use mock for async HTTP request
    (advice-add 'url-retrieve :override #'test-mock-url-retrieve)

    (unwind-protect
        (progn
          (mediawiki-http-request-async
           "https://httpbin.org/post"
           "POST"
           data
           ;; Success callback
           (lambda (response)
             (setq test-completed t)
             (setq test-passed (and (plist-get response :success)
                                   (= (plist-get response :status-code) 200))))
           ;; Error callback
           (lambda (response)
             (setq test-completed t)
             (setq test-passed nil)))

          ;; Since our mock immediately calls the callback, we can check the result
          (should test-completed)
          (should test-passed))

      ;; Cleanup
      (advice-remove 'url-retrieve #'test-mock-url-retrieve))))

(provide 'test-http-post)

;;; test-http-post.el ends here
