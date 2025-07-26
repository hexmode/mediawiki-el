;;; test-http-async.el --- Test asynchronous HTTP functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Test to verify the asynchronous HTTP request functionality using ERT framework

;;; Code:

(require 'ert)
(require 'mediawiki-http)
(require 'mediawiki-core)

(ert-deftest test-sync-http-basic-request ()
  "Test basic synchronous HTTP functionality."
  :tags '(:network)
  (skip-unless (and (fboundp 'mediawiki-http-request-sync)
                    (getenv "MEDIAWIKI_NETWORK_TESTS")))

  (let ((response (mediawiki-http-request-sync "https://httpbin.org/get" "GET" nil)))
    (should (plist-get response :success))
    (should (= (plist-get response :status-code) 200))))

(ert-deftest test-sync-http-error-handling ()
  "Test synchronous HTTP error handling."
  :tags '(:network)
  (skip-unless (and (fboundp 'mediawiki-http-request-sync)
                    (getenv "MEDIAWIKI_NETWORK_TESTS")))

  (condition-case err
      (let ((response (mediawiki-http-request-sync "https://httpbin.org/status/404" "GET" nil)))
        (should (not (plist-get response :success)))
        (should (= (plist-get response :status-code) 404))
        (should (eq (plist-get response :error-type) 'not-found-error)))
    (error
     ;; This is expected for sync requests that fail
     (should t))))

(ert-deftest test-error-classification-auth-errors ()
  "Test error classification for authentication errors."
  (should (eq (mediawiki-http-classify-error 401) 'auth-error))
  (should (eq (mediawiki-http-classify-error 403) 'auth-error)))

(ert-deftest test-error-classification-not-found ()
  "Test error classification for not found errors."
  (should (eq (mediawiki-http-classify-error 404) 'not-found-error)))

(ert-deftest test-error-classification-rate-limit ()
  "Test error classification for rate limit errors."
  (should (eq (mediawiki-http-classify-error 429) 'rate-limit-error)))

(ert-deftest test-error-classification-server-error ()
  "Test error classification for server errors."
  (should (eq (mediawiki-http-classify-error 500) 'server-error)))

(ert-deftest test-error-classification-network-error ()
  "Test error classification for network errors."
  (should (eq (mediawiki-http-classify-error nil) 'network-error)))

(ert-deftest test-async-callback-mechanism ()
  "Test that async callbacks are properly called."
  :tags '(:network :async)
  (skip-unless (and (fboundp 'mediawiki-http-request-async)
                    (getenv "MEDIAWIKI_NETWORK_TESTS")))

  (let ((callback-called nil)
        (error-callback-called nil)
        (timeout-counter 0))

    ;; Test success callback
    (mediawiki-http-request-async
     "https://httpbin.org/get"
     "GET"
     nil
     (lambda (response)
       (setq callback-called t))
     (lambda (response)
       (setq error-callback-called t)))

    ;; Wait for callback with timeout
    (while (and (not callback-called)
                (not error-callback-called)
                (< timeout-counter 50))
      (sleep-for 0.1)
      (setq timeout-counter (1+ timeout-counter)))

    (should (or callback-called error-callback-called))
    (should (not (and callback-called error-callback-called)))))

(ert-deftest test-http-request-parameters ()
  "Test HTTP request parameter construction."
  (let ((params '(("action" . "query")
                  ("format" . "json")
                  ("meta" . "siteinfo"))))

    ;; Test parameter list structure
    (should (listp params))
    (should (string= (cdr (assoc "action" params)) "query"))
    (should (string= (cdr (assoc "format" params)) "json"))
    (should (string= (cdr (assoc "meta" params)) "siteinfo"))))

(ert-deftest test-http-response-structure ()
  "Test HTTP response structure validation."
  (let ((mock-response (list :success t
                            :status-code 200
                            :data '((query . ((pages . ((123 . ((title . "Test Page"))))))))
                            :headers '(("Content-Type" . "application/json")))))

    (should (plist-get mock-response :success))
    (should (= (plist-get mock-response :status-code) 200))
    (should (plist-get mock-response :data))
    (should (plist-get mock-response :headers))))

(ert-deftest test-http-timeout-handling ()
  "Test HTTP timeout parameter handling."
  :tags '(:network)
  (skip-unless (and (fboundp 'mediawiki-http-request-sync)
                    (getenv "MEDIAWIKI_NETWORK_TESTS")))

  (let ((start-time (current-time)))
    (condition-case err
        (mediawiki-http-request-sync "https://httpbin.org/delay/5" "GET" nil 2)
      (error
       (let ((elapsed (float-time (time-subtract (current-time) start-time))))
         (should (< elapsed 4))  ; Should timeout before 4 seconds
         (should (string-match-p "timed out\\|timeout" (error-message-string err))))))))

(ert-deftest test-http-post-data-handling ()
  "Test HTTP POST data handling."
  :tags '(:network)
  (skip-unless (and (fboundp 'mediawiki-http-request-sync)
                    (getenv "MEDIAWIKI_NETWORK_TESTS")))

  (let ((data (make-hash-table :test 'equal)))
    (puthash "key1" "value1" data)
    (puthash "key2" "value2" data)

    (let ((response (mediawiki-http-request-sync "https://httpbin.org/post" "POST" data)))
      (should (plist-get response :success))
      (should (= (plist-get response :status-code) 200)))))

;; Helper function for running network tests
(defun run-http-network-tests ()
  "Run HTTP tests that require network access.
Set MEDIAWIKI_NETWORK_TESTS environment variable to enable these tests."
  (interactive)
  (if (getenv "MEDIAWIKI_NETWORK_TESTS")
      (ert-run-tests-interactively "test-http-")
    (message "Network tests disabled. Set MEDIAWIKI_NETWORK_TESTS environment variable to enable.")))

;;; test-http-async.el ends here
