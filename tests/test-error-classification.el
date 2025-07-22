;;; test-error-classification.el --- Test error classification system -*- lexical-binding: t; -*-

;; Test implementation of task 7.1: Create comprehensive error classification system
;; Tests error creation, classification, and handling

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-errors)
(require 'mediawiki-api)
(require 'mediawiki-http)

;;; Test Error Creation

(ert-deftest test-error-create ()
  "Test basic error creation."
  (let ((error (mediawiki-error-create 'network 'error "Test error" "test-code")))
    (should (eq (mediawiki-error-category error) 'network))
    (should (eq (mediawiki-error-severity error) 'error))
    (should (string= (mediawiki-error-message error) "Test error"))
    (should (string= (mediawiki-error-code error) "test-code"))))

(ert-deftest test-error-create-from-api ()
  "Test error creation from API error code."
  (let ((error (mediawiki-error-create-from-api "badtoken" "Invalid CSRF token" '(:sitename "test"))))
    (should (eq (mediawiki-error-category error) 'auth))
    (should (eq (mediawiki-error-severity error) 'error))
    (should (eq (mediawiki-error-recovery error) 'retry))
    (should (string= (mediawiki-error-code error) "badtoken"))
    (should (string-match-p "Invalid token: Invalid CSRF token" (mediawiki-error-message error)))
    (should (eq (mediawiki-error-source error) 'api))))

(ert-deftest test-error-create-from-http ()
  "Test error creation from HTTP status code."
  (let ((error (mediawiki-error-create-from-http 404 "Page not found" '(:url "https://example.org"))))
    (should (eq (mediawiki-error-category error) 'client))
    (should (eq (mediawiki-error-severity error) 'error))
    (should (eq (mediawiki-error-recovery error) 'user))
    (should (= (mediawiki-error-code error) 404))
    (should (string-match-p "Not Found" (mediawiki-error-message error)))
    (should (eq (mediawiki-error-source error) 'http))))

(ert-deftest test-error-create-from-exception ()
  "Test error creation from Emacs error."
  (let ((error (mediawiki-error-create-from-exception 'file-error "File not found" nil)))
    (should (eq (mediawiki-error-category error) 'client))
    (should (eq (mediawiki-error-severity error) 'error))
    (should (eq (mediawiki-error-recovery error) 'user))
    (should (string= (mediawiki-error-code error) "file-error"))
    (should (string-match-p "Error: File not found" (mediawiki-error-message error)))
    (should (eq (mediawiki-error-source error) 'client))))

;;; Test Error Classification

(ert-deftest test-error-classification ()
  "Test error classification by category."
  ;; Network errors
  (let ((error (mediawiki-error-create-from-api "timeout" "Request timed out")))
    (should (eq (mediawiki-error-category error) 'network))
    (should (mediawiki-error-retryable-p error)))

  ;; Authentication errors
  (let ((error (mediawiki-error-create-from-api "badlogin" "Invalid username or password")))
    (should (eq (mediawiki-error-category error) 'auth))
    (should (mediawiki-error-requires-user-intervention-p error)))

  ;; Permission errors
  (let ((error (mediawiki-error-create-from-api "protectedpage" "Page is protected")))
    (should (eq (mediawiki-error-category error) 'permission))
    (should (mediawiki-error-requires-user-intervention-p error)))

  ;; Rate limiting errors
  (let ((error (mediawiki-error-create-from-api "ratelimited" "Too many requests")))
    (should (eq (mediawiki-error-category error) 'rate-limit))
    (should (mediawiki-error-retryable-p error)))

  ;; Edit conflicts
  (let ((error (mediawiki-error-create-from-api "editconflict" "Edit conflict detected")))
    (should (eq (mediawiki-error-category error) 'conflict))
    (should (mediawiki-error-requires-user-intervention-p error))))

;;; Test Error Message Formatting

(ert-deftest test-error-message-formatting ()
  "Test error message formatting."
  (let* ((error (mediawiki-error-create 'network 'error "Connection failed" "timeout"
                                      'retry nil 'http '(:url "https://example.org")))
         (basic-message (mediawiki-error-format-message error nil))
         (verbose-message (mediawiki-error-format-message error t)))

    ;; Basic message should be concise
    (should (string= basic-message "Connection failed"))

    ;; Verbose message should include more details
    (should (string-match-p "Category: Network errors" verbose-message))
    (should (string-match-p "Code: timeout" verbose-message))
    (should (string-match-p "Source: http" verbose-message))))

(ert-deftest test-error-solution-suggestions ()
  "Test error solution suggestions."
  ;; Authentication error
  (let ((error (mediawiki-error-create-from-api "badtoken" "Invalid token")))
    (should (string-match-p "refresh your authentication tokens"
                           (mediawiki-error-suggest-solution error))))

  ;; Rate limiting error
  (let ((error (mediawiki-error-create-from-api "ratelimited" "Too many requests")))
    (should (string-match-p "Wait a few minutes"
                           (mediawiki-error-suggest-solution error))))

  ;; Edit conflict
  (let ((error (mediawiki-error-create-from-api "editconflict" "Edit conflict")))
    (should (string-match-p "merge your changes"
                           (mediawiki-error-suggest-solution error)))))

;;; Test Error Handling

(defvar test-error-handling-retry-count 0
  "Counter for retry attempts in tests.")

(defun test-error-handling-retry-function (&rest _args)
  "Mock retry function for testing error handling."
  (setq test-error-handling-retry-count (1+ test-error-handling-retry-count))
  t)

(ert-deftest test-error-handling-retry ()
  "Test error handling with retries."
  (setq test-error-handling-retry-count 0)

  ;; Test retryable error
  (let* ((error (mediawiki-error-create-from-api "ratelimited" "Rate limited"))
         (options (list :retry-function #'test-error-handling-retry-function
                        :max-retries 2
                        :quiet t))
         (handled (mediawiki-error-handle error options)))

    (should handled)
    (should (= test-error-handling-retry-count 1))))

(ert-deftest test-error-handling-user-intervention ()
  "Test error handling requiring user intervention."
  (let* ((user-intervention-called nil)
         (error (mediawiki-error-create-from-api "permissiondenied" "Permission denied"))
         (options (list :on-user-intervention (lambda (_) (setq user-intervention-called t))
                        :quiet t))
         (handled (mediawiki-error-handle error options)))

    (should handled)
    (should user-intervention-called)))

(ert-deftest test-error-handling-fatal ()
  "Test handling of fatal errors."
  (let* ((fatal-called nil)
         (error (mediawiki-error-create 'server 'critical "Fatal server error" nil 'fatal))
         (options (list :on-fatal (lambda (_) (setq fatal-called t))
                        :quiet t))
         (handled (mediawiki-error-handle error options)))

    (should handled)
    (should fatal-called)))

;;; Test Integration with API and HTTP Layers

(defun test-mock-http-request-sync (url method data &optional timeout headers)
  "Mock HTTP request for testing error integration."
  (cond
   ;; Simulate network error
   ((string-match-p "network-error" url)
    (list :success nil
          :status-code nil
          :error "Network connection failed"
          :error-type 'network-error))

   ;; Simulate authentication error
   ((string-match-p "auth-error" url)
    (list :success nil
          :status-code 401
          :error "Authentication required"
          :error-type 'auth-error))

   ;; Simulate rate limiting
   ((string-match-p "rate-limit" url)
    (list :success nil
          :status-code 429
          :error "Rate limit exceeded"
          :error-type 'rate-limit-error))

   ;; Simulate server error
   ((string-match-p "server-error" url)
    (list :success nil
          :status-code 500
          :error "Internal server error"
          :error-type 'server-error))

   ;; Default success response
   (t
    (list :success t
          :status-code 200
          :body "{\"success\":true}"))))

(defun test-mock-api-parse-response (http-response)
  "Mock API response parsing for testing error integration."
  (if (plist-get http-response :success)
      (make-mediawiki-api-response
       :success t
       :data '((result . "Success")))

    ;; Create error response based on HTTP error
    (let ((error-code (plist-get http-response :error-type))
          (error-message (plist-get http-response :error)))
      (make-mediawiki-api-response
       :success nil
       :errors (list (list :code (symbol-name error-code)
                          :info error-message))
       :raw-response http-response))))

(defun test-mock-api-make-url (sitename)
  "Mock API URL creation for testing error integration."
  (cond
   ((string= sitename "network-error") "https://network-error.example.org/api.php")
   ((string= sitename "auth-error") "https://auth-error.example.org/api.php")
   ((string= sitename "rate-limit") "https://rate-limit.example.org/api.php")
   ((string= sitename "server-error") "https://server-error.example.org/api.php")
   (t (format "https://%s.example.org/api.php" sitename))))

(ert-deftest test-error-integration ()
  "Test integration of error system with API and HTTP layers."
  ;; Mock the HTTP and API functions
  (advice-add 'mediawiki-http-request-sync :override #'test-mock-http-request-sync)
  (advice-add 'mediawiki-api-parse-response :override #'test-mock-api-parse-response)
  (advice-add 'mediawiki-api-make-url :override #'test-mock-api-make-url)
  (advice-add 'mediawiki-get-site :override (lambda (sitename) t))

  (unwind-protect
      (progn
        ;; Test network error
        (let* ((sitename "network-error")
               (action "query")
               (params '(("titles" . "Test")))
               (options (list :context (list :sitename sitename :action action)
                             :quiet t))
               (response (mediawiki-api-call-with-error-handling
                         sitename action params options)))

          ;; Should return nil for error
          (should-not response))

        ;; Test authentication error
        (let* ((sitename "auth-error")
               (action "query")
               (params '(("titles" . "Test")))
               (user-intervention-called nil)
               (options (list :context (list :sitename sitename :action action)
                             :on-error (lambda (_err)
                                        (setq user-intervention-called t))
                             :quiet t))
               (response (mediawiki-api-call-with-error-handling
                         sitename action params options)))

          ;; Should return nil for error and call user intervention
          (should-not response)
          (should user-intervention-called)))

    ;; Cleanup
    (advice-remove 'mediawiki-http-request-sync #'test-mock-http-request-sync)
    (advice-remove 'mediawiki-api-parse-response #'test-mock-api-parse-response)
    (advice-remove 'mediawiki-api-make-url #'test-mock-api-make-url)
    (advice-remove 'mediawiki-get-site #'(lambda (sitename) t))))

(provide 'test-error-classification)

;;; test-error-classification.el ends here
