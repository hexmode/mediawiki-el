;;; test-csrf-token-management.el --- Test CSRF token management system -*- lexical-binding: t; -*-

;; Test implementation of task 5.2: Add token management system
;; Tests CSRF token handling, caching, refresh, validation, and error recovery

(require 'ert)
(require 'mediawiki-session)
(require 'mediawiki-core)
(require 'mediawiki-api)

;;; Test Configuration

(defvar test-csrf-sitename "test-wiki"
  "Test site name for CSRF token tests.")

(defvar test-csrf-mock-token "test-csrf-token-12345"
  "Mock CSRF token for testing.")

(defvar test-csrf-mock-expired-token "expired-csrf-token-67890"
  "Mock expired CSRF token for testing.")

;;; Mock Functions for Testing

(defun test-csrf-setup-mock-session ()
  "Set up a mock session for testing CSRF token functionality."
  (let ((session (make-mediawiki-session
                  :site-name test-csrf-sitename
                  :tokens (make-hash-table :test 'equal)
                  :user-info '(:username "testuser" :userid 123)
                  :login-time (current-time)
                  :last-activity (current-time))))
    (mediawiki-set-session test-csrf-sitename session)
    session))

(defun test-csrf-mock-api-call-sync (sitename action params &optional timeout)
  "Mock API call for testing token refresh."
  (cond
   ;; Mock successful token request
   ((and (string= action "query")
         (assoc "meta" params)
         (string= (cdr (assoc "meta" params)) "tokens"))
    (let ((token-type (cdr (assoc "type" params))))
      (make-mediawiki-api-response
       :success t
       :data `((query . ((tokens . ((,(intern (concat token-type "token")) . ,test-csrf-mock-token)))))))))

   ;; Mock failed token request (authentication error)
   ((and (string= action "query")
         (string= sitename "auth-fail-site"))
    (make-mediawiki-api-response
     :success nil
     :errors (list (list :code "mustbeloggedin" :info "You must be logged in"))))

   ;; Default successful response
   (t
    (make-mediawiki-api-response
     :success t
     :data '((result . "Success"))))))

;;; Test Functions

(ert-deftest test-csrf-token-basic-functionality ()
  "Test basic CSRF token functionality."
  ;; Set up test environment
  (test-csrf-setup-mock-session)

  ;; Mock the API call function
  (advice-add 'mediawiki-api-call-sync :override #'test-csrf-mock-api-call-sync)

  (unwind-protect
      (progn
        ;; Test 1: Get CSRF token (should trigger refresh since none cached)
        (let ((token (mediawiki-session-get-csrf-token test-csrf-sitename)))
          (should (string= token test-csrf-mock-token)))

        ;; Test 2: Validate the cached token
        (should (mediawiki-session-validate-csrf-token test-csrf-sitename test-csrf-mock-token))

        ;; Test 3: Get token again (should use cached version)
        (let ((cached-token (mediawiki-session-get-csrf-token test-csrf-sitename)))
          (should (string= cached-token test-csrf-mock-token)))

        ;; Test 4: Get edit token (alias for CSRF token)
        (let ((edit-token (mediawiki-session-get-edit-token test-csrf-sitename)))
          (should (string= edit-token test-csrf-mock-token)))

        ;; Test 5: Validate edit token (alias for CSRF validation)
        (should (mediawiki-session-validate-edit-token test-csrf-sitename test-csrf-mock-token)))

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-sync #'test-csrf-mock-api-call-sync)
    (mediawiki-remove-session test-csrf-sitename)))

(ert-deftest test-csrf-token-invalidation-and-refresh ()
  "Test CSRF token invalidation and refresh functionality."
  ;; Set up test environment
  (test-csrf-setup-mock-session)

  ;; Mock the API call function
  (advice-add 'mediawiki-api-call-sync :override #'test-csrf-mock-api-call-sync)

  (unwind-protect
      (progn
        ;; First get a token to cache it
        (mediawiki-session-get-csrf-token test-csrf-sitename)

        ;; Test 1: Invalidate CSRF token
        (mediawiki-session-invalidate-csrf-token test-csrf-sitename)
        (should-not (mediawiki-session-validate-csrf-token test-csrf-sitename test-csrf-mock-token))

        ;; Test 2: Force refresh CSRF token
        (let ((refreshed-token (mediawiki-session-refresh-csrf-token test-csrf-sitename)))
          (should (string= refreshed-token test-csrf-mock-token)))

        ;; Test 3: Ensure valid CSRF token
        (let ((valid-token (mediawiki-session-ensure-valid-csrf-token test-csrf-sitename)))
          (should (string= valid-token test-csrf-mock-token))))

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-sync #'test-csrf-mock-api-call-sync)
    (mediawiki-remove-session test-csrf-sitename)))

(ert-deftest test-csrf-token-error-handling ()
  "Test CSRF token error handling and recovery."
  ;; Set up test environment
  (test-csrf-setup-mock-session)

  (unwind-protect
      (progn
        ;; Test 1: Handle token errors
        (let ((result (mediawiki-session-handle-token-error test-csrf-sitename "badtoken")))
          (should (eq result 'csrf-token-invalidated)))

        (let ((result (mediawiki-session-handle-token-error test-csrf-sitename "sessionfailure")))
          (should (eq result 'session-invalidated)))

        (let ((result (mediawiki-session-handle-token-error test-csrf-sitename "mustbeloggedin")))
          (should (eq result 'authentication-required)))

        ;; Test 2: Validate token for operations
        (let ((validation (mediawiki-session-validate-token-for-operation test-csrf-sitename 'edit)))
          (should (plist-get validation :token-type)))

        ;; Test 3: Get token type for operations
        (should (string= (mediawiki-session-get-token-type-for-operation 'edit) "csrf")))

    ;; Cleanup
    (mediawiki-remove-session test-csrf-sitename)))

(ert-deftest test-csrf-token-caching-and-expiry ()
  "Test CSRF token caching and expiry functionality."
  ;; Set up test environment
  (let ((session (test-csrf-setup-mock-session)))

    (unwind-protect
        (progn
          ;; Test 1: Cache a token manually
          (mediawiki-session-cache-token session "csrf" test-csrf-mock-token)
          (let ((cached-token (gethash "csrf" (mediawiki-session-tokens session))))
            (should (string= cached-token test-csrf-mock-token)))

          ;; Test 2: Check token info
          (let ((token-info (mediawiki-session-get-token-info test-csrf-sitename "csrf")))
            (should (plist-get token-info :token-exists)))

          ;; Test 3: Get all token info
          (let ((all-info (mediawiki-session-get-all-token-info test-csrf-sitename)))
            (should all-info))

          ;; Test 4: Get token statistics
          (let ((stats (mediawiki-session-get-token-statistics test-csrf-sitename)))
            (should (> (plist-get stats :total-tokens) 0)))

          ;; Test 5: Token cache health check
          (let ((health (mediawiki-session-token-cache-health-check test-csrf-sitename)))
            (should (plist-get health :healthy)))

          ;; Test 6: Clear specific token
          (mediawiki-session-clear-token test-csrf-sitename "csrf")
          (let ((token-info (mediawiki-session-get-token-info test-csrf-sitename "csrf")))
            (should-not (plist-get token-info :token-exists))))

      ;; Cleanup
      (mediawiki-remove-session test-csrf-sitename))))

(ert-deftest test-csrf-token-advanced-features ()
  "Test advanced CSRF token management features."
  ;; Set up test environment
  (test-csrf-setup-mock-session)

  ;; Mock the API call function
  (advice-add 'mediawiki-api-call-sync :override #'test-csrf-mock-api-call-sync)

  (unwind-protect
      (progn
        ;; Test 1: Preload tokens
        (let ((result (mediawiki-session-preload-tokens test-csrf-sitename '("csrf" "login"))))
          (should (plist-get result :loaded)))

        ;; Test 2: Prepare operation token
        (let ((token (mediawiki-session-prepare-operation-token test-csrf-sitename 'edit)))
          (should token))

        ;; Test 3: Export token cache (for debugging)
        (let ((export (mediawiki-session-export-token-cache test-csrf-sitename)))
          (should (plist-get export :sitename)))

        ;; Test 4: Cleanup token cache
        (let ((cleanup-count (mediawiki-session-cleanup-token-cache test-csrf-sitename)))
          (should (numberp cleanup-count))))

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-sync #'test-csrf-mock-api-call-sync)
    (mediawiki-remove-session test-csrf-sitename)))

(ert-deftest test-csrf-token-integration ()
  "Test CSRF token integration with API operations."
  ;; Set up test environment
  (test-csrf-setup-mock-session)

  ;; Mock the API call function
  (advice-add 'mediawiki-api-call-sync :override #'test-csrf-mock-api-call-sync)

  (unwind-protect
      (progn
        ;; Test 1: Get token with recovery (should work normally)
        (let ((token (mediawiki-session-get-token-with-recovery test-csrf-sitename "csrf")))
          (should token))

        ;; Test 2: Handle operation token error
        (let ((mock-error-response (make-mediawiki-api-response
                                   :success nil
                                   :errors (list (list :code "badtoken" :info "Bad token")))))
          (let ((result (mediawiki-session-handle-operation-token-error
                        test-csrf-sitename 'edit mock-error-response)))
            (should (eq (plist-get result :action) 'token-invalidated)))))

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-sync #'test-csrf-mock-api-call-sync)
    (mediawiki-remove-session test-csrf-sitename)))

(provide 'test-csrf-token-management)

;;; test-csrf-token-management.el ends here
