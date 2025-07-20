;;; test-csrf-token-management.el --- Test CSRF token management system -*- lexical-binding: t; -*-

;; Test implementation of task 5.2: Add token management system
;; Tests CSRF token handling, caching, refresh, validation, and error recovery

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

(defun test-csrf-token-basic-functionality ()
  "Test basic CSRF token functionality."
  (message "Testing basic CSRF token functionality...")
  
  ;; Set up test environment
  (test-csrf-setup-mock-session)
  
  ;; Mock the API call function
  (advice-add 'mediawiki-api-call-sync :override #'test-csrf-mock-api-call-sync)
  
  (unwind-protect
      (progn
        ;; Test 1: Get CSRF token (should trigger refresh since none cached)
        (let ((token (mediawiki-session-get-csrf-token test-csrf-sitename)))
          (unless (string= token test-csrf-mock-token)
            (error "Failed to get CSRF token: expected %s, got %s" 
                   test-csrf-mock-token token))
          (message "✓ Successfully retrieved CSRF token: %s" token))
        
        ;; Test 2: Validate the cached token
        (unless (mediawiki-session-validate-csrf-token test-csrf-sitename test-csrf-mock-token)
          (error "Failed to validate cached CSRF token"))
        (message "✓ Successfully validated CSRF token")
        
        ;; Test 3: Get token again (should use cached version)
        (let ((cached-token (mediawiki-session-get-csrf-token test-csrf-sitename)))
          (unless (string= cached-token test-csrf-mock-token)
            (error "Failed to get cached CSRF token"))
          (message "✓ Successfully retrieved cached CSRF token"))
        
        ;; Test 4: Get edit token (alias for CSRF token)
        (let ((edit-token (mediawiki-session-get-edit-token test-csrf-sitename)))
          (unless (string= edit-token test-csrf-mock-token)
            (error "Failed to get edit token alias"))
          (message "✓ Successfully retrieved edit token alias"))
        
        ;; Test 5: Validate edit token (alias for CSRF validation)
        (unless (mediawiki-session-validate-edit-token test-csrf-sitename test-csrf-mock-token)
          (error "Failed to validate edit token alias"))
        (message "✓ Successfully validated edit token alias")
        
        (message "✓ All basic CSRF token functionality tests passed"))
    
    ;; Cleanup
    (advice-remove 'mediawiki-api-call-sync #'test-csrf-mock-api-call-sync)
    (mediawiki-remove-session test-csrf-sitename)))

(defun test-csrf-token-invalidation-and-refresh ()
  "Test CSRF token invalidation and refresh functionality."
  (message "Testing CSRF token invalidation and refresh...")
  
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
        (when (mediawiki-session-validate-csrf-token test-csrf-sitename test-csrf-mock-token)
          (error "Token should be invalidated but validation still passes"))
        (message "✓ Successfully invalidated CSRF token")
        
        ;; Test 2: Force refresh CSRF token
        (let ((refreshed-token (mediawiki-session-refresh-csrf-token test-csrf-sitename)))
          (unless (string= refreshed-token test-csrf-mock-token)
            (error "Failed to refresh CSRF token"))
          (message "✓ Successfully refreshed CSRF token"))
        
        ;; Test 3: Ensure valid CSRF token
        (let ((valid-token (mediawiki-session-ensure-valid-csrf-token test-csrf-sitename)))
          (unless (string= valid-token test-csrf-mock-token)
            (error "Failed to ensure valid CSRF token"))
          (message "✓ Successfully ensured valid CSRF token"))
        
        (message "✓ All CSRF token invalidation and refresh tests passed"))
    
    ;; Cleanup
    (advice-remove 'mediawiki-api-call-sync #'test-csrf-mock-api-call-sync)
    (mediawiki-remove-session test-csrf-sitename)))

(defun test-csrf-token-error-handling ()
  "Test CSRF token error handling and recovery."
  (message "Testing CSRF token error handling and recovery...")
  
  ;; Set up test environment
  (test-csrf-setup-mock-session)
  
  (unwind-protect
      (progn
        ;; Test 1: Handle token errors
        (let ((result (mediawiki-session-handle-token-error test-csrf-sitename "badtoken")))
          (unless (eq result 'csrf-token-invalidated)
            (error "Failed to handle badtoken error correctly"))
          (message "✓ Successfully handled badtoken error"))
        
        (let ((result (mediawiki-session-handle-token-error test-csrf-sitename "sessionfailure")))
          (unless (eq result 'session-invalidated)
            (error "Failed to handle sessionfailure error correctly"))
          (message "✓ Successfully handled sessionfailure error"))
        
        (let ((result (mediawiki-session-handle-token-error test-csrf-sitename "mustbeloggedin")))
          (unless (eq result 'authentication-required)
            (error "Failed to handle mustbeloggedin error correctly"))
          (message "✓ Successfully handled mustbeloggedin error"))
        
        ;; Test 2: Validate token for operations
        (let ((validation (mediawiki-session-validate-token-for-operation test-csrf-sitename 'edit)))
          (unless (plist-get validation :token-type)
            (error "Failed to validate token for edit operation"))
          (message "✓ Successfully validated token for edit operation"))
        
        ;; Test 3: Get token type for operations
        (unless (string= (mediawiki-session-get-token-type-for-operation 'edit) "csrf")
          (error "Failed to get correct token type for edit operation"))
        (message "✓ Successfully got token type for edit operation")
        
        (message "✓ All CSRF token error handling tests passed"))
    
    ;; Cleanup
    (mediawiki-remove-session test-csrf-sitename)))

(defun test-csrf-token-caching-and-expiry ()
  "Test CSRF token caching and expiry functionality."
  (message "Testing CSRF token caching and expiry...")
  
  ;; Set up test environment
  (let ((session (test-csrf-setup-mock-session)))
    
    (unwind-protect
        (progn
          ;; Test 1: Cache a token manually
          (mediawiki-session-cache-token session "csrf" test-csrf-mock-token)
          (let ((cached-token (gethash "csrf" (mediawiki-session-tokens session))))
            (unless (string= cached-token test-csrf-mock-token)
              (error "Failed to cache CSRF token"))
            (message "✓ Successfully cached CSRF token"))
          
          ;; Test 2: Check token info
          (let ((token-info (mediawiki-session-get-token-info test-csrf-sitename "csrf")))
            (unless (plist-get token-info :token-exists)
              (error "Token info should show token exists"))
            (message "✓ Successfully retrieved token info"))
          
          ;; Test 3: Get all token info
          (let ((all-info (mediawiki-session-get-all-token-info test-csrf-sitename)))
            (unless all-info
              (error "Should have token info available"))
            (message "✓ Successfully retrieved all token info"))
          
          ;; Test 4: Get token statistics
          (let ((stats (mediawiki-session-get-token-statistics test-csrf-sitename)))
            (unless (> (plist-get stats :total-tokens) 0)
              (error "Should have at least one token in statistics"))
            (message "✓ Successfully retrieved token statistics"))
          
          ;; Test 5: Token cache health check
          (let ((health (mediawiki-session-token-cache-health-check test-csrf-sitename)))
            (unless (plist-get health :healthy)
              (error "Token cache should be healthy"))
            (message "✓ Token cache health check passed"))
          
          ;; Test 6: Clear specific token
          (mediawiki-session-clear-token test-csrf-sitename "csrf")
          (let ((token-info (mediawiki-session-get-token-info test-csrf-sitename "csrf")))
            (when (plist-get token-info :token-exists)
              (error "Token should be cleared"))
            (message "✓ Successfully cleared specific token"))
          
          (message "✓ All CSRF token caching and expiry tests passed"))
      
      ;; Cleanup
      (mediawiki-remove-session test-csrf-sitename))))

(defun test-csrf-token-advanced-features ()
  "Test advanced CSRF token management features."
  (message "Testing advanced CSRF token management features...")
  
  ;; Set up test environment
  (test-csrf-setup-mock-session)
  
  ;; Mock the API call function
  (advice-add 'mediawiki-api-call-sync :override #'test-csrf-mock-api-call-sync)
  
  (unwind-protect
      (progn
        ;; Test 1: Preload tokens
        (let ((result (mediawiki-session-preload-tokens test-csrf-sitename '("csrf" "login"))))
          (unless (plist-get result :loaded)
            (error "Failed to preload tokens"))
          (message "✓ Successfully preloaded tokens"))
        
        ;; Test 2: Prepare operation token
        (let ((token (mediawiki-session-prepare-operation-token test-csrf-sitename 'edit)))
          (unless token
            (error "Failed to prepare operation token"))
          (message "✓ Successfully prepared operation token"))
        
        ;; Test 3: Export token cache (for debugging)
        (let ((export (mediawiki-session-export-token-cache test-csrf-sitename)))
          (unless (plist-get export :sitename)
            (error "Failed to export token cache"))
          (message "✓ Successfully exported token cache"))
        
        ;; Test 4: Cleanup token cache
        (let ((cleanup-count (mediawiki-session-cleanup-token-cache test-csrf-sitename)))
          (message "✓ Successfully cleaned up token cache (%d items)" cleanup-count))
        
        (message "✓ All advanced CSRF token management tests passed"))
    
    ;; Cleanup
    (advice-remove 'mediawiki-api-call-sync #'test-csrf-mock-api-call-sync)
    (mediawiki-remove-session test-csrf-sitename)))

(defun test-csrf-token-integration ()
  "Test CSRF token integration with API operations."
  (message "Testing CSRF token integration with API operations...")
  
  ;; Set up test environment
  (test-csrf-setup-mock-session)
  
  ;; Mock the API call function
  (advice-add 'mediawiki-api-call-sync :override #'test-csrf-mock-api-call-sync)
  
  (unwind-protect
      (progn
        ;; Test 1: Get token with recovery (should work normally)
        (let ((token (mediawiki-session-get-token-with-recovery test-csrf-sitename "csrf")))
          (unless token
            (error "Failed to get token with recovery"))
          (message "✓ Successfully got token with recovery"))
        
        ;; Test 2: Handle operation token error
        (let ((mock-error-response (make-mediawiki-api-response
                                   :success nil
                                   :errors (list (list :code "badtoken" :info "Bad token")))))
          (let ((result (mediawiki-session-handle-operation-token-error 
                        test-csrf-sitename 'edit mock-error-response)))
            (unless (eq (plist-get result :action) 'token-invalidated)
              (error "Failed to handle operation token error"))
            (message "✓ Successfully handled operation token error")))
        
        (message "✓ All CSRF token integration tests passed"))
    
    ;; Cleanup
    (advice-remove 'mediawiki-api-call-sync #'test-csrf-mock-api-call-sync)
    (mediawiki-remove-session test-csrf-sitename)))

;;; Main Test Runner

(defun test-csrf-token-management-all ()
  "Run all CSRF token management tests."
  (interactive)
  (message "Starting CSRF Token Management System Tests")
  (message "==============================================")
  
  (condition-case err
      (progn
        (test-csrf-token-basic-functionality)
        (message "")
        (test-csrf-token-invalidation-and-refresh)
        (message "")
        (test-csrf-token-error-handling)
        (message "")
        (test-csrf-token-caching-and-expiry)
        (message "")
        (test-csrf-token-advanced-features)
        (message "")
        (test-csrf-token-integration)
        (message "")
        (message "==============================================")
        (message "✅ ALL CSRF TOKEN MANAGEMENT TESTS PASSED!")
        (message "Task 5.2 implementation is working correctly.")
        t)
    (error
     (message "")
     (message "==============================================")
     (message "❌ CSRF TOKEN MANAGEMENT TEST FAILED!")
     (message "Error: %s" (error-message-string err))
     (message "Task 5.2 implementation needs fixes.")
     nil)))

;; Run the tests when this file is loaded
(when (called-interactively-p 'any)
  (test-csrf-token-management-all))

(provide 'test-csrf-token-management)

;;; test-csrf-token-management.el ends here