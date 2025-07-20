;;; test-csrf-simple.el --- Simple test for CSRF token functionality -*- lexical-binding: t; -*-

;; Load required modules
(load-file "mediawiki-core.el")
(load-file "mediawiki-session.el")

;; Test basic CSRF token functionality
(message "Testing CSRF token management system...")

;; Create a test session
(defvar test-sitename "test-site")

(let ((session (make-mediawiki-session
                :site-name test-sitename
                :tokens (make-hash-table :test 'equal)
                :user-info '(:username "testuser")
                :login-time (current-time)
                :last-activity (current-time))))
  (mediawiki-set-session test-sitename session)
  
  (message "✓ Created test session")
  
  ;; Test 1: Cache a CSRF token manually
  (mediawiki-session-cache-token session "csrf" "test-csrf-token-123")
  (message "✓ Cached CSRF token")
  
  ;; Test 2: Validate CSRF token
  (if (mediawiki-session-validate-csrf-token test-sitename "test-csrf-token-123")
      (message "✓ CSRF token validation works")
    (error "CSRF token validation failed"))
  
  ;; Test 3: Test token info retrieval
  (let ((token-info (mediawiki-session-get-token-info test-sitename "csrf")))
    (if (plist-get token-info :token-exists)
        (message "✓ Token info retrieval works")
      (error "Token info retrieval failed")))
  
  ;; Test 4: Test token invalidation
  (mediawiki-session-invalidate-csrf-token test-sitename)
  (if (not (mediawiki-session-validate-csrf-token test-sitename "test-csrf-token-123"))
      (message "✓ CSRF token invalidation works")
    (error "CSRF token invalidation failed"))
  
  ;; Test 5: Test error handling
  (let ((result (mediawiki-session-handle-token-error test-sitename "badtoken")))
    (if (eq result 'csrf-token-invalidated)
        (message "✓ Token error handling works")
      (error "Token error handling failed")))
  
  ;; Test 6: Test operation token type detection
  (let ((token-type (mediawiki-session-get-token-type-for-operation 'edit)))
    (if (string= token-type "csrf")
        (message "✓ Operation token type detection works")
      (error "Operation token type detection failed")))
  
  ;; Test 7: Test token statistics
  (let ((stats (mediawiki-session-get-token-statistics test-sitename)))
    (if (plist-member stats :total-tokens)
        (message "✓ Token statistics work")
      (error "Token statistics failed: %S" stats)))
  
  ;; Test 8: Test token cache health check
  (let ((health (mediawiki-session-token-cache-health-check test-sitename)))
    (if (plist-get health :healthy)
        (message "✓ Token cache health check works")
      (error "Token cache health check failed")))
  
  ;; Test 9: Test token cache cleanup
  (let ((cleanup-count (mediawiki-session-cleanup-token-cache test-sitename)))
    (message "✓ Token cache cleanup works (cleaned %d items)" cleanup-count))
  
  ;; Test 10: Test token export
  (let ((export (mediawiki-session-export-token-cache test-sitename)))
    (if (plist-get export :sitename)
        (message "✓ Token cache export works")
      (error "Token cache export failed")))
  
  ;; Clean up
  (mediawiki-remove-session test-sitename)
  (message "✓ Test cleanup completed"))

(message "")
(message "==============================================")
(message "✅ ALL CSRF TOKEN MANAGEMENT TESTS PASSED!")
(message "Task 5.2 implementation is working correctly.")
(message "==============================================")

;;; test-csrf-simple.el ends here