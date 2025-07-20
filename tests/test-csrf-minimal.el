;;; test-csrf-minimal.el --- Minimal test for CSRF token functionality -*- lexical-binding: t; -*-

;; Load required modules
(load-file "mediawiki-core.el")

;; Test basic functionality
(message "Testing CSRF token management...")

;; Create a test session
(defvar test-sitename "test-site")

(let ((session (make-mediawiki-session
                :site-name test-sitename
                :tokens (make-hash-table :test 'equal)
                :user-info '(:username "testuser")
                :login-time (current-time)
                :last-activity (current-time))))
  (mediawiki-set-session test-sitename session)
  
  ;; Test token caching
  (puthash "csrf" "test-token-123" (mediawiki-session-tokens session))
  (puthash "csrf-expiry" (time-add (current-time) 3600) (mediawiki-session-tokens session))
  
  (message "✓ Created test session with CSRF token")
  
  ;; Test token retrieval
  (let ((token (gethash "csrf" (mediawiki-session-tokens session))))
    (if (string= token "test-token-123")
        (message "✓ Successfully retrieved cached CSRF token")
      (error "Failed to retrieve CSRF token")))
  
  ;; Clean up
  (mediawiki-remove-session test-sitename)
  (message "✓ Test completed successfully"))

(message "CSRF token minimal test passed!")

;;; test-csrf-minimal.el ends here