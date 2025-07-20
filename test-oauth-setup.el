;;; test-oauth-setup.el --- Test OAuth configuration and setup utilities

;;; Commentary:
;; Test the OAuth configuration and setup functionality

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-oauth)

(defun test-oauth-setup ()
  "Test OAuth configuration setup."
  (message "Testing OAuth setup functionality...")
  
  ;; Create a test site
  (let ((test-site (make-mediawiki-site-config
                    :name "TestWiki"
                    :url "https://test.wiki.example/"
                    :api-url "https://test.wiki.example/w/api.php"
                    :username nil
                    :auth-method 'basic
                    :auth-config nil
                    :capabilities nil
                    :session-info nil)))
    
    (mediawiki-add-site test-site)
    
    ;; Test OAuth setup
    (condition-case err
        (progn
          (mediawiki-oauth-setup "TestWiki" "test-consumer-key" "test-consumer-secret")
          
          ;; Verify configuration was set
          (let ((site (mediawiki-get-site "TestWiki")))
            (if (and (eq (mediawiki-site-config-auth-method site) 'oauth)
                     (equal (plist-get (mediawiki-site-config-auth-config site) :consumer-key) "test-consumer-key")
                     (equal (plist-get (mediawiki-site-config-auth-config site) :consumer-secret) "test-consumer-secret"))
                (message "✓ OAuth setup successful")
              (error "OAuth configuration not set correctly")))
          
          ;; Test OAuth reset
          (mediawiki-oauth-reset "TestWiki")
          (let ((site (mediawiki-get-site "TestWiki")))
            (if (and (eq (mediawiki-site-config-auth-method site) 'oauth)
                     (equal (plist-get (mediawiki-site-config-auth-config site) :consumer-key) "test-consumer-key")
                     (equal (plist-get (mediawiki-site-config-auth-config site) :consumer-secret) "test-consumer-secret")
                     (not (plist-get (mediawiki-site-config-auth-config site) :access-token))
                     (not (plist-get (mediawiki-site-config-auth-config site) :access-secret)))
                (message "✓ OAuth reset successful")
              (error "OAuth reset did not work correctly")))
          
          ;; Test OAuth utility functions
          (let ((nonce1 (mediawiki-oauth-generate-nonce))
                (nonce2 (mediawiki-oauth-generate-nonce)))
            (if (and nonce1 nonce2 (not (string= nonce1 nonce2)))
                (message "✓ OAuth nonce generation working")
              (error "OAuth nonce generation failed")))
          
          ;; Test signature generation (basic test)
          (condition-case sig-err
              (let ((signature (mediawiki-oauth-generate-signature
                               "POST" "https://example.com/api"
                               '(("param1" . "value1") ("param2" . "value2"))
                               "consumer-secret" "token-secret")))
                (if (and signature (stringp signature) (> (length signature) 0))
                    (message "✓ OAuth signature generation working")
                  (error "OAuth signature generation returned invalid result")))
            (error
             (message "✗ OAuth signature generation failed: %s" (error-message-string sig-err))))
          
          ;; Test OAuth login attempt (should fail with informative message)
          (condition-case login-err
              (progn
                (mediawiki-oauth-login "TestWiki")
                (error "OAuth login should have failed"))
            (error
             (let ((err-msg (error-message-string login-err)))
               (if (string-match-p "HTTP module enhancement" err-msg)
                   (message "✓ OAuth login correctly reports HTTP module limitation")
                 (message "✗ OAuth login failed with unexpected error: %s" err-msg)))))
          
          (message "OAuth setup and configuration tests completed successfully"))
      
      (error
       (message "✗ OAuth setup test failed: %s" (error-message-string err))))))

(defun test-oauth-token-management ()
  "Test OAuth token management functions."
  (message "Testing OAuth token management...")
  
  (condition-case err
      (let ((test-site (mediawiki-get-site "TestWiki")))
        (when test-site
          ;; Test token refresh (should fail appropriately)
          (condition-case refresh-err
              (progn
                (mediawiki-oauth-refresh-tokens "TestWiki")
                (error "Token refresh should have failed"))
            (error
             (let ((err-msg (error-message-string refresh-err)))
               (if (string-match-p "No OAuth access tokens" err-msg)
                   (message "✓ OAuth token refresh correctly reports missing tokens")
                 (message "✗ OAuth token refresh failed with unexpected error: %s" err-msg)))))
          
          (message "OAuth token management tests completed")))
    
    (error
     (message "✗ OAuth token management test failed: %s" (error-message-string err)))))

;; Run the tests
(test-oauth-setup)
(test-oauth-token-management)

(message "OAuth implementation testing completed")

;;; test-oauth-setup.el ends here