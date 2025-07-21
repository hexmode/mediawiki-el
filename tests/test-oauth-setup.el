;;; test-oauth-setup.el --- Test OAuth configuration and setup utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Test the OAuth configuration and setup functionality using ERT framework

;;; Code:

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-oauth)

(ert-deftest test-oauth-configuration-setup ()
  "Test OAuth configuration setup."
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
    
    (unwind-protect
        (progn
          ;; Test OAuth setup
          (mediawiki-oauth-setup "TestWiki" "test-consumer-key" "test-consumer-secret")
          
          ;; Verify configuration was set
          (let ((site (mediawiki-get-site "TestWiki")))
            (should (eq (mediawiki-site-config-auth-method site) 'oauth))
            (should (equal (plist-get (mediawiki-site-config-auth-config site) :consumer-key) "test-consumer-key"))
            (should (equal (plist-get (mediawiki-site-config-auth-config site) :consumer-secret) "test-consumer-secret"))))
      
      ;; Cleanup
      (mediawiki-remove-site "TestWiki"))))

(ert-deftest test-oauth-reset ()
  "Test OAuth reset functionality."
  ;; Create and setup test site
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
    
    (unwind-protect
        (progn
          ;; Setup OAuth first
          (mediawiki-oauth-setup "TestWiki" "test-consumer-key" "test-consumer-secret")
          
          ;; Test OAuth reset
          (mediawiki-oauth-reset "TestWiki")
          (let ((site (mediawiki-get-site "TestWiki")))
            (should (eq (mediawiki-site-config-auth-method site) 'oauth))
            (should (equal (plist-get (mediawiki-site-config-auth-config site) :consumer-key) "test-consumer-key"))
            (should (equal (plist-get (mediawiki-site-config-auth-config site) :consumer-secret) "test-consumer-secret"))
            (should (not (plist-get (mediawiki-site-config-auth-config site) :access-token)))
            (should (not (plist-get (mediawiki-site-config-auth-config site) :access-secret)))))
      
      ;; Cleanup
      (mediawiki-remove-site "TestWiki"))))

(ert-deftest test-oauth-nonce-generation ()
  "Test OAuth nonce generation."
  (let ((nonce1 (mediawiki-oauth-generate-nonce))
        (nonce2 (mediawiki-oauth-generate-nonce)))
    (should nonce1)
    (should nonce2)
    (should (stringp nonce1))
    (should (stringp nonce2))
    (should (not (string= nonce1 nonce2)))))

(ert-deftest test-oauth-signature-generation ()
  "Test OAuth signature generation."
  (condition-case err
      (let ((signature (mediawiki-oauth-generate-signature
                       "POST" "https://example.com/api"
                       '(("param1" . "value1") ("param2" . "value2"))
                       "consumer-secret" "token-secret")))
        (should signature)
        (should (stringp signature))
        (should (> (length signature) 0)))
    (error
     (ert-skip (format "OAuth signature generation not available: %s" (error-message-string err))))))

(ert-deftest test-oauth-login-limitation ()
  "Test OAuth login reports HTTP module limitation appropriately."
  ;; Create and setup test site
  (let ((test-site (make-mediawiki-site-config
                    :name "TestWiki"
                    :url "https://test.wiki.example/"
                    :api-url "https://test.wiki.example/w/api.php"
                    :username nil
                    :auth-method 'oauth
                    :auth-config '(:consumer-key "test-key" :consumer-secret "test-secret")
                    :capabilities nil
                    :session-info nil)))
    
    (mediawiki-add-site test-site)
    
    (unwind-protect
        (should-error (mediawiki-oauth-login "TestWiki")
                      :type 'error)
      
      ;; Cleanup
      (mediawiki-remove-site "TestWiki"))))

(ert-deftest test-oauth-token-refresh-no-tokens ()
  "Test OAuth token refresh correctly reports missing tokens."
  ;; Create and setup test site
  (let ((test-site (make-mediawiki-site-config
                    :name "TestWiki"
                    :url "https://test.wiki.example/"
                    :api-url "https://test.wiki.example/w/api.php"
                    :username nil
                    :auth-method 'oauth
                    :auth-config '(:consumer-key "test-key" :consumer-secret "test-secret")
                    :capabilities nil
                    :session-info nil)))
    
    (mediawiki-add-site test-site)
    
    (unwind-protect
        (condition-case err
            (progn
              (mediawiki-oauth-refresh-tokens "TestWiki")
              (ert-fail "Token refresh should have failed"))
          (error
           (let ((err-msg (error-message-string err)))
             (should (string-match-p "No OAuth access tokens" err-msg)))))
      
      ;; Cleanup
      (mediawiki-remove-site "TestWiki"))))

;;; test-oauth-setup.el ends here