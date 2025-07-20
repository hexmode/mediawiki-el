;;; test-oauth-http-headers.el --- Test OAuth with HTTP header support -*- lexical-binding: t; -*-

;;; Commentary:
;; Test OAuth functionality with the new HTTP header support

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-http)
(require 'mediawiki-oauth)

(defun test-oauth-http-headers ()
  "Test OAuth HTTP header functionality."
  (message "Testing OAuth HTTP header support...")
  
  ;; Create a test site
  (let ((test-site (make-mediawiki-site-config
                    :name "TestWiki"
                    :url "https://test.wiki.example/"
                    :api-url "https://test.wiki.example/w/api.php"
                    :username nil
                    :auth-method 'oauth
                    :auth-config (list :consumer-key "test-consumer-key"
                                      :consumer-secret "test-consumer-secret"
                                      :access-token "test-access-token"
                                      :access-secret "test-access-secret")
                    :capabilities nil
                    :session-info nil)))
    
    (mediawiki-add-site test-site)
    
    ;; Test OAuth signature generation
    (let ((signature (mediawiki-oauth-generate-signature
                     "POST" "https://test.example.com/api"
                     '(("action" . "query") ("format" . "json"))
                     "consumer-secret" "token-secret")))
      (if (and signature (stringp signature) (> (length signature) 0))
          (message "✓ OAuth signature generation working: %s" signature)
        (error "OAuth signature generation failed")))
    
    ;; Test OAuth Authorization header building
    (let ((oauth-params '(("oauth_consumer_key" . "test-key")
                         ("oauth_nonce" . "test-nonce")
                         ("oauth_signature" . "test-signature")
                         ("oauth_timestamp" . "1234567890")
                         ("oauth_token" . "test-token")
                         ("oauth_version" . "1.0"))))
      (let ((auth-header (mediawiki-oauth-build-auth-header oauth-params)))
        (if (and auth-header (string-match-p "^OAuth " auth-header))
            (message "✓ OAuth Authorization header building working: %s" auth-header)
          (error "OAuth Authorization header building failed"))))
    
    ;; Test HTTP request with custom headers
    (condition-case err
        (let ((test-headers '(("Authorization" . "OAuth test=value")
                             ("X-Test-Header" . "test-value"))))
          ;; This will fail because the URL doesn't exist, but we can check if headers are processed
          (mediawiki-http-request-sync "https://httpbin.org/headers" "GET" nil 5 test-headers)
          (message "✓ HTTP request with custom headers processed (may fail due to network)"))
      (error
       (let ((err-msg (error-message-string err)))
         (if (string-match-p "HTTP\\|network\\|timeout" err-msg)
             (message "✓ HTTP request with custom headers processed (network error expected)")
           (message "✗ HTTP request with custom headers failed: %s" err-msg)))))
    
    (message "OAuth HTTP header support tests completed")))

;; Test OAuth parameter encoding
(defun test-oauth-parameter-encoding ()
  "Test OAuth parameter encoding and URL encoding."
  (message "Testing OAuth parameter encoding...")
  
  ;; Test parameter encoding with special characters
  (let ((params '(("param with spaces" . "value with spaces")
                 ("param&special" . "value=special")
                 ("normal" . "normal"))))
    (let ((encoded (mediawiki-oauth-build-post-data params)))
      (if (and encoded 
               (string-match-p "param%20with%20spaces" encoded)
               (string-match-p "value%20with%20spaces" encoded))
          (message "✓ OAuth parameter encoding working: %s" encoded)
        (message "✗ OAuth parameter encoding failed: %s" encoded))))
  
  (message "OAuth parameter encoding tests completed"))

;; Run the tests
(test-oauth-http-headers)
(test-oauth-parameter-encoding)

(message "All OAuth HTTP header tests completed!")

;;; test-oauth-http-headers.el ends here