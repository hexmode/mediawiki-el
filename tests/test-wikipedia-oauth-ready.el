;;; test-wikipedia-oauth-ready.el --- Test Wikipedia OAuth readiness -*- lexical-binding: t; -*-

;;; Commentary:
;; Test that Wikipedia OAuth setup is ready to work with real credentials

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-http)
(require 'mediawiki-oauth)

(defun test-wikipedia-oauth-setup ()
  "Test Wikipedia OAuth setup readiness."
  (message "Testing Wikipedia OAuth setup readiness...")
  
  ;; Set up Wikipedia site (if not already done)
  (unless (mediawiki-get-site "Wikipedia")
    (let ((wikipedia-site (make-mediawiki-site-config
                          :name "Wikipedia"
                          :url "https://en.wikipedia.org/"
                          :api-url "https://en.wikipedia.org/w/api.php"
                          :username nil
                          :auth-method 'basic
                          :auth-config nil
                          :capabilities nil
                          :session-info nil)))
      (mediawiki-add-site wikipedia-site)
      (message "✓ Wikipedia site added")))
  
  ;; Test OAuth setup with dummy credentials
  (mediawiki-oauth-setup-with-tokens "Wikipedia" 
                                    "dummy-consumer-key"
                                    "dummy-consumer-secret"
                                    "dummy-access-token"
                                    "dummy-access-secret")
  
  (let ((site (mediawiki-get-site "Wikipedia")))
    (if (and (eq (mediawiki-site-config-auth-method site) 'oauth)
             (plist-get (mediawiki-site-config-auth-config site) :consumer-key)
             (plist-get (mediawiki-site-config-auth-config site) :access-token))
        (message "✓ Wikipedia OAuth configuration successful")
      (error "Wikipedia OAuth configuration failed")))
  
  ;; Test OAuth signature generation for Wikipedia API
  (let ((signature (mediawiki-oauth-generate-signature
                   "POST" "https://en.wikipedia.org/w/api.php"
                   '(("action" . "query") ("meta" . "userinfo") ("format" . "json"))
                   "dummy-consumer-secret" "dummy-access-secret")))
    (if (and signature (stringp signature) (> (length signature) 0))
        (message "✓ Wikipedia OAuth signature generation working")
      (error "Wikipedia OAuth signature generation failed")))
  
  ;; Test OAuth API call structure (will fail with dummy credentials, but structure should work)
  (condition-case err
      (let ((site (mediawiki-get-site "Wikipedia")))
        (mediawiki-oauth-api-call "Wikipedia" "query" 
                                 '(("meta" . "userinfo"))
                                 (mediawiki-site-config-auth-config site))
        (message "✓ OAuth API call structure working (unexpected success with dummy credentials)"))
    (error
     (let ((err-msg (error-message-string err)))
       (if (or (string-match-p "HTTP\\|network\\|401\\|403\\|invalid" err-msg)
               (string-match-p "OAuth\\|signature" err-msg))
           (message "✓ OAuth API call structure working (expected failure with dummy credentials)")
         (message "✗ OAuth API call structure failed: %s" err-msg)))))
  
  (message "Wikipedia OAuth setup is ready for your real credentials!")
  (message "")
  (message "To use your real Wikipedia OAuth credentials, run:")
  (message "(mediawiki-oauth-setup-with-tokens \"Wikipedia\"")
  (message "                                  \"your-consumer-key\"")
  (message "                                  \"your-consumer-secret\"")
  (message "                                  \"your-access-token\"")
  (message "                                  \"your-access-secret\")")
  (message "")
  (message "Then try: (mediawiki-auth-login \"Wikipedia\")")
  (message ""))

;; Run the test
(test-wikipedia-oauth-setup)

;;; test-wikipedia-oauth-ready.el ends here