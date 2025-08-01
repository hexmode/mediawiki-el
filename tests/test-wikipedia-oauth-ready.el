;;; test-wikipedia-oauth-ready.el --- Test Wikipedia OAuth readiness -*- lexical-binding: t; -*-

;;; Commentary:
;; Test that Wikipedia OAuth setup is ready to work with real credentials

;;; Code:

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-http)
(require 'mediawiki-oauth)

(defconst test-wikipedia-oauth-site "Wikipedia")
(defun test-wikipedia-oauth-setup ()
  "Set up test environment for modern login tests."
  ;; Set up Wikipedia site (if not already done)
  (let ((wikipedia-site (make-mediawiki-site-config
                         :name test-wikipedia-oauth-site
                         :url "https://en.wikipedia.org/"
                         :api-url "https://en.wikipedia.org/w/api.php"
                         :username nil
                         :auth-method 'basic
                         :auth-config nil
                         :capabilities nil
                         :session-info nil)))
    (mediawiki-add-site wikipedia-site)))

(defun test-wikipedia-oauth-teardown ()
  "Clean up test environment."
  (mediawiki-remove-session test-wikipedia-oauth-site)
  (mediawiki-remove-site test-wikipedia-oauth-site))

(ert-deftest test-wikipedia-oauth-configuration ()
  "Test Wikipedia OAuth configuration."
  (test-wikipedia-oauth-setup)

  ;; Test OAuth setup with dummy credentials
  (mediawiki-oauth-setup-with-tokens "Wikipedia"
                                    "dummy-consumer-key"
                                    "dummy-consumer-secret"
                                    "dummy-access-token"
                                    "dummy-access-secret")

  (let ((site (mediawiki-get-site "Wikipedia")))
    (should (eq (mediawiki-site-config-auth-method site) 'oauth))
    (should (plist-get (mediawiki-site-config-auth-config site) :consumer-key))
    (should (plist-get (mediawiki-site-config-auth-config site) :access-token)))
  (test-wikipedia-oauth-teardown))

(ert-deftest test-wikipedia-oauth-signature ()
  "Test OAuth signature generation for Wikipedia API."
  (test-wikipedia-oauth-setup)

  (let ((signature (mediawiki-oauth-generate-signature
                   "POST" "https://en.wikipedia.org/w/api.php"
                   '(("action" . "query") ("meta" . "userinfo") ("format" . "json"))
                   "dummy-consumer-secret" "dummy-access-secret")))
    (should signature)
    (should (stringp signature))
    (should (> (length signature) 0)))
  (test-wikipedia-oauth-teardown))

(ert-deftest test-wikipedia-oauth-api-call-structure ()
  "Test OAuth API call structure with dummy credentials."
  :expected-result :failed
  (test-wikipedia-oauth-setup)

  (let ((site (mediawiki-get-site "Wikipedia")))
    (condition-case err
        (progn
          (mediawiki-oauth-api-call "Wikipedia" "query"
                                   '(("meta" . "userinfo"))
                                   (mediawiki-site-config-auth-config site))
          (ert-fail "API call should have failed with dummy credentials"))
      (error
       (let ((err-msg (error-message-string err)))
         (should (or (string-match-p "HTTP\\|network\\|401\\|403\\|invalid" err-msg)
                     (string-match-p "OAuth\\|signature" err-msg)))))))
  (test-wikipedia-oauth-teardown))

(defun show-wikipedia-oauth-instructions ()
  "Show instructions for using real Wikipedia OAuth credentials."
  (interactive)
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

(provide 'test-wikipedia-oauth-ready)

;;; test-wikipedia-oauth-ready.el ends here
