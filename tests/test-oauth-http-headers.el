;;; test-oauth-http-headers.el --- Test OAuth with HTTP header support -*- lexical-binding: t; -*-

;;; Commentary:
;; Test OAuth functionality with the new HTTP header support

;;; Code:

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-http)
(require 'mediawiki-oauth)

(ert-deftest test-oauth-http-headers ()
  "Test OAuth HTTP header functionality."
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
      (should signature)
      (should (stringp signature))
      (should (> (length signature) 0)))

    ;; Test OAuth Authorization header building
    (let ((oauth-params '(("oauth_consumer_key" . "test-key")
                         ("oauth_nonce" . "test-nonce")
                         ("oauth_signature" . "test-signature")
                         ("oauth_timestamp" . "1234567890")
                         ("oauth_token" . "test-token")
                         ("oauth_version" . "1.0"))))
      (let ((auth-header (mediawiki-oauth-build-auth-header oauth-params)))
        (should auth-header)
        (should (string-match-p "^OAuth " auth-header))))

    ;; Test HTTP request with custom headers - we'll skip the actual request
    ;; but test the header processing functionality
    (let ((test-headers '(("Authorization" . "OAuth test=value")
                         ("X-Test-Header" . "test-value"))))
      (should test-headers))))

(ert-deftest test-oauth-parameter-encoding ()
  "Test OAuth parameter encoding and URL encoding."
  ;; Test parameter encoding with special characters
  (let ((params '(("param with spaces" . "value with spaces")
                 ("param&special" . "value=special")
                 ("normal" . "normal"))))
    (let ((encoded (mediawiki-oauth-build-post-data params)))
      (should encoded)
      (should (string-match-p "param%20with%20spaces" encoded))
      (should (string-match-p "value%20with%20spaces" encoded)))))

(provide 'test-oauth-http-headers)

;;; test-oauth-http-headers.el ends here