;;; test-token-refresh-simple.el --- Simple test for automatic token refresh -*- lexical-binding: t; -*-

;; Simple validation test for automatic token refresh functionality

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-session)

(ert-deftest test-token-expiration-detection ()
  "Test token expiration detection."
  (let ((past-time (time-subtract (current-time) 7200))    ; 2 hours ago
        (future-time (time-add (current-time) 7200)))      ; 2 hours from now

    (should-not (mediawiki-session-token-valid-p past-time))
    (should (mediawiki-session-token-valid-p future-time))))

(ert-deftest test-session-and-token-management ()
  "Test session creation and token caching."
  (let* ((sitename "TestSite")
         (session (make-mediawiki-session
                   :site-name sitename
                   :tokens (make-hash-table :test 'equal)
                   :login-time (current-time)
                   :last-activity (current-time))))

    ;; Cache a token
    (mediawiki-session-cache-token session "csrf" "test-token")

    ;; Verify token is cached
    (let ((cached-token (gethash "csrf" (mediawiki-session-tokens session))))
      (should (string= cached-token "test-token")))

    ;; Test token info
    (mediawiki-set-session sitename session)
    (let ((info (mediawiki-session-get-token-info sitename "csrf")))
      (should (plist-get info :token-exists)))

    ;; Test token clearing
    (mediawiki-session-clear-token sitename "csrf")
    (let ((cleared-token (gethash "csrf" (mediawiki-session-tokens session))))
      (should-not cleared-token))

    (mediawiki-remove-session sitename)))

(ert-deftest test-session-persistence ()
  "Test session persistence."
  (let* ((sitename "PersistTest")
         (session (make-mediawiki-session
                   :site-name sitename
                   :tokens (make-hash-table :test 'equal)
                   :login-time (current-time)
                   :last-activity (current-time)
                   :user-info '(:username "testuser" :userid 123))))

    ;; Test serialization
    (let ((serialized (mediawiki-session-serialize session)))
      (should (plist-get serialized :site-name))
      (should (plist-get serialized :version)))

    ;; Test deserialization
    (let* ((serialized (mediawiki-session-serialize session))
           (deserialized (mediawiki-session-deserialize serialized)))
      (should (string= (mediawiki-session-site-name deserialized) sitename))
      (should (mediawiki-session-is-restored-p deserialized)))

    ;; Test validation
    (let* ((serialized (mediawiki-session-serialize session))
           (deserialized (mediawiki-session-deserialize serialized)))
      (should (mediawiki-session-validate-loaded deserialized)))))

(ert-deftest test-token-refresh-configuration ()
  "Test token refresh configuration."
  (should (numberp mediawiki-token-cache-duration))
  (should (numberp mediawiki-token-refresh-threshold))
  (should (numberp mediawiki-max-token-refresh-attempts)))

(provide 'test-token-refresh-simple)

;;; test-token-refresh-simple.el ends here
