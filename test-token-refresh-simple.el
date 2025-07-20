;;; test-token-refresh-simple.el --- Simple test for automatic token refresh -*- lexical-binding: t; -*-

;; Simple validation test for automatic token refresh functionality

(require 'mediawiki-core)
(require 'mediawiki-session)

(defun test-token-refresh-simple ()
  "Simple test of token refresh functionality."
  (message "Testing automatic token refresh functionality...")
  
  ;; Test 1: Token expiration detection
  (let ((past-time (time-subtract (current-time) 7200))    ; 2 hours ago
        (future-time (time-add (current-time) 7200)))      ; 2 hours from now
    
    (message "Testing token expiration detection...")
    (unless (not (mediawiki-session-token-valid-p past-time))
      (error "Failed: Past time should be invalid"))
    (unless (mediawiki-session-token-valid-p future-time)
      (error "Failed: Future time should be valid"))
    (message "✓ Token expiration detection works"))
  
  ;; Test 2: Session creation and token caching
  (message "Testing session and token management...")
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
      (unless (string= cached-token "test-token")
        (error "Failed: Token not cached correctly"))
      (message "✓ Token caching works"))
    
    ;; Test token info
    (mediawiki-set-session sitename session)
    (let ((info (mediawiki-session-get-token-info sitename "csrf")))
      (unless (plist-get info :token-exists)
        (error "Failed: Token info should show token exists"))
      (message "✓ Token info works"))
    
    ;; Test token clearing
    (mediawiki-session-clear-token sitename "csrf")
    (let ((cleared-token (gethash "csrf" (mediawiki-session-tokens session))))
      (when cleared-token
        (error "Failed: Token should be cleared"))
      (message "✓ Token clearing works"))
    
    (mediawiki-remove-session sitename))
  
  ;; Test 3: Session persistence
  (message "Testing session persistence...")
  (let* ((sitename "PersistTest")
         (session (make-mediawiki-session
                   :site-name sitename
                   :tokens (make-hash-table :test 'equal)
                   :login-time (current-time)
                   :last-activity (current-time)
                   :user-info '(:username "testuser" :userid 123))))
    
    ;; Test serialization
    (let ((serialized (mediawiki-session-serialize session)))
      (unless (plist-get serialized :site-name)
        (error "Failed: Serialized session missing site name"))
      (unless (plist-get serialized :version)
        (error "Failed: Serialized session missing version"))
      (message "✓ Session serialization works"))
    
    ;; Test deserialization
    (let* ((serialized (mediawiki-session-serialize session))
           (deserialized (mediawiki-session-deserialize serialized)))
      (unless (string= (mediawiki-session-site-name deserialized) sitename)
        (error "Failed: Deserialized session has wrong site name"))
      (unless (mediawiki-session-is-restored-p deserialized)
        (error "Failed: Deserialized session should be marked as restored"))
      (message "✓ Session deserialization works"))
    
    ;; Test validation
    (let* ((serialized (mediawiki-session-serialize session))
           (deserialized (mediawiki-session-deserialize serialized)))
      (unless (mediawiki-session-validate-loaded deserialized)
        (error "Failed: Valid session should pass validation"))
      (message "✓ Session validation works")))
  
  ;; Test 4: Configuration validation
  (message "Testing configuration...")
  (unless (numberp mediawiki-token-cache-duration)
    (error "Failed: Token cache duration should be a number"))
  (unless (numberp mediawiki-token-refresh-threshold)
    (error "Failed: Token refresh threshold should be a number"))
  (unless (numberp mediawiki-max-token-refresh-attempts)
    (error "Failed: Max token refresh attempts should be a number"))
  (message "✓ Configuration validation works")
  
  (message "All automatic token refresh tests passed! ✓"))

;; Run the test
(test-token-refresh-simple)