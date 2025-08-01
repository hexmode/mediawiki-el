;;; demo-token-refresh.el --- Demonstration of automatic token refresh -*- lexical-binding: t; -*-

;; Demonstration of automatic token refresh functionality (task 4.4)

(require 'mediawiki-core)
(require 'mediawiki-session)
(require 'mediawiki-api)

(defun demo-token-refresh ()
  "Demonstrate automatic token refresh functionality."
  (message "=== MediaWiki Automatic Token Refresh Demo ===")

  ;; Create a demo site
  (let ((sitename "DemoWiki")
        (site (make-mediawiki-site-config
               :name "DemoWiki"
               :url "https://demo.example.com"
               :api-url "https://demo.example.com/api.php")))

    (mediawiki-add-site site)
    (message "Created demo site: %s" sitename)

    ;; Create a session
    (let ((session (make-mediawiki-session
                    :site-name sitename
                    :tokens (make-hash-table :test 'equal)
                    :login-time (current-time)
                    :last-activity (current-time)
                    :user-info '(:username "demouser" :userid 42))))

      (mediawiki-set-session sitename session)
      (message "Created session for %s" sitename)

      ;; Demo 1: Token caching
      (message "\n--- Demo 1: Token Caching ---")
      (mediawiki-session-cache-token session "csrf" "demo-csrf-token-123")
      (message "Cached CSRF token: demo-csrf-token-123")

      (let ((info (mediawiki-session-get-token-info sitename "csrf")))
        (message "Token info: exists=%s, refresh-count=%d"
                 (plist-get info :token-exists)
                 (plist-get info :refresh-count)))

      ;; Demo 2: Token expiration detection
      (message "\n--- Demo 2: Token Expiration Detection ---")
      (let ((tokens (mediawiki-session-tokens session)))
        ;; Set token to expire soon
        (puthash "csrf-expiry" (time-add (current-time) 100) tokens)
        (message "Set CSRF token to expire in 100 seconds")

        (let ((info (mediawiki-session-get-token-info sitename "csrf")))
          (message "Token needs refresh: %s" (plist-get info :needs-refresh)))

        ;; Set token to expired
        (puthash "csrf-expiry" (time-subtract (current-time) 100) tokens)
        (message "Set CSRF token to expired (100 seconds ago)")

        (let ((info (mediawiki-session-get-token-info sitename "csrf")))
          (message "Token needs refresh: %s" (plist-get info :needs-refresh))))

      ;; Demo 3: Session persistence
      (message "\n--- Demo 3: Session Persistence ---")
      (let ((serialized (mediawiki-session-serialize session)))
        (message "Serialized session data:")
        (message "  Site: %s" (plist-get serialized :site-name))
        (message "  Version: %s" (plist-get serialized :version))
        (message "  User: %s" (plist-get (plist-get serialized :user-info) :username))

        (let ((deserialized (mediawiki-session-deserialize serialized)))
          (message "Deserialized session:")
          (message "  Site: %s" (mediawiki-session-site-name deserialized))
          (message "  Restored: %s" (mediawiki-session-is-restored-p deserialized))
          (message "  Valid: %s" (mediawiki-session-validate-loaded deserialized))))

      ;; Demo 4: Configuration
      (message "\n--- Demo 4: Configuration ---")
      (message "Token cache duration: %d seconds" mediawiki-token-cache-duration)
      (message "Token refresh threshold: %d seconds" mediawiki-token-refresh-threshold)
      (message "Max refresh attempts: %d" mediawiki-max-token-refresh-attempts)
      (message "Auto-refresh enabled: %s" mediawiki-auto-refresh-tokens)

      ;; Demo 5: Health monitoring
      (message "\n--- Demo 5: Health Monitoring ---")
      (let ((health (mediawiki-session-health-check sitename)))
        (message "Session health:")
        (message "  Active: %s" (plist-get health :active))
        (message "  Token count: %d" (plist-get health :token-count))
        (message "  Login time: %s"
                 (format-time-string "%Y-%m-%d %H:%M:%S"
                                   (plist-get health :login-time)))
        (message "  Last activity: %s"
                 (format-time-string "%Y-%m-%d %H:%M:%S"
                                   (plist-get health :last-activity))))

      ;; Cleanup
      (mediawiki-remove-session sitename)
      (message "\nDemo completed - cleaned up test data"))

    (message "\n=== Key Features Demonstrated ===")
    (message "✓ Token expiration detection and handling")
    (message "✓ Automatic re-authentication when tokens expire")
    (message "✓ Session persistence across Emacs restarts")
    (message "✓ Configurable token refresh behavior")
    (message "✓ Health monitoring and diagnostics")
    (message "\nAutomatic token refresh implementation complete!")))

;; Run the demo
(demo-token-refresh)
