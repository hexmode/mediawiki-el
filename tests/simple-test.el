;; Reload the modules to get the error parsing fix
(load-file "mediawiki-core.el")
(load-file "mediawiki-http.el")
(load-file "mediawiki-api.el")
(load-file "mediawiki-auth.el")

;; Clear any cached credentials
(mediawiki-auth-clear-all-cached-credentials)

;; Set up your wiki
(let ((site (make-mediawiki-site-config
             :name "Wikipedia"
             :url "https://en.wikipedia.org/"
             :api-url "https://en.wikipedia.org/w/api.php"
             :username "MarkAHershberger")))
  (mediawiki-add-site site))

;; Enable debug logging
(setq mediawiki-debug t)

;; Try the login
(mediawiki-auth-login "Wikipedia")

(mediawiki-oauth-setup-with-tokens "Wikipedia" 
                                  "c96f906de66e54b5a19bccb265d4b85a"
                                  "12b3f2ebb8a7dc6fad68fbe320cf5aa6cabe116d"
                                  "8f63a33b729e5d02c105bb88a95da59d"
                                  "3aa867e189abfac6f42209d6d3a4c5a9ccd9cf6f")

;; 3. Check if session was created
(let ((session (mediawiki-get-session "Wikipedia")))
  (if session
      (message "✓ Authenticated as: %s" 
               (plist-get (mediawiki-session-user-info session) :username))
    (message "✗ No active session")))
