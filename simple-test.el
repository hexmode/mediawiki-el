;; Reload the modules to get the error parsing fix
(load-file "mediawiki-core.el")
(load-file "mediawiki-http.el")
(load-file "mediawiki-api.el")
(load-file "mediawiki-auth.el")

;; Clear any cached credentials
(mediawiki-auth-clear-all-cached-credentials)

;; Set up your wiki
(let ((site (make-mediawiki-site
             :name "my-wiki-test"
             :url "https://wiki-t.nichework.com/"
             :api-url "https://wiki-t.nichework.com/w/api.php"
             :username "MarkAHershberger@simple")))
  (mediawiki-add-site site))

;; Enable debug logging
(setq mediawiki-debug t)

;; Try the login
(mediawiki-auth-login "my-wiki-test")
