;;; test-direct-login.el --- Direct login test with known credentials -*- lexical-binding: t; -*-

;; Test script to directly test login with your known bot credentials

(require 'mediawiki-core)
(require 'mediawiki-http)
(require 'mediawiki-api)
(require 'mediawiki-auth)

(defun test-direct-bot-login ()
  "Test login with direct bot credentials."
  (interactive)
  
  ;; Enable debug logging
  (setq mediawiki-debug t)
  
  ;; Set up the site
  (let ((site (make-mediawiki-site-config
               :name "my-wiki"
               :url "https://wiki.nichework.com/"
               :api-url "https://wiki.nichework.com/w/api.php"
               :username "MarkAHershberger@emacs")))
    (mediawiki-add-site site))
  
  ;; Get the bot password from user
  (let ((bot-password (read-passwd "Enter bot password for MarkAHershberger@emacs: ")))
    
    ;; Override the credential function to use our known credentials
    (cl-letf (((symbol-function 'mediawiki-auth-get-credentials)
               (lambda (_sitename) 
                 (list :username "MarkAHershberger@emacs"
                       :password bot-password))))
      
      (message "Attempting login with bot credentials...")
      
      (condition-case err
          (progn
            (mediawiki-auth-basic-login "my-wiki")
            
            ;; Check if session was created
            (let ((session (mediawiki-get-session "my-wiki")))
              (if session
                  (let ((user-info (mediawiki-session-user-info session)))
                    (message "✓ Login successful!")
                    (message "  User: %s" (plist-get user-info :username))
                    (message "  User ID: %s" (plist-get user-info :userid))
                    (message "  Login time: %s" (mediawiki-session-login-time session)))
                (message "✗ Login appeared to succeed but no session was created"))))
        
        (error
         (message "✗ Login failed: %s" (error-message-string err))
         (message "Check the *MediaWiki Debug* buffer for detailed logs"))))))

(defun test-manual-api-login ()
  "Test the login API calls manually step by step."
  (interactive)
  
  ;; Enable debug logging
  (setq mediawiki-debug t)
  
  ;; Set up the site
  (let ((site (make-mediawiki-site-config
               :name "my-wiki"
               :url "https://wiki.nichework.com/"
               :api-url "https://wiki.nichework.com/w/api.php")))
    (mediawiki-add-site site))
  
  (let ((username "MarkAHershberger@emacs")
        (password (read-passwd "Enter bot password: ")))
    
    (message "Step 1: Getting login token...")
    
    ;; Step 1: Get login token
    (let ((token-response (mediawiki-api-call-sync
                          "my-wiki" "query"
                          (list (cons "meta" "tokens")
                                (cons "type" "login")))))
      
      (if (mediawiki-api-response-success token-response)
          (let ((login-token (mediawiki-auth-extract-token token-response "login")))
            (message "✓ Got login token: %s" login-token)
            
            (message "Step 2: Performing clientlogin...")
            
            ;; Step 2: Perform login
            (let ((login-params (list (cons "loginreturnurl" "http://localhost/")
                                     (cons "username" username)
                                     (cons "password" password)
                                     (cons "logintoken" login-token)
                                     (cons "rememberme" "1"))))
              
              (message "Login parameters: %S" login-params)
              
              (let ((login-response (mediawiki-api-call-sync
                                    "my-wiki" "clientlogin" login-params)))
                
                (message "Login response success: %s" 
                         (mediawiki-api-response-success login-response))
                (message "Login response data: %S" 
                         (mediawiki-api-response-data login-response))
                
                (if (mediawiki-api-response-success login-response)
                    (let* ((data (mediawiki-api-response-data login-response))
                           (clientlogin-data (cdr (assq 'clientlogin data)))
                           (status (cdr (assq 'status clientlogin-data))))
                      (message "Login status: %s" status)
                      (message "Full clientlogin data: %S" clientlogin-data))
                  (message "✗ API call failed: %s" 
                           (mediawiki-api-get-error-info login-response))))))
        
        (message "✗ Failed to get login token: %s" 
                 (mediawiki-api-get-error-info token-response))))))

;; Instructions
(message "Bot login test functions loaded:")
(message "1. M-x test-direct-bot-login - Test with credential override")
(message "2. M-x test-manual-api-login - Step-by-step manual test")

(provide 'test-direct-login)

;;; test-direct-login.el ends here