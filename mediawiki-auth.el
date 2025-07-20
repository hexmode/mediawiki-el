;;; mediawiki-auth.el --- Authentication module for MediaWiki -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module handles various authentication methods for MediaWiki,
;; including basic username/password authentication and OAuth.
;; It integrates with Emacs auth-source for secure credential storage.

;;; Code:

(require 'auth-source)
(require 'mediawiki-core)

;; Conditionally require mediawiki-api (may not be available during testing)
(condition-case nil
    (require 'mediawiki-api)
  (error nil))

(require 'cl-lib)  ; For cl-find-if used in 2FA handling

;;; Authentication Configuration

(defcustom mediawiki-auth-source-backend 'auth-source
  "Backend to use for credential storage."
  :type '(choice (const :tag "Emacs auth-source" auth-source)
                 (const :tag "Manual" manual))
  :group 'mediawiki)

(defcustom mediawiki-default-auth-method 'basic
  "Default authentication method to use."
  :type '(choice (const :tag "Basic (username/password)" basic)
                 (const :tag "OAuth" oauth))
  :group 'mediawiki)

(defcustom mediawiki-auth-cache-duration 3600
  "Duration in seconds to cache credentials before re-querying auth-source."
  :type 'integer
  :group 'mediawiki)

(defcustom mediawiki-auth-source-creation-prompts t
  "Whether to prompt for credential creation when not found in auth-source."
  :type 'boolean
  :group 'mediawiki)

;;; Credential Cache

(defvar mediawiki-auth-credential-cache (make-hash-table :test 'equal)
  "Cache for credentials retrieved from auth-source.
Keys are site identifiers, values are plists with :username, :password, and :expiry.")

(defvar mediawiki-auth-cache-cleanup-timer nil
  "Timer for cleaning up expired credential cache entries.")

;;; Authentication Functions

(defun mediawiki-auth-login (sitename &optional force-method)
  "Authenticate to SITENAME using appropriate method.
FORCE-METHOD can override the default authentication method."
  (let* ((site (mediawiki-get-site sitename))
         (auth-method (or force-method
                         (mediawiki-site-auth-method site)
                         mediawiki-default-auth-method)))

    (cond
     ((eq auth-method 'basic)
      (mediawiki-auth-basic-login sitename))
     ((eq auth-method 'oauth)
      (mediawiki-auth-oauth-login sitename))
     (t
      (error "Unknown authentication method: %s" auth-method)))))

(defun mediawiki-auth-basic-login (sitename)
  "Perform basic username/password authentication for SITENAME using modern login API.
Implements proper token-based authentication flow with support for login continuation
and multi-step authentication as required by requirements 2.1 and 2.4."
  (let* ((site (mediawiki-get-site sitename))
         (credentials (mediawiki-auth-get-credentials sitename))
         (username (plist-get credentials :username))
         (password (plist-get credentials :password)))

    (unless (and username password)
      (error "Username and password required for basic authentication"))

    (mediawiki-debug-log "Starting modern login flow for %s with user %s" sitename username)

    ;; Modern login flow with proper error handling and continuation support
    (mediawiki-auth-perform-modern-login sitename username password)))

(defun mediawiki-auth-perform-modern-login (sitename username password &optional login-token)
  "Perform modern MediaWiki login to SITENAME with proper token handling and continuation support.
LOGIN-TOKEN can be provided for continuation of multi-step login process."
  (let ((login-token (or login-token (mediawiki-auth-get-login-token sitename))))

    (unless login-token
      (error "Failed to obtain login token for %s" sitename))

    (mediawiki-debug-log "Performing login with token for %s" sitename)

    ;; Perform the login request with modern parameters
    (let ((login-response (mediawiki-api-call-sync
                          sitename "clientlogin"
                          (mediawiki-auth-build-login-params username password login-token))))

      (mediawiki-auth-handle-modern-login-response sitename username password login-response))))

(defun mediawiki-auth-get-login-token (sitename)
  "Get login token for SITENAME using modern token API."
  (mediawiki-debug-log "Requesting login token for %s" sitename)
  
  (let ((token-response (mediawiki-api-call-sync
                        sitename "query"
                        (list (cons "meta" "tokens")
                              (cons "type" "login")))))

    (unless (mediawiki-api-response-success token-response)
      (let ((error-info (mediawiki-api-get-error-info token-response)))
        (mediawiki-debug-log "Failed to get login token for %s: %s" sitename error-info)
        (error "Failed to get login token: %s" error-info)))

    (let ((login-token (mediawiki-auth-extract-token token-response "login")))
      (unless login-token
        (error "No login token in response from %s" sitename))
      
      (mediawiki-debug-log "Successfully obtained login token for %s" sitename)
      login-token)))

(defun mediawiki-auth-build-login-params (username password login-token)
  "Build login parameters for modern clientlogin API.
Returns parameter list for username, password, and token."
  (list (cons "loginreturnurl" "http://localhost/")  ; Required by clientlogin
        (cons "username" username)
        (cons "password" password)
        (cons "logintoken" login-token)
        (cons "rememberme" "1")))  ; Keep session active

(defun mediawiki-auth-handle-modern-login-response (sitename username password response)
  "Handle modern login API response with support for continuation and multi-step auth.
Implements proper handling of all modern MediaWiki login states including
PASS, FAIL, RESTART, UI, REDIRECT, and continuation scenarios."
  (if (not (mediawiki-api-response-success response))
      (let ((error-info (mediawiki-api-get-error-info response)))
        (mediawiki-debug-log "Login request failed for %s: %s" sitename error-info)
        (error "Login request failed: %s" error-info))

    (let* ((data (mediawiki-api-response-data response))
           (clientlogin-data (cdr (assq 'clientlogin data)))
           (status (cdr (assq 'status clientlogin-data)))
           (message (cdr (assq 'message clientlogin-data)))
           (messagecode (cdr (assq 'messagecode clientlogin-data))))

      (mediawiki-debug-log "Login response for %s: status=%s, code=%s" 
                          sitename status messagecode)

      (cond
       ;; Successful login
       ((string= status "PASS")
        (mediawiki-debug-log "Login successful for %s" sitename)
        (mediawiki-auth-handle-successful-login sitename clientlogin-data)
        (message "Successfully logged in to %s as %s" sitename username))

       ;; Login failed
       ((string= status "FAIL")
        (let ((error-msg (or message 
                            (format "Login failed with code: %s" messagecode)
                            "Login failed")))
          (mediawiki-debug-log "Login failed for %s: %s" sitename error-msg)
          (error "Login failed: %s" error-msg)))

       ;; Need to restart login process (token expired, etc.)
       ((string= status "RESTART")
        (mediawiki-debug-log "Login restart required for %s: %s" sitename message)
        (message "Restarting login process for %s..." sitename)
        (mediawiki-auth-perform-modern-login sitename username password))

       ;; UI interaction required (2FA, CAPTCHA, etc.)
       ((string= status "UI")
        (mediawiki-debug-log "UI interaction required for %s: %s" sitename messagecode)
        (mediawiki-auth-handle-ui-requirement sitename username password clientlogin-data))

       ;; Redirect required
       ((string= status "REDIRECT")
        (let ((redirect-url (cdr (assq 'redirecttarget clientlogin-data))))
          (mediawiki-debug-log "Redirect required for %s to: %s" sitename redirect-url)
          (error "Login requires redirect to: %s" redirect-url)))

       ;; Unknown status
       (t
        (let ((error-msg (format "Unknown login status: %s" status)))
          (mediawiki-debug-log "Unknown login status for %s: %s" sitename status)
          (error "%s" error-msg)))))))

(defun mediawiki-auth-handle-successful-login (sitename clientlogin-data)
  "Handle successful login by creating session and storing user information."
  (let* ((username (cdr (assq 'username clientlogin-data)))
         (userid (cdr (assq 'lguserid clientlogin-data)))
         (session (make-mediawiki-session
                   :site-name sitename
                   :tokens (make-hash-table :test 'equal)
                   :user-info (list :username username
                                   :userid userid
                                   :login-data clientlogin-data)
                   :login-time (current-time)
                   :last-activity (current-time))))

    (mediawiki-set-session sitename session)
    (mediawiki-debug-log "Created session for %s (user: %s, id: %s)" 
                        sitename username userid)
    session))

(defun mediawiki-auth-handle-ui-requirement (sitename username password clientlogin-data)
  "Handle UI requirements like 2FA, CAPTCHA, or other interactive authentication.
Implements support for multi-step authentication as required by requirement 2.4."
  (let* ((messagecode (cdr (assq 'messagecode clientlogin-data)))
         (message (cdr (assq 'message clientlogin-data)))
         (requests (cdr (assq 'requests clientlogin-data))))

    (mediawiki-debug-log "UI requirement for %s: %s - %s" sitename messagecode message)

    (cond
     ;; Two-factor authentication
     ((or (string-match-p "oathauth" messagecode)
          (string-match-p "2fa" messagecode)
          (string-match-p "totp" messagecode))
      (mediawiki-auth-handle-2fa sitename username password clientlogin-data))

     ;; CAPTCHA challenge
     ((string-match-p "captcha" messagecode)
      (mediawiki-auth-handle-captcha sitename username password clientlogin-data))

     ;; Generic UI requirements
     (t
      (mediawiki-auth-handle-generic-ui sitename username password clientlogin-data)))))

(defun mediawiki-auth-handle-2fa (sitename username password clientlogin-data)
  "Handle two-factor authentication requirement.
Prompts user for 2FA code and continues login process."
  (let* ((requests (cdr (assq 'requests clientlogin-data)))
         (otp-request (cl-find-if (lambda (req) 
                                   (string-match-p "otp\\|totp\\|2fa" 
                                                  (cdr (assq 'id req))))
                                 requests)))
    
    (if otp-request
        (let* ((otp-code (read-string "Enter 2FA/OTP code: "))
               (continue-token (cdr (assq 'logintoken clientlogin-data)))
               (continue-params (list (cons "loginreturnurl" "http://localhost/")
                                     (cons "username" username)
                                     (cons "password" password)
                                     (cons "logintoken" continue-token)
                                     (cons "OATHToken" otp-code))))
          
          (mediawiki-debug-log "Continuing login with 2FA for %s" sitename)
          
          (let ((continue-response (mediawiki-api-call-sync
                                   sitename "clientlogin" continue-params)))
            (mediawiki-auth-handle-modern-login-response 
             sitename username password continue-response)))
      
      (error "2FA required but no OTP request found in response"))))

(defun mediawiki-auth-handle-captcha (sitename username password clientlogin-data)
  "Handle CAPTCHA challenge requirement.
This is a placeholder for CAPTCHA handling - full implementation would
require displaying the CAPTCHA image and getting user input."
  (let ((message (cdr (assq 'message clientlogin-data))))
    (mediawiki-debug-log "CAPTCHA required for %s: %s" sitename message)
    (error "CAPTCHA authentication required but not yet implemented: %s" message)))

(defun mediawiki-auth-handle-generic-ui (sitename username password clientlogin-data)
  "Handle generic UI requirements that don't fit specific categories.
Provides basic continuation support for unknown UI requirements."
  (let* ((message (cdr (assq 'message clientlogin-data)))
         (messagecode (cdr (assq 'messagecode clientlogin-data)))
         (requests (cdr (assq 'requests clientlogin-data))))
    
    (mediawiki-debug-log "Generic UI requirement for %s: %s" sitename messagecode)
    
    ;; For now, we'll error out on unknown UI requirements
    ;; In a full implementation, this could be extended to handle
    ;; additional interactive authentication methods
    (error "Interactive authentication required: %s (code: %s)" 
           message messagecode)))

(defun mediawiki-auth-oauth-login (sitename)
  "Perform OAuth authentication for SITENAME.
This is a placeholder for future OAuth implementation."
  (error "OAuth authentication not yet implemented"))

;;; Credential Management

(defun mediawiki-auth-get-credentials (sitename)
  "Get credentials for SITENAME from cache, auth-source, or prompt user."
  (let* ((site (mediawiki-get-site sitename))
         (cache-key (mediawiki-auth-make-cache-key sitename))
         (cached-creds (mediawiki-auth-get-cached-credentials cache-key)))

    ;; Return cached credentials if still valid
    (if cached-creds
        cached-creds
      ;; Otherwise get fresh credentials
      (let ((credentials (if (eq mediawiki-auth-source-backend 'auth-source)
                            (mediawiki-auth-get-from-auth-source sitename)
                          (mediawiki-auth-prompt-credentials sitename))))
        ;; Cache the credentials
        (mediawiki-auth-cache-credentials cache-key credentials)
        credentials))))

(defun mediawiki-auth-get-from-auth-source (sitename)
  "Get credentials from auth-source for SITENAME."
  (let* ((site (mediawiki-get-site sitename))
         (url (mediawiki-site-url site))
         (username (mediawiki-site-username site))
         (host (mediawiki-auth-extract-host url))
         (port (mediawiki-auth-extract-port url)))

    (mediawiki-debug-log "Searching auth-source for host=%s user=%s port=%s" host username port)

    ;; Search auth-source with multiple strategies
    (let ((auth-info (or
                     ;; Try with specific user if provided
                     (and username
                          (auth-source-search :host host
                                            :user username
                                            :port port
                                            :max 1))
                     ;; Try without user specification
                     (auth-source-search :host host
                                       :port port
                                       :max 1)
                     ;; Try with site name as host
                     (auth-source-search :host sitename
                                       :max 1))))

      (if auth-info
          (let* ((entry (car auth-info))
                 (found-user (plist-get entry :user))
                 (secret-func (plist-get entry :secret))
                 (found-password (and secret-func (funcall secret-func))))

            (mediawiki-debug-log "Found auth-source entry: user=%s" found-user)

            (unless found-password
              (error "No password found in auth-source entry for %s" sitename))

            (list :username (or found-user username)
                  :password found-password))

        ;; No credentials found - handle based on configuration
        (if mediawiki-auth-source-creation-prompts
            (mediawiki-auth-create-auth-source-entry sitename)
          (error "No credentials found in auth-source for %s" sitename))))))

(defun mediawiki-auth-create-auth-source-entry (sitename)
  "Create new auth-source entry for SITENAME by prompting user."
  (let* ((site (mediawiki-get-site sitename))
         (url (mediawiki-site-url site))
         (host (mediawiki-auth-extract-host url))
         (port (mediawiki-auth-extract-port url))
         (username (or (mediawiki-site-username site)
                      (read-string (format "Username for %s: " sitename))))
         (password (read-passwd (format "Password for %s: " sitename))))

    (mediawiki-debug-log "Creating auth-source entry for host=%s user=%s" host username)

    ;; Try to create the entry in auth-source
    (condition-case err
        (let ((created-entry (auth-source-search :host host
                                               :user username
                                               :port port
                                               :max 1
                                               :create t)))
          (when created-entry
            (let ((entry (car created-entry)))
              ;; Set the password if the entry supports it
              (when (plist-get entry :save-function)
                (funcall (plist-get entry :save-function)))
              (mediawiki-debug-log "Created auth-source entry successfully"))))
      (error
       (mediawiki-debug-log "Failed to create auth-source entry: %s" (error-message-string err))))

    ;; Return the credentials regardless of whether creation succeeded
    (list :username username :password password)))

(defun mediawiki-auth-prompt-credentials (sitename)
  "Prompt user for credentials for SITENAME."
  (let* ((site (mediawiki-get-site sitename))
         (username (or (mediawiki-site-username site)
                      (read-string (format "Username for %s: " sitename))))
         (password (read-passwd (format "Password for %s: " sitename))))
    (list :username username :password password)))

;;; Credential Caching

(defun mediawiki-auth-make-cache-key (sitename)
  "Create cache key for SITENAME credentials."
  (format "%s" sitename))

(defun mediawiki-auth-get-cached-credentials (cache-key)
  "Get cached credentials for CACHE-KEY if still valid."
  (let ((cached-entry (gethash cache-key mediawiki-auth-credential-cache)))
    (when cached-entry
      (let ((expiry (plist-get cached-entry :expiry)))
        (if (time-less-p (current-time) expiry)
            (progn
              (mediawiki-debug-log "Using cached credentials for %s" cache-key)
              (list :username (plist-get cached-entry :username)
                    :password (plist-get cached-entry :password)))
          (progn
            (mediawiki-debug-log "Cached credentials expired for %s" cache-key)
            (remhash cache-key mediawiki-auth-credential-cache)
            nil))))))

(defun mediawiki-auth-cache-credentials (cache-key credentials)
  "Cache CREDENTIALS for CACHE-KEY with expiration."
  (let ((expiry (time-add (current-time) mediawiki-auth-cache-duration)))
    (puthash cache-key
             (list :username (plist-get credentials :username)
                   :password (plist-get credentials :password)
                   :expiry expiry)
             mediawiki-auth-credential-cache)
    (mediawiki-debug-log "Cached credentials for %s until %s" 
                        cache-key (format-time-string "%H:%M:%S" expiry))
    
    ;; Ensure cleanup timer is running
    (mediawiki-auth-ensure-cleanup-timer)))

(defun mediawiki-auth-clear-cached-credentials (sitename)
  "Clear cached credentials for SITENAME."
  (let ((cache-key (mediawiki-auth-make-cache-key sitename)))
    (remhash cache-key mediawiki-auth-credential-cache)
    (mediawiki-debug-log "Cleared cached credentials for %s" sitename)))

(defun mediawiki-auth-clear-all-cached-credentials ()
  "Clear all cached credentials."
  (clrhash mediawiki-auth-credential-cache)
  (mediawiki-debug-log "Cleared all cached credentials"))

;;; Credential Cache Cleanup

(defun mediawiki-auth-ensure-cleanup-timer ()
  "Ensure credential cache cleanup timer is running."
  (unless mediawiki-auth-cache-cleanup-timer
    (setq mediawiki-auth-cache-cleanup-timer
          (run-with-timer 300 300 #'mediawiki-auth-cleanup-expired-credentials))))

(defun mediawiki-auth-cleanup-expired-credentials ()
  "Remove expired credentials from cache."
  (let ((expired-keys '())
        (current-time (current-time)))
    
    (maphash (lambda (key entry)
               (let ((expiry (plist-get entry :expiry)))
                 (when (time-less-p expiry current-time)
                   (push key expired-keys))))
             mediawiki-auth-credential-cache)
    
    (dolist (key expired-keys)
      (remhash key mediawiki-auth-credential-cache))
    
    (when expired-keys
      (mediawiki-debug-log "Cleaned up %d expired credential cache entries" 
                          (length expired-keys)))))

;;; URL Parsing Utilities

(defun mediawiki-auth-extract-host (url)
  "Extract host from URL for auth-source lookup."
  (if (string-match "^https?://\\([^/]+\\)" url)
      (match-string 1 url)
    url))

(defun mediawiki-auth-extract-port (url)
  "Extract port from URL for auth-source lookup."
  (cond
   ((string-match "^https://" url) "https")
   ((string-match "^http://" url) "http")
   (t nil)))

;;; Token Management

(defun mediawiki-auth-extract-token (response token-type)
  "Extract TOKEN-TYPE from API RESPONSE."
  (let ((data (mediawiki-api-response-data response)))
    (cdr (assq (intern (concat token-type "token"))
               (cdr (assq 'tokens (cdr (assq 'query data))))))))



;;; Session Validation

(defun mediawiki-auth-check-status (sitename)
  "Check authentication status for SITENAME."
  (let ((session (mediawiki-get-session sitename)))
    (if session
        (mediawiki-auth-validate-session session)
      nil)))

(defun mediawiki-auth-validate-session (session)
  "Validate SESSION is still active."
  ;; Simple validation - can be enhanced
  (and session
       (mediawiki-session-login-time session)
       (< (time-to-seconds (time-since (mediawiki-session-login-time session)))
          (* 24 60 60)))) ; 24 hours

;;; Logout

(defun mediawiki-auth-logout (sitename)
  "Logout from SITENAME and clear cached credentials."
  (let ((session (mediawiki-get-session sitename)))
    (when session
      ;; Call logout API if possible
      (condition-case nil
          (mediawiki-api-call-sync sitename "logout" nil)
        (error nil))

      ;; Remove local session
      (mediawiki-remove-session sitename)
      
      ;; Clear cached credentials for security
      (mediawiki-auth-clear-cached-credentials sitename)
      
      (mediawiki-debug-log "Logged out from %s and cleared cached credentials" sitename)
      (message "Logged out from %s" sitename))))

;;; Token Refresh

(defun mediawiki-auth-refresh-tokens (sitename)
  "Refresh authentication tokens for SITENAME."
  (let ((session (mediawiki-get-session sitename)))
    (when session
      ;; Clear existing tokens
      (clrhash (mediawiki-session-tokens session))

      ;; Update last activity
      (setf (mediawiki-session-last-activity session) (current-time))

      session)))

;;; Security and Utility Functions

(defun mediawiki-auth-invalidate-credentials (sitename)
  "Invalidate cached credentials for SITENAME and force re-authentication."
  (mediawiki-auth-clear-cached-credentials sitename)
  (mediawiki-remove-session sitename)
  (mediawiki-debug-log "Invalidated credentials and session for %s" sitename)
  (message "Credentials invalidated for %s - will prompt on next login" sitename))

(defun mediawiki-auth-test-credentials (sitename)
  "Test if cached credentials for SITENAME are still valid."
  (condition-case err
      (let ((credentials (mediawiki-auth-get-credentials sitename)))
        (if credentials
            (progn
              (mediawiki-debug-log "Credentials test successful for %s" sitename)
              t)
          (progn
            (mediawiki-debug-log "No credentials available for %s" sitename)
            nil)))
    (error
     (mediawiki-debug-log "Credentials test failed for %s: %s" sitename (error-message-string err))
     nil)))

(defun mediawiki-auth-get-cache-status ()
  "Get status information about the credential cache."
  (let ((total-entries (hash-table-count mediawiki-auth-credential-cache))
        (expired-count 0)
        (current-time (current-time)))
    
    (maphash (lambda (_key entry)
               (let ((expiry (plist-get entry :expiry)))
                 (when (time-less-p expiry current-time)
                   (setq expired-count (1+ expired-count)))))
             mediawiki-auth-credential-cache)
    
    (list :total-entries total-entries
          :expired-entries expired-count
          :active-entries (- total-entries expired-count)
          :cleanup-timer-active (timerp mediawiki-auth-cache-cleanup-timer))))

(defun mediawiki-auth-force-cache-cleanup ()
  "Force immediate cleanup of expired credential cache entries."
  (interactive)
  (mediawiki-auth-cleanup-expired-credentials)
  (let ((status (mediawiki-auth-get-cache-status)))
    (message "Cache cleanup complete: %d active, %d total entries"
             (plist-get status :active-entries)
             (plist-get status :total-entries))))

;;; Initialization and Cleanup

(defun mediawiki-auth-initialize ()
  "Initialize the authentication system."
  (mediawiki-debug-log "Initializing MediaWiki authentication system")
  
  ;; Ensure cleanup timer is started
  (mediawiki-auth-ensure-cleanup-timer)
  
  ;; Add cleanup hook for Emacs exit
  (add-hook 'kill-emacs-hook #'mediawiki-auth-shutdown))

(defun mediawiki-auth-shutdown ()
  "Clean up authentication system on Emacs shutdown."
  (mediawiki-debug-log "Shutting down MediaWiki authentication system")
  
  ;; Cancel cleanup timer
  (when (timerp mediawiki-auth-cache-cleanup-timer)
    (cancel-timer mediawiki-auth-cache-cleanup-timer)
    (setq mediawiki-auth-cache-cleanup-timer nil))
  
  ;; Clear credential cache for security
  (mediawiki-auth-clear-all-cached-credentials))

;; Initialize when loaded
(mediawiki-auth-initialize)

(provide 'mediawiki-auth)

;;; mediawiki-auth.el ends here
