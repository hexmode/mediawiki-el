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
(require 'mediawiki-api)

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
  "Perform basic username/password authentication for SITENAME."
  (let* ((site (mediawiki-get-site sitename))
         (credentials (mediawiki-auth-get-credentials sitename))
         (username (plist-get credentials :username))
         (password (plist-get credentials :password)))

    (unless (and username password)
      (error "Username and password required for basic authentication"))

    ;; First, get a login token
    (let ((token-response (mediawiki-api-call-sync
                          sitename "query"
                          (list (cons "meta" "tokens")
                                (cons "type" "login")))))

      (unless (mediawiki-api-response-success token-response)
        (error "Failed to get login token: %s"
               (mediawiki-api-get-error-info token-response)))

      (let ((login-token (mediawiki-auth-extract-token token-response "login")))
        (unless login-token
          (error "No login token in response"))

        ;; Perform login
        (let ((login-response (mediawiki-api-call-sync
                              sitename "login"
                              (list (cons "lgname" username)
                                    (cons "lgpassword" password)
                                    (cons "lgtoken" login-token)))))

          (mediawiki-auth-handle-login-response sitename login-response))))))

(defun mediawiki-auth-oauth-login (sitename)
  "Perform OAuth authentication for SITENAME.
This is a placeholder for future OAuth implementation."
  (error "OAuth authentication not yet implemented"))

;;; Credential Management

(defun mediawiki-auth-get-credentials (sitename)
  "Get credentials for SITENAME from auth-source or prompt user."
  (let* ((site (mediawiki-get-site sitename))
         (url (mediawiki-site-url site))
         (username (mediawiki-site-username site)))

    (if (eq mediawiki-auth-source-backend 'auth-source)
        (mediawiki-auth-get-from-auth-source url username)
      (mediawiki-auth-prompt-credentials sitename))))

(defun mediawiki-auth-get-from-auth-source (url username)
  "Get credentials from auth-source for URL and USERNAME."
  (let ((auth-info (auth-source-search :host (url-host (url-generic-parse-url url))
                                      :user username
                                      :max 1)))
    (if auth-info
        (let ((entry (car auth-info)))
          (list :username (plist-get entry :user)
                :password (funcall (plist-get entry :secret))))
      (error "No credentials found in auth-source"))))

(defun mediawiki-auth-prompt-credentials (sitename)
  "Prompt user for credentials for SITENAME."
  (let ((username (read-string (format "Username for %s: " sitename)))
        (password (read-passwd (format "Password for %s: " sitename))))
    (list :username username :password password)))

;;; Token Management

(defun mediawiki-auth-extract-token (response token-type)
  "Extract TOKEN-TYPE from API RESPONSE."
  (let ((data (mediawiki-api-response-data response)))
    (cdr (assq (intern (concat token-type "token"))
               (cdr (assq 'tokens (cdr (assq 'query data))))))))

(defun mediawiki-auth-handle-login-response (sitename response)
  "Handle login RESPONSE for SITENAME."
  (if (mediawiki-api-response-success response)
      (let ((login-data (cdr (assq 'login (mediawiki-api-response-data response)))))
        (let ((result (cdr (assq 'result login-data))))
          (cond
           ((string= result "Success")
            (mediawiki-auth-create-session sitename login-data)
            (message "Successfully logged in to %s" sitename))
           ((string= result "NeedToken")
            (error "Login token handling error"))
           (t
            (error "Login failed: %s" result)))))
    (error "Login request failed: %s" (mediawiki-api-get-error-info response))))

(defun mediawiki-auth-create-session (sitename login-data)
  "Create session for SITENAME from LOGIN-DATA."
  (let ((session (make-mediawiki-session
                  :site-name sitename
                  :tokens (make-hash-table :test 'equal)
                  :user-info login-data
                  :login-time (current-time)
                  :last-activity (current-time))))

    (mediawiki-set-session sitename session)
    session))

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
  "Logout from SITENAME."
  (let ((session (mediawiki-get-session sitename)))
    (when session
      ;; Call logout API if possible
      (condition-case nil
          (mediawiki-api-call-sync sitename "logout" nil)
        (error nil))

      ;; Remove local session
      (mediawiki-remove-session sitename)
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

(provide 'mediawiki-auth)

;;; mediawiki-auth.el ends here
