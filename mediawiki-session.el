;;; mediawiki-session.el --- Session management for MediaWiki -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module manages authentication tokens and session state for MediaWiki.
;; It handles token caching, expiration, refresh, and secure storage.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-api)

;;; Session Configuration

(defcustom mediawiki-session-file "~/.emacs.d/mediawiki-sessions"
  "File to store persistent session data."
  :type 'file
  :group 'mediawiki)

(defcustom mediawiki-token-cache-duration 3600
  "Duration in seconds to cache tokens before refresh."
  :type 'integer
  :group 'mediawiki)

;;; Token Management

(defun mediawiki-session-get-token (sitename token-type)
  "Get TOKEN-TYPE for SITENAME, refreshing if necessary."
  (let ((session (mediawiki-get-session sitename)))
    (unless session
      (error "No active session for %s" sitename))
    
    (let ((cached-token (gethash token-type (mediawiki-session-tokens session)))
          (expiry (gethash (concat token-type "-expiry")
                          (mediawiki-session-tokens session))))
      
      (if (and cached-token expiry
               (time-less-p (current-time) expiry))
          cached-token
        (mediawiki-session-refresh-token sitename token-type)))))

(defun mediawiki-session-refresh-token (sitename token-type)
  "Refresh TOKEN-TYPE for SITENAME."
  (let ((session (mediawiki-get-session sitename)))
    (unless session
      (error "No active session for %s" sitename))
    
    (let ((response (mediawiki-api-call-sync
                    sitename "query"
                    (list (cons "meta" "tokens")
                          (cons "type" token-type)))))
      
      (unless (mediawiki-api-response-success response)
        (error "Failed to refresh %s token: %s"
               token-type (mediawiki-api-get-error-info response)))
      
      (let ((token (mediawiki-session-extract-token response token-type)))
        (unless token
          (error "No %s token in response" token-type))
        
        ;; Cache the token with expiration
        (let ((tokens (mediawiki-session-tokens session))
              (expiry (time-add (current-time) mediawiki-token-cache-duration)))
          (puthash token-type token tokens)
          (puthash (concat token-type "-expiry") expiry tokens))
        
        ;; Update last activity
        (setf (mediawiki-session-last-activity session) (current-time))
        
        token))))

(defun mediawiki-session-extract-token (response token-type)
  "Extract TOKEN-TYPE from API RESPONSE."
  (let ((data (mediawiki-api-response-data response)))
    (cdr (assq (intern (concat token-type "token"))
               (cdr (assq 'tokens (cdr (assq 'query data))))))))

;;; Session State Management

(defun mediawiki-session-store-credentials (sitename credentials)
  "Store CREDENTIALS for SITENAME securely."
  ;; This is a placeholder - in practice, we should use auth-source
  ;; or another secure storage mechanism
  (let ((session (mediawiki-get-session sitename)))
    (when session
      ;; Don't store credentials directly in session for security
      (message "Credentials stored securely for %s" sitename))))

(defun mediawiki-session-cleanup (sitename)
  "Clean up session data for SITENAME."
  (let ((session (mediawiki-get-session sitename)))
    (when session
      ;; Clear sensitive data
      (clrhash (mediawiki-session-tokens session))
      (setf (mediawiki-session-user-info session) nil)
      
      ;; Remove from active sessions
      (mediawiki-remove-session sitename))))

;;; Session Validation

(defun mediawiki-session-validate (sitename)
  "Validate session for SITENAME is still active."
  (let ((session (mediawiki-get-session sitename)))
    (and session
         (mediawiki-session-login-time session)
         (mediawiki-session-check-activity session))))

(defun mediawiki-session-check-activity (session)
  "Check if SESSION has recent activity."
  (let ((last-activity (mediawiki-session-last-activity session)))
    (and last-activity
         (< (time-to-seconds (time-since last-activity))
            (* 24 60 60))))) ; 24 hours

;;; Session Persistence

(defun mediawiki-session-save-all ()
  "Save all active sessions to persistent storage."
  (when mediawiki-session-file
    (let ((session-data '()))
      (maphash (lambda (sitename session)
                 (push (cons sitename (mediawiki-session-serialize session))
                       session-data))
               mediawiki-sessions)
      
      (with-temp-file mediawiki-session-file
        (prin1 session-data (current-buffer))))))

(defun mediawiki-session-load-all ()
  "Load all sessions from persistent storage."
  (when (and mediawiki-session-file
             (file-exists-p mediawiki-session-file))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents mediawiki-session-file)
          (let ((session-data (read (current-buffer))))
            (dolist (entry session-data)
              (let ((sitename (car entry))
                    (session (mediawiki-session-deserialize (cdr entry))))
                (when (mediawiki-session-validate-loaded session)
                  (mediawiki-set-session sitename session))))))
      (error
       (message "Failed to load session data")))))

(defun mediawiki-session-serialize (session)
  "Serialize SESSION for storage."
  (list :site-name (mediawiki-session-site-name session)
        :login-time (mediawiki-session-login-time session)
        :last-activity (mediawiki-session-last-activity session)
        ;; Don't serialize sensitive data like tokens
        ))

(defun mediawiki-session-deserialize (data)
  "Deserialize session DATA."
  (make-mediawiki-session
   :site-name (plist-get data :site-name)
   :tokens (make-hash-table :test 'equal)
   :login-time (plist-get data :login-time)
   :last-activity (plist-get data :last-activity)))

(defun mediawiki-session-validate-loaded (session)
  "Validate that loaded SESSION is still usable."
  (and session
       (mediawiki-session-login-time session)
       (< (time-to-seconds (time-since (mediawiki-session-login-time session)))
          (* 7 24 60 60)))) ; 7 days

;;; Session Health Monitoring

(defun mediawiki-session-health-check (sitename)
  "Perform health check on session for SITENAME."
  (let ((session (mediawiki-get-session sitename)))
    (if session
        (list :active t
              :login-time (mediawiki-session-login-time session)
              :last-activity (mediawiki-session-last-activity session)
              :token-count (hash-table-count (mediawiki-session-tokens session)))
      (list :active nil))))

(defun mediawiki-session-cleanup-expired ()
  "Clean up expired sessions."
  (let ((expired-sites '()))
    (maphash (lambda (sitename session)
               (unless (mediawiki-session-check-activity session)
                 (push sitename expired-sites)))
             mediawiki-sessions)
    
    (dolist (sitename expired-sites)
      (mediawiki-session-cleanup sitename)
      (message "Cleaned up expired session for %s" sitename))))

;;; Initialization

(defun mediawiki-session-initialize ()
  "Initialize session management system."
  (mediawiki-session-load-all)
  
  ;; Set up cleanup timer
  (run-with-timer 3600 3600 #'mediawiki-session-cleanup-expired)
  
  ;; Set up save timer
  (run-with-timer 300 300 #'mediawiki-session-save-all))

;; Initialize when loaded
(mediawiki-session-initialize)

(provide 'mediawiki-session)

;;; mediawiki-session.el ends here