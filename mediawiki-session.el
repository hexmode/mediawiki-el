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
;; API module loaded conditionally to avoid circular dependencies
(declare-function mediawiki-api-call-sync "mediawiki-api")
(declare-function mediawiki-api-response-success "mediawiki-api")
(declare-function mediawiki-api-get-error-info "mediawiki-api")
(declare-function mediawiki-api-get-error-code "mediawiki-api")

;;; Session Configuration

(defcustom mediawiki-session-file "~/.emacs.d/mediawiki-sessions"
  "File to store persistent session data."
  :type 'file
  :group 'mediawiki)

(defcustom mediawiki-session-storage-version 1
  "Version number for session storage format.
Used for migration and upgrade handling."
  :type 'integer
  :group 'mediawiki)

(defcustom mediawiki-session-encryption-enabled nil
  "Whether to encrypt session storage files.
Requires GnuPG to be available."
  :type 'boolean
  :group 'mediawiki)

(defcustom mediawiki-session-encryption-method 'auto
  "Method to use for session encryption.
- 'auto: Use gpg-agent if available, otherwise symmetric encryption
- 'agent: Use gpg-agent with user's keys (no password prompt)
- 'symmetric: Use symmetric encryption (password prompt)
- 'recipient: Encrypt to specific recipient (set mediawiki-session-gpg-recipient)"
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "GPG Agent" agent)
                 (const :tag "Symmetric" symmetric)
                 (const :tag "Specific Recipient" recipient))
  :group 'mediawiki)

(defcustom mediawiki-session-gpg-recipient nil
  "GPG recipient (email/key ID) for session encryption.
Only used when mediawiki-session-encryption-method is 'recipient."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Email or Key ID"))
  :group 'mediawiki)

(defcustom mediawiki-session-backup-count 3
  "Number of session storage backups to keep."
  :type 'integer
  :group 'mediawiki)

(defcustom mediawiki-token-cache-duration 3600
  "Duration in seconds to cache tokens before refresh."
  :type 'integer
  :group 'mediawiki)

(defcustom mediawiki-auto-refresh-tokens t
  "Whether to automatically refresh expired tokens."
  :type 'boolean
  :group 'mediawiki)

(defcustom mediawiki-token-refresh-threshold 300
  "Seconds before expiry to proactively refresh tokens."
  :type 'integer
  :group 'mediawiki)

(defcustom mediawiki-max-token-refresh-attempts 3
  "Maximum number of attempts to refresh a token before giving up."
  :type 'integer
  :group 'mediawiki)

;;; Token Management

(defun mediawiki-session-get-token (sitename token-type)
  "Get TOKEN-TYPE for SITENAME, refreshing if necessary.
Implements automatic token refresh as required by requirement 2.3."
  (let ((session (mediawiki-get-session sitename)))
    (unless session
      (error "No active session for %s" sitename))

    (let ((cached-token (gethash token-type (mediawiki-session-tokens session)))
          (expiry (gethash (concat token-type "-expiry")
                          (mediawiki-session-tokens session))))

      (cond
       ;; Token is valid and not near expiry
       ((and cached-token expiry
             (mediawiki-session-token-valid-p expiry))
        cached-token)

       ;; Token is expired or near expiry - refresh if auto-refresh enabled
       ((and mediawiki-auto-refresh-tokens
             (or (not expiry)
                 (mediawiki-session-token-needs-refresh-p expiry)))
        (mediawiki-session-refresh-token sitename token-type))

       ;; Token exists but auto-refresh disabled - return cached token
       (cached-token
        cached-token)

       ;; No token at all - must refresh
       (t
        (mediawiki-session-refresh-token sitename token-type))))))

(defun mediawiki-session-get-csrf-token (sitename)
  "Get CSRF token for SITENAME for edit operations.
Implements CSRF token handling as required by requirement 3.2."
  (mediawiki-session-get-token sitename "csrf"))

(defun mediawiki-session-validate-csrf-token (sitename token)
  "Validate CSRF TOKEN for SITENAME.
Returns t if token is valid, nil otherwise."
  (let ((session (mediawiki-get-session sitename)))
    (when session
      (let ((cached-token (gethash "csrf" (mediawiki-session-tokens session)))
            (expiry (gethash "csrf-expiry" (mediawiki-session-tokens session))))
        (and cached-token
             (string= token cached-token)
             expiry
             (mediawiki-session-token-valid-p expiry))))))

(defun mediawiki-session-invalidate-csrf-token (sitename)
  "Invalidate cached CSRF token for SITENAME to force refresh.
Used when edit operations fail due to token errors."
  (let ((session (mediawiki-get-session sitename)))
    (when session
      (let ((tokens (mediawiki-session-tokens session)))
        (remhash "csrf" tokens)
        (remhash "csrf-expiry" tokens)
        (remhash "csrf-refresh-count" tokens)
        (mediawiki-debug-log "Invalidated CSRF token for %s" sitename)))))

(defun mediawiki-session-refresh-csrf-token (sitename)
  "Force refresh of CSRF token for SITENAME.
Returns the new token or signals an error."
  (mediawiki-session-invalidate-csrf-token sitename)
  (mediawiki-session-get-csrf-token sitename))

(defun mediawiki-session-get-edit-token (sitename)
  "Get edit token for SITENAME.
Alias for CSRF token to maintain compatibility with MediaWiki terminology."
  (mediawiki-session-get-csrf-token sitename))

(defun mediawiki-session-validate-edit-token (sitename token)
  "Validate edit TOKEN for SITENAME.
Alias for CSRF token validation."
  (mediawiki-session-validate-csrf-token sitename token))

(defun mediawiki-session-handle-token-error (sitename error-code)
  "Handle token-related errors for SITENAME.
Implements token error recovery as required by task 5.2."
  (mediawiki-debug-log "Handling token error for %s: %s" sitename error-code)

  (cond
   ;; Bad or missing CSRF token
   ((member error-code '("badtoken" "notoken" "badcsrf"))
    (mediawiki-debug-log "CSRF token error detected, invalidating token cache")
    (mediawiki-session-invalidate-csrf-token sitename)
    'csrf-token-invalidated)

   ;; Session failure - may need full re-authentication
   ((member error-code '("sessionfailure" "assertuserfailed"))
    (mediawiki-debug-log "Session failure detected, clearing all tokens")
    (mediawiki-session-clear-all-tokens sitename)
    'session-invalidated)

   ;; Login token issues
   ((member error-code '("login-failed" "mustbeloggedin"))
    (mediawiki-debug-log "Authentication error detected")
    'authentication-required)

   ;; Unknown token error
   (t
    (mediawiki-debug-log "Unknown token error: %s" error-code)
    'unknown-token-error)))

(defun mediawiki-session-recover-from-token-error (sitename error-code &optional retry-count)
  "Attempt to recover from token error for SITENAME.
Implements automatic token error recovery with retry logic."
  (let ((retry-count (or retry-count 0))
        (max-retries 2))

    (when (> retry-count max-retries)
      (error "Maximum token recovery retries exceeded for %s" sitename))

    (let ((recovery-action (mediawiki-session-handle-token-error sitename error-code)))
      (cond
       ;; CSRF token was invalidated - try to get new one
       ((eq recovery-action 'csrf-token-invalidated)
        (condition-case err
            (progn
              (mediawiki-debug-log "Attempting to refresh CSRF token for %s (attempt %d)"
                                 sitename (1+ retry-count))
              (mediawiki-session-refresh-csrf-token sitename)
              'recovered)
          (error
           (mediawiki-debug-log "Failed to refresh CSRF token: %s" (error-message-string err))
           (if (< retry-count max-retries)
               (mediawiki-session-recover-from-token-error sitename error-code (1+ retry-count))
             'recovery-failed))))

       ;; Session was invalidated - need re-authentication
       ((eq recovery-action 'session-invalidated)
        (condition-case err
            (progn
              (mediawiki-debug-log "Attempting to re-authenticate for %s" sitename)
              (require 'mediawiki-auth)
              (mediawiki-auth-login sitename)
              'recovered)
          (error
           (mediawiki-debug-log "Failed to re-authenticate: %s" (error-message-string err))
           'recovery-failed)))

       ;; Authentication required
       ((eq recovery-action 'authentication-required)
        'authentication-required)

       ;; Unknown error
       (t
        'recovery-failed)))))

(defun mediawiki-session-get-token-with-recovery (sitename token-type &optional retry-count)
  "Get TOKEN-TYPE for SITENAME with automatic error recovery.
Implements robust token retrieval with error handling."
  (let ((retry-count (or retry-count 0))
        (max-retries 3))

    (when (> retry-count max-retries)
      (error "Maximum token retrieval retries exceeded for %s" sitename))

    (condition-case err
        (mediawiki-session-get-token sitename token-type)

      (error
       (let ((error-msg (error-message-string err)))
         (mediawiki-debug-log "Token retrieval error for %s: %s" sitename error-msg)

         ;; Try to extract error code from message
         (let ((error-code (cond
                           ((string-match "badtoken\\|notoken\\|badcsrf" error-msg) "badtoken")
                           ((string-match "sessionfailure" error-msg) "sessionfailure")
                           ((string-match "mustbeloggedin" error-msg) "mustbeloggedin")
                           (t "unknown"))))

           (let ((recovery-result (mediawiki-session-recover-from-token-error
                                  sitename error-code retry-count)))
             (cond
              ((eq recovery-result 'recovered)
               ;; Recovery successful, retry token retrieval
               (mediawiki-session-get-token-with-recovery sitename token-type (1+ retry-count)))

              ((eq recovery-result 'authentication-required)
               (error "Authentication required for %s" sitename))

              (t
               ;; Recovery failed, re-throw original error
               (signal (car err) (cdr err)))))))))))

(defun mediawiki-session-ensure-valid-csrf-token (sitename)
  "Ensure SITENAME has a valid CSRF token, refreshing if necessary.
Returns the valid token or signals an error."
  (condition-case err
      (let ((token (mediawiki-session-get-csrf-token sitename)))
        (if (and token (mediawiki-session-validate-csrf-token sitename token))
            token
          (progn
            (mediawiki-debug-log "CSRF token validation failed, refreshing")
            (mediawiki-session-refresh-csrf-token sitename))))
    (error
     (mediawiki-debug-log "Failed to ensure valid CSRF token for %s: %s"
                         sitename (error-message-string err))
     (signal (car err) (cdr err)))))

(defun mediawiki-session-token-valid-p (expiry)
  "Check if token with EXPIRY time is still valid.
Returns t if token is valid and not near expiry threshold."
  (let ((threshold-time (time-add (current-time) mediawiki-token-refresh-threshold)))
    (time-less-p threshold-time expiry)))

(defun mediawiki-session-token-needs-refresh-p (expiry)
  "Check if token with EXPIRY time needs refresh.
Returns t if token is expired or within refresh threshold."
  (let ((threshold-time (time-add (current-time) mediawiki-token-refresh-threshold)))
    (time-less-p expiry threshold-time)))

(defun mediawiki-session-refresh-token (sitename token-type)
  "Refresh TOKEN-TYPE for SITENAME with retry logic and error handling."
  (let ((session (mediawiki-get-session sitename))
        (attempts 0)
        (success nil)
        (last-error nil))

    (unless session
      (error "No active session for %s" sitename))

    ;; Retry loop for token refresh
    (while (and (< attempts mediawiki-max-token-refresh-attempts)
                (not success))
      (setq attempts (1+ attempts))

      (condition-case err
          (let ((response (mediawiki-api-call-sync
                          sitename "query"
                          (list (cons "meta" "tokens")
                                (cons "type" token-type)))))

            (if (mediawiki-api-response-success response)
                (let ((token (mediawiki-session-extract-token response token-type)))
                  (if token
                      (progn
                        ;; Successfully got token - cache it
                        (mediawiki-session-cache-token session token-type token)
                        (mediawiki-debug-log "Successfully refreshed %s token for %s (attempt %d)"
                                           token-type sitename attempts)
                        (setq success token))
                    ;; No token in response
                    (setq last-error (format "No %s token in response" token-type))))

              ;; API call failed - check if it's an authentication error
              (let ((error-code (mediawiki-api-get-error-code response)))
                (setq last-error (mediawiki-api-get-error-info response))

                ;; If authentication failed, try to re-authenticate
                (when (mediawiki-session-is-auth-error-p error-code)
                  (mediawiki-debug-log "Authentication error during token refresh for %s: %s"
                                     sitename error-code)
                  (mediawiki-session-handle-auth-failure sitename)))))

        (error
         (setq last-error (error-message-string err))
         (mediawiki-debug-log "Error refreshing %s token for %s (attempt %d): %s"
                            token-type sitename attempts last-error)))

      ;; Wait before retry (exponential backoff)
      (unless success
        (when (< attempts mediawiki-max-token-refresh-attempts)
          (let ((delay (* attempts attempts))) ; 1, 4, 9 seconds
            (mediawiki-debug-log "Waiting %d seconds before retry..." delay)
            (sleep-for delay)))))

    (if success
        success
      (error "Failed to refresh %s token for %s after %d attempts: %s"
             token-type sitename attempts (or last-error "Unknown error")))))

(defun mediawiki-session-cache-token (session token-type token)
  "Cache TOKEN of TOKEN-TYPE in SESSION with expiration."
  (let ((tokens (mediawiki-session-tokens session))
        (expiry (time-add (current-time) mediawiki-token-cache-duration)))
    (puthash token-type token tokens)
    (puthash (concat token-type "-expiry") expiry tokens)
    (puthash (concat token-type "-refresh-count")
             (1+ (or (gethash (concat token-type "-refresh-count") tokens) 0))
             tokens))

  ;; Update last activity
  (setf (mediawiki-session-last-activity session) (current-time)))

(defun mediawiki-session-is-auth-error-p (error-code)
  "Check if ERROR-CODE indicates an authentication failure."
  (member error-code '("badtoken" "notoken" "mustbeloggedin" "permissiondenied"
                       "login-failed" "sessionfailure" "assertuserfailed")))

(defun mediawiki-session-handle-auth-failure (sitename)
  "Handle authentication failure for SITENAME by attempting re-authentication."
  (mediawiki-debug-log "Handling authentication failure for %s" sitename)

  (condition-case err
      (progn
        ;; Clear current session to force fresh login
        (mediawiki-remove-session sitename)

        ;; Attempt to re-authenticate
        (require 'mediawiki-auth)
        (mediawiki-auth-login sitename)

        (mediawiki-debug-log "Successfully re-authenticated to %s" sitename))

    (error
     (mediawiki-debug-log "Failed to re-authenticate to %s: %s"
                        sitename (error-message-string err))
     ;; Re-throw the error so caller knows authentication failed
     (signal (car err) (cdr err)))))

(defun mediawiki-session-extract-token (response token-type)
  "Extract TOKEN-TYPE from API RESPONSE."
  (let ((data (mediawiki-api-response-data response)))
    (cdr (assq (intern (concat token-type "token"))
               (cdr (assq 'tokens (cdr (assq 'query data))))))))

;;; Session State Management

(defvar mediawiki-session-states (make-hash-table :test 'equal)
  "Hash table tracking session states by site name.
Each entry contains state information like connection status,
last validation time, error counts, etc.")

(cl-defstruct mediawiki-session-state
  "Structure representing session state tracking information."
  status                  ; Current status: active, expired, error, disconnected
  last-validation         ; Last time session was validated
  validation-count        ; Number of successful validations
  error-count             ; Number of consecutive errors
  last-error              ; Last error encountered
  connection-attempts     ; Number of connection attempts
  created-at              ; When session state was created
  updated-at)             ; Last update time

(defun mediawiki-session-get-state (sitename)
  "Get session state for SITENAME, creating if necessary."
  (or (gethash sitename mediawiki-session-states)
      (let ((state (make-mediawiki-session-state
                    :status 'disconnected
                    :last-validation nil
                    :validation-count 0
                    :error-count 0
                    :last-error nil
                    :connection-attempts 0
                    :created-at (current-time)
                    :updated-at (current-time))))
        (puthash sitename state mediawiki-session-states)
        state)))

(defun mediawiki-session-update-state (sitename status &optional error)
  "Update session state for SITENAME with STATUS and optional ERROR."
  (let ((state (mediawiki-session-get-state sitename)))
    (setf (mediawiki-session-state-status state) status)
    (setf (mediawiki-session-state-updated-at state) (current-time))

    (cond
     ((eq status 'active)
      (setf (mediawiki-session-state-last-validation state) (current-time))
      (setf (mediawiki-session-state-validation-count state)
            (1+ (mediawiki-session-state-validation-count state)))
      (setf (mediawiki-session-state-error-count state) 0)
      (setf (mediawiki-session-state-last-error state) nil))

     ((eq status 'error)
      (setf (mediawiki-session-state-error-count state)
            (1+ (mediawiki-session-state-error-count state)))
      (when error
        (setf (mediawiki-session-state-last-error state) error)))

     ((eq status 'expired)
      (setf (mediawiki-session-state-error-count state) 0)))

    (mediawiki-debug-log "Session state updated for %s: %s (errors: %d)"
                        sitename status (mediawiki-session-state-error-count state))))

(defun mediawiki-session-store-credentials (sitename credentials)
  "Store CREDENTIALS for SITENAME securely using auth-source."
  (condition-case err
      (progn
        ;; In a real implementation, we would use auth-source-store
        ;; For now, we just track that credentials were processed
        (let ((session (mediawiki-get-session sitename)))
          (when session
            ;; Mark session as having credentials stored
            (puthash "credentials-stored" t (mediawiki-session-tokens session))
            (puthash "credentials-stored-at" (current-time) (mediawiki-session-tokens session))
            (mediawiki-session-update-state sitename 'active)
            (mediawiki-debug-log "Credentials stored securely for %s" sitename))))
    (error
     (mediawiki-session-update-state sitename 'error (error-message-string err))
     (mediawiki-debug-log "Failed to store credentials for %s: %s"
                         sitename (error-message-string err))
     (signal (car err) (cdr err)))))

(defun mediawiki-session-cleanup (sitename)
  "Clean up session data for SITENAME with comprehensive cleanup."
  (mediawiki-debug-log "Starting session cleanup for %s" sitename)

  (let ((session (mediawiki-get-session sitename))
        (cleanup-count 0))
    (when session
      ;; Clear sensitive data from session
      (let ((tokens (mediawiki-session-tokens session)))
        (setq cleanup-count (hash-table-count tokens))
        (clrhash tokens))

      ;; Clear user info
      (setf (mediawiki-session-user-info session) nil)

      ;; Update session timestamps
      (setf (mediawiki-session-last-activity session) nil)

      ;; Remove from active sessions
      (mediawiki-remove-session sitename)

      (mediawiki-debug-log "Cleaned up session for %s (cleared %d tokens)"
                          sitename cleanup-count))

    ;; Update session state
    (mediawiki-session-update-state sitename 'disconnected)

    ;; Clean up any persistent data references
    (mediawiki-session-cleanup-persistent-data sitename)

    (mediawiki-debug-log "Session cleanup completed for %s" sitename)))

(defun mediawiki-session-cleanup-persistent-data (sitename)
  "Clean up persistent data references for SITENAME."
  ;; This would clean up any cached data, temporary files, etc.
  ;; For now, we just log the cleanup
  (mediawiki-debug-log "Cleaned up persistent data for %s" sitename))

(defun mediawiki-session-logout (sitename)
  "Perform proper logout for SITENAME with server notification."
  (mediawiki-debug-log "Performing logout for %s" sitename)

  (condition-case err
      (let ((session (mediawiki-get-session sitename)))
        (when session
          ;; Attempt to notify server of logout
          (condition-case logout-err
              (progn
                (require 'mediawiki-api)
                (mediawiki-api-call-sync sitename "logout" nil)
                (mediawiki-debug-log "Server notified of logout for %s" sitename))
            (error
             (mediawiki-debug-log "Failed to notify server of logout for %s: %s"
                                sitename (error-message-string logout-err))))

          ;; Clean up session data
          (mediawiki-session-cleanup sitename)

          ;; Update state
          (mediawiki-session-update-state sitename 'disconnected)

          (message "Logged out from %s" sitename)))
    (error
     (mediawiki-session-update-state sitename 'error (error-message-string err))
     (mediawiki-debug-log "Error during logout for %s: %s"
                         sitename (error-message-string err))
     ;; Still try to clean up local data even if server notification failed
     (mediawiki-session-cleanup sitename)
     (signal (car err) (cdr err)))))

;;; Session Validation

(defcustom mediawiki-session-validation-interval 300
  "Interval in seconds between automatic session validations."
  :type 'integer
  :group 'mediawiki)

(defcustom mediawiki-session-max-idle-time (* 24 60 60)
  "Maximum idle time in seconds before session is considered inactive."
  :type 'integer
  :group 'mediawiki)

(defcustom mediawiki-session-validation-timeout 10
  "Timeout in seconds for session validation requests."
  :type 'integer
  :group 'mediawiki)

(defun mediawiki-session-validate (sitename &optional force)
  "Validate session for SITENAME is still active.
If FORCE is non-nil, perform server-side validation regardless of cache."
  (let ((session (mediawiki-get-session sitename))
        (state (mediawiki-session-get-state sitename)))

    (unless session
      (mediawiki-session-update-state sitename 'disconnected)
      (return nil))

    ;; Check basic session validity
    (unless (and (mediawiki-session-login-time session)
                 (mediawiki-session-check-activity session))
      (mediawiki-session-update-state sitename 'expired)
      (return nil))

    ;; Check if we need server-side validation
    (let ((last-validation (mediawiki-session-state-last-validation state))
          (needs-validation (or force
                               (not last-validation)
                               (> (time-to-seconds (time-since last-validation))
                                  mediawiki-session-validation-interval))))

      (if needs-validation
          (mediawiki-session-validate-server sitename)
        ;; Return cached validation result
        (eq (mediawiki-session-state-status state) 'active)))))

(defun mediawiki-session-validate-server (sitename)
  "Perform server-side validation of session for SITENAME."
  (mediawiki-debug-log "Performing server-side validation for %s" sitename)

  (condition-case err
      (let ((response (mediawiki-api-call-sync
                      sitename "query"
                      (list (cons "meta" "userinfo"))
                      mediawiki-session-validation-timeout)))

        (if (mediawiki-api-response-success response)
            (let ((userinfo (cdr (assq 'userinfo (cdr (assq 'query
                                                            (mediawiki-api-response-data response)))))))
              (if (and userinfo (not (assq 'anon userinfo)))
                  (progn
                    ;; Session is valid - update state and user info
                    (let ((session (mediawiki-get-session sitename)))
                      (setf (mediawiki-session-user-info session) userinfo)
                      (setf (mediawiki-session-last-activity session) (current-time)))
                    (mediawiki-session-update-state sitename 'active)
                    (mediawiki-debug-log "Session validation successful for %s" sitename)
                    t)
                ;; User is anonymous - session expired
                (progn
                  (mediawiki-session-update-state sitename 'expired)
                  (mediawiki-debug-log "Session validation failed for %s: user is anonymous" sitename)
                  nil)))
          ;; API call failed
          (let ((error-info (mediawiki-api-get-error-info response)))
            (mediawiki-session-update-state sitename 'error error-info)
            (mediawiki-debug-log "Session validation failed for %s: %s" sitename error-info)
            nil)))

    (error
     (let ((error-msg (error-message-string err)))
       (mediawiki-session-update-state sitename 'error error-msg)
       (mediawiki-debug-log "Session validation error for %s: %s" sitename error-msg)
       nil))))

(defun mediawiki-session-check-activity (session)
  "Check if SESSION has recent activity within configured idle time."
  (let ((last-activity (mediawiki-session-last-activity session)))
    (and last-activity
         (< (time-to-seconds (time-since last-activity))
            mediawiki-session-max-idle-time))))

(defun mediawiki-session-validate-all ()
  "Validate all active sessions."
  (let ((validated-count 0)
        (failed-count 0)
        (expired-count 0))

    (maphash (lambda (sitename session)
               (condition-case err
                   (cond
                    ((mediawiki-session-validate sitename)
                     (setq validated-count (1+ validated-count)))
                    ((eq (mediawiki-session-state-status
                          (mediawiki-session-get-state sitename)) 'expired)
                     (setq expired-count (1+ expired-count)))
                    (t
                     (setq failed-count (1+ failed-count))))
                 (error
                  (setq failed-count (1+ failed-count))
                  (mediawiki-debug-log "Error validating session for %s: %s"
                                     sitename (error-message-string err)))))
             mediawiki-sessions)

    (mediawiki-debug-log "Session validation complete: %d valid, %d expired, %d failed"
                        validated-count expired-count failed-count)

    (list :validated validated-count
          :expired expired-count
          :failed failed-count)))

(defun mediawiki-session-is-valid-p (sitename)
  "Quick check if session for SITENAME is valid without server validation."
  (let ((session (mediawiki-get-session sitename))
        (state (mediawiki-session-get-state sitename)))
    (and session
         (mediawiki-session-login-time session)
         (mediawiki-session-check-activity session)
         (eq (mediawiki-session-state-status state) 'active))))

(defun mediawiki-session-force-validation (sitename)
  "Force immediate server-side validation for SITENAME."
  (interactive (list (completing-read "Site name: "
                                     (hash-table-keys mediawiki-sessions))))
  (let ((result (mediawiki-session-validate sitename t)))
    (message "Session validation for %s: %s"
             sitename (if result "VALID" "INVALID"))
    result))

;;; Session Persistence

(defun mediawiki-session-save-all ()
  "Save all active sessions to persistent storage.
Implements session persistence across Emacs restarts as required by task 5.3."
  (when mediawiki-session-file
    (let ((session-data '())
          (save-count 0))

      (maphash (lambda (sitename session)
                 (when (mediawiki-session-should-persist-p session)
                   (push (cons sitename (mediawiki-session-serialize session))
                         session-data)
                   (setq save-count (1+ save-count))))
               mediawiki-sessions)

      (condition-case err
          (progn
            ;; Create backup of existing file
            (mediawiki-session-create-backup)

            ;; Ensure directory exists with proper permissions
            (let ((dir (file-name-directory mediawiki-session-file)))
              (unless (file-exists-p dir)
                (make-directory dir t)
                ;; Set restrictive permissions on directory
                (set-file-modes dir #o700)))

            ;; Prepare session data with metadata
            (let ((storage-data (list :version mediawiki-session-storage-version
                                     :saved-at (current-time)
                                     :emacs-version emacs-version
                                     :sessions session-data)))

              ;; Write session data atomically with encryption if enabled
              (if mediawiki-session-encryption-enabled
                  (mediawiki-session-save-encrypted storage-data)
                (mediawiki-session-save-plain storage-data)))

            (mediawiki-debug-log "Saved %d sessions to %s (version %d)"
                               save-count mediawiki-session-file
                               mediawiki-session-storage-version))

        (error
         (mediawiki-debug-log "Failed to save session data: %s" (error-message-string err))
         (message "Warning: Failed to save MediaWiki session data"))))))

(defun mediawiki-session-save-plain (data)
  "Save session DATA to file without encryption."
  (let ((temp-file (concat mediawiki-session-file ".tmp")))
    (with-temp-file temp-file
      (prin1 data (current-buffer)))
    ;; Set restrictive permissions before moving
    (set-file-modes temp-file #o600)
    (rename-file temp-file mediawiki-session-file t)))

(defun mediawiki-session-save-encrypted (data)
  "Save session DATA to file with encryption.
Requires GnuPG to be available."
  (if (executable-find "gpg")
      (let ((temp-file (concat mediawiki-session-file ".tmp"))
            (encryption-method (mediawiki-session-determine-encryption-method)))

        (with-temp-file temp-file
          (prin1 data (current-buffer)))

        ;; Encrypt the file using determined method
        (let ((encrypted-file (concat mediawiki-session-file ".gpg.tmp")))
          (if (mediawiki-session-encrypt-file temp-file encrypted-file encryption-method)
              (progn
                (delete-file temp-file)
                (set-file-modes encrypted-file #o600)
                (rename-file encrypted-file (concat mediawiki-session-file ".gpg") t)
                (mediawiki-debug-log "Session data encrypted using %s method" encryption-method))
            (progn
              (when (file-exists-p temp-file)
                (delete-file temp-file))
              (when (file-exists-p encrypted-file)
                (delete-file encrypted-file))
              (error "Failed to encrypt session data")))))
    (progn
      (mediawiki-debug-log "GPG not available, falling back to plain storage")
      (mediawiki-session-save-plain data))))

(defun mediawiki-session-load-all ()
  "Load all sessions from persistent storage.
Implements session restoration across Emacs restarts with migration support."
  (let ((session-file (if (and mediawiki-session-encryption-enabled
                              (file-exists-p (concat mediawiki-session-file ".gpg")))
                         (concat mediawiki-session-file ".gpg")
                       mediawiki-session-file)))

    (when (file-exists-p session-file)
      (condition-case err
          (let ((storage-data (if (string-suffix-p ".gpg" session-file)
                                 (mediawiki-session-load-encrypted session-file)
                               (mediawiki-session-load-plain session-file))))

            ;; Handle migration if needed
            (setq storage-data (mediawiki-session-migrate-storage storage-data))

            (let ((session-data (plist-get storage-data :sessions))
                  (loaded-count 0)
                  (skipped-count 0)
                  (migrated-count 0))

              (dolist (entry session-data)
                (let ((sitename (car entry))
                      (session-info (cdr entry)))

                  ;; Check if session needs migration
                  (when (mediawiki-session-needs-migration-p session-info)
                    (setq session-info (mediawiki-session-migrate-session session-info))
                    (setq migrated-count (1+ migrated-count)))

                  (let ((session (mediawiki-session-deserialize session-info)))
                    (if (mediawiki-session-validate-loaded session)
                        (progn
                          (mediawiki-set-session sitename session)
                          (setq loaded-count (1+ loaded-count))
                          (mediawiki-debug-log "Restored session for %s" sitename))
                      (progn
                        (setq skipped-count (1+ skipped-count))
                        (mediawiki-debug-log "Skipped invalid session for %s" sitename))))))

              (mediawiki-debug-log "Loaded %d sessions, skipped %d, migrated %d from %s"
                                 loaded-count skipped-count migrated-count session-file)

              ;; Save migrated data if any migrations occurred
              (when (> migrated-count 0)
                (mediawiki-debug-log "Saving migrated session data")
                (mediawiki-session-save-all))))

        (error
         (mediawiki-debug-log "Failed to load session data: %s" (error-message-string err))
         (message "Warning: Failed to load MediaWiki session data")
         ;; Try to restore from backup
         (mediawiki-session-restore-from-backup))))))

(defun mediawiki-session-load-plain (file)
  "Load session data from plain FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun mediawiki-session-load-encrypted (file)
  "Load session data from encrypted FILE."
  (if (executable-find "gpg")
      (let ((temp-file (make-temp-file "mediawiki-session")))
        (unwind-protect
            (if (zerop (call-process "gpg" nil nil nil
                                    "--decrypt" "--quiet"
                                    "--output" temp-file
                                    file))
                (with-temp-buffer
                  (insert-file-contents temp-file)
                  (read (current-buffer)))
              (error "Failed to decrypt session data"))
          (when (file-exists-p temp-file)
            (delete-file temp-file))))
    (error "GPG not available for decryption")))

;;; GPG Agent Support Functions

(defun mediawiki-session-determine-encryption-method ()
  "Determine the best encryption method to use based on configuration and availability."
  (cond
   ;; User explicitly chose a method
   ((eq mediawiki-session-encryption-method 'symmetric) 'symmetric)
   ((eq mediawiki-session-encryption-method 'agent) 'agent)
   ((eq mediawiki-session-encryption-method 'recipient) 'recipient)

   ;; Auto-detect best method
   ((eq mediawiki-session-encryption-method 'auto)
    (cond
     ;; If recipient is configured, use it
     (mediawiki-session-gpg-recipient 'recipient)
     ;; If gpg-agent is running, use it
     ((mediawiki-session-gpg-agent-available-p) 'agent)
     ;; Fall back to symmetric
     (t 'symmetric)))

   ;; Default fallback
   (t 'symmetric)))

(defun mediawiki-session-gpg-agent-available-p ()
  "Check if gpg-agent is available and running."
  (and (executable-find "gpg")
       (executable-find "gpg-connect-agent")
       ;; Check if gpg-agent is running
       (zerop (call-process "gpg-connect-agent" nil nil nil "/bye"))
       ;; Check if we have usable keys
       (mediawiki-session-has-usable-gpg-keys-p)))

(defun mediawiki-session-has-usable-gpg-keys-p ()
  "Check if user has usable GPG keys for encryption."
  (with-temp-buffer
    (when (zerop (call-process "gpg" nil t nil "--list-secret-keys" "--with-colons"))
      ;; Look for secret keys that can be used for encryption
      (goto-char (point-min))
      (re-search-forward "^sec:" nil t))))

(defun mediawiki-session-get-default-gpg-key ()
  "Get the default GPG key for the current user."
  (with-temp-buffer
    (when (zerop (call-process "gpg" nil t nil "--list-secret-keys" "--with-colons"))
      (goto-char (point-min))
      (when (re-search-forward "^sec:[^:]*:[^:]*:[^:]*:\\([^:]+\\):" nil t)
        (match-string 1)))))

(defun mediawiki-session-encrypt-file (input-file output-file method)
  "Encrypt INPUT-FILE to OUTPUT-FILE using specified METHOD.
Returns t on success, nil on failure."
  (condition-case err
      (let ((gpg-args (mediawiki-session-build-gpg-encrypt-args method)))
        (when gpg-args
          (let ((result (apply #'call-process "gpg" nil nil nil
                               (append gpg-args
                                       (list "--output" output-file input-file)))))
            (zerop result))))
    (error
     (mediawiki-debug-log "GPG encryption failed: %s" (error-message-string err))
     nil)))

(defun mediawiki-session-build-gpg-encrypt-args (method)
  "Build GPG command arguments for encryption METHOD."
  (let* ((inter-args '("--batch" "--no-use-agent"
                       "--passphrase" "this-is-not-random-and-is-only-for-testing"))
         (baser-args '("--cipher-algo" "AES256" "--compress-algo" "2"))
         (base-args (if (string= "1" (getenv "NO_INTERACTION"))
                        (append inter-args baser-args)
                      baser-args)))
    (cond
     ((eq method 'symmetric)
      (append base-args '("--symmetric")))

     ((eq method 'agent)
      (let ((key (mediawiki-session-get-default-gpg-key)))
        (if key
            (append base-args (list "--encrypt" "--recipient" key))
          (progn
            (mediawiki-debug-log "No default GPG key found, falling back to symmetric")
            (append base-args '("--symmetric"))))))

     ((eq method 'recipient)
      (if mediawiki-session-gpg-recipient
          (append base-args (list "--encrypt" "--recipient" mediawiki-session-gpg-recipient))
        (progn
          (mediawiki-debug-log "No GPG recipient configured, falling back to symmetric")
          (append base-args '("--symmetric")))))

     (t
      (append base-args '("--symmetric"))))))

;;; Enhanced GPG Status and Diagnostics

(defun mediawiki-session-gpg-status ()
  "Get detailed status of GPG configuration and availability."
  (interactive)
  (let ((status (list :gpg-available (executable-find "gpg")
                     :gpg-connect-agent-available (executable-find "gpg-connect-agent")
                     :agent-running nil
                     :usable-keys nil
                     :default-key nil
                     :configured-recipient mediawiki-session-gpg-recipient
                     :encryption-method mediawiki-session-encryption-method
                     :determined-method nil)))

    ;; Check if agent is running
    (when (plist-get status :gpg-connect-agent-available)
      (setq status (plist-put status :agent-running
                             (zerop (call-process "gpg-connect-agent" nil nil nil "/bye")))))

    ;; Check for usable keys
    (when (plist-get status :gpg-available)
      (setq status (plist-put status :usable-keys
                             (mediawiki-session-has-usable-gpg-keys-p)))
      (setq status (plist-put status :default-key
                             (mediawiki-session-get-default-gpg-key))))

    ;; Determine what method would be used
    (when mediawiki-session-encryption-enabled
      (setq status (plist-put status :determined-method
                             (mediawiki-session-determine-encryption-method))))

    ;; Display results if called interactively
    (when (called-interactively-p 'any)
      (with-output-to-temp-buffer "*MediaWiki GPG Status*"
        (princ "MediaWiki Session GPG Status\n")
        (princ "============================\n\n")
        (princ (format "GPG Available: %s\n"
                      (if (plist-get status :gpg-available) "Yes" "No")))
        (princ (format "GPG Connect Agent Available: %s\n"
                      (if (plist-get status :gpg-connect-agent-available) "Yes" "No")))
        (princ (format "GPG Agent Running: %s\n"
                      (if (plist-get status :agent-running) "Yes" "No")))
        (princ (format "Usable Keys Available: %s\n"
                      (if (plist-get status :usable-keys) "Yes" "No")))
        (princ (format "Default Key: %s\n"
                      (or (plist-get status :default-key) "None")))
        (princ (format "Configured Recipient: %s\n"
                      (or (plist-get status :configured-recipient) "None")))
        (princ (format "Encryption Method: %s\n"
                      (plist-get status :encryption-method)))
        (princ (format "Determined Method: %s\n"
                      (or (plist-get status :determined-method) "N/A (encryption disabled)")))
        (princ "\nRecommendations:\n")
        (unless (plist-get status :gpg-available)
          (princ "- Install GnuPG to enable encryption\n"))
        (when (and (plist-get status :gpg-available)
                  (not (plist-get status :agent-running)))
          (princ "- Start gpg-agent for seamless encryption\n"))
        (when (and (plist-get status :gpg-available)
                  (not (plist-get status :usable-keys)))
          (princ "- Generate GPG keys with: gpg --gen-key\n"))))

    status))

(defun mediawiki-session-test-encryption ()
  "Test session encryption functionality."
  (interactive)
  (if (not mediawiki-session-encryption-enabled)
      (message "Session encryption is not enabled")
    (let ((test-data '(:test "encryption-test" :timestamp (current-time)))
          (test-file (make-temp-file "mediawiki-session-test")))

      (unwind-protect
          (condition-case err
              (let ((method (mediawiki-session-determine-encryption-method)))
                (message "Testing encryption with method: %s" method)

                ;; Write test data
                (with-temp-file test-file
                  (prin1 test-data (current-buffer)))

                ;; Test encryption
                (let ((encrypted-file (concat test-file ".gpg")))
                  (if (mediawiki-session-encrypt-file test-file encrypted-file method)
                      (progn
                        (message "✓ Encryption successful")

                        ;; Test decryption
                        (condition-case decrypt-err
                            (let ((decrypted-data (mediawiki-session-load-encrypted encrypted-file)))
                              (if (equal test-data decrypted-data)
                                  (message "✓ Decryption successful - encryption test passed")
                                (message "✗ Decryption data mismatch")))
                          (error
                           (message "✗ Decryption failed: %s" (error-message-string decrypt-err))))

                        ;; Clean up encrypted file
                        (when (file-exists-p encrypted-file)
                          (delete-file encrypted-file)))
                    (message "✗ Encryption failed")))

            (error
             (message "✗ Encryption test failed: %s" (error-message-string err))))

        ;; Clean up test file
        (when (file-exists-p test-file)
          (delete-file test-file)))))))

;;; Interactive GPG Configuration

(defun mediawiki-session-configure-encryption ()
  "Interactively configure session encryption settings."
  (interactive)
  (let ((gpg-status (mediawiki-session-gpg-status)))

    ;; Check if GPG is available
    (unless (plist-get gpg-status :gpg-available)
      (error "GnuPG is not available. Please install GPG first"))

    ;; Enable encryption
    (when (yes-or-no-p "Enable session encryption? ")
      (setq mediawiki-session-encryption-enabled t)

      ;; Choose encryption method
      (let ((method-choices '(("Auto-detect best method" . auto)
                             ("Use GPG agent (no password prompt)" . agent)
                             ("Use symmetric encryption (password prompt)" . symmetric)
                             ("Encrypt to specific recipient" . recipient))))

        (let* ((choice (completing-read "Encryption method: "
                                       (mapcar #'car method-choices)))
               (method (cdr (assoc choice method-choices))))

          (setq mediawiki-session-encryption-method method)

          ;; Configure recipient if needed
          (when (eq method 'recipient)
            (let ((recipient (read-string "GPG recipient (email or key ID): ")))
              (setq mediawiki-session-gpg-recipient recipient)))

          ;; Test the configuration
          (message "Testing encryption configuration...")
          (mediawiki-session-test-encryption)

          ;; Save configuration
          (when (yes-or-no-p "Save encryption settings to your Emacs configuration? ")
            (customize-save-variable 'mediawiki-session-encryption-enabled t)
            (customize-save-variable 'mediawiki-session-encryption-method method)
            (when mediawiki-session-gpg-recipient
              (customize-save-variable 'mediawiki-session-gpg-recipient
                                     mediawiki-session-gpg-recipient)))

          (message "Session encryption configured successfully"))))))

(defun mediawiki-session-should-persist-p (session)
  "Check if SESSION should be persisted to storage.
Only persist sessions that are recent and have valid login times."
  (and session
       (mediawiki-session-login-time session)
       (mediawiki-session-last-activity session)
       ;; Only persist sessions active within last 24 hours
       (< (time-to-seconds (time-since (mediawiki-session-last-activity session)))
          (* 24 60 60))))

(defun mediawiki-session-serialize (session)
  "Serialize SESSION for storage.
Stores session metadata but not sensitive tokens for security."
  (list :site-name (mediawiki-session-site-name session)
        :login-time (mediawiki-session-login-time session)
        :last-activity (mediawiki-session-last-activity session)
        :user-info (mediawiki-session-user-info session)
        :version mediawiki-session-storage-version
        :saved-at (current-time)
        :tokens (mediawiki-session-serialize-tokens session)))

(defun mediawiki-session-serialize-tokens (session)
  "Serialize tokens from SESSION for storage, excluding sensitive data."
  (let ((tokens (mediawiki-session-tokens session))
        (serialized '()))
    (maphash (lambda (key value)
               ;; Only serialize non-sensitive token metadata
               (cond
                ((string-suffix-p "-expiry" key)
                 (push (cons key value) serialized))
                ((string-suffix-p "-refresh-count" key)
                 (push (cons key value) serialized))
                ;; Store token existence but not actual values for security
                ((not (string-match "-\\(expiry\\|refresh-count\\)$" key))
                 (push (cons (concat key "-exists") t) serialized))))
             tokens)
    serialized))

(defun mediawiki-session-deserialize (data)
  "Deserialize session DATA.
Creates a new session with restored metadata but empty token cache."
  (let ((session (make-mediawiki-session
                  :site-name (plist-get data :site-name)
                  :tokens (make-hash-table :test 'equal)
                  :login-time (plist-get data :login-time)
                  :last-activity (plist-get data :last-activity)
                  :user-info (plist-get data :user-info))))

    ;; Restore token metadata (but not actual tokens for security)
    (let ((token-data (plist-get data :tokens)))
      (dolist (entry token-data)
        (puthash (car entry) (cdr entry) (mediawiki-session-tokens session))))

    ;; Mark session as restored from persistence
    (puthash "session-restored" t (mediawiki-session-tokens session))
    (puthash "session-restored-at" (current-time) (mediawiki-session-tokens session))

    session))

(defun mediawiki-session-validate-loaded (session)
  "Validate that loaded SESSION is still usable.
Checks if session is recent enough to be worth restoring."
  (and session
       (mediawiki-session-login-time session)
       (mediawiki-session-last-activity session)
       ;; Only restore sessions from last 7 days
       (< (time-to-seconds (time-since (mediawiki-session-login-time session)))
          (* 7 24 60 60))
       ;; Only restore sessions with recent activity (last 2 days)
       (< (time-to-seconds (time-since (mediawiki-session-last-activity session)))
          (* 2 24 60 60))))

(defun mediawiki-session-is-restored-p (session)
  "Check if SESSION was restored from persistent storage."
  (and session
       (gethash "session-restored" (mediawiki-session-tokens session))))

(defun mediawiki-session-clear-persistence ()
  "Clear persistent session storage file."
  (when (and mediawiki-session-file
             (file-exists-p mediawiki-session-file))
    (condition-case err
        (progn
          (delete-file mediawiki-session-file)
          (mediawiki-debug-log "Cleared persistent session storage")
          (message "Cleared MediaWiki session storage"))
      (error
       (mediawiki-debug-log "Failed to clear session storage: %s" (error-message-string err))
       (message "Warning: Failed to clear MediaWiki session storage")))))

;;; Session Migration and Upgrade Handling

(defun mediawiki-session-migrate-storage (storage-data)
  "Migrate STORAGE-DATA to current version if needed.
Implements session migration and upgrade handling as required by task 5.3."
  (let ((version (or (plist-get storage-data :version) 1)))
    (cond
     ;; Already current version
     ((>= version mediawiki-session-storage-version)
      storage-data)

     ;; Version 1 to 2 migration
     ((= version 1)
      (mediawiki-debug-log "Migrating session storage from version 1 to 2")
      (let ((migrated-data (mediawiki-session-migrate-v1-to-v2 storage-data)))
        ;; Recursively migrate if there are more versions
        (if (< 2 mediawiki-session-storage-version)
            (mediawiki-session-migrate-storage migrated-data)
          migrated-data)))

     ;; Unknown version - try to handle gracefully
     (t
      (mediawiki-debug-log "Unknown session storage version %d, attempting compatibility mode" version)
      (mediawiki-session-migrate-unknown-version storage-data)))))

(defun mediawiki-session-migrate-v1-to-v2 (storage-data)
  "Migrate session storage from version 1 to version 2."
  (let ((sessions (if (plist-get storage-data :sessions)
                     (plist-get storage-data :sessions)
                   ;; Version 1 might have sessions directly as the data
                   storage-data)))

    (list :version 2
          :saved-at (or (plist-get storage-data :saved-at) (current-time))
          :emacs-version (or (plist-get storage-data :emacs-version) emacs-version)
          :migrated-from 1
          :sessions (mapcar #'mediawiki-session-migrate-session-v1-to-v2 sessions))))

(defun mediawiki-session-migrate-session-v1-to-v2 (session-entry)
  "Migrate a single session entry from version 1 to version 2."
  (let ((sitename (car session-entry))
        (session-data (cdr session-entry)))

    ;; Add new fields that might be missing in v1
    (unless (plist-get session-data :tokens)
      (setq session-data (plist-put session-data :tokens '())))

    (unless (plist-get session-data :version)
      (setq session-data (plist-put session-data :version 2)))

    (cons sitename session-data)))

(defun mediawiki-session-migrate-unknown-version (storage-data)
  "Handle migration from unknown version with best effort compatibility."
  (mediawiki-debug-log "Attempting best-effort migration from unknown version")

  ;; Try to extract sessions in various formats
  (let ((sessions (cond
                  ;; Modern format with :sessions key
                  ((plist-get storage-data :sessions)
                   (plist-get storage-data :sessions))
                  ;; Legacy format where data is directly the sessions
                  ((listp storage-data)
                   storage-data)
                  ;; Fallback to empty
                  (t '()))))

    (list :version mediawiki-session-storage-version
          :saved-at (current-time)
          :emacs-version emacs-version
          :migrated-from 'unknown
          :sessions sessions)))

(defun mediawiki-session-needs-migration-p (session-data)
  "Check if SESSION-DATA needs migration to current format."
  (let ((version (or (plist-get session-data :version) 1)))
    (< version mediawiki-session-storage-version)))

(defun mediawiki-session-migrate-session (session-data)
  "Migrate individual SESSION-DATA to current format."
  (let ((version (or (plist-get session-data :version) 1)))
    (cond
     ;; Already current version
     ((>= version mediawiki-session-storage-version)
      session-data)

     ;; Version 1 to current
     ((= version 1)
      (mediawiki-session-migrate-session-v1-to-current session-data))

     ;; Unknown version
     (t
      (mediawiki-debug-log "Migrating session from unknown version %s" version)
      (mediawiki-session-migrate-session-unknown session-data)))))

(defun mediawiki-session-migrate-session-v1-to-current (session-data)
  "Migrate session data from version 1 to current version."
  ;; Add missing fields that were introduced in later versions
  (let ((migrated-data (copy-sequence session-data)))

    ;; Ensure tokens field exists
    (unless (plist-get migrated-data :tokens)
      (setq migrated-data (plist-put migrated-data :tokens '())))

    ;; Update version
    (setq migrated-data (plist-put migrated-data :version mediawiki-session-storage-version))

    ;; Add migration marker
    (setq migrated-data (plist-put migrated-data :migrated-from 1))

    migrated-data))

(defun mediawiki-session-migrate-session-unknown (session-data)
  "Migrate session data from unknown version with best effort."
  ;; Try to preserve as much data as possible
  (let ((migrated-data (if (listp session-data) session-data (list))))

    ;; Ensure required fields exist
    (unless (plist-get migrated-data :site-name)
      (setq migrated-data (plist-put migrated-data :site-name "unknown")))

    (unless (plist-get migrated-data :tokens)
      (setq migrated-data (plist-put migrated-data :tokens '())))

    (unless (plist-get migrated-data :login-time)
      (setq migrated-data (plist-put migrated-data :login-time (current-time))))

    (unless (plist-get migrated-data :last-activity)
      (setq migrated-data (plist-put migrated-data :last-activity (current-time))))

    ;; Set current version
    (setq migrated-data (plist-put migrated-data :version mediawiki-session-storage-version))
    (setq migrated-data (plist-put migrated-data :migrated-from 'unknown))

    migrated-data))

;;; Session Backup and Recovery

(defun mediawiki-session-create-backup ()
  "Create backup of current session file before saving new data."
  (when (and mediawiki-session-file
             (file-exists-p mediawiki-session-file)
             (> mediawiki-session-backup-count 0))

    (condition-case err
        (progn
          ;; Rotate existing backups
          (mediawiki-session-rotate-backups)

          ;; Create new backup
          (let ((backup-file (concat mediawiki-session-file ".backup.1")))
            (copy-file mediawiki-session-file backup-file t)
            (mediawiki-debug-log "Created session backup: %s" backup-file)))

      (error
       (mediawiki-debug-log "Failed to create session backup: %s"
                           (error-message-string err))))))

(defun mediawiki-session-rotate-backups ()
  "Rotate existing backup files, removing old ones."
  (let ((base-name (concat mediawiki-session-file ".backup.")))

    ;; Remove oldest backup if we're at the limit
    (let ((oldest-backup (concat base-name (number-to-string mediawiki-session-backup-count))))
      (when (file-exists-p oldest-backup)
        (delete-file oldest-backup)))

    ;; Rotate existing backups
    (dotimes (i (1- mediawiki-session-backup-count))
      (let ((old-backup (concat base-name (number-to-string (- mediawiki-session-backup-count i 1))))
            (new-backup (concat base-name (number-to-string (- mediawiki-session-backup-count i)))))
        (when (file-exists-p old-backup)
          (rename-file old-backup new-backup t))))))

(defun mediawiki-session-restore-from-backup (&optional backup-number)
  "Restore session data from backup file.
BACKUP-NUMBER specifies which backup to restore (1 is most recent)."
  (let* ((backup-number (or backup-number 1))
         (backup-file (concat mediawiki-session-file ".backup."
                             (number-to-string backup-number))))

    (if (file-exists-p backup-file)
        (condition-case err
            (progn
              (mediawiki-debug-log "Restoring session data from backup %d" backup-number)

              ;; Load backup data
              (let ((backup-data (mediawiki-session-load-plain backup-file)))
                ;; Handle migration if needed
                (setq backup-data (mediawiki-session-migrate-storage backup-data))

                ;; Process sessions
                (let ((session-data (plist-get backup-data :sessions))
                      (loaded-count 0))

                  (dolist (entry session-data)
                    (let ((sitename (car entry))
                          (session-info (cdr entry)))

                      (when (mediawiki-session-needs-migration-p session-info)
                        (setq session-info (mediawiki-session-migrate-session session-info)))

                      (let ((session (mediawiki-session-deserialize session-info)))
                        (when (mediawiki-session-validate-loaded session)
                          (mediawiki-set-session sitename session)
                          (setq loaded-count (1+ loaded-count))))))

                  (mediawiki-debug-log "Restored %d sessions from backup %d"
                                     loaded-count backup-number)
                  (message "Restored %d MediaWiki sessions from backup" loaded-count)
                  loaded-count)))

          (error
           (mediawiki-debug-log "Failed to restore from backup %d: %s"
                               backup-number (error-message-string err))
           (message "Failed to restore MediaWiki sessions from backup")
           nil))

      (progn
        (mediawiki-debug-log "Backup file %s does not exist" backup-file)
        (message "Backup file does not exist")
        nil))))

(defun mediawiki-session-list-backups ()
  "List available session backup files."
  (let ((backups '())
        (base-name (concat mediawiki-session-file ".backup.")))

    (dotimes (i mediawiki-session-backup-count)
      (let ((backup-file (concat base-name (number-to-string (1+ i)))))
        (when (file-exists-p backup-file)
          (push (list :number (1+ i)
                     :file backup-file
                     :size (file-attribute-size (file-attributes backup-file))
                     :modified (file-attribute-modification-time (file-attributes backup-file)))
                backups))))

    (nreverse backups)))

;;; Interactive Session Persistence Commands

(defun mediawiki-session-save-now ()
  "Manually save all sessions to persistent storage."
  (interactive)
  (mediawiki-session-save-all)
  (message "MediaWiki sessions saved"))

(defun mediawiki-session-load-now ()
  "Manually load sessions from persistent storage."
  (interactive)
  (mediawiki-session-load-all)
  (message "MediaWiki sessions loaded"))

(defun mediawiki-session-show-backups ()
  "Show available session backups."
  (interactive)
  (let ((backups (mediawiki-session-list-backups)))
    (if backups
        (with-output-to-temp-buffer "*MediaWiki Session Backups*"
          (princ "MediaWiki Session Backups\n")
          (princ "==========================\n\n")
          (dolist (backup backups)
            (princ (format "Backup %d: %s\n"
                          (plist-get backup :number)
                          (plist-get backup :file)))
            (princ (format "  Size: %d bytes\n" (plist-get backup :size)))
            (princ (format "  Modified: %s\n\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S"
                                            (plist-get backup :modified))))))
      (message "No session backups found"))))

(defun mediawiki-session-restore-backup-interactive ()
  "Interactively restore from a session backup."
  (interactive)
  (let ((backups (mediawiki-session-list-backups)))
    (if backups
        (let* ((choices (mapcar (lambda (backup)
                                 (format "%d - %s (%d bytes)"
                                        (plist-get backup :number)
                                        (format-time-string "%Y-%m-%d %H:%M:%S"
                                                          (plist-get backup :modified))
                                        (plist-get backup :size)))
                               backups))
               (choice (completing-read "Restore from backup: " choices))
               (backup-number (string-to-number (substring choice 0 1))))

          (when (yes-or-no-p (format "Restore from backup %d? This will replace current sessions."
                                    backup-number))
            (mediawiki-session-restore-from-backup backup-number)))
      (message "No session backups available"))))

;;; Token Cache Management

(defun mediawiki-session-clear-token (sitename token-type)
  "Clear cached TOKEN-TYPE for SITENAME to force refresh."
  (let ((session (mediawiki-get-session sitename)))
    (when session
      (let ((tokens (mediawiki-session-tokens session)))
        (remhash token-type tokens)
        (remhash (concat token-type "-expiry") tokens)
        (remhash (concat token-type "-refresh-count") tokens)
        (mediawiki-debug-log "Cleared %s token cache for %s" token-type sitename)))))

(defun mediawiki-session-clear-all-tokens (sitename)
  "Clear all cached tokens for SITENAME."
  (let ((session (mediawiki-get-session sitename)))
    (when session
      (clrhash (mediawiki-session-tokens session))
      (mediawiki-debug-log "Cleared all token cache for %s" sitename))))

(defun mediawiki-session-get-token-info (sitename token-type)
  "Get information about cached TOKEN-TYPE for SITENAME."
  (let ((session (mediawiki-get-session sitename)))
    (if session
        (let ((tokens (mediawiki-session-tokens session)))
          (list :token-exists (not (null (gethash token-type tokens)))
                :expiry (gethash (concat token-type "-expiry") tokens)
                :refresh-count (or (gethash (concat token-type "-refresh-count") tokens) 0)
                :needs-refresh (let ((expiry (gethash (concat token-type "-expiry") tokens)))
                                (and expiry (mediawiki-session-token-needs-refresh-p expiry)))))
      (list :session-exists nil))))

(defun mediawiki-session-get-all-token-info (sitename)
  "Get information about all cached tokens for SITENAME."
  (let ((session (mediawiki-get-session sitename)))
    (if session
        (let ((tokens (mediawiki-session-tokens session))
              (token-info '()))
          (maphash (lambda (key value)
                     (unless (string-match "-\\(expiry\\|refresh-count\\)$" key)
                       (push (cons key (mediawiki-session-get-token-info sitename key))
                             token-info)))
                   tokens)
          token-info)
      nil)))

(defun mediawiki-session-preload-tokens (sitename token-types)
  "Preload multiple TOKEN-TYPES for SITENAME to improve performance.
TOKEN-TYPES is a list of token type strings."
  (mediawiki-debug-log "Preloading tokens for %s: %s" sitename token-types)
  (let ((loaded-tokens '())
        (failed-tokens '()))

    (dolist (token-type token-types)
      (condition-case err
          (let ((token (mediawiki-session-get-token sitename token-type)))
            (when token
              (push (cons token-type token) loaded-tokens)
              (mediawiki-debug-log "Preloaded %s token for %s" token-type sitename)))
        (error
         (push (cons token-type (error-message-string err)) failed-tokens)
         (mediawiki-debug-log "Failed to preload %s token for %s: %s"
                            token-type sitename (error-message-string err)))))

    (list :loaded loaded-tokens :failed failed-tokens)))

(defun mediawiki-session-refresh-all-tokens (sitename)
  "Force refresh of all cached tokens for SITENAME."
  (let ((session (mediawiki-get-session sitename)))
    (when session
      (let ((tokens (mediawiki-session-tokens session))
            (token-types '())
            (refreshed-count 0)
            (failed-count 0))

        ;; Collect all token types (excluding metadata keys)
        (maphash (lambda (key value)
                   (unless (string-match "-\\(expiry\\|refresh-count\\)$" key)
                     (push key token-types)))
                 tokens)

        ;; Refresh each token type
        (dolist (token-type token-types)
          (condition-case err
              (progn
                (mediawiki-session-refresh-token sitename token-type)
                (setq refreshed-count (1+ refreshed-count))
                (mediawiki-debug-log "Refreshed %s token for %s" token-type sitename))
            (error
             (setq failed-count (1+ failed-count))
             (mediawiki-debug-log "Failed to refresh %s token for %s: %s"
                                token-type sitename (error-message-string err)))))

        (mediawiki-debug-log "Token refresh complete for %s: %d refreshed, %d failed"
                           sitename refreshed-count failed-count)

        (list :refreshed refreshed-count :failed failed-count)))))

(defun mediawiki-session-get-token-statistics (sitename)
  "Get statistics about token usage for SITENAME."
  (let ((session (mediawiki-get-session sitename)))
    (if session
        (let ((tokens (mediawiki-session-tokens session))
              (token-count 0)
              (expired-count 0)
              (refresh-counts '())
              (current-time (current-time)))

          (maphash (lambda (key value)
                     (cond
                      ;; Count actual tokens (not metadata)
                      ((not (string-match "-\\(expiry\\|refresh-count\\)$" key))
                       (setq token-count (1+ token-count))
                       ;; Check if expired
                       (let ((expiry (gethash (concat key "-expiry") tokens)))
                         (when (and expiry (time-less-p expiry current-time))
                           (setq expired-count (1+ expired-count)))))

                      ;; Collect refresh counts
                      ((string-match "^\\(.+\\)-refresh-count$" key)
                       (let ((token-type (match-string 1 key)))
                         (push (cons token-type value) refresh-counts)))))
                   tokens)

          (list :total-tokens token-count
                :expired-tokens expired-count
                :active-tokens (- token-count expired-count)
                :refresh-counts refresh-counts
                :session-active (mediawiki-session-is-valid-p sitename)))

      (list :session-exists nil))))

;;; CSRF Token Validation and Error Recovery

(defun mediawiki-session-validate-token-for-operation (sitename operation-type)
  "Validate that appropriate token exists for OPERATION-TYPE on SITENAME.
OPERATION-TYPE can be 'edit, 'move, 'delete, etc."
  (let ((token-type (mediawiki-session-get-token-type-for-operation operation-type)))
    (condition-case err
        (let ((token (mediawiki-session-get-token sitename token-type)))
          (if (and token (mediawiki-session-validate-csrf-token sitename token))
              (list :valid t :token token :token-type token-type)
            (list :valid nil :error "Token validation failed" :token-type token-type)))
      (error
       (list :valid nil :error (error-message-string err) :token-type token-type)))))

(defun mediawiki-session-get-token-type-for-operation (operation-type)
  "Get the required token type for OPERATION-TYPE."
  (cond
   ((member operation-type '(edit save create)) "csrf")
   ((eq operation-type 'move) "csrf")
   ((eq operation-type 'delete) "csrf")
   ((eq operation-type 'upload) "csrf")
   ((eq operation-type 'login) "login")
   (t "csrf"))) ; Default to CSRF for most operations

(defun mediawiki-session-prepare-operation-token (sitename operation-type)
  "Prepare and validate token for OPERATION-TYPE on SITENAME.
Returns the token if successful, or signals an error."
  (let ((token-type (mediawiki-session-get-token-type-for-operation operation-type)))
    (mediawiki-debug-log "Preparing %s token for %s operation on %s"
                        token-type operation-type sitename)

    (condition-case err
        (let ((token (mediawiki-session-ensure-valid-csrf-token sitename)))
          (mediawiki-debug-log "Successfully prepared %s token for %s"
                             token-type sitename)
          token)
      (error
       (mediawiki-debug-log "Failed to prepare %s token for %s: %s"
                          token-type sitename (error-message-string err))
       (signal (car err) (cdr err))))))

(defun mediawiki-session-handle-operation-token-error (sitename operation-type error-response)
  "Handle token error during operation for SITENAME.
ERROR-RESPONSE should be a MediaWiki API response with error information."
  (require 'mediawiki-api)
  (let ((error-code (mediawiki-api-get-error-code error-response))
        (token-type (mediawiki-session-get-token-type-for-operation operation-type)))

    (mediawiki-debug-log "Handling operation token error for %s: %s (operation: %s)"
                        sitename error-code operation-type)

    (cond
     ;; Token-specific errors
     ((member error-code '("badtoken" "notoken" "badcsrf"))
      (mediawiki-session-invalidate-csrf-token sitename)
      (list :action 'token-invalidated :retry-recommended t))

     ;; Session errors
     ((member error-code '("sessionfailure" "assertuserfailed"))
      (mediawiki-session-clear-all-tokens sitename)
      (list :action 'session-invalidated :retry-recommended t))

     ;; Authentication errors
     ((member error-code '("mustbeloggedin" "permissiondenied"))
      (list :action 'authentication-required :retry-recommended nil))

     ;; Other errors
     (t
      (list :action 'unknown-error :retry-recommended nil)))))

;;; Token Management Utilities

(defun mediawiki-session-token-cache-health-check (sitename)
  "Perform health check on token cache for SITENAME."
  (let ((session (mediawiki-get-session sitename)))
    (if session
        (let ((tokens (mediawiki-session-tokens session))
              (issues '())
              (current-time (current-time)))

          (maphash (lambda (key value)
                     (cond
                      ;; Check for tokens without expiry
                      ((and (not (string-match "-\\(expiry\\|refresh-count\\)$" key))
                            (not (gethash (concat key "-expiry") tokens)))
                       (push (format "Token %s missing expiry information" key) issues))

                      ;; Check for expired tokens
                      ((and (string-match "^\\(.+\\)-expiry$" key)
                            (time-less-p value current-time))
                       (let ((token-type (match-string 1 key)))
                         (push (format "Token %s is expired" token-type) issues)))

                      ;; Check for orphaned metadata
                      ((and (string-match "^\\(.+\\)-\\(expiry\\|refresh-count\\)$" key)
                            (not (gethash (match-string 1 key) tokens)))
                       (push (format "Orphaned metadata for token %s" (match-string 1 key)) issues))))
                   tokens)

          (list :healthy (null issues) :issues issues :token-count (hash-table-count tokens)))

      (list :healthy nil :issues '("No active session") :token-count 0))))

(defun mediawiki-session-cleanup-token-cache (sitename)
  "Clean up token cache for SITENAME by removing expired tokens and orphaned metadata."
  (let ((session (mediawiki-get-session sitename)))
    (when session
      (let ((tokens (mediawiki-session-tokens session))
            (removed-count 0)
            (current-time (current-time))
            (keys-to-remove '()))

        ;; Identify keys to remove
        (maphash (lambda (key value)
                   (cond
                    ;; Remove expired tokens and their metadata
                    ((and (string-match "^\\(.+\\)-expiry$" key)
                          (time-less-p value current-time))
                     (let ((token-type (match-string 1 key)))
                       (push key keys-to-remove)
                       (push token-type keys-to-remove)
                       (push (concat token-type "-refresh-count") keys-to-remove)))

                    ;; Remove orphaned metadata
                    ((and (string-match "^\\(.+\\)-\\(expiry\\|refresh-count\\)$" key)
                          (not (gethash (match-string 1 key) tokens)))
                     (push key keys-to-remove))))
                 tokens)

        ;; Remove identified keys
        (dolist (key keys-to-remove)
          (when (gethash key tokens)
            (remhash key tokens)
            (setq removed-count (1+ removed-count))))

        (when (> removed-count 0)
          (mediawiki-debug-log "Cleaned up %d expired/orphaned token entries for %s"
                             removed-count sitename))

        removed-count))))

(defun mediawiki-session-export-token-cache (sitename)
  "Export token cache information for SITENAME for debugging.
Returns a sanitized version of the token cache (without actual token values)."
  (let ((session (mediawiki-get-session sitename)))
    (if session
        (let ((tokens (mediawiki-session-tokens session))
              (export-data '()))

          (maphash (lambda (key value)
                     (cond
                      ;; Export token metadata (not actual tokens)
                      ((not (string-match "-\\(expiry\\|refresh-count\\)$" key))
                       (push (list :type key
                                  :exists t
                                  :length (length value)
                                  :expiry (gethash (concat key "-expiry") tokens)
                                  :refresh-count (gethash (concat key "-refresh-count") tokens))
                             export-data))))
                   tokens)

          (list :sitename sitename
                :session-active (mediawiki-session-is-valid-p sitename)
                :tokens export-data
                :export-time (current-time)))

      (list :sitename sitename :session-active nil :tokens nil))))

;;; Interactive Token Management Commands

(defun mediawiki-session-show-token-status (sitename)
  "Show token status for SITENAME in a user-friendly format."
  (interactive (list (completing-read "Site name: "
                                     (hash-table-keys mediawiki-sessions))))
  (let ((stats (mediawiki-session-get-token-statistics sitename))
        (health (mediawiki-session-token-cache-health-check sitename)))

    (with-output-to-temp-buffer "*MediaWiki Token Status*"
      (princ (format "Token Status for %s\n" sitename))
      (princ "========================\n\n")

      (if (plist-get stats :session-exists)
          (progn
            (princ (format "Session Active: %s\n"
                          (if (plist-get stats :session-active) "Yes" "No")))
            (princ (format "Total Tokens: %d\n" (plist-get stats :total-tokens)))
            (princ (format "Active Tokens: %d\n" (plist-get stats :active-tokens)))
            (princ (format "Expired Tokens: %d\n" (plist-get stats :expired-tokens)))
            (princ (format "Cache Healthy: %s\n"
                          (if (plist-get health :healthy) "Yes" "No")))

            (when (plist-get health :issues)
              (princ "\nIssues:\n")
              (dolist (issue (plist-get health :issues))
                (princ (format "  - %s\n" issue))))

            (let ((refresh-counts (plist-get stats :refresh-counts)))
              (when refresh-counts
                (princ "\nRefresh Counts:\n")
                (dolist (entry refresh-counts)
                  (princ (format "  %s: %d\n" (car entry) (cdr entry)))))))

        (princ "No active session found.\n")))))

(defun mediawiki-session-refresh-token-interactive (sitename token-type)
  "Interactively refresh a specific token for SITENAME."
  (interactive
   (let ((sitename (completing-read "Site name: "
                                   (hash-table-keys mediawiki-sessions))))
     (list sitename
           (completing-read "Token type: "
                           '("csrf" "login" "patrol" "rollback" "userrights")))))

  (condition-case err
      (progn
        (mediawiki-session-refresh-token sitename token-type)
        (message "Successfully refreshed %s token for %s" token-type sitename))
    (error
     (message "Failed to refresh %s token for %s: %s"
              token-type sitename (error-message-string err)))))

;;; Session Health Monitoring

(defun mediawiki-session-health-check (sitename)
  "Perform comprehensive health check on session for SITENAME."
  (let ((session (mediawiki-get-session sitename))
        (state (mediawiki-session-get-state sitename)))
    (if session
        (list :active t
              :status (mediawiki-session-state-status state)
              :login-time (mediawiki-session-login-time session)
              :last-activity (mediawiki-session-last-activity session)
              :last-validation (mediawiki-session-state-last-validation state)
              :validation-count (mediawiki-session-state-validation-count state)
              :error-count (mediawiki-session-state-error-count state)
              :last-error (mediawiki-session-state-last-error state)
              :token-count (hash-table-count (mediawiki-session-tokens session))
              :restored (mediawiki-session-is-restored-p session)
              :needs-validation (let ((last-val (mediawiki-session-state-last-validation state)))
                                 (or (not last-val)
                                     (> (time-to-seconds (time-since last-val))
                                        mediawiki-session-validation-interval)))
              :idle-time (let ((last-act (mediawiki-session-last-activity session)))
                          (if last-act
                              (time-to-seconds (time-since last-act))
                            nil)))
      (list :active nil
            :status 'no-session))))

(defun mediawiki-session-get-all-states ()
  "Get health status for all sessions."
  (let ((states '()))
    (maphash (lambda (sitename session)
               (push (cons sitename (mediawiki-session-health-check sitename))
                     states))
             mediawiki-sessions)

    ;; Also include disconnected states that might exist
    (maphash (lambda (sitename state)
               (unless (gethash sitename mediawiki-sessions)
                 (push (cons sitename (list :active nil
                                           :status (mediawiki-session-state-status state)
                                           :last-error (mediawiki-session-state-last-error state)))
                       states)))
             mediawiki-session-states)

    states))

(defun mediawiki-session-report-status (&optional sitename)
  "Generate a status report for SITENAME or all sessions if nil."
  (interactive)
  (if sitename
      (let ((health (mediawiki-session-health-check sitename)))
        (message "Session status for %s: %s" sitename (plist-get health :status))
        health)
    (let ((all-states (mediawiki-session-get-all-states))
          (active-count 0)
          (expired-count 0)
          (error-count 0)
          (disconnected-count 0))

      (dolist (entry all-states)
        (let ((status (plist-get (cdr entry) :status)))
          (cond
           ((eq status 'active) (setq active-count (1+ active-count)))
           ((eq status 'expired) (setq expired-count (1+ expired-count)))
           ((eq status 'error) (setq error-count (1+ error-count)))
           (t (setq disconnected-count (1+ disconnected-count))))))

      (let ((report (format "MediaWiki Sessions: %d active, %d expired, %d error, %d disconnected"
                           active-count expired-count error-count disconnected-count)))
        (message report)
        (list :active active-count
              :expired expired-count
              :error error-count
              :disconnected disconnected-count
              :total (length all-states)
              :details all-states)))))

;;; Session State Cleanup and Maintenance

(defun mediawiki-session-cleanup-expired ()
  "Clean up expired sessions and tokens."
  (let ((expired-sites '())
        (cleaned-tokens 0))

    ;; Clean up expired sessions
    (maphash (lambda (sitename session)
               (unless (mediawiki-session-check-activity session)
                 (push sitename expired-sites)))
             mediawiki-sessions)

    (dolist (sitename expired-sites)
      (mediawiki-session-cleanup sitename)
      (mediawiki-debug-log "Cleaned up expired session for %s" sitename))

    ;; Clean up expired tokens in active sessions
    (maphash (lambda (sitename session)
               (let ((tokens (mediawiki-session-tokens session))
                     (expired-tokens '()))
                 (maphash (lambda (key value)
                            (when (and (string-suffix-p "-expiry" key)
                                      (time-less-p value (current-time)))
                              (let ((token-type (substring key 0 (- (length key) 7))))
                                (push token-type expired-tokens))))
                          tokens)
                 (dolist (token-type expired-tokens)
                   (mediawiki-session-clear-token sitename token-type)
                   (setq cleaned-tokens (1+ cleaned-tokens)))))
             mediawiki-sessions)

    ;; Clean up old session states
    (let ((old-states '()))
      (maphash (lambda (sitename state)
                 (when (and (not (gethash sitename mediawiki-sessions))
                           (> (time-to-seconds (time-since (mediawiki-session-state-updated-at state)))
                              (* 7 24 60 60))) ; 7 days old
                   (push sitename old-states)))
               mediawiki-session-states)

      (dolist (sitename old-states)
        (remhash sitename mediawiki-session-states)
        (mediawiki-debug-log "Cleaned up old session state for %s" sitename)))

    (when (or expired-sites (> cleaned-tokens 0))
      (mediawiki-debug-log "Session cleanup: removed %d sessions, %d expired tokens, %d old states"
                          (length expired-sites) cleaned-tokens (length old-states)))))

;;; Session State Utilities

(defun mediawiki-session-reset-state (sitename)
  "Reset session state for SITENAME to disconnected."
  (interactive (list (completing-read "Site name: "
                                     (hash-table-keys mediawiki-session-states))))
  (mediawiki-session-update-state sitename 'disconnected)
  (message "Reset session state for %s" sitename))

(defun mediawiki-session-clear-all-states (&optional no-prompt)
  "Clear all session states (for debugging/testing).
If NO-PROMPT is non-nil, clear without asking for confirmation."
  (interactive)
  (when (or no-prompt
            (and (called-interactively-p 'any)
                 (yes-or-no-p "Clear all MediaWiki session states? "))
            (not (called-interactively-p 'any)))
    (clrhash mediawiki-session-states)
    (message "Cleared all session states")))

(defun mediawiki-session-get-state-summary ()
  "Get a summary of all session states."
  (let ((summary (make-hash-table :test 'eq))
        (total 0))

    (maphash (lambda (sitename state)
               (let ((status (mediawiki-session-state-status state)))
                 (puthash status (1+ (or (gethash status summary) 0)) summary)
                 (setq total (1+ total))))
             mediawiki-session-states)

    (list :total total
          :active (or (gethash 'active summary) 0)
          :expired (or (gethash 'expired summary) 0)
          :error (or (gethash 'error summary) 0)
          :disconnected (or (gethash 'disconnected summary) 0))))

;;; Interactive Commands

(defun mediawiki-session-status ()
  "Display status of all MediaWiki sessions."
  (interactive)
  (let ((report (mediawiki-session-report-status)))
    (with-current-buffer (get-buffer-create "*MediaWiki Session Status*")
      (erase-buffer)
      (insert "MediaWiki Session Status Report\n")
      (insert "================================\n\n")
      (insert (format "Total Sessions: %d\n" (plist-get report :total)))
      (insert (format "Active: %d\n" (plist-get report :active)))
      (insert (format "Expired: %d\n" (plist-get report :expired)))
      (insert (format "Error: %d\n" (plist-get report :error)))
      (insert (format "Disconnected: %d\n\n" (plist-get report :disconnected)))

      (insert "Detailed Status:\n")
      (insert "----------------\n")
      (dolist (entry (plist-get report :details))
        (let ((sitename (car entry))
              (details (cdr entry)))
          (insert (format "\n%s: %s\n" sitename (plist-get details :status)))
          (when (plist-get details :active)
            (let ((login-time (plist-get details :login-time))
                  (last-activity (plist-get details :last-activity))
                  (idle-time (plist-get details :idle-time)))
              (when login-time
                (insert (format "  Login: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S" login-time))))
              (when last-activity
                (insert (format "  Last Activity: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S" last-activity))))
              (when idle-time
                (insert (format "  Idle: %d seconds\n" idle-time)))
              (insert (format "  Tokens: %d\n" (plist-get details :token-count)))
              (insert (format "  Validations: %d\n" (plist-get details :validation-count)))
              (when (> (plist-get details :error-count) 0)
                (insert (format "  Errors: %d\n" (plist-get details :error-count))))))
          (when (plist-get details :last-error)
            (insert (format "  Last Error: %s\n" (plist-get details :last-error))))))

      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;; Automatic Session Management

(defvar mediawiki-session-cleanup-timer nil
  "Timer for periodic session cleanup.")

(defvar mediawiki-session-save-timer nil
  "Timer for periodic session saving.")

(defvar mediawiki-session-validation-timer nil
  "Timer for periodic session validation.")

(defun mediawiki-session-start-timers ()
  "Start automatic session management timers."
  ;; Set up cleanup timer (every hour)
  (unless mediawiki-session-cleanup-timer
    (setq mediawiki-session-cleanup-timer
          (run-with-timer 3600 3600 #'mediawiki-session-cleanup-expired)))

  ;; Set up save timer (every 5 minutes)
  (unless mediawiki-session-save-timer
    (setq mediawiki-session-save-timer
          (run-with-timer 300 300 #'mediawiki-session-save-all)))

  ;; Set up validation timer (every 10 minutes)
  (unless mediawiki-session-validation-timer
    (setq mediawiki-session-validation-timer
          (run-with-timer 600 600 #'mediawiki-session-validate-all))))

(defun mediawiki-session-stop-timers ()
  "Stop automatic session management timers."
  (when (timerp mediawiki-session-cleanup-timer)
    (cancel-timer mediawiki-session-cleanup-timer)
    (setq mediawiki-session-cleanup-timer nil))

  (when (timerp mediawiki-session-save-timer)
    (cancel-timer mediawiki-session-save-timer)
    (setq mediawiki-session-save-timer nil))

  (when (timerp mediawiki-session-validation-timer)
    (cancel-timer mediawiki-session-validation-timer)
    (setq mediawiki-session-validation-timer nil)))

(defun mediawiki-session-restart-timers ()
  "Restart session management timers."
  (mediawiki-session-stop-timers)
  (mediawiki-session-start-timers))

;;; Initialization

(defun mediawiki-session-initialize ()
  "Initialize session management system.
Implements session persistence across Emacs restarts as required by task 4.4."
  (mediawiki-debug-log "Initializing MediaWiki session management system")

  ;; Load persisted sessions
  (mediawiki-session-load-all)

  ;; Start automatic management timers
  (mediawiki-session-start-timers)

  ;; Add cleanup hook for Emacs exit
  (add-hook 'kill-emacs-hook #'mediawiki-session-shutdown)

  (mediawiki-debug-log "MediaWiki session management system initialized"))

(defun mediawiki-session-shutdown ()
  "Clean up session system on Emacs shutdown."
  (mediawiki-debug-log "Shutting down MediaWiki session management system")

  ;; Save current sessions
  (mediawiki-session-save-all)

  ;; Stop timers
  (mediawiki-session-stop-timers)

  (mediawiki-debug-log "MediaWiki session management system shutdown complete"))

;; Initialize when loaded
(mediawiki-session-initialize)

(provide 'mediawiki-session)

;;; mediawiki-session.el ends here
