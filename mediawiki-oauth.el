;;; mediawiki-oauth.el --- OAuth 1.0a authentication for MediaWiki -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module implements OAuth 1.0a authentication for MediaWiki.
;; It provides OAuth configuration, token management, and authentication flow.

;;; Code:

(require 'mediawiki-core)
(require 'gnutls)

;;; OAuth Configuration

(defcustom mediawiki-oauth-callback-url "oob"
  "OAuth callback URL for desktop applications (out-of-band)."
  :type 'string
  :group 'mediawiki)

;;; OAuth 1.0a Implementation

(defun mediawiki-oauth-login (sitename)
  "Perform OAuth authentication for SITENAME using OAuth 1.0a flow."
  (let* ((site (mediawiki-get-site sitename))
         (oauth-config (mediawiki-site-auth-config site)))

    (unless oauth-config
      (error "OAuth configuration required for %s. Use mediawiki-oauth-setup to configure" sitename))

    (let ((consumer-key (plist-get oauth-config :consumer-key))
          (consumer-secret (plist-get oauth-config :consumer-secret))
          (access-token (plist-get oauth-config :access-token))
          (access-secret (plist-get oauth-config :access-secret)))

      (unless (and consumer-key consumer-secret)
        (error "OAuth consumer key and secret required for %s" sitename))

      (if (and access-token access-secret)
          ;; Use existing access tokens
          (mediawiki-oauth-verify-access sitename oauth-config)
        ;; Perform OAuth authorization flow
        (mediawiki-oauth-authorize sitename oauth-config)))))

(defun mediawiki-oauth-verify-access (sitename oauth-config)
  "Verify existing OAuth access tokens for SITENAME."
  (let ((consumer-key (plist-get oauth-config :consumer-key))
        (consumer-secret (plist-get oauth-config :consumer-secret))
        (access-token (plist-get oauth-config :access-token))
        (access-secret (plist-get oauth-config :access-secret)))

    (mediawiki-debug-log "Verifying OAuth access tokens for %s" sitename)

    ;; For now, OAuth verification requires HTTP module enhancement
    ;; This is a placeholder that would test the access tokens
    (error "OAuth token verification requires HTTP module enhancement for Authorization headers")))

(defun mediawiki-oauth-authorize (sitename oauth-config)
  "Perform OAuth 1.0a authorization flow for SITENAME."
  (let ((consumer-key (plist-get oauth-config :consumer-key))
        (consumer-secret (plist-get oauth-config :consumer-secret)))

    (mediawiki-debug-log "Starting OAuth authorization flow for %s" sitename)

    ;; For now, OAuth authorization requires HTTP module enhancement
    ;; This would implement the full OAuth 1.0a flow
    (error "OAuth authorization requires HTTP module enhancement for Authorization headers")))

;;; OAuth Utility Functions

(defun mediawiki-oauth-generate-nonce ()
  "Generate OAuth nonce."
  (format "%d%d" (time-to-seconds) (random 10000)))

(defun mediawiki-oauth-generate-signature (method url params consumer-secret token-secret)
  "Generate OAuth 1.0a signature."
  (let* ((sorted-params (sort (copy-sequence params)
                             (lambda (a b) (string< (car a) (car b)))))
         (param-string (mapconcat (lambda (param)
                                   (format "%s=%s"
                                          (url-hexify-string (car param))
                                          (url-hexify-string (cdr param))))
                                 sorted-params "&"))
         (base-string (format "%s&%s&%s"
                             (upcase method)
                             (url-hexify-string url)
                             (url-hexify-string param-string)))
         (signing-key (format "%s&%s"
                             (url-hexify-string consumer-secret)
                             (url-hexify-string (or token-secret "")))))

    ;; Generate HMAC-SHA1 signature using gnutls-hash-mac
    (base64-encode-string
     (gnutls-hash-mac 'SHA1 signing-key base-string))))

(defun mediawiki-oauth-build-auth-header (oauth-params)
  "Build OAuth Authorization header from OAUTH-PARAMS."
  (let ((auth-params (mapconcat (lambda (param)
                                 (format "%s=\"%s\""
                                        (car param)
                                        (url-hexify-string (cdr param))))
                               oauth-params ", ")))
    (format "OAuth %s" auth-params)))

(defun mediawiki-oauth-build-post-data (params)
  "Build POST data string from PARAMS."
  (mapconcat (lambda (param)
              (format "%s=%s"
                     (url-hexify-string (car param))
                     (url-hexify-string (cdr param))))
            params "&"))

;;; OAuth Configuration and Setup

(defun mediawiki-oauth-setup (sitename consumer-key consumer-secret)
  "Set up OAuth configuration for SITENAME with CONSUMER-KEY and CONSUMER-SECRET."
  (interactive "sSite name: \nsConsumer key: \nsConsumer secret: ")
  
  (let ((site (mediawiki-get-site sitename)))
    (unless site
      (error "Site %s not found. Add it first with mediawiki-add-site" sitename))

    ;; Set OAuth configuration
    (setf (mediawiki-site-auth-method site) 'oauth)
    (setf (mediawiki-site-auth-config site)
          (list :consumer-key consumer-key
                :consumer-secret consumer-secret))

    (message "OAuth configuration set for %s. Note: OAuth authentication requires HTTP module enhancement" sitename)))

(defun mediawiki-oauth-setup-with-tokens (sitename consumer-key consumer-secret access-token access-secret)
  "Set up OAuth configuration for SITENAME with both consumer and access tokens.
This is for when you already have all OAuth credentials from the OAuth provider."
  (interactive "sSite name: \nsConsumer key: \nsConsumer secret: \nsAccess token: \nsAccess secret: ")
  
  (let ((site (mediawiki-get-site sitename)))
    (unless site
      (error "Site %s not found. Add it first with mediawiki-add-site" sitename))

    ;; Set OAuth configuration with all tokens
    (setf (mediawiki-site-auth-method site) 'oauth)
    (setf (mediawiki-site-auth-config site)
          (list :consumer-key consumer-key
                :consumer-secret consumer-secret
                :access-token access-token
                :access-secret access-secret))

    (message "OAuth configuration set for %s with access tokens. Ready for authentication!" sitename)))

(defun mediawiki-oauth-reset (sitename)
  "Reset OAuth configuration for SITENAME, removing stored tokens."
  (interactive "sSite name: ")
  
  (let ((site (mediawiki-get-site sitename)))
    (unless site
      (error "Site %s not found" sitename))

    (when (eq (mediawiki-site-auth-method site) 'oauth)
      (let ((oauth-config (mediawiki-site-auth-config site)))
        (when oauth-config
          ;; Remove access tokens but keep consumer credentials
          (setf (mediawiki-site-auth-config site)
                (list :consumer-key (plist-get oauth-config :consumer-key)
                      :consumer-secret (plist-get oauth-config :consumer-secret)))
          
          ;; Clear any existing session
          (mediawiki-remove-session sitename)
          
          (message "OAuth tokens reset for %s" sitename))))))

;;; Token Management and Refresh

(defun mediawiki-oauth-refresh-tokens (sitename)
  "Refresh OAuth tokens for SITENAME by verifying current tokens."
  (let* ((site (mediawiki-get-site sitename))
         (oauth-config (mediawiki-site-auth-config site)))

    (unless oauth-config
      (error "No OAuth configuration found for %s" sitename))

    (let ((access-token (plist-get oauth-config :access-token))
          (access-secret (plist-get oauth-config :access-secret)))

      (if (and access-token access-secret)
          ;; Try to verify existing tokens
          (condition-case err
              (mediawiki-oauth-verify-access sitename oauth-config)
            (error
             (mediawiki-debug-log "OAuth token verification failed for %s: %s" 
                                 sitename (error-message-string err))
             ;; If verification fails, clear tokens and re-authorize
             (mediawiki-oauth-reset sitename)
             (error "OAuth tokens expired for %s. Please re-authenticate" sitename)))
        
        ;; No access tokens, need to authorize
        (error "No OAuth access tokens for %s. Use mediawiki-oauth-login to authenticate" sitename)))))

(provide 'mediawiki-oauth)

;;; mediawiki-oauth.el ends here