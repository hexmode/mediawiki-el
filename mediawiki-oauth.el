;;; mediawiki-oauth.el --- OAuth 2.0 authentication for MediaWiki  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mark A. Hershberger

;; Author: Mark A. Hershberger <mah@everybody.org>
;; URL: https://github.com/hexmode/mediawiki-el

;; This file is NOT (yet) part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides OAuth 2.0 authentication support for MediaWiki.
;;
;; MediaWiki bot passwords are deprecated in favor of OAuth 2.0 owner-only
;; consumers.  This module implements the OAuth 2.0 client credentials flow
;; to obtain access tokens, plus Bearer token authentication for API calls.
;;
;; To use OAuth for a site, add the following properties to your site
;; configuration in `mediawiki-site-alist':
;;
;;   :oauth-client-id     - The OAuth consumer client ID
;;   :oauth-client-secret - The OAuth consumer client secret
;;   :oauth-access-token  - (Optional) Pre-obtained access token
;;   :oauth-refresh-token - (Optional) Refresh token for token renewal
;;   :oauth-token-url     - (Optional) Custom token endpoint URL
;;
;; If an access token is provided, it will be used directly.  Otherwise,
;; if a client ID and client secret are provided, the client credentials
;; flow will be used to obtain an access token automatically.
;;
;; The module maintains backward compatibility with username/password
;; authentication.  OAuth is used only when configured for a site.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-site)
(require 'json)
(require 'auth-source)
(require 'url-parse)

;;; OAuth Site Properties

(defun mediawiki-oauth--auth-source-secret (sitename port)
  "Look up a secret from auth-source for SITENAME identified by PORT.
Uses the client ID as the :user field in auth-source.
Returns the secret string or nil if not found."
  (when-let* ((client-id (mediawiki-site-property sitename :oauth-client-id))
              (url (url-generic-parse-url (mediawiki-site-url sitename)))
              (host (url-host url))
              (found (auth-source-search :host host :user client-id
                                         :port port :max 1
                                         :require '(:user :secret))))
    (let ((secret (plist-get (car found) :secret)))
      (if (functionp secret) (funcall secret) secret))))

(defun mediawiki-oauth-configured-p (sitename)
  "Return t if OAuth authentication is configured for SITENAME.
OAuth is considered configured if either an access token is stored
(or available from auth-source), or if a client ID and client secret
are available (either in the site properties or from auth-source)."
  (or (mediawiki-oauth-access-token sitename)
      (and (mediawiki-site-property sitename :oauth-client-id)
           (mediawiki-oauth-client-secret sitename))))

(defun mediawiki-oauth-token-endpoint (sitename)
  "Return the OAuth 2.0 token endpoint URL for SITENAME.
If a custom endpoint is configured via :oauth-token-url property,
that is used.  Otherwise, the endpoint is derived from the site URL."
  (or (mediawiki-site-property sitename :oauth-token-url)
      (let ((url (mediawiki-site-url sitename)))
        (cond
          ((string-match "/w/\\'" url)
           (replace-regexp-in-string "/w/\\'" "/w/rest.php/oauth2/access_token" url))
          ((string-match "/wiki/\\'" url)
           (replace-regexp-in-string "/wiki/\\'" "/wiki/rest.php/oauth2/access_token" url))
          (t
           (concat (replace-regexp-in-string "/\\'" "" url)
                   "/rest.php/oauth2/access_token"))))))

(defun mediawiki-oauth-make-auth-header (access-token)
  "Create an Authorization header value using ACCESS-TOKEN."
  (cons "Authorization" (concat "Bearer " access-token)))

;;; Token Storage and Retrieval

(defun mediawiki-oauth-access-token (sitename)
  "Return the stored OAuth access token for SITENAME, or nil.
If the token has expired, return nil.
If not stored in `mediawiki-site-alist', falls back to auth-source
using port \"mediawiki-access-token\"."
  (let ((token (mediawiki-site-property sitename :oauth-access-token))
        (expiry (mediawiki-site-property sitename :oauth-token-expiry)))
    (if (and token expiry (time-less-p expiry (current-time)))
        nil
      (or token
          (mediawiki-oauth--auth-source-secret sitename "mediawiki-access-token")))))

(defun mediawiki-oauth-refresh-token (sitename)
  "Return the stored OAuth refresh token for SITENAME, or nil."
  (mediawiki-site-property sitename :oauth-refresh-token))

(defun mediawiki-oauth-client-id (sitename)
  "Return the OAuth client ID for SITENAME, or nil."
  (mediawiki-site-property sitename :oauth-client-id))

(defun mediawiki-oauth-client-secret (sitename)
  "Return the OAuth client secret for SITENAME, or nil.
If not stored in `mediawiki-site-alist', falls back to auth-source
using port \"mediawiki-client-secret\"."
  (or (mediawiki-site-property sitename :oauth-client-secret)
      (mediawiki-oauth--auth-source-secret sitename "mediawiki-client-secret")))

(defun mediawiki-oauth--update-site-properties (sitename new-props)
  "Replace the plist tail of SITENAME's site entry with NEW-PROPS.
This mutates `mediawiki-site-alist' in place so the change persists."
  (let ((site (assoc sitename mediawiki-site-alist)))
    (unless site
      (error "Site %s not found in mediawiki-site-alist" sitename))
    (let ((plist-pos (mediawiki-site--plist-start site)))
      (setcdr (nthcdr (1- plist-pos) site) new-props))))

(defun mediawiki-oauth-set-tokens (sitename access-token &optional refresh-token expiry-seconds)
  "Store OAuth tokens for SITENAME.
ACCESS-TOKEN is the new access token.  REFRESH-TOKEN is the optional
refresh token.  EXPIRY-SECONDS is the optional token lifetime in seconds.
The tokens are stored as properties in `mediawiki-site-alist'."
  (let* ((site (assoc sitename mediawiki-site-alist))
         (plist-start (mediawiki-site--plist-start site))
         (tail (nthcdr plist-start site))
         (new-props
          (if (keywordp (car tail))
              ;; Already a plist — copy and update.
              (let ((p (copy-sequence tail)))
                (setq p (plist-put p :oauth-access-token access-token))
                (setq p (plist-put p :oauth-refresh-token refresh-token))
                (setq p (plist-put p :oauth-token-expiry
                                   (if expiry-seconds
                                       (time-add (current-time) expiry-seconds)
                                     nil)))
                p)
            ;; Legacy format — build a plist, preserving first-page.
            (let ((p nil))
              (when (and (stringp (car tail))
                         (not (string= "" (car tail))))
                (setq p (plist-put p :first-page (car tail))))
              (setq p (plist-put p :oauth-access-token access-token))
              (setq p (plist-put p :oauth-refresh-token refresh-token))
              (setq p (plist-put p :oauth-token-expiry
                                 (if expiry-seconds
                                     (time-add (current-time) expiry-seconds)
                                   nil)))
              p))))
    (mediawiki-oauth--update-site-properties sitename new-props)))

;;; HTTP Utilities

(defun mediawiki-oauth-retrieve-body (buffer)
  "Extract the response body from BUFFER after url-retrieve.
Returns the decoded body string, or signals an error if the response
is malformed."
  (with-current-buffer buffer
    (if (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
        (progn
          (goto-char url-http-end-of-headers)
          (forward-line)
          (decode-coding-string
           (buffer-substring-no-properties (point) (point-max))
           'utf-8))
      (error "OAuth request failed: no response body found"))))

;;; Token Acquisition

(defun mediawiki-oauth-request-access-token (sitename)
  "Request an OAuth 2.0 access token for SITENAME using client credentials.
This implements the OAuth 2.0 client credentials grant type.
The client ID and client secret must be configured for the site.
On success, the access token (and refresh token, if provided) are stored
in the site's properties and the access token is returned."
  (let* ((client-id (mediawiki-oauth-client-id sitename))
         (client-secret (mediawiki-oauth-client-secret sitename))
         (token-url (mediawiki-oauth-token-endpoint sitename)))
    (unless (and client-id client-secret)
      (error "OAuth client ID and secret not configured for %s" sitename))
    (let* ((url-request-method "POST")
           (url-request-extra-headers
            '(("Content-Type" . "application/x-www-form-urlencoded")))
           (url-request-data
            (mm-url-encode-www-form-urlencoded
             `(("grant_type" . "client_credentials")
               ("client_id" . ,client-id)
               ("client_secret" . ,client-secret))))
           (buffer (condition-case err
                       (url-retrieve-synchronously token-url)
                     (error (error "OAuth token request failed: %s"
                                   (error-message-string err)))))
           (raw (mediawiki-oauth-retrieve-body buffer))
           (result (condition-case _err
                       (json-parse-string raw
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil)
                     (error
                      (error "OAuth token response parse error: %s" raw)))))
      (when-let* ((err-data (alist-get 'error result)))
        (error "OAuth token request failed: %s - %s"
               (alist-get 'error result)
               (or (alist-get 'error_description result) "no description")))
      (let ((access-token (alist-get 'access_token result))
            (refresh-token (alist-get 'refresh_token result))
            (expires-in (alist-get 'expires_in result)))
        (unless access-token
          (error "OAuth token response did not contain access_token: %s" raw))
        (mediawiki-oauth-set-tokens sitename access-token refresh-token expires-in)
        access-token))))

;;; Token Refresh

(defun mediawiki-oauth-refresh-access-token (sitename)
  "Refresh the OAuth access token for SITENAME using the refresh token.
If no refresh token is available, signal an error.
On success, the new tokens are stored and the new access token is returned."
  (let* ((refresh-token (mediawiki-oauth-refresh-token sitename))
         (client-id (mediawiki-oauth-client-id sitename))
         (client-secret (mediawiki-oauth-client-secret sitename))
         (token-url (mediawiki-oauth-token-endpoint sitename)))
    (unless refresh-token
      (error "No refresh token available for %s" sitename))
    (let* ((url-request-method "POST")
           (url-request-extra-headers
            '(("Content-Type" . "application/x-www-form-urlencoded")))
           (url-request-data
            (mm-url-encode-www-form-urlencoded
             `(("grant_type" . "refresh_token")
               ("refresh_token" . ,refresh-token)
               ("client_id" . ,client-id)
               ("client_secret" . ,client-secret))))
           (buffer (condition-case err
                       (url-retrieve-synchronously token-url)
                     (error (error "OAuth token refresh failed: %s"
                                   (error-message-string err)))))
           (raw (mediawiki-oauth-retrieve-body buffer))
           (result (condition-case _err
                       (json-parse-string raw
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil)
                     (error
                      (error "OAuth refresh response parse error: %s" raw)))))
      (when-let* ((err-data (alist-get 'error result)))
        (error "OAuth token refresh failed: %s - %s"
               (alist-get 'error result)
               (or (alist-get 'error_description result) "no description")))
      (let ((access-token (alist-get 'access_token result))
            (new-refresh-token (alist-get 'refresh_token result))
            (expires-in (alist-get 'expires_in result)))
        (unless access-token
          (error "OAuth refresh response did not contain access_token: %s" raw))
        (mediawiki-oauth-set-tokens sitename access-token new-refresh-token expires-in)
        access-token))))

;;; Public Token Access

(defun mediawiki-oauth-get-access-token (sitename)
  "Return a valid OAuth access token for SITENAME.
If a non-expired access token is stored, return it.
If no token exists or it has expired, request a new one using
the client credentials flow.  If a refresh token is available and
the access token is expired, attempt to refresh it first."
  (or (mediawiki-oauth-access-token sitename)
      (if (mediawiki-oauth-refresh-token sitename)
          (mediawiki-oauth-refresh-access-token sitename)
        (mediawiki-oauth-request-access-token sitename))))

;;; Interactive Setup

;;;###autoload
(defun mediawiki-oauth-setup-site (sitename client-id client-secret &optional access-token)
  "Configure OAuth authentication for SITENAME.
CLIENT-ID is the OAuth consumer client ID.
CLIENT-SECRET is the OAuth consumer client secret.
Optional ACCESS-TOKEN is a pre-obtained access token.  If not
provided, the client credentials flow will be used to obtain one
automatically when needed.

This mutates the site entry in `mediawiki-site-alist' in place so
the configuration persists across the Emacs session.  To save it
permanently, use `customize-save-variable' or persist your init file."
  (interactive
   (let* ((site (mediawiki-prompt-for-site))
          (id (read-string (format "OAuth client ID for %s: " site)))
          (secret (read-passwd (format "OAuth client secret for %s: " site)))
          (token (read-string
                  (format "Access token for %s (leave empty to use client credentials): " site)
                  nil nil "")))
     (list site id secret (if (string= token "") nil token))))
  (let* ((site (assoc sitename mediawiki-site-alist))
         (plist-start (mediawiki-site--plist-start site))
         (tail (nthcdr plist-start site))
         (new-props
          (if (keywordp (car tail))
              ;; Already a plist — copy and update.
              (let ((p (copy-sequence tail)))
                (setq p (plist-put p :oauth-client-id client-id))
                (setq p (plist-put p :oauth-client-secret client-secret))
                (setq p (plist-put p :oauth-access-token access-token))
                (setq p (plist-put p :oauth-refresh-token nil))
                (setq p (plist-put p :oauth-token-expiry nil))
                p)
            ;; Legacy format — build a plist, preserving first-page.
            (let ((p nil))
              (when (and (stringp (car tail))
                         (not (string= "" (car tail))))
                (setq p (plist-put p :first-page (car tail))))
              (setq p (plist-put p :oauth-client-id client-id))
              (setq p (plist-put p :oauth-client-secret client-secret))
              (setq p (plist-put p :oauth-access-token access-token))
              (setq p (plist-put p :oauth-refresh-token nil))
              (setq p (plist-put p :oauth-token-expiry nil))
              p))))
    (mediawiki-oauth--update-site-properties sitename new-props))
  (message "OAuth configured for %s" sitename)
  (when (and (called-interactively-p 'any)
             (y-or-n-p (format "Save OAuth configuration for %s to Customize? " sitename)))
    (customize-save-variable 'mediawiki-site-alist mediawiki-site-alist)
    (message "OAuth configuration for %s saved" sitename)))

;;;###autoload
(defun mediawiki-oauth-clear-tokens (sitename)
  "Clear all OAuth tokens for SITENAME.
This removes the access token, refresh token, and expiry information
from the site's configuration, but preserves the client ID and secret."
  (interactive (list (mediawiki-prompt-for-site)))
  (let* ((site (assoc sitename mediawiki-site-alist))
         (plist-start (mediawiki-site--plist-start site))
         (tail (nthcdr plist-start site))
         (new-props
          (if (keywordp (car tail))
              ;; Already a plist — copy and update.
              (let ((p (copy-sequence tail)))
                (setq p (plist-put p :oauth-access-token nil))
                (setq p (plist-put p :oauth-refresh-token nil))
                (setq p (plist-put p :oauth-token-expiry nil))
                p)
            ;; Legacy format — build a plist, preserving first-page.
            (let ((p nil))
              (when (and (stringp (car tail))
                         (not (string= "" (car tail))))
                (setq p (plist-put p :first-page (car tail))))
              (setq p (plist-put p :oauth-access-token nil))
              (setq p (plist-put p :oauth-refresh-token nil))
              (setq p (plist-put p :oauth-token-expiry nil))
              p))))
    (mediawiki-oauth--update-site-properties sitename new-props))
  (message "OAuth tokens cleared for %s" sitename))

(provide 'mediawiki-oauth)

;;; mediawiki-oauth.el ends here
