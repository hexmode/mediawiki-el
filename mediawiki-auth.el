;;; mediawiki-auth.el --- Authentication functionality for MediaWiki  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Mark A. Hershberger

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

;; This module provides authentication functionality for MediaWiki including
;; login/logout functions, token management, authentication state checking,
;; and cookie handling utilities.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-api)
(require 'mediawiki-site)
(require 'url-cookie)
(require 'url-parse)

(declare-function mediawiki-oauth-configured-p "mediawiki-oauth")
(declare-function mediawiki-oauth-get-access-token "mediawiki-oauth")

;;; Authentication Constants

(defvar mediawiki-login-success "pt-logout"
  "String to look for in HTML response.
This will be used to verify a successful login.")

;;; Authentication State Checking

(defun mediawiki-logged-in-p (&optional sitename)
  "Return t if we have cookies or an OAuth token for SITENAME.
When OAuth 2.0 is configured for the site and a valid access token
is available, return t even if no cookies are present."
  (or (and (fboundp 'mediawiki-oauth-configured-p)
           (fboundp 'mediawiki-oauth-access-token)
           sitename
           (mediawiki-oauth-configured-p sitename)
           (mediawiki-oauth-access-token sitename))
      (let ((urlobj (url-generic-parse-url
                      (mediawiki-site-url (or sitename mediawiki-site)))))
        (url-cookie-retrieve
          (url-host urlobj)
          (url-filename urlobj)
          (equal "https" (url-type urlobj))))))

;;; Login and Logout Functions

;;;###autoload
(defun mediawiki-do-login (&optional sitename username password)
  "Log into SITENAME using USERNAME, PASSWORD and DOMAIN.
Store cookies for future authentication.

If OAuth 2.0 is configured for SITENAME, this function will use
OAuth authentication instead of username/password login.  The OAuth
access token will be obtained (via client credentials flow) or
refreshed as needed."
  (interactive)
  (when (not sitename)
    (setq sitename (mediawiki-prompt-for-site)))

  (setq mediawiki-site nil)             ; This wil be set once we are
                                        ; logged in

  ;; Check if OAuth is configured for this site
  (if (and (fboundp 'mediawiki-oauth-configured-p)
           (mediawiki-oauth-configured-p sitename))
      ;; Use OAuth authentication
      (progn
        (mediawiki-oauth-get-access-token sitename)
        (setq mediawiki-site sitename))

    ;; Fall back to traditional username/password login
    ;; Possibly save info once we have it, eh?
    (let* ((user (or (mediawiki-site-username sitename)
                   username
                   (read-string "Username: ")))
            (pass (or (mediawiki-site-password sitename)
                    password
                    (read-passwd "Password: ")))
            (dom-loaded (mediawiki-site-domain sitename))
            (dom (when dom-loaded
                   (if (string= "" dom-loaded)
                     (read-string "LDAP Domain: ")
                     dom-loaded)))
            (sitename sitename)
            (token (condition-case nil
                     (mediawiki-site-get-token sitename "login")
                     (error nil)))           ; pre-1.27 wikis lack the token API
            (args (list (cons "lgname" user)
                    (cons "lgpassword" pass)
                    (when token
                      (cons "lgtoken" token))
                    (when dom
                      (cons "lgdomain" dom))))
            (result (alist-get 'login (mediawiki-api-call sitename "login" args))))
      (when (string= (alist-get 'result result) "NeedToken")
        (setq result
          (alist-get 'login
            (mediawiki-api-call sitename "login"
              (append
                args (list (cons "lgtoken"
                             (alist-get 'token result))))))))
      (cond
        ((string= "Success" (alist-get 'result result))
          sitename)
        ((string= "Failed" (alist-get 'result result))
          (error "Login failed: %s" (alist-get 'reason result)))
        (t
          (let ((result-str (alist-get 'result result))
                (reason (alist-get 'reason result)))
            (if (and (string= "Aborted" result-str)
                     (string-match-p "user interaction" reason))
                (error "Login aborted: %s\n\nThis site no longer supports direct password login for this account.\nConfigure OAuth 2.0 authentication with M-x mediawiki-oauth-setup-site,\nor add :oauth-access-token to the site configuration in mediawiki-site-alist.\nSee OAUTH-SETUP.org for details."
                  reason)
              (error "Login returned unexpected result: %s (%s)"
                result-str
                (or reason "see documentation at https://www.mediawiki.org/wiki/API:Login#Error_types")))))))))

;;;###autoload
(defun mediawiki-do-logout (&optional sitename)
  "Log out of SITENAME."
  (interactive)
  (let ((sitename (or sitename (mediawiki-prompt-for-site))))
    (mediawiki-api-call sitename "logout"
      (list (cons "token" (mediawiki-site-get-token sitename "csrf"))))
    (setq mediawiki-site nil)))

(provide 'mediawiki-auth)

;;; mediawiki-auth.el ends here
