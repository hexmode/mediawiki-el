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

(defcustom mediawiki-clientlogin-return-url "https://hexmode.com/"
  "Return URL for third-party authentication flows in action=clientlogin.
This URL is never actually visited by the user during Emacs-based login,
but the MediaWiki API requires it as a parameter."
  :type 'string
  :group 'mediawiki)

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
OAuth authentication instead of username/password login.

Logs in using action=clientlogin with fallback to action=login
for older wikis and bot passwords."
  (interactive)
  (when (not sitename)
    (setq sitename (mediawiki-prompt-for-site)))

  (setq mediawiki-site nil)             ; This wil be set once we are
                                        ; logged in

  ;; Check if OAuth is configured for this site
  (if (and (fboundp 'mediawiki-oauth-configured-p)
           (mediawiki-oauth-configured-p sitename))
      (progn
        (mediawiki-oauth-get-access-token sitename)
        (setq mediawiki-site sitename))

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
            (sitename sitename))
      (condition-case err
          (mediawiki-do-clientlogin sitename user pass dom)
        (error
         (let ((msg (error-message-string err)))
           (if (string-match-p "unknown_action" msg)
               (mediawiki-do-login-fallback sitename user pass dom)
             (error "%s" msg))))))))

(defun mediawiki-do-clientlogin (sitename username password &optional domain)
  "Log into SITENAME using action=clientlogin.
USERNAME and PASSWORD are the credentials.  DOMAIN is optional.
Returns SITENAME on success.

Handles multi-step auth including captcha.  Falls back to
`mediawiki-do-login-fallback' if clientlogin is not supported by the wiki."
  (let* ((token (mediawiki-site-get-token sitename "login"))
          (args (list (cons "username" username)
                  (cons "password" password)
                  (cons "logintoken" token)
                  (cons "loginreturnurl"
                    mediawiki-clientlogin-return-url)))
          (result (alist-get 'clientlogin
                    (mediawiki-api-call sitename "clientlogin" args)))
          (status (alist-get 'status result)))
    (cond
      ((string= "PASS" status)
        (setq mediawiki-site sitename)
        sitename)
      ((string= "FAIL" status)
        (error "Login failed: %s" (alist-get 'message result)))
      ((string= "UI" status)
        (mediawiki-do-clientlogin-continue sitename username password domain
          (alist-get 'requests result)))
      ((string= "REDIRECT" status)
        (error "Login requires browser-based authentication.\n\
This site requires a redirect to %s to complete login.\n\
Use M-x mediawiki-oauth-setup-site to configure OAuth 2.0,\n\
or create a bot password at [[Special:BotPassengers]] on the wiki."
          (alist-get 'redirecttarget result)))
      ((string= "RESTART" status)
        (error "Login process needs to restart: %s"
          (alist-get 'message result)))
      (t
        (error "Login returned unexpected result %s: (%s)"
          (or status "?")
          (or (alist-get 'message result) "no message"))))))

(defun mediawiki-do-clientlogin-continue (sitename username password domain requests)
  "Handle the UI step of clientlogin, such as captcha.
SITENAME, USERNAME, PASSWORD, DOMAIN are the login credentials.
REQUESTS is the list of auth requests from the API response.
Returns SITENAME on success."
  (let (result)
    (dolist (request requests)
      (let ((id (alist-get 'id request))
            (metadata (alist-get 'metadata request)))
        (cond
          ((string= id "captcha")
            (let* ((captcha-id (alist-get 'captchaId metadata))
                    (question (or (alist-get 'captchaQuestion metadata)
                               (alist-get 'question metadata)))
                    (captcha-url (or (alist-get 'captchaUrl metadata)
                                  (alist-get 'url metadata)))
                    (login-token (mediawiki-site-get-token sitename "login"))
                    (answer (progn
                              (when question
                                (message "%s" question))
                              (when captcha-url
                                (message "Captcha image: %s" captcha-url))
                              (read-string "Captcha answer: ")))
                    (args (list (cons "username" username)
                            (cons "password" password)
                            (cons "logintoken" login-token)
                            (cons "loginreturnurl"
                              mediawiki-clientlogin-return-url)
                            (cons "logincontinue" "1")
                            (cons "captchaId" captcha-id)
                            (cons "captchaWord" answer)))
                    (api-result (alist-get 'clientlogin
                                  (mediawiki-api-call sitename "clientlogin" args)))
                    (status (alist-get 'status api-result)))
              (setq result
                (cond
                  ((string= "PASS" status)
                    (setq mediawiki-site sitename)
                    sitename)
                  ((string= "FAIL" status)
                    (error "Login failed: %s" (alist-get 'message api-result)))
                  (t
                    (error "Login returned unexpected result %s: (%s)"
                      (or status "?")
                      (or (alist-get 'message api-result) "no message")))))))
          (t
            (error "Login requires unsupported user interaction: %s" id)))))
    (or result (error "No authentication requests were handled"))))

(defun mediawiki-do-login-fallback (sitename username password &optional domain)
  "Fall back to the old action=login method.
This works for older wikis that do not support action=clientlogin
and for bot passwords."
  (let* ((token (condition-case nil
                   (mediawiki-site-get-token sitename "login")
                   (error nil)))
          (args (list (cons "lgname" username)
                  (cons "lgpassword" password)
                  (when token
                    (cons "lgtoken" token))
                  (when domain
                    (cons "lgdomain" domain))))
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
        (setq mediawiki-site sitename)
        sitename)
      ((string= "Failed" (alist-get 'result result))
        (error "Login failed: %s" (alist-get 'reason result)))
      (t
        (let ((result-str (alist-get 'result result))
              (reason (alist-get 'reason result)))
          (if (and (string= "Aborted" result-str)
                   (string-match-p "user interaction" reason))
              (error "Login aborted: %s\n\n\
This site no longer supports direct password login for this account.\n\
Use M-x mediawiki-oauth-setup-site to configure OAuth 2.0,\n\
or create a bot password at [[Special:BotPassengers]] on the wiki."
                reason)
            (error "Login returned unexpected result: %s (%s)"
              result-str
              (or reason "see documentation at https://www.mediawiki.org/wiki/API:Login#Error_types"))))))))

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
