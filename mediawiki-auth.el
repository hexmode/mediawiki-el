;;; mediawiki-auth.el --- Authentication functionality for MediaWiki  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2025 Mark A. Hershberger

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
(require 'cl)

;;; Authentication Constants

(defvar mediawiki-login-success "pt-logout"
  "String to look for in HTML response.
This will be used to verify a successful login.")

;;; Authentication State Checking

(defun mediawiki-logged-in-p (&optional sitename)
  "Return t if we have cookies for the SITENAME."
  (let ((urlobj (url-generic-parse-url
                  (mediawiki-site-url (or sitename mediawiki-site)))))
    (url-cookie-retrieve
      (url-host urlobj)
      (url-filename urlobj)
      (equal "https" (url-type urlobj)))))

;;; Login and Logout Functions

;;;###autoload
(defun mediawiki-do-login (&optional sitename username password)
  "Log into SITENAME using USERNAME, PASSWORD and DOMAIN.
Store cookies for future authentication."
  (interactive)
  (when (not sitename)
    (setq sitename (mediawiki-prompt-for-site)))

  (setq mediawiki-site nil)             ; This wil be set once we are
                                        ; logged in

  ;; Possibly save info once we have it, eh?
  (lexical-let* ((user (or (mediawiki-site-username sitename)
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
                  (token (mediawiki-site-get-token sitename "login"))
                  (args (list (cons "lgname" user)
                          (cons "lgpassword" pass)
                          (when token
                            (cons "lgtoken" token))
                          (when dom
                            (cons "lgdomain" dom))))
                  (result (cadr (mediawiki-api-call sitename "login" args))))
    (when (string= (cdr (assq 'result result)) "NeedToken")
      (setq result
        (cadr (mediawiki-api-call sitename "login"
                (append
                  args (list (cons "lgtoken"
                               (cdr (assq 'token result)))))))))
    (when (string= "Success" (cdr (assoc 'result result)))
      sitename)))

(defun mediawiki-do-logout (&optional sitename)
  "Log out of SITENAME."
  (interactive)
  (when (not sitename)
    (setq sitename (mediawiki-prompt-for-site)))

  (mediawiki-api-call sitename "logout" nil)
  (setq mediawiki-site nil))

(provide 'mediawiki-auth)

;;; mediawiki-auth.el ends here
