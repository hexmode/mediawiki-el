;;; mediawiki-site.el --- Site configuration and management for mediawiki.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Mark A. Hershberger

;; Author: Mark A. Hershberger <mah@everybody.org>
;; URL: https://github.com/hexmode/mediawiki-el

;; This file is part of mediawiki.el.

;; mediawiki.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mediawiki.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mediawiki.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides site configuration and management functionality
;; for mediawiki.el.  It handles site definitions, URL construction,
;; credential management, and site selection.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-http)

;;; Site Configuration

(defcustom mediawiki-pop-buffer-hook '()
  "List of functions to execute after popping to a buffer.
Can be used to to open the whole buffer."
  :options '(delete-other-windows)
  :type 'hook
  :group 'mediawiki)

(defcustom mediawiki-site-alist '(("Wikipedia"
                                    "https://en.wikipedia.org/w/"
                                    "username"
                                    "password"
                                    ""
                                    :description "English Wikipedia"
                                    :first-page "Main Page")
                                   ("Wiktionary"
                                     "https://en.wiktionary.org/w/"
                                     "username"
                                     "password"
                                     ""
                                     :description "English Wiktionary"
                                     :first-page "Main Page")
                                   ("Wikimedia Commons"
                                     "https://commons.wikimedia.org/w/"
                                     "username"
                                     "password"
                                     ""
                                     :description "Wikimedia Commons"
                                     :first-page "Main Page"))
  "List of MediaWiki sites and their configurations.
Each entry is a list of the form:
  (SITENAME URL USERNAME PASSWORD DOMAIN &rest PROPERTIES)

Where:
  SITENAME is a string identifying the site
  URL is the base URL for the MediaWiki installation
  USERNAME is the default username (can be empty string)
  PASSWORD is the default password (can be empty string)
  DOMAIN is the LDAP domain (can be empty string)
  PROPERTIES is a plist of additional properties like:
    :description - Human readable description
    :first-page - Default page to open when selecting this site
    :oauth-client-id - OAuth 2.0 consumer client ID
    :oauth-client-secret - OAuth 2.0 consumer client secret
    :oauth-access-token - Pre-obtained OAuth 2.0 access token
    :oauth-refresh-token - OAuth 2.0 refresh token for token renewal
    :oauth-token-url - Custom OAuth 2.0 token endpoint URL"
  :group 'mediawiki
  :type '(repeat (list (string :tag "Site name")
                   (string :tag "URL")
                   (string :tag "Username")
                   (string :tag "Password")
                   (string :tag "Domain")
                    (plist :tag "Properties"
                      :options ((:description string
                                  :description "Description of this site")
                                 (:first-page string
                                   :description "First page to open when `mediawiki-site' is called for this site")
                                 (:oauth-client-id string
                                   :description "OAuth 2.0 client ID (for client-credentials flow)")
                                 (:oauth-client-secret string
                                   :description "OAuth 2.0 client secret (for client-credentials flow)")
                                 (:oauth-access-token string
                                   :description "OAuth 2.0 access token (for owner-only consumers)")
                                 (:oauth-refresh-token string
                                   :description "OAuth 2.0 refresh token")
                                 (:oauth-token-url string
                                   :description "Custom OAuth 2.0 token endpoint URL"))))))

;;; Site Extraction Functions

(defun mediawiki-site--plist-start (site)
  "Return the position of the first keyword in SITE entry after the URL.
Site entries have the form (SITENAME URL . REST) where REST contains
optional positional fields (USERNAME PASSWORD DOMAIN) followed by a
keyword plist.  This function finds where the plist begins.
If no keyword is found, returns 5 (legacy position) for entries with
5+ elements, or the list length otherwise."
  (let ((i 2)
        (len (length site)))
    (while (and (< i len) (not (keywordp (nth i site))))
      (setq i (1+ i)))
    (if (< i len)
        ;; Found a keyword at position i
        i
      ;; No keyword found — legacy format
      (if (>= len 5) 5 len))))

(defun mediawiki-site-extract (sitename index)
  "Using `mediawiki-site-alist' and SITENAME, find the nth item using INDEX.
For indices 2-4 (USERNAME, PASSWORD, DOMAIN), if any earlier element
is a keyword the plist has started and those positional fields are
absent — returns nil (caller should fall back to auth-source)."
  (let* ((site (assoc sitename mediawiki-site-alist))
         (plist-start (mediawiki-site--plist-start site))
         (bit (nth index site)))
    (cond
      ((keywordp bit)
        ;; Keyword in a positional slot means that field is absent
        nil)
      ((stringp bit)
        ;; If we're at or past the plist start at indices 2-4, the string
        ;; is a plist value, not a positional field.  Use strict < because
        ;; strings at the plist-start index (e.g., legacy first-page at
        ;; position 5) are legitimate positional values.
        (if (and (> index 1) (< plist-start index))
            nil
          bit))
      ((and (listp bit) (> (length bit) 0))
        (car bit))
      ((symbolp bit)
        bit)
      (nil))))

;;;###autoload
(defun mediawiki-browse (&optional buffer)
  "Open the BUFFER in a browser.
If BUFFER is not given, the current buffer is used."
  (interactive)
  (if mediawiki-page-title
    (browse-url (mediawiki-make-url mediawiki-page-title "view"))
    (with-current-buffer buffer
      (browse-url (mediawiki-make-url mediawiki-page-title "view")))))

(declare-function mediawiki-do-login "mediawiki-auth")
(declare-function mediawiki-edit "mediawiki-page")
;;;###autoload
(defun mediawiki-site (&optional site)
  "Set up mediawiki.el for a SITE.
Without an argument, use `mediawiki-site-default'.
Interactively, prompt for a SITE."
  (interactive)
  (require 'mediawiki)
  (when (not site)
    (setq site (mediawiki-prompt-for-site)))
  (when (or (eq nil mediawiki-site)
          (not (string-equal site mediawiki-site)))
    (setq mediawiki-site (mediawiki-do-login site)))
  (mediawiki-edit site (mediawiki-site-first-page site)))

(defun mediawiki-site-url (sitename)
  "Get the url for a given SITENAME."
  (mediawiki-site-extract sitename 1))

(defmacro mediawiki-site-user-pass (sitename index method)
  "Fetch the user or pass if provided, or use authinfo if not."
  `(let* ((arg (mediawiki-site-extract ,sitename ,index))
           (auth (funcall ,method (mediawiki-site-url ,sitename))))
     (if (and (stringp arg) (> (string-width arg) 0))
       arg
       auth)))

(defun mediawiki-site-username (sitename)
  "Get the username for a given SITENAME."
  (mediawiki-site-user-pass sitename 2 'url-user-for-url))

(defun mediawiki-site-password (sitename)
  "Get the password for a given SITENAME."
  (mediawiki-site-user-pass sitename 3 'url-password-for-url))

(defun mediawiki-site-domain (sitename)
  "Get the LDAP domain for a given SITENAME."
  (let ((domain (mediawiki-site-extract sitename 4)))
    (when (and (stringp domain) (not (string= "" domain)))
      domain)))

(defun mediawiki-site-first-page (sitename)
  "Get the first page for a given SITENAME.
Supports both the keyword format (:first-page \"Page\") and the legacy
positional format where the 6th element (index 5) is the first page."
  (let ((page (or (mediawiki-site-property sitename :first-page)
                  ;; Legacy positional format: index 5 as plain string
                  (mediawiki-site-extract sitename 5))))
    (if (or (not page) (string= page ""))
      "Main Page"
      page)))

;;; Site Selection Functions

(defun mediawiki-prompt-for-site ()
  "Prompt the user for a site."
  (let* ((prompt (concat "Sitename"
                   (when mediawiki-site
                     (format " (default %s)" mediawiki-site))
                   ": "))
          (answer (completing-read prompt mediawiki-site-alist nil t)))
    (if (string= "" answer)
      mediawiki-site
      answer)))

;;; Site Management Functions

(defun mediawiki-site-list ()
  "Return a list of all configured site names."
  (mapcar 'car mediawiki-site-alist))

(defun mediawiki-site-description (sitename)
  "Get the description for a given SITENAME."
  (mediawiki-site-property sitename :description))

(defun mediawiki-site-property (sitename property)
  "Get a PROPERTY for a given SITENAME.
Site entries can be in legacy format (5 or 6 positional elements
with an optional plist) or modern format (2 positional elements
followed by a keyword plist).  The username, password, and domain
fields are optional — omit them to fall back to auth-source."
  (let ((site (assoc sitename mediawiki-site-alist)))
    (when site
      (let ((props (nthcdr (mediawiki-site--plist-start site) site)))
        (plist-get props property)))))

(defun mediawiki-site-valid-p (sitename)
  "Return t if SITENAME is a valid configured site."
  (not (null (assoc sitename mediawiki-site-alist))))

(provide 'mediawiki-site)

;;; mediawiki-site.el ends here
