;;; mediawiki-site.el --- Site configuration and management for mediawiki.el -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2024 Mark A. Hershberger

;; Author: Mark A. Hershberger <mah@everybody.org>
;; Version: 2.3.1
;; Created: Sep 11 2008
;; Keywords: mediawiki wikipedia network wiki
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
;; for mediawiki.el. It handles site definitions, URL construction,
;; credential management, and site selection.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-http)

;;; Site Configuration

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
    :first-page - Default page to open when selecting this site"
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
                                                     :description "First page to open when `mediawiki-site' is called for this site"))))))

;;; Site Extraction Functions

(defun mediawiki-site-extract (sitename index)
  "Using 'mediawiki-site-alist' and SITENAME, find the nth item using INDEX."
  (let* ((site (assoc sitename mediawiki-site-alist))
         (bit (nth index site)))
    (cond
     ((stringp bit)
      bit)
     ((and (listp bit) (> (length bit) 0))
      (car bit))
     (nil))))

(defun mediawiki-site-url (sitename)
  "Get the url for a given SITENAME."
  (mediawiki-site-extract sitename 1))

(defmacro mediawiki-site-user-pass (sitename index method)
  "Fetch the user or pass if provided, or use authinfo if not."
  `(let* ((arg (mediawiki-site-extract ,sitename ,index))
          (auth (funcall ,method (mediawiki-site-url ,sitename))))
     (if (and arg (> (string-width arg) 0))
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
    (when (and domain (not (string= "" domain)))
      domain)))

(defun mediawiki-site-first-page (sitename)
  "Get the first page for a given SITENAME."
  (let ((page (mediawiki-site-extract sitename 5)))
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
  (let ((site (assoc sitename mediawiki-site-alist)))
    (when site
      (plist-get (nthcdr 5 site) :description))))

(defun mediawiki-site-property (sitename property)
  "Get a PROPERTY for a given SITENAME."
  (let ((site (assoc sitename mediawiki-site-alist)))
    (when site
      (plist-get (nthcdr 5 site) property))))

(defun mediawiki-site-valid-p (sitename)
  "Return t if SITENAME is a valid configured site."
  (not (null (assoc sitename mediawiki-site-alist))))

(provide 'mediawiki-site)

;;; mediawiki-site.el ends here
