;;; mediawiki-core.el --- Core variables and constants for mediawiki.el  -*- lexical-binding: t; -*-

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

;; This file contains the core variables, constants, and fundamental
;; utilities that other mediawiki.el modules depend on.

;;; Code:

(require 'mm-url)

;;; Version and Core Constants

(defconst mediawiki-version "2.4.6"
  "Current version of mediawiki.el.")

;;; Core Customization Groups

(defgroup mediawiki nil
  "A mode for editting pages on MediaWiki sites."
  :tag "MediaWiki"
  :group 'applications)

;;; Core Variables

(defvar mediawiki-permission-denied
  "[^;]The action you have requested is limited"
  "String that indicates permission has been denied.
Note that it should not match the mediawiki.el file itself since
it is sometimes put on MediaWiki sites.")

(defvar mediawiki-view-source
  "ca-viewsource"
  "String that indicates you cannot edit this page.")

(defvar mediawiki-page-uri nil
  "The URI of the page corresponding to the current buffer.
This is used to determine the base URI of the wiki engine as well
as group and page name.")

(defcustom mediawiki-site-default "Wikipedia"
  "The default mediawiki site to point to.
Set here for the default and use `mediawiki-site' to set it
per-session later."
  :type 'string
  :tag "MediaWiki Site Default"
  :group 'mediawiki)

(defvar mediawiki-site nil
  "The current mediawiki site from `mediawiki-site-alist'.
If not set, defaults to `mediawiki-site-default'.")

(defvar mediawiki-page-history '()
  "Assoc list of visited pages on this MW site.")

(defvar mediawiki-site-info nil
  "Holds the site information fetched in this session.")

;;; Core Constants and Patterns

(defvar mediawiki-argument-pattern "?title=%s&action=%s"
  "Format of the string to append to URLs.
Two string arguments are expected: first is a title and then an
action.")

(defvar mediawiki-URI-pattern
  "https?://\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?/"
  "Pattern match for a URI.
Expected to match something like this:
	https://mediawiki.sf.net/index.php
Passwords in the URL are not supported yet")

;;; Fundamental Utility Functions

(defun mediawiki-translate-pagename (name)
  "Given NAME, return the typical name that MediaWiki would use.
Right now, this only means replacing \"_\" with \" \"."
  (when name
    (replace-regexp-in-string "_" " " name)))

(declare-function mediawiki-site-url "mediawiki-site")
(defun mediawiki-make-url (title action &optional sitename)
  "Return a url when given a TITLE, ACTION and, optionally, SITENAME."
  (format (concat (mediawiki-site-url (or sitename mediawiki-site))
            (if action
              mediawiki-argument-pattern
              "?title=%s"))
    (mm-url-form-encode-xwfu
      (mediawiki-translate-pagename title))
    action))

(provide 'mediawiki-core)

;;; mediawiki-core.el ends here
