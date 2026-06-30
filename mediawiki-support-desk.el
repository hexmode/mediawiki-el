;;; mediawiki-support-desk.el --- Support desk convenience wrapper  -*- lexical-binding: t; -*-

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

;; This module is a thin wrapper around `mediawiki-discussion-tools.el'
;; configured for the MediaWiki support desk at
;; https://www.mediawiki.org/wiki/Project:Support_desk.
;;
;; The support desk is a standard wikitext discussion page.  Threads are
;; wiki sections with `:`-indented replies, auto-archived by ArchiverBot
;; after 14 days of inactivity, and optionally marked resolved with the
;; {{Resolved}} template.
;;
;; Key functions:
;;
;;   `mediawiki-support-desk'           - Open the support desk thread list
;;
;; See SUPPORT-DESK-WIKI-IMPLEMENTATION.md and SUPPORT-DESK-PLAN.md for
;; details on how the support desk works and the implementation plan.

;;; Code:

(require 'mediawiki-discussion-tools)

;;; Customization

(defgroup mediawiki-support-desk nil
  "Support desk convenience wrapper for mediawiki.el."
  :tag "MediaWiki Support Desk"
  :group 'mediawiki)

(defcustom mediawiki-support-desk-page "Project:Support_desk"
  "Page name for the support desk on the wiki.
This is the page whose sections are displayed as threads."
  :type 'string
  :group 'mediawiki-support-desk)

;;; Entry Point

;;;###autoload
(defun mediawiki-support-desk (&optional sitename)
  "Open the MediaWiki support desk thread list.
If SITENAME is nil, use the current `mediawiki-site' or prompt.
This is a convenience wrapper around `mediawiki-discussion-tools'
configured for the `mediawiki-support-desk-page'."
  (interactive)
  (mediawiki-discussion-tools mediawiki-support-desk-page sitename))

(provide 'mediawiki-support-desk)

;;; mediawiki-support-desk.el ends here
