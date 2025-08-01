;;; mediawiki.el --- mediawiki frontend  -*- lexical-binding: t; -*-

;; Copyright (C) 2008, 2009, 2010, 2011, 2015 Mark A. Hershberger

;; Original Authors: Jerry <unidevel@yahoo.com.cn>,
;;      Chong Yidong <cyd at stupidchicken com> for wikipedia.el,
;;      Uwe Brauer <oub at mat.ucm.es> for wikimedia.el
;; Author: Mark A. Hershberger <mah@everybody.org>
;; Package-Requires: ((emacs "28.1"))
;; Version: 2.4.3
;; Created: Sep 17 2004
;; Keywords: mediawiki wikipedia network wiki
;; URL: https://github.com/hexmode/mediawiki-el
;; Package-Type: multi
;; Last Modified: <2025-08-01 02:38:11 mah>

(defconst mediawiki-version "2.4.3"
  "Current version of mediawiki.el.")

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

;; This version of mediawiki.el represents a merging of
;; wikipedia-mode.el (maintained by Uwe Brauer <oub at mat.ucm.es>)
;; from https://www.emacswiki.org/emacs/wikipedia-mode.el for its
;; font-lock code, menu, draft mode, replying and convenience
;; functions to produce mediawiki.el 2.0.

;;; Installation

;; If you use ELPA, you can install via the M-x package-list-packages
;; interface. This is preferrable as you will have access to updates
;; automatically.

;; Otherwise, just make sure this file is in your load-path (usually
;; ~/.emacs.d is included) and put (require 'mediawiki.el) in your
;; ~/.emacs or ~/.emacs.d/init.el file.

;;; Howto:
;;  M-x customize-group RET mediawiki RET
;;  *dink* *dink*
;;  M-x mediawiki-site RET Wikipedia RET
;;
;; Open a wiki file:    M-x mediawiki-open
;; Save a wiki buffer:  C-x C-s
;; Save a wiki buffer with a different name:  C-x C-w

;;; TODO

;;  * Optionally use org-mode formatting for editing and translate
;;    that to mw
;;  * Move url-* methods to url-http
;;  * Use the MW API to support searching, etc.
;;  * Clean up and thoroughly test imported wikimedia.el code
;;  * Improve language support.  Currently there is a toggle for
;;    English or German.  This should probably just be replaced with
;;    customizable words given MediaWiki's wide language support.

;;; Changes

;; 2.2.7:
;;  * Add the ability to accept the domain
;;  * Fix false failures when site isn't found.

;; 2.2.6:
;;  * Moved to github
;;  * Code cleanup, flycheck

;; Since 2.2.4.2
;;  * Move to github
;;  * Added Readme.mediawiki to with information about security.

;; Since 2.2.4.1:
;;  * Add the forgotten customizable mediawiki-debug.

;; Since 2.2.4:
;;  * Made it clearer where debugging information is found when
;;    mediawiki-debug is non-nil by adding messages to the message
;;    buffer when debug buffers are killed.

;;; History

;; From the News section of wikipedia.el comes this bit, kept here for
;; reference later.
;;     (4) "Draft", "send" and "reply" (for discussion pages)
;;         abilities `based' on ideas of John Wigleys remember.el: see
;;         the functions wikipedia-draft-*
;;         RATIONALE: This comes handy in 2 situations
;;            1. You are editing articles which various authors (this I
;;               think is the usual case), you then want not to submit
;;               your edit immediately but want to copy it somewhere and
;;               to continue later. You can use the following functions
;;               for doing that:
;;               wikipedia-draft-buffer \C-c\C-b
;;               wikipedia-draft-region \C-c\C-r
;;               then the buffer/region will be appended to the
;;               wikipedia-draft-data-file (default is
;;               "~/Wiki/discussions/draft.wiki", which you can visit via
;;               wikipedia-draft-view-draft) and it will be
;;               surrounded by the ^L marks in order to set a page.
;;               moreover on top on that a section header == will be
;;               inserted, which consists of the Word Draft, a subject
;;               you are asked for and a date stamp.
;;
;;               Another possibility consists in using the function
;;               wikipedia-draft, bound to \C-c \C-m then a new buffer
;;               will opened already in wikipedia mode. You edit and then
;;               either can send the content of the buffer to the
;;               wikipedia-draft-data-file in the same manner as
;;               described above using the function
;;               wikipedia-draft-buffer (bound to \C-c\C-k)
;;
;;               BACK: In order to copy/send the content of temporary
;;               buffer or of a page in the wikipedia-draft-data-file
;;               back in to your wikipedia file, use the function
;;               wikipedia-send-draft-to-mozex bound to "\C-c\C-c". You
;;               will be asked to which buffer to copy your text!
;;
;;
;;            2. You want to reply  in a discussion page to a specific
;;               contribution, you can use either the function
;;
;;               \\[wikipedia-reply-at-point-simple] bound to [(meta shift r)]
;;               which inserts a newline, a hline, and the signature of
;;               the author. Or can use
;;               \\[wikipedia-draft-reply] bound  [(meta r)]
;;               which does the same as wikipedia-reply-at-point-simple
;;               but in a temporary draft buffer.
;;
;;               BACK: In order to copy/send the content of that buffer
;;               back in to your wikipedia file, use the function
;;               \\[wikipedia-send-draft-to-mozex] bound to "\C-c\C-c". You
;;               will be asked to which buffer to copy your text! If
;;               you want a copy to be send to your draft file, use
;;               the variable  wikipedia-draft-send-archive
;;

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-utils)
(require 'mediawiki-http)
(require 'mediawiki-site)
(require 'mediawiki-api)
(require 'mediawiki-auth)
(require 'mediawiki-page)
(require 'mediawiki-faces)
(require 'mediawiki-font-lock)
(require 'mediawiki-draft)
(require 'mediawiki-mode)
(require 'ring)
(require 'subr-x)

(defcustom mediawiki-pop-buffer-hook '()
  "List of functions to execute after popping to a buffer.
Can be used to to open the whole buffer."
  :options '(delete-other-windows)
  :type 'hook
  :group 'mediawiki)

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

(defvar mediawiki-edittoken nil
  "The edit token for this page.")
(defvar mediawiki-starttimestamp nil
  "The starttimestamp for this page.")
(defvar mediawiki-basetimestamp nil
  "The base timestamp for this page.")

(defvar mediawiki-edit-form-vars nil)

(defun mediawiki-make-url (title &optional sitename)
  "Return a url when given a TITLE, ACTION and, optionally, SITENAME."
  (format (concat (mediawiki-site-url (or sitename mediawiki-site)) "%s")
          (mm-url-form-encode-xwfu
           (mediawiki-translate-pagename title))))

(defun mediawiki-open (name)
  "Open a wiki page specified by NAME from the mediawiki engine."
  (interactive
   (let* ((hist (cdr (assoc-string mediawiki-site mediawiki-page-history))))
     (list (read-string "Wiki Page: " nil 'hist))))
  (when (or (not (stringp name))
            (string-equal "" name))
    (error "Need to specify a name"))
  (mediawiki-edit mediawiki-site name))

(defun mediawiki-browse (&optional buffer)
  "Open the BUFFER in a browser.
If BUFFER is not given, the current buffer is used."
  (interactive)
  (if mediawiki-page-title
      (browse-url (mediawiki-make-url mediawiki-page-title))
    (with-current-buffer buffer
      (browse-url (mediawiki-make-url mediawiki-page-title)))))



;;;###autoload
(defun mediawiki-site (&optional site)
  "Set up mediawiki.el for a SITE.
Without an argument, use `mediawiki-site-default'.
Interactively, prompt for a SITE."
  (interactive)
  (when (not site)
    (setq site (mediawiki-prompt-for-site)))
  (when (or (eq nil mediawiki-site)
            (not (string-equal site mediawiki-site)))
    (setq mediawiki-site (mediawiki-do-login site)))
  (mediawiki-edit site (mediawiki-site-first-page site)))

;; (defvar mw-pagelist-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (suppress-keymap map)
;;     (define-key map [(return)] 'mw-pl-goto-page-at-point)
;;     (define-key map "n"        'mw-pl-page-down)
;;     (define-key map "C-v"      'mw-pl-page-down)
;;     (define-key map [(next)]  'mw-pl-page-down)
;;     (define-key map "p"        'mw-pl-page-up)
;;     (define-key map "M-v"      'mw-pl-page-up)
;;     (define-key map [(prior)]  'mw-pl-page-up)))

;; (define-derived-mode mw-pagelist-mode special-mode "MW-PageList")

(provide 'mediawiki)

;; Local Variables:
;; time-stamp-pattern: "20/^;; Last Modified: <%%>$"
;; End:

;;; mediawiki.el ends here
