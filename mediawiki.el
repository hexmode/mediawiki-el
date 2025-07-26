;;; mediawiki.el --- mediawiki frontend  -*- lexical-binding: t; -*-

;; Copyright (C) 2008, 2009, 2010, 2011, 2015, 2025 Mark A. Hershberger

;; Original Authors: Jerry <unidevel@yahoo.com.cn>,
;;      Chong Yidong <cyd at stupidchicken com> for wikipedia.el,
;;      Uwe Brauer <oub at mat.ucm.es> for wikimedia.el
;; Author: Mark A. Hershberger <mah@everybody.org>
;; Created: Sep 17 2004
;; Package-Requires: ((emacs "28.1"))
;; Keywords: mediawiki wikipedia network wiki
;; URL: https://github.com/hexmode/mediawiki-el
;; Version: 3.0.0
;; Package-Type: multi
;; Last Modified: <2025-07-26 18:25:29 mah>

(defconst mediawiki-version "3.0.0"
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

;; Load the new modular components
(require 'mediawiki-core)
(require 'mediawiki-http)
(require 'mediawiki-api)
(require 'mediawiki-auth)
(require 'mediawiki-session)
(require 'mediawiki-page)
(require 'mediawiki-ui)
(require 'mediawiki-compat)
(require 'mediawiki-legacy)

(eval-when-compile
  (require 'cl))

;; Local Variables:
;; time-stamp-pattern: "20/^;; Last Modified: <%%>$"
;; End:

;;; mediawiki.el ends here
