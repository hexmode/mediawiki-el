;;; mediawiki-utils.el --- General utility functions for mediawiki.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2008, 2009, 2010, 2011, 2015 Mark A. Hershberger

;; Author: Mark A. Hershberger <mah@everybody.org>
;; Package-Requires: ((emacs "28.1"))
;; Keywords: mediawiki wikipedia network wiki
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

;; This file contains general utility functions for mediawiki.el,
;; including debug functions and helper functions for data structure
;; manipulation.

;;; Code:

(require 'mediawiki-core)

;;; Debug Variables and Functions

(defcustom mediawiki-debug nil
  "Turn on debugging (non-nil)."
  :type 'boolean
  :tag "MediaWiki Debugging"
  :group 'mediawiki)

(defvar mediawiki-debug-buffer " *MediaWiki Debug*")

(defun mediawiki-debug-line (line)
  "Log a LINE to BUFFER."
  (when mediawiki-debug
    (with-current-buffer (get-buffer-create mediawiki-debug-buffer)
      (goto-char (point-max))
      (insert "\n")
      (insert line))))

(defun mediawiki-debug (buffer function)
  "The debug handler.
When debugging is turned on, log the name of the BUFFER with the
FUNCTION that called the debugging function, so it can be
examined.  If debugging is off, just kill the buffer.  This
allows you to see what is being sent to and from the server."
  (when mediawiki-debug
    (mediawiki-debug-line
     (concat
      "\n\n=-=-=-=-=-=-=-=\n"
      function "\n\n"
      (with-current-buffer buffer
        (buffer-string)))))
  (kill-buffer buffer))

;;; General Utility Functions

(defun mediawiki-page-get-metadata (page item)
  "Using PAGE, extract ITEM."
  (cdr (assoc item (cadr page))))

(provide 'mediawiki-utils)

;;; mediawiki-utils.el ends here
