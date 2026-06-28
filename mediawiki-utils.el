;;; mediawiki-utils.el --- General utility functions for mediawiki.el  -*- lexical-binding: t; -*-

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

;; This file contains general utility functions for mediawiki.el,
;; including debug functions and helper functions for data structure
;; manipulation.

;;; Code:

(require 'mediawiki-core)
(require 'iso8601)

(declare-function mediawiki-site-url "mediawiki-site")
(declare-function url-filename "url-parse")

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
  "Using PAGE alist, extract ITEM."
  (alist-get item page))

(defun mediawiki-format-timestamp (timestamp)
  "Format TIMESTAMP string (ISO 8601) for display as relative time.
Returns \"just now\", \"5 minutes ago\", \"2 hours ago\", \"3 days ago\", etc."
  (let* ((time (encode-time (iso8601-parse timestamp)))
         (seconds (float-time (time-subtract (current-time) time))))
    (cond
     ((< seconds 60)
      "just now")
     ((< seconds 3600)
      (let ((minutes (floor seconds 60)))
        (format "%s minute%s ago" minutes (if (= minutes 1) "" "s"))))
     ((< seconds 86400)
      (let ((hours (floor seconds 3600)))
        (format "%s hour%s ago" hours (if (= hours 1) "" "s"))))
     ((< seconds 604800)
      (let ((days (floor seconds 86400)))
        (format "%s day%s ago" days (if (= days 1) "" "s"))))
     ((< seconds 2419200)
      (let ((weeks (floor seconds 604800)))
        (format "%s week%s ago" weeks (if (= weeks 1) "" "s"))))
     (t
      (let ((months (floor seconds 2419200)))
        (format "%s month%s ago" months (if (= months 1) "" "s")))))))

(defun mediawiki-format-size-change (sizediff)
  "Format a byte size change for display.
Returns \"+123\", \"-45\", or \"0\" for nil/zero.  Adds propertize for colors:
green for positive, red for negative."
  (cond
   ((or (null sizediff) (= sizediff 0))
    " 0")
   ((> sizediff 0)
    (propertize (format "+%d" sizediff) 'face 'font-lock-type-face))
   (t
    (propertize (format "%d" sizediff) 'face 'font-lock-warning-face))))

(defun mediawiki-make-page-url (sitename title)
  "Return the full URL to view TITLE on SITENAME."
  (concat (mediawiki-site-url sitename)
          "index.php?title="
          (mm-url-form-encode-xwfu title)))

(defun mediawiki-make-revision-url (sitename title revid)
  "Return the full URL to view a specific revision of TITLE on SITENAME."
  (format "%s&oldid=%d" (mediawiki-make-page-url sitename title) revid))

(defun mediawiki-make-user-page-url (sitename username)
  "Return the full URL to view USERNAME's user page on SITENAME."
  (mediawiki-make-page-url sitename (concat "User:" username)))

(defun mediawiki-extract-title-from-url (url)
  "Extract the page title from a MediaWiki URL.
Returns the title string, or nil if not found."
  (let* ((parsed (url-generic-parse-url url))
         (filename (url-filename parsed))
         (start (and filename (string-match "[?&]title=" filename))))
    (when start
      (let* ((val-start (+ start 7))  ; skip "?title=" or "&title="
             (val-end (or (string-match "&" filename val-start)
                          (length filename)))
             (encoded (substring filename val-start val-end)))
        (url-unhex-string (replace-regexp-in-string "\\+" " " encoded) t)))))

(defun mediawiki-revid-at-point ()
  "Return the revision ID at point, or nil.
Works in `tabulated-list-mode' buffers by calling `tabulated-list-get-id'.
Returns the numeric revision ID."
  (when (derived-mode-p 'tabulated-list-mode)
    (tabulated-list-get-id)))

(defun mediawiki-user-at-point ()
  "Return the username at point, or nil.
Works in history and contributions buffers by looking up the entry at point."
  nil)

(provide 'mediawiki-utils)

;;; mediawiki-utils.el ends here
