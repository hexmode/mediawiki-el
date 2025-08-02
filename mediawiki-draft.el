;;; mediawiki-draft.el --- Draft functionality for MediaWiki mode -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2025 MediaWiki.el contributors

;; This file is part of mediawiki.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides draft functionality for MediaWiki mode, including
;; draft buffer management, draft saving and loading, and reply functionality.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-utils)

;;; Forward Declarations

(declare-function mediawiki-mode "mediawiki")
(declare-function mediawiki-reply-at-point-simple "mediawiki")
(declare-function mediawiki-terminate-paragraph-and-indent "mediawiki")

(defvar mediawiki-reply-with-hline)
(defvar mediawiki-reply-with-quote)
(defvar mediawiki-english-or-german)
(defvar mediawiki-user-simplify-signature)

;;; Customization Groups

(defgroup mediawiki-draft nil
  "A mode to mediawiki-draft information."
  :group 'mediawiki)

;;; User Variables

(defcustom mediawiki-draft-mode-hook nil
  "*Functions run upon entering mediawiki-draft-mode."
  :type 'hook
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-filter-functions nil
  "*Functions run to filter mediawiki-draft data.
All functions are run in the mediawiki-draft buffer."
  :type 'hook
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-handler-functions '(mediawiki-draft-append-to-file)
  "*Functions run to process mediawiki-draft data.
Each function is called with the current buffer narrowed to what the
user wants mediawiki-drafted.
If any function returns non-nil, the data is assumed to have been
recorded somewhere by that function."
  :type 'hook
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-data-file "~/Wiki/discussions/draft.wiki"
  "*The file in which to store the wikipedia drafts."
  :type 'file
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-reply-register ?M
  "The register in which the window configuration is stored."
  :type 'character
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-page ?S		;Version:1.37
  "The register in which the a page of the wiki draft file is stored."
  :type 'character
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-leader-text "== "
  "*The text used to begin each mediawiki-draft item."
  :type 'string
  :group 'mediawiki-draft)

;;; Variables

(defvar mediawiki-draft-buffer "*MW-Draft*"
  "The name of the wikipedia-draft (temporary) data entry buffer.")

(defvar mediawiki-draft-send-archive t
  "*Archive the reply.")

(defvar mediawiki-draft-mode-map ())

;;; Utility Functions

(defsubst mediawiki-draft-time-to-seconds (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
    (cadr time)
    (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defsubst mediawiki-draft-mail-date (&optional rfc822-p)
  "Return a simple date.
If RFC822-P is passed, use RFC822 format."
  (if rfc822-p
    (format-time-string "%a, %e %b %Y %T %z" (current-time))
    (format-time-string "%c" (current-time))))

(defun mediawiki-draft-buffer-desc ()
  "Using the first line of the current buffer, create a short description."
  (buffer-substring (point-min)
    (save-excursion
      (goto-char (point-min))
      (end-of-line)
      (if (> (- (point) (point-min)) 60)
	(goto-char (+ (point-min) 60)))
      (point))))

;;; Draft File Management

(defun mediawiki-draft-append-to-file ()
  "Append a draft to the drafts file."
  (let ((text (buffer-string)))
    (with-temp-buffer
      (insert (concat "\n\n"  mediawiki-draft-leader-text "Draft: "
                (read-string "Enter Subject: ") " "
                (current-time-string) " "
                mediawiki-draft-leader-text
                "\n\n\f\n\n" text "\n\f\n"))
      (if (not (bolp))
        (insert "\n\n"))
      (if (find-buffer-visiting mediawiki-draft-data-file)
        (let ((mediawiki-draft-text (buffer-string)))
          (set-buffer (get-file-buffer mediawiki-draft-data-file))
          (save-excursion
            (goto-char (point-max))
            (insert (concat "\n" mediawiki-draft-text "\n"))
            (save-buffer)))
        (append-to-file (point-min) (point-max) mediawiki-draft-data-file)))))

;;;###autoload
(defun mediawiki-draft-view-draft ()
  "Simple shortcut to visit the drafts file."
  (interactive)
  (find-file mediawiki-draft-data-file))

;;; Draft Buffer Management

;;;###autoload
(defun mediawiki-draft ()
  "Open a temporary buffer in mediawiki-mode.
This is for editing a draft.  After finishing the editing either
use \\[mediawiki-draft-buffer] to send the data into the
mediawiki-draft-data-file, or send the buffer using
\\[mediawiki-save] and insert it later into a mediawiki article."
  (interactive)
  (window-configuration-to-register mediawiki-draft-register)
  (let ((buf (get-buffer-create mediawiki-draft-buffer)))
    (switch-to-buffer-other-window buf)
    (mediawiki-mode)
    (message " C-c C-k sends to draft file, C-c C-c sends to org buffer.")))

;;;###autoload
(defun mediawiki-draft-page ()
  "Set the current buffer as a draft buffer."
  (interactive)
  (mark-page)
  (copy-region-as-kill (region-beginning) (region-end))
  (mediawiki-draft)
  (yank nil))

;;;###autoload
(defun mediawiki-draft-region (&optional begin end)
  "Mediawiki-draft the data from BEGIN to END.
If called from within the mediawiki-draft buffer, BEGIN and END are ignored,
and the entire buffer will be mediawiki-drafted.  If called from any other
buffer, that region, plus any context information specific to that
region, will be mediawiki-drafted."
  (interactive)
  (let ((b (or begin (min (point) (or (mark) (point-min)))))
	 (e (or end (max (point) (or (mark) (point-max))))))
    (save-restriction
      (narrow-to-region b e)
      (run-hook-with-args-until-success 'mediawiki-draft-handler-functions)
      (when (equal mediawiki-draft-buffer (buffer-name))
        (mediawiki-debug (current-buffer) "mediawiki-draft-region")
        (jump-to-register mediawiki-draft-register)))))

;;;###autoload
(defun mediawiki-draft-buffer ()
  "Mediawiki-draft-buffer sends the contents of the current (temporary)
buffer to the mediawiki-draft-buffer, see the variable
mediawiki-draft-data-file."
  (interactive)
  (mediawiki-draft-region  (point-min) (point-max)))

;;;###autoload
(defun mediawiki-draft-clipboard ()
  "Mediawiki-Draft the contents of the current clipboard.
Most useful for mediawiki-drafting things from Netscape or other X Windows
application."
  (interactive)
  (with-temp-buffer
    (insert (gui-get-selection))
    (run-hook-with-args-until-success 'mediawiki-draft-handler-functions)))

;;; Register Operations

;;;###autoload
(defun mediawiki-draft-copy-page-to-register ()
  "Copy a page via the mediawiki-draft-register."
  (interactive)
  (save-excursion
    (narrow-to-page nil)
    (copy-to-register mediawiki-draft-page (point-min) (point-max) nil)
    (message "draft page copied to wikipedia register mediawiki-draft-page.")
    (widen)))

;;;###autoload
(defun mediawiki-draft-yank-page-to-register ()
  "Insert a page via the mediawiki-draft-register."
  (interactive)
  (insert-register mediawiki-draft-page nil))

;;;###autoload
(defun mediawiki-draft-send (target-buffer)
  "Copy the current page in the drafts file to TARGET-BUFFER.
If `mediawiki-draft-send-archive' is t, then additionally the
text will be archived in the draft.wiki file."
  (interactive "bTarget buffer: ")
  (mediawiki-draft-copy-page-to-register)
  (switch-to-buffer target-buffer)
  (end-of-line 1)
  (newline 1)
  (mediawiki-draft-yank-page-to-register)
  (message "The page has been sent (copied) to the mozex file!")
  (switch-to-buffer "*MW-Draft*")
  (when mediawiki-draft-send-archive
    (let ((text (buffer-string)))
      (with-temp-buffer
	(insert (concat "\n\n" mediawiki-draft-leader-text)
	  (insert-register mediawiki-draft-reply-register 1)
	  (insert (concat " " (current-time-string) " "
		    mediawiki-draft-leader-text  "\n\n\f\n\n"
		    text "\n\f\n"))
	  (if (not (bolp))
	    (insert "\n\n"))
	  (if (find-buffer-visiting mediawiki-draft-data-file)
	    (let ((mediawiki-draft-text (buffer-string)))
	      (set-buffer (get-file-buffer mediawiki-draft-data-file))
	      (save-excursion
		(goto-char (point-max))
		(insert (concat "\n" mediawiki-draft-text "\n"))
		(save-buffer)))
	    (append-to-file (point-min) (point-max)
	      mediawiki-draft-data-file)))))
    (when (equal mediawiki-draft-buffer (buffer-name))
      (mediawiki-debug (current-buffer) "mediawiki-draft-send"))
    (switch-to-buffer target-buffer)))

;;; Reply Functionality

;;;###autoload
(defun mediawiki-draft-reply ()
  "Open a temporary buffer to edit a draft.
After finishing the editing: either use `mediawiki-draft-buffer'
to send the data into the `mediawiki-draft-data-file'.  Check the
variable `mediawiki-draft-send-archive'."
  (interactive)
  (mediawiki-reply-at-point-simple)
  (beginning-of-line 1)
  (kill-line nil)
  (save-excursion
    (window-configuration-to-register mediawiki-draft-register)
    (let ((buf (get-buffer-create mediawiki-draft-buffer)))
      (switch-to-buffer-other-window buf)
      (mediawiki-mode)
      (if mediawiki-reply-with-quote
        (progn
	  (insert "{{Quotation|")
	  (yank)
	  (insert "'''Re: ")
	  (insert-register mediawiki-draft-reply-register 1)
	  (insert "''' |~~~~}}")
	  (backward-char 7))
        (when mediawiki-reply-with-hline
          (insert "----")
          (newline 1))
        (yank)
        (end-of-line 1))
      (message " C-c C-k sends to draft, C-c C-c sends to org buffer."))))

;;; Draft Mode Definition

(define-derived-mode mediawiki-draft-mode text-mode "MW-Draft"
  "Major mode for output from \\[mediawiki-draft].
\\<mediawiki-draft-mode-map> This buffer is used to collect data that
you want mediawiki-draft.  Just hit \\[mediawiki-draft-region] when
you're done entering, and it will go ahead and file the data for
latter retrieval, and possible indexing.
\\{mediawiki-draft-mode-map}"
  (kill-all-local-variables)
  (text-mode)
  (define-key mediawiki-draft-mode-map "\C-c\C-k" 'mediawiki-draft-buffer)
  (define-key mediawiki-draft-mode-map "\C-c\C-d" 'mediawiki-draft-buffer))

(provide 'mediawiki-draft)

;;; mediawiki-draft.el ends here
