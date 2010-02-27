;;; mediawiki.el --- mediawiki frontend

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Jerry <unidevel@yahoo.com.cn>
;; Keywords: extensions, convenience, lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Howto: 
;;  (add-to-list 'load-path "path to mediawiki.el")
;;  (setq mediawiki-url "http://localhost/wiki/index.php")
;;  (require 'mediawiki)
;; Open a wiki server:  C-c C-u
;; Open a wiki file:    C-c C-o
;; Open a wiki file:    M-x mediawiki-open 
;; Save a wiki buffer:  C-x C-s 
;; Save a wiki buffer with a different name:  C-x C-w

;; Todo:
;;   add username and password support

;; Note:
;;   part of this code port from pmwiki.el

;; Bug report:
;;   unidevel@yahoo.com.cn
;;   http://zhdotemacs.sourceforge.net

;; Latest version can be found at
;;     http://sourceforge.net/projects/zhdotemacs
;; or  http://meta.wikimedia.org/wiki/mediawiki.el
;; ^_^

;;; Code:

(require 'http-get)
(require 'http-post)

(defcustom mediawiki-url nil
  "Supply mediawiki-url for example:
http://mediawiki.sf.net/index.php
")

(defcustom mediawiki-argument-pattern "?title=%s&action=%s"
  "Supply mediawiki-url for example:
?title=%s&action=%s
")

(defvar mediawiki-URI-pattern
  "http://\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?/"
  "Pattern matching a URI like this:
	http://mediawiki.sf.net/index.php
Password not support yet")

(defvar mediawiki-password-URI-pattern
  "http://\\(.*?:.*?\\)@\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?/\\(.*/\\)?\\([^:]*\\)"
  "Pattern matching a URI like this:
	http://mediawiki.sf.net/index.php/WikiSandbox
Password not support yet")

(defvar mediawiki-page-uri nil
  "The URI of the page corresponding to the current buffer, thus defining
the base URI of the wiki engine as well as group and page name.")

(defvar mediawiki-encoded-password nil
  "The password when authorization is required")

(defvar mediawiki-page-title nil
  "The title of the page corresponding to the current buffer")

(defvar mediawiki-page-timestamp nil
  "The timestamp of the page corresponding to the current buffer")

(defun mediawiki-set-url (&optional url)
  (interactive "sURL: ")
  (if (string-match mediawiki-URI-pattern url) 
      (setq mediawiki-url url)
    (message (concat "Error: [" url "] is not a valid mediawiki url.")))
  )

(defun mediawiki-make-url (title action)
  (format (concat mediawiki-url mediawiki-argument-pattern)
	  (http-url-encode title 'utf-8)
	  action)
  )


(defun mediawiki-open (name)
  "Open a wiki page specified by NAME from the mediawiki engine"
  (interactive "sWikiName: ")
  (mediawiki-edit name "raw")
  (mediawiki-mode t)
  ;;(use-local-map mediawiki-mode-map)
  )

(defun mediawiki-reload ()
  (interactive)
  (let ((title mediawiki-page-title))
    (if title
	(mediawiki-open title)
      (minibuffer-message (concat "Error: " (buffer-name) " is not a mediawiki document."))
      )
    ))


(defun mediawiki-edit (title action &optional goto-end http-ver content-type)
  "Edit wiki file with the name of title"
  ;;(unless content-type (setq content-type 'iso-8859-1))
  (mediawiki-get (mediawiki-make-url title action) (concat "MediaWiki: " title)
		 goto-end http-ver content-type)
  (mediawiki-mode t)
  ;;(use-local-map mediawiki-mode-map)
  )

(defun mediawiki-get-internal (title bufname)
  (mediawiki-get (mediawiki-make-url title "raw") bufname)
  )

(defun mediawiki-get (url bufname &optional goto-end http-ver content-type)
  ;;(unless content-type (setq content-type 'iso-8859-1))
  (let ((page-uri url)
	(page-timestamp nil)
	(doc-timestamp nil)
	(proc nil)
	(headers '(("Connection" "close")))
	(userAndPassword nil))
	(when (string-match mediawiki-password-URI-pattern page-uri)
	  (setq userAndPassword (base64-encode-string
				 (match-string 1 page-uri)))
	  (setq page-uri (replace-regexp-in-string
			  "http://.*?:.*?@" "http://" page-uri))
	  (add-to-list 'headers (list "Authorization"
				      (concat "Basic " userAndPassword))))
	(set 'proc
	     (http-get page-uri         ; Source URI
		       headers		; headers
		       nil		; ?
		       http-ver		; nil -> 1.0
		       bufname          ; output buffer
		       content-type)	) ; typically nil
	(emacs-wiki-mode)
	(set-buffer-file-coding-system 'utf-8)
	;;(set-fill-column pmwiki-fill-column)
	(make-local-variable 'mediawiki-page-timestamp)
	(setq mediawiki-page-timestamp nil)
	(set (make-local-variable 'mediawiki-page-title) title)
	(set (make-local-variable 'mediawiki-page-uri) page-uri)
	(set (make-local-variable 'mediawiki-encoded-password) userAndPassword)
	(unless goto-end
	  (while (string-equal (process-status proc) "open") (sleep-for 0.050))
	  (goto-char 1))
	(not-modified)
	proc))

(defun mediawiki-save (&optional summary)
  (interactive "sSummary: ")
  (let ((title mediawiki-page-title)
	(content (buffer-string)))
    (if title
	(mediawiki-save-page title content)
      (minibuffer-message (concat "Error: " (buffer-name) " is not a mediawiki document."))
      )
    ))

(defun mediawiki-save-as (&optional title summary)
  (interactive "sTitle: \nsSummay: ")
  (if (and title (not (string= title "")))
      (progn
	(mediawiki-save-page title  (buffer-string) summary)
	(mediawiki-open title)
	)
    ))

(defun mediawiki-save-page (title content &optional summary)
  "Save the current page to a PmWiki wiki."
  (let ((cur-buffer (buffer-name))
	(temp-buffer (concat "* " (buffer-name) " *"))
	(page-uri (mediawiki-make-url title "submit"))
	(page-timestamp mediawiki-page-timestamp)
	(doc-timestamp mediawiki-page-timestamp)
	(password mediawiki-encoded-password)
	(headers nil))
    (mediawiki-get-internal title temp-buffer)
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (insert-string content)
    (if password
	(setq headers
	      (list (list "Authorization" (concat "Basic " password)))))
    (if (not page-timestamp)
	(setq page-timestamp
	      (cdr (assoc "last-modified" http-headers))))
    (if (not page-timestamp)
	(setq page-timestamp
	      (cdr (assoc "date" http-headers))))
    (if (not page-timestamp)
	(setq page-timestamp
	      (current-time-string)))
    (setq doc-timestamp (format-time-string "%Y%m%d%H%M%S" 
					    (date-to-time page-timestamp) t))
    (minibuffer-message (concat "Any output is in " 
				(buffer-name 
				 (process-buffer
				  (http-post
				   page-uri  ;; Get destination
				   (list (cons "wpSection" "")
					 (cons "wpEdittime" doc-timestamp)
					 (cons "wpSave" "Save")
					 (cons "wpSummary" pmwiki-author)
					 (cons "wpTextbox1" (buffer-string)))
				   buffer-file-coding-system
				   headers)))))
    (setq page-timestamp 
	  (format "%s" (cdr (assoc "date" http-headers))))
    (kill-buffer temp-buffer)
    (switch-to-buffer cur-buffer)
    (make-local-variable 'mediawiki-page-timestamp)
    (setq mediawiki-page-timestamp nil)
    (not-modified)
    ))

(defun mediawiki-browse (&optional buf)
  "Open the buffer BUF in a browser. If BUF is not given,
the current buffer is used."
  (interactive)
  (if mediawiki-page-title 
      (browse-url (mediawiki-make-url mediawiki-page-title "view"))
    )
  )


(defvar mediawiki-mode-map nil
  "Keymap for mediawiki mode.")

(defvar mediawiki-mode nil
  "nil disables mediawiki, non-nil enables.")

(make-variable-buffer-local 'mediawiki-mode)

(if mediawiki-mode-map
    nil
  (setq mediawiki-mode-map (make-keymap))
  ;;(suppress-keymap mediawiki-mode-map)
  (define-key mediawiki-mode-map "\C-x\C-s"   'mediawiki-save)
  ;;(define-key mediawiki-mode-map "\C-x\C-w"   'mediawiki-save-as)
  )
(global-set-key "\C-c\C-w"   'mediawiki-save-as)
(global-set-key "\C-c\C-o"   'mediawiki-open)
(global-set-key "\C-c\C-u"   'mediawiki-set-url)

(define-minor-mode mediawiki-mode 
  "Minor mode for using mediawiki"
  t
  " mediawiki"
  mediawiki-mode-map
  )

(provide 'mediawiki)

;;; mediawiki.el ends here