;;; mediawiki-page.el --- Page operations for mediawiki.el -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2025 Mark A. Hershberger

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

;; This module provides page operations for mediawiki.el including
;; loading, editing, saving, and managing page content and metadata.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-api)
(require 'mediawiki-auth)
(require 'mediawiki-site)
(require 'mediawiki-utils)
(require 'mediawiki-mode)
(require 'ring)

;;; Page History Management
(defun mediawiki-add-page-history (sitename title)
  "Update SITENAME's page history with TITLE."
  (let ((hist (cdr (assoc-string sitename mediawiki-page-history))))
    (unless hist
      (add-to-list 'mediawiki-page-history (cons sitename "")))
    (setcdr (assoc-string sitename mediawiki-page-history) (append (list title) hist))))

(defvar mediawiki-page-title nil
  "The title of the page corresponding to the current buffer.")

;;; Page Loading Functions

(defun mediawiki-get (sitename title)
  "Query SITENAME for the content of TITLE."
  (let ((page (mediawiki-api-query-title sitename title)))
    (mediawiki-save-metadata sitename page)
    (mediawiki-page-get-revision page 0 'content)))

(defun mediawiki-edit (sitename title)
  "Edit wiki page on SITENAME named TITLE."
  (when (not (ring-p mediawiki-page-ring))
    (setq mediawiki-page-ring (make-ring 30)))

  (let ((pagetitle (mediawiki-translate-pagename title)))

    (mediawiki-add-page-history sitename title)
    (with-current-buffer (get-buffer-create
                           (concat sitename ": " pagetitle))
      (unless (mediawiki-logged-in-p sitename)
        (mediawiki-do-login sitename))
      (ring-insert mediawiki-page-ring (current-buffer))
      (delete-region (point-min) (point-max))
      (mediawiki-mode)
      (setq mediawiki-site sitename)
      (set-buffer-file-coding-system 'utf-8)
      (insert (or (mediawiki-get sitename pagetitle) ""))

      (set-buffer-modified-p nil)
      (setq buffer-undo-list t)
      (buffer-enable-undo)
      (mediawiki-pop-to-buffer (current-buffer))
      (setq mediawiki-page-title pagetitle)
      (goto-char (point-min))
      (current-buffer))))

;;;###autoload
(defun mediawiki-open (name)
  "Open a wiki page specified by NAME from the mediawiki engine."
  (interactive
    (let* ((hist (cdr (assoc-string mediawiki-site mediawiki-page-history)))
           (temp-history-symbol (make-symbol "mediawiki-temp-history")))
      (set temp-history-symbol hist)
      (list (read-string "Wiki Page: " nil temp-history-symbol))))
  (when (or (not (stringp name))
          (string-equal "" name))
    (error "Need to specify a name"))
  (mediawiki-edit mediawiki-site name))

;;; Page Metadata Handling

(defun mediawiki-save-metadata (sitename page)
  "Set per-buffer variables for all the SITENAME data for PAGE."
  (setq mediawiki-site sitename)
  (setq mediawiki-page-title
    (mediawiki-page-get-metadata page 'title))
  (setq mediawiki-edittoken
    (mediawiki-page-get-metadata page 'edittoken))
  (setq mediawiki-basetimestamp
    (mediawiki-page-get-revision page 0 'timestamp))
  (setq mediawiki-starttimestamp
    (mediawiki-page-get-metadata page 'starttimestamp)))

;;; Page Saving Functions

;;;###autoload
(defun mediawiki-save (&optional summary)
  "Save the current buffer as a page on the current site.
Prompt for a SUMMARY if one isn't given."
  (interactive "sSummary: ")
  (when (not (eq major-mode 'mediawiki-mode))
    (error "Not a mediawiki-mode buffer"))
  (if mediawiki-page-title
    (mediawiki-save-page
      mediawiki-site
      mediawiki-page-title
      summary
      (buffer-substring-no-properties (point-min) (point-max)))
    (error "Error: %s is not a mediawiki document" (buffer-name))))

;;;###autoload
(defun mediawiki-save-on (&optional sitename name summary)
  "On SITENAME, save a page with NAME and SUMMARY."
  (interactive)
  (when (not sitename)
    (setq sitename (mediawiki-prompt-for-site)))
  (when (not name)
    (setq name (mediawiki-translate-pagename (mediawiki-prompt-for-page))))
  (when (not summary)
    (setq summary (mediawiki-prompt-for-summary)))

  (setq mediawiki-site (mediawiki-do-login sitename))
  (mediawiki-get mediawiki-site name)
  (mediawiki-save-as name summary))

;;;###autoload
(defun mediawiki-save-as (&optional name summary)
  "Save a page on the current site wite NAME and SUMMARY."
  (interactive "sSave As: \nsSummary: ")
  (if name
    (mediawiki-save-page
      mediawiki-site
      name
      summary
      (buffer-substring-no-properties (point-min) (point-max)))
    (error "Error: %s is not a mediawiki document" (buffer-name))))

;;;###autoload
(defun mediawiki-save-and-bury (&optional summary)
  "Prompt for a SUMMARY, save the page, then bury the buffer."
  (interactive "sSummary: ")
  (mediawiki-save summary)
  (bury-buffer))

(defun mediawiki-save-page (sitename title summary content &optional trynum)
  "On SITENAME, save the current page using TITLE, SUMMARY, and CONTENT."
  ;; FIXME error checking, conflicts!
  (when (and trynum (< trynum 0))
    (error "Too many tries."))
  (let ((trynum (or trynum 3))
         (token (mediawiki-site-get-token sitename "csrf")))
    (condition-case err
      (progn
        (mediawiki-api-call sitename "edit"
          (list (cons "title"
                  (mediawiki-translate-pagename title))
            (cons "text" content)
            (cons "summary" summary)
            (cons "token" token)
            (cons "basetimestamp"
              (or mediawiki-basetimestamp "now"))
            (cons "starttimestamp"
              (or mediawiki-starttimestamp "now"))))
        (message "Saved %s to %s" title sitename))
      (error (progn (message "try #%d: %s " trynum
                      (concat "Retry because of error: " (cadr err)))
               (mediawiki-retry-save-page
                 sitename title summary content trynum)))))
  (set-buffer-modified-p nil))

(defun mediawiki-retry-save-page (sitename title summary content trynum)
  "Refresh the edit token and then try to save the current page using TITLE,
SUMMARY, and CONTENT on SITENAME."
  (let ((try (if trynum
               (- trynum 1)
               3)))
    (mediawiki-do-login sitename)
    (setq mediawiki-edittoken (mediawiki-site-get-token sitename "csrf"))
    (mediawiki-save-page sitename title summary content try)))

;;; Page Navigation and Utility Functions

(defun mediawiki-prompt-for-page ()
  "Prompt for a page name and return the answer."
  (let* ((prompt (concat "Page"
                   (when mediawiki-page-title
                     (format " (default %s)" mediawiki-page-title))
                   ": "))
          (answer (completing-read prompt '())))
    (if (string= "" answer)
      mediawiki-page-title
      answer)))

(defun mediawiki-prompt-for-summary ()
  "Prompt for a summary and return the answer."
  (completing-read  "Summary: " '()))

;;;###autoload
(defun mediawiki-open-page-at-point ()
  "Open a new buffer with the page at point."
  (interactive)
  (mediawiki-open (mediawiki-page-at-point)))

(defun mediawiki-page-at-point ()
  "Return the page name under point.
Typically, this means anything enclosed in [[PAGE]]."
  (let ((pos (point))
         (eol (pos-eol))
         (bol (pos-bol)))
    (save-excursion
      (let* ((start  (when (search-backward "[[" bol t)
                       (+ (point) 2)))
              (end    (when (search-forward "]]" eol t)
                        (- (point) 2)))
              (middle (progn
                        (goto-char start)
                        (when (search-forward  "|" end t)
                          (1- (point)))))
              (pagename (when (and
                                (not (eq nil start))
                                (not (eq nil end))
                                (<= pos end)
                                (>= pos start))
                          (buffer-substring-no-properties
                            start (or middle end)))))
        (if (string= "/"
              (substring pagename 0 1))
          (concat mediawiki-page-title pagename)
          pagename)))))

;;; Buffer Management

(defun mediawiki-pop-to-buffer (buffer)
  "Pop to BUFFER and then execute a hook."
  (pop-to-buffer buffer)
  (run-hooks 'mediawiki-pop-buffer-hook))

(provide 'mediawiki-page)

;;; mediawiki-page.el ends here
