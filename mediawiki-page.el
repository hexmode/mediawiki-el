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
(require 'mediawiki-recent)
(require 'ring)

;;; Page History Management
(defun mediawiki-add-page-history (sitename title)
  "Update SITENAME's page history with TITLE."
  (let ((hist (cdr (assoc-string sitename mediawiki-page-history))))
    (unless (equal title (car hist))
      (unless hist
        (add-to-list 'mediawiki-page-history (cons sitename "")))
      (setcdr (assoc-string sitename mediawiki-page-history) (append (list title) hist)))))

(defvar-local mediawiki-page-title nil
  "The title of the page corresponding to the current buffer.")

;;; Page Loading Functions

(defun mediawiki-get (sitename title)
  "Query SITENAME for the content of TITLE.
Returns a cons cell (PAGE . CONTENT) where PAGE is the page metadata
and CONTENT is the page content string."
  (let ((page (mediawiki-api-query-title sitename title)))
    (cons page (mediawiki-page-get-revision page 0 'content))))

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

      ;; Get page content and metadata, ensuring metadata is saved in current buffer
      (let* ((page-data (mediawiki-get sitename pagetitle))
             (page (car page-data))
             (content (cdr page-data)))
        (mediawiki-save-metadata sitename page)
        (insert (or content "")))

      (set-buffer-modified-p nil)
      (setq buffer-undo-list t)
      (buffer-enable-undo)
      (mediawiki-pop-to-buffer (current-buffer))
      (setq mediawiki-page-title pagetitle)
      (goto-char (point-min))
      (current-buffer))))

;;;###autoload
(defun mediawiki-open (name &optional sitename)
  "Open a wiki page specified by NAME from the mediawiki engine.
SITENAME defaults to `mediawiki-site'.  Interactively, a remembered page
brings its own site along, so the page opens on the wiki it came from."
  (interactive
   (let* ((in-wiki (and mediawiki-site (derived-mode-p 'mediawiki-mode)))
          (pick (mediawiki-recent-read
                 'pages
                 (and in-wiki mediawiki-site)
                 (mediawiki-page-at-point)
                 (and in-wiki (cdr (assoc-string mediawiki-site
                                                 mediawiki-page-history))))))
     (list (cdr pick)
           (or (car pick)
               (and in-wiki mediawiki-site)
               (mediawiki-prompt-for-site)))))
  (when (or (not (stringp name))
          (string-equal "" name))
    (error "Need to specify a name"))
  (let ((site (or sitename mediawiki-site)))
    (unless site
      (setq site (mediawiki-prompt-for-site)))
    (setq mediawiki-site site)
    (mediawiki-edit site name)))

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
  (if (null mediawiki-site)
    (mediawiki-site))
  (if mediawiki-page-title
    (mediawiki-save-page
      mediawiki-site
      mediawiki-page-title
      summary
      (without-restriction
        (buffer-substring-no-properties (point-min) (point-max))))
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
        (message "Saved %s to %s" title sitename)
        (mediawiki-recent-record 'pages sitename
                                 (mediawiki-translate-pagename title)))
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
  "Open a new buffer with the page linked at point."
  (interactive)
  (let ((page (mediawiki-page-at-point)))
    (if page
        (mediawiki-open page)
      (call-interactively #'mediawiki-open))))

;;;###autoload
(defun mediawiki-open-at-point ()
  "Open the page linked at point, or prompt for a page.
On an internal link such as [[Page|text]] this opens Page, whether
point is on the target or on the text."
  (interactive)
  (mediawiki-open-page-at-point))

(defun mediawiki-page-at-point ()
  "Return the page name of the wiki link at point, or nil.
For [[Page]] and [[Page|text]] this is Page, whether point is on the
target or on the text.  A subpage link (/Archive) is returned relative
to the current page.  Returns nil when point is not on a link."
  (let ((here (point))
        (bol (line-beginning-position))
        (eol (line-end-position)))
    (save-excursion
      (let ((open (progn (goto-char here)
                         (when (search-backward "[[" bol t) (point)))))
        (when (and open
                   (< open here)
                   (save-excursion (goto-char here)
                                   (search-forward "]]" eol t))
                   (not (save-excursion (goto-char (+ open 2))
                                        (search-forward "]]" here t))))
          (let* ((close (save-excursion (goto-char here)
                                        (search-forward "]]" eol t)
                                        (match-beginning 0)))
                 (body (buffer-substring-no-properties (+ open 2) close))
                 (bar (string-search "|" body))
                 (page (if bar
                           (substring-no-properties body 0 bar)
                         body)))
            (when (and page (>= here open) (<= here close))
              (if (and (> (length page) 0)
                       (string= "/" (substring page 0 1)))
                  (concat mediawiki-page-title page)
                page))))))))

;;; Buffer Management

(defun mediawiki-pop-to-buffer (buffer)
  "Pop to BUFFER and then execute a hook."
  (pop-to-buffer buffer)
  (run-hooks 'mediawiki-pop-buffer-hook))

(provide 'mediawiki-page)

;;; mediawiki-page.el ends here
