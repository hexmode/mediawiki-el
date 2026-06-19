;;; mediawiki-history.el --- Revision history browser for mediawiki.el  -*- lexical-binding: t; -*-

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

;; This module provides a revision history browser for MediaWiki pages,
;; displayed as a sortable `tabulated-list-mode' table with operations
;; for viewing, diffing, reverting, and browsing individual revisions.
;;
;; Key functions:
;;
;;   `mediawiki-history'                - Display revision history for a page
;;   `mediawiki-history-view-revision'  - View wikitext of revision at point
;;   `mediawiki-history-diff-to-previous'   - Diff revision against its parent
;;   `mediawiki-history-diff-to-current'    - Diff revision against latest
;;   `mediawiki-history-diff-revisions'     - Diff two arbitrary revisions
;;   `mediawiki-history-restore-revision'   - Restore page to selected revision
;;   `mediawiki-history-browse-revision'    - Open revision URL in browser
;;   `mediawiki-history-refresh'            - Re-fetch and redisplay history
;;   `mediawiki-history-open-page'          - Open the page for editing
;;
;; User option:
;;   `mediawiki-history-limit'          - Max revisions to fetch (default 50)

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-api)
(require 'mediawiki-utils)
(require 'mediawiki-diff)
(require 'tabulated-list)
(require 'cl-lib)
(require 'diff-mode)

(declare-function mediawiki-diff-show-diff "mediawiki-diff")
(declare-function mediawiki-diff-show-revision "mediawiki-diff")
(declare-function mediawiki-format-timestamp "mediawiki-utils")
(declare-function mediawiki-format-size-change "mediawiki-utils")
(declare-function mediawiki-make-revision-url "mediawiki-utils")
(declare-function mediawiki-edit "mediawiki-page")

;; Declare variables from other mediawiki modules
(defvar mediawiki-page-title)
(defvar mediawiki-site)

(defgroup mediawiki-history nil
  "Revision history browser for mediawiki.el."
  :tag "MediaWiki History"
  :group 'mediawiki)

(defcustom mediawiki-history-limit 50
  "Maximum number of revisions to fetch when displaying history."
  :type 'integer
  :group 'mediawiki-history)

(defvar mediawiki-history-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'mediawiki-history-view-revision)
    (define-key map (kbd "v")   #'mediawiki-history-view-revision)
    (define-key map (kbd "d")   #'mediawiki-history-diff-to-previous)
    (define-key map (kbd "D")   #'mediawiki-history-diff-to-current)
    (define-key map (kbd "e")   #'mediawiki-history-diff-revisions)
    (define-key map (kbd "R")   #'mediawiki-history-restore-revision)
    (define-key map (kbd "b")   #'mediawiki-history-browse-revision)
    (define-key map (kbd "c")   #'mediawiki-history-diff-to-current)
    (define-key map (kbd "g")   #'mediawiki-history-refresh)
    (define-key map (kbd "o")   #'mediawiki-history-open-page)
    map)
  "Keymap for `mediawiki-history-mode'.")

;;; Internal Variables

(defvar-local mediawiki-history--page-title nil
  "The page title for the current history buffer.")

(defvar-local mediawiki-history--revisions nil
  "List of revision alists for the current history buffer.")

(defvar-local mediawiki-history--sitename nil
  "The site name for the current history buffer.")

;;; Internal Functions

(defun mediawiki-history--annotate-diffs (revisions)
  "Annotate each revision in REVISIONS with a `sizediff' key.
Compares each revision's size against the next (older) revision.
Returns a copy of the list with `sizediff' cons cells added.
The newest revision is first in the list."
  (cl-loop for rev in revisions
           for next in (cdr revisions)
           collect
           (let ((current-size (alist-get 'size rev))
                 (next-size (alist-get 'size next)))
             (if (and current-size next-size)
                 (cons (cons 'sizediff (- current-size next-size)) rev)
               (cons (cons 'sizediff nil) rev)))
           into result
           finally return
           ;; Handle the last (oldest) revision separately
           (let ((last-rev (car (last revisions))))
             (if last-rev
                 (nconc result
                        (list (cons (cons 'sizediff nil) last-rev)))
               result))))

(defun mediawiki-history--make-entry (rev)
  "Create a `tabulated-list' entry vector from revision alist REV.
The vector contains: Rev ID, Date, User, Change size, and Comment.
Text properties are placed on the first column for follow-mode
compatibility."
  (let* ((revid (alist-get 'revid rev))
         (parentid (alist-get 'parentid rev))
         (timestamp (alist-get 'timestamp rev))
         (user (alist-get 'user rev))
         (comment (alist-get 'comment rev))
         (sizediff (alist-get 'sizediff rev))
         (minor (alist-get 'minor rev))
         (size-str (mediawiki-format-size-change sizediff))
         (date-str (if timestamp (mediawiki-format-timestamp timestamp) ""))
         (user-str (or user ""))
         (summary (or comment ""))
         (rev-str (propertize (format "%d" revid)
                              'rev-id revid
                              'parent-id parentid
                              'mw-title (or mediawiki-history--page-title
                                            mediawiki-page-title))))
    (when minor
      (setq rev-str (propertize rev-str 'face 'italic)))
    (vector rev-str date-str user-str size-str summary)))

(defun mediawiki-history--revision-at-point ()
  "Return the revision alist for the entry at point.
Uses `tabulated-list-get-id' to find the revision ID, then looks up
the corresponding revision in `mediawiki-history--revisions'."
  (let ((id (tabulated-list-get-id)))
    (when id
      (cl-find id mediawiki-history--revisions
               :key (lambda (r) (alist-get 'revid r))
               :test #'equal))))

(defun mediawiki-history--title ()
  "Return the page title for the current history buffer.
Falls back to `mediawiki-page-title' if the buffer-local is nil."
  (or mediawiki-history--page-title mediawiki-page-title))

;;;###autoload
(defun mediawiki-history (title &optional sitename)
  "Display revision history for TITLE in a tabulated list.
If SITENAME is nil, use the current `mediawiki-site' or prompt.
Interactively, prompt for TITLE and use the current site."
  (interactive
   (let ((site (or mediawiki-site (mediawiki-prompt-for-site))))
     (list (read-string "Page title: "
                        (or mediawiki-page-title ""))
           site)))
  (let* ((site (or sitename mediawiki-site (mediawiki-prompt-for-site)))
         (buf-name (format "*MW History: %s*" title))
         (revisions (mediawiki-api-get-page-history
                     site title mediawiki-history-limit))
         (annotated (mediawiki-history--annotate-diffs revisions)))
    (unless revisions
      (user-error "No revisions found for %s" title))
    (with-current-buffer (get-buffer-create buf-name)
      (mediawiki-history-mode)
      (setq mediawiki-history--page-title title
            mediawiki-history--sitename site
            mediawiki-history--revisions annotated
            mediawiki-site site)
      (mediawiki-history--refresh-table)
      (display-buffer (current-buffer)))))

;;; Interactive Commands

(defun mediawiki-history-view-revision ()
  "View the wikitext content of the revision at point.
Opens a read-only buffer showing the revision content."
  (interactive)
  (let* ((rev (mediawiki-history--revision-at-point))
         (revid (alist-get 'revid rev))
         (title (mediawiki-history--title)))
    (unless rev
      (user-error "No revision at point"))
    (mediawiki-diff-show-revision revid title)))

(defun mediawiki-history-diff-to-previous ()
  "Show diff between the revision at point and its parent revision."
  (interactive)
  (let* ((rev (mediawiki-history--revision-at-point))
         (revid (alist-get 'revid rev))
         (parentid (alist-get 'parentid rev))
         (title (mediawiki-history--title)))
    (unless rev
      (user-error "No revision at point"))
    (unless (and parentid (not (zerop parentid)))
      (user-error "This revision has no parent"))
    (mediawiki-diff-show-diff parentid revid title)))

(defun mediawiki-history-diff-to-current ()
  "Show diff between the revision at point and the latest revision."
  (interactive)
  (let* ((rev (mediawiki-history--revision-at-point))
         (revid (alist-get 'revid rev))
         (title (mediawiki-history--title))
         (latest (alist-get 'revid (car mediawiki-history--revisions))))
    (unless rev
      (user-error "No revision at point"))
    (unless latest
      (user-error "No latest revision available"))
    (if (= revid latest)
        (message "This is the latest revision")
      (mediawiki-diff-show-diff revid latest title))))

(defun mediawiki-history-diff-revisions (from-rev to-rev)
  "Show diff between two arbitrary revisions.
Prompts for FROM-REV and TO-REV revision IDs."
  (interactive
   (let* ((rev (mediawiki-history--revision-at-point))
          (default-from (if rev (alist-get 'parentid rev) nil))
          (default-to (if rev (alist-get 'revid rev) nil))
          (from (read-number "From revision: " default-from))
          (to (read-number "To revision: " default-to)))
     (list from to)))
  (let ((title (mediawiki-history--title)))
    (unless title
      (user-error "No page title available"))
    (mediawiki-diff-show-diff from-rev to-rev title)))

(defun mediawiki-history-restore-revision ()
  "Restore the page to the revision at point.
Prompts for confirmation and an optional edit summary."
  (interactive)
  (let* ((rev (mediawiki-history--revision-at-point))
         (revid (alist-get 'revid rev))
         (title (mediawiki-history--title))
         (sitename mediawiki-history--sitename))
    (unless rev
      (user-error "No revision at point"))
    (when (yes-or-no-p (format "Restore %s to revision %d? " title revid))
      (let ((summary (read-string "Edit summary: "
                                  (format "Restored revision %d" revid))))
        (when (mediawiki-api-restore-revision sitename title revid summary)
          (message "Page %s restored to revision %d" title revid)
          (mediawiki-history-refresh))))))

(defun mediawiki-history-browse-revision ()
  "Open the revision at point in an external web browser."
  (interactive)
  (let* ((rev (mediawiki-history--revision-at-point))
         (revid (alist-get 'revid rev))
         (title (mediawiki-history--title))
         (sitename mediawiki-history--sitename))
    (unless rev
      (user-error "No revision at point"))
    (browse-url (mediawiki-make-revision-url sitename title revid))))

(defun mediawiki-history-refresh ()
  "Re-fetch the revision history and refresh the table display."
  (interactive)
  (let ((title (mediawiki-history--title))
        (sitename mediawiki-history--sitename))
    (unless (and title sitename)
      (user-error "No page or site set for this buffer"))
    (let ((revisions (mediawiki-api-get-page-history
                      sitename title mediawiki-history-limit)))
      (unless revisions
        (user-error "No revisions found for %s" title))
      (setq mediawiki-history--revisions
            (mediawiki-history--annotate-diffs revisions)))
    (mediawiki-history--refresh-table)))

(defun mediawiki-history-open-page ()
  "Open the current page for editing in `mediawiki-mode'."
  (interactive)
  (let ((title (mediawiki-history--title))
        (sitename mediawiki-history--sitename))
    (unless title
      (user-error "No page title available"))
    (mediawiki-edit sitename title)))

;;; Mode Definition

(defun mediawiki-history--refresh-table ()
  "Rebuild the `tabulated-list' entries and refresh the display."
  (setq tabulated-list-entries
        (mapcar (lambda (rev)
                  (let ((revid (alist-get 'revid rev)))
                    (list revid (mediawiki-history--make-entry rev))))
                mediawiki-history--revisions))
  (tabulated-list-print t))

(define-derived-mode mediawiki-history-mode tabulated-list-mode
  "MediaWiki History"
  "Major mode for browsing MediaWiki page revision history.
Provides a sortable table of revisions with operations for viewing,
diffing, and reverting.

\\{mediawiki-history-mode-map}"
  (setq tabulated-list-format
        [("Rev" 10 t :right-align t)
         ("Date" 20 t)
         ("User" 20 t)
         ("Change" 8 t :right-align t)
         ("Summary" 0 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Date" t))
  (add-hook 'tabulated-list-revert-hook #'mediawiki-history-refresh nil t)
  (tabulated-list-init-header))

(provide 'mediawiki-history)

;;; mediawiki-history.el ends here
