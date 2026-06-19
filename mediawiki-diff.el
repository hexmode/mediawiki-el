;;; mediawiki-diff.el --- Diff engine for mediawiki.el  -*- lexical-binding: t; -*-

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

;; This module provides diff functionality for mediawiki.el.  It
;; offers unified diff display using the system `diff` command,
;; diff-to-live comparison against the latest published revision,
;; single revision viewing, and a diff-follow minor mode that
;; automatically shows diffs when moving through history, watchlist,
;; and user contributions buffers.
;;
;; Key functions:
;;
;;   `mediawiki-diff-show-diff'    - Show diff between two revisions
;;   `mediawiki-diff-show-revision' - Show a single revision's content
;;   `mediawiki-diff-to-live'      - Diff current buffer against live revision
;;   `mediawiki-diff-last-edit'    - Show diff for the most recent edit
;;   `mediawiki-diff-follow-mode'  - Minor mode for auto-diff on cursor move
;;
;; User option:
;;   `mediawiki-diff-function'     - Choose unified or ediff display

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-api)
(require 'mediawiki-site)
(require 'mediawiki-utils)
(require 'diff-mode)

;; Declare variables from other mediawiki modules
(defvar mediawiki-page-title)
(defvar mediawiki-site)

(defgroup mediawiki-diff nil
  "Diff display options for mediawiki.el."
  :tag "MediaWiki Diff"
  :group 'mediawiki)

(defcustom mediawiki-diff-function 'unified
  "Function for displaying diffs.
If `unified', show diffs in a `diff-mode' buffer.
If `ediff', use `ediff-buffers' for visual comparison."
  :type '(choice (const unified) (const ediff))
  :group 'mediawiki-diff)

;;; Internal Helper Functions

(defun mediawiki-diff--write-temp-file (content revid)
  "Write CONTENT to a temp file named after REVID.
Returns the temp file path."
  (let ((file (make-temp-file (format "mw-rev-%d-" revid))))
    (with-temp-file file
      (insert content))
    file))

(defun mediawiki-diff--generate-unified (from-file to-file
                                                   from-label to-label
                                                   &optional context-lines)
  "Generate unified diff between FROM-FILE and TO-FILE with labels.
FROM-LABEL and TO-LABEL are used as file header labels in the diff output.
CONTEXT-LINES defaults to 3.
Returns the diff output string, or nil if files are identical."
  (with-temp-buffer
    (let ((exit-code (call-process "diff" nil t nil
                       (format "-U%d" (or context-lines 3))
                       (format "--label=%s" from-label)
                       (format "--label=%s" to-label)
                       from-file to-file)))
      (cond
       ((= exit-code 0)
        nil)                           ; no differences
       ((= exit-code 1)
        (buffer-string))               ; differences found
       (t
        (error "Diff command failed with exit code %d" exit-code))))))

(defun mediawiki-diff--cleanup-files (files)
  "Delete each file in FILES list."
  (dolist (f files)
    (when (and f (file-exists-p f))
      (delete-file f))))

;;; Unified Diff Display

(defun mediawiki-diff--show-unified (diff-string from-rev to-rev title)
  "Display DIFF-STRING in a `diff-mode' buffer for TITLE from FROM-REV to TO-REV."
  (let ((buf (get-buffer-create
              (format "*MW Diff: %s (%d → %d)*" title from-rev to-rev))))
    (with-current-buffer buf
      (read-only-mode -1)
      (delete-region (point-min) (point-max))
      (insert diff-string)
      (diff-mode)
      (setq-local mediawiki-page-title title)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (display-buffer buf)))

;;; Ediff Display

(defun mediawiki-diff-ediff (from-content to-content from-rev to-rev title)
  "Display ediff between FROM-CONTENT and TO-CONTENT for TITLE.
FROM-REV and TO-REV are revision identifiers used for buffer names.
Buffers are named after the revision IDs and TITLE."
  (let* ((from-buf (get-buffer-create
                    (format "*MW Revision %d: %s*" from-rev title)))
         (to-buf (get-buffer-create
                  (format "*MW Revision %d: %s*" to-rev title)))
         (ediff-buffers
          (lambda ()
            (ediff-buffers from-buf to-buf))))
    (with-current-buffer from-buf
      (read-only-mode -1)
      (delete-region (point-min) (point-max))
      (insert from-content)
      (setq-local mediawiki-page-title title)
      (setq buffer-read-only t))
    (with-current-buffer to-buf
      (read-only-mode -1)
      (delete-region (point-min) (point-max))
      (insert to-content)
      (setq-local mediawiki-page-title title)
      (setq buffer-read-only t))
    (when (fboundp 'ediff-buffers)
      (funcall ediff-buffers))))

;;; Core Diff Functions

;;;###autoload
(defun mediawiki-diff-show-revision (revid title)
  "Show revision REVID of TITLE in a read-only buffer.
Fetches content via the MediaWiki API and displays it."
  (interactive
   (let* ((title (or mediawiki-page-title
                     (read-string "Page title: ")))
          (revid (read-number "Revision ID: ")))
     (list revid title)))
  (let* ((sitename (or mediawiki-site (mediawiki-prompt-for-site)))
         (content (mediawiki-api-get-revision-content
                   sitename title revid)))
    (unless content
      (error "Revision %d of %s not found" revid title))
    (let ((buf (get-buffer-create
              (format "*MW Revision %d: %s*" revid title))))
      (with-current-buffer buf
        (read-only-mode -1)
        (delete-region (point-min) (point-max))
        (insert content)
        (setq-local mediawiki-page-title title)
        (setq buffer-read-only t)
        (goto-char (point-min)))
      (display-buffer buf))))

;;;###autoload
(defun mediawiki-diff-show-diff (from-rev to-rev title)
  "Show diff between FROM-REV and TO-REV of TITLE.
Fetches both revision contents, runs `diff -u', and displays the result
in a `diff-mode' buffer."
(interactive
   (let* ((title (or mediawiki-page-title
                     (read-string "Page title: ")))
          (from-rev (read-number "From revision: "))
          (to-rev (read-number "To revision: ")))
     (list from-rev to-rev title)))
  (let* ((sitename (or mediawiki-site (mediawiki-prompt-for-site)))
         (from-content (mediawiki-api-get-revision-content
                        sitename title from-rev))
         (to-content (mediawiki-api-get-revision-content
                      sitename title to-rev))
         from-file to-file diff-string result)
    (unless from-content
      (error "Revision %d of %s not found" from-rev title))
    (unless to-content
      (error "Revision %d of %s not found" to-rev title))
    (unwind-protect
        (progn
          (setq from-file (mediawiki-diff--write-temp-file from-content from-rev)
                to-file (mediawiki-diff--write-temp-file to-content to-rev))
          (setq diff-string
                (mediawiki-diff--generate-unified
                 from-file to-file
                 (format "Revision %d" from-rev)
                 (format "Revision %d" to-rev)))
          (if diff-string
              (progn
                (setq result t)
                (mediawiki-diff--show-unified
                 diff-string from-rev to-rev title))
            (message "No differences between revision %d and %d of %s"
                     from-rev to-rev title)))
      (mediawiki-diff--cleanup-files (list from-file to-file)))
    result))

;;;###autoload
(defun mediawiki-diff-to-live ()
  "Diff the current buffer against the latest published revision.
Displays which local changes have been made since the last save.
Only available when `mediawiki-page-title' is set."
  (interactive)
  (unless mediawiki-page-title
    (user-error "Not editing a wiki page (no `mediawiki-page-title' set)"))
  (unless mediawiki-site
    (user-error "No `mediawiki-site' set for current buffer"))
  (let* ((sitename mediawiki-site)
         (title mediawiki-page-title)
         (latest (mediawiki-api-get-latest-revision-content sitename title))
         (live-revid (car latest))
         (live-content (cdr latest))
         (local-content (buffer-string)))
    (unless latest
      (error "Could not fetch latest revision of %s" title))
    (mediawiki-diff-buffer-to-revision
     local-content live-content live-revid title)))

(defun mediawiki-diff-buffer-to-revision (local-content live-content
                                                          live-revid title)
  "Compare LOCAL-CONTENT against LIVE-CONTENT (revision LIVE-REVID) of TITLE.
Dispatches to unified diff or ediff based on `mediawiki-diff-function'."
  (if (string-equal local-content live-content)
        (message "No changes (buffer matches live revision %d)" live-revid)
      (pcase mediawiki-diff-function
        ('unified
         (let* ((from-file (mediawiki-diff--write-temp-file live-content live-revid))
                (to-file (mediawiki-diff--write-temp-file
                          local-content (float-time)))
                diff-string)
           (unwind-protect
               (progn
                  (setq diff-string
                        (mediawiki-diff--generate-unified
                         from-file to-file
                         (format "Live revision %d" live-revid)
                         "Local buffer"))
                  (if diff-string
                      (let ((buf (get-buffer-create
                                  (format "*MW Diff: %s (live → local)*" title))))
                        (with-current-buffer buf
                          (read-only-mode -1)
                          (delete-region (point-min) (point-max))
                          (insert diff-string)
                          (diff-mode)
                          (setq-local mediawiki-page-title title)
                          (setq buffer-read-only t)
                          (goto-char (point-min)))
                        (display-buffer buf))
                    (message "No changes (buffer matches live revision %d)"
                             live-revid)))
             (mediawiki-diff--cleanup-files (list from-file to-file)))))
        ('ediff
         (mediawiki-diff-ediff live-content local-content
                               live-revid 0 title))
        (_
         (error "Unknown `mediawiki-diff-function': %S"
                mediawiki-diff-function)))))

;;;###autoload
(defun mediawiki-diff-compare-edit ()
  "Compare current edit buffer against the latest published revision.
This is an alias for `mediawiki-diff-to-live' intended for use
from the page editing context."
  (interactive)
  (mediawiki-diff-to-live))

;;;###autoload
(defun mediawiki-diff-last-edit ()
  "Show diff for the most recent edit to the current page.
Fetches the latest two revisions and displays the diff between them.
Quick way to see what changed since your last visit."
  (interactive)
  (let* ((sitename (or mediawiki-site (mediawiki-prompt-for-site)))
         (title (or mediawiki-page-title
                    (read-string "Page title: ")))
         (result (mediawiki-api-get-latest-revision-content sitename title))
         (to-rev (car result))
         (to-content (cdr result)))
    (unless result
      (error "Could not fetch revision info for %s" title))
    ;; We need the parent revision.  Fetch the revision metadata.
    (let* ((query-result (mediawiki-api-call sitename "query"
                           (list (cons "titles" title)
                                 (cons "prop" "revisions")
                                 (cons "rvprop" "ids|content")
                                 (cons "rvslots" "main")
                                 (cons "rvlimit" 2))))
           (pages (alist-get 'pages (alist-get 'query query-result)))
           (page (cdar pages))
           (revisions (alist-get 'revisions page))
           from-rev from-content)
      (when (and revisions (cdr revisions))
        (let ((prev-rev (cadr revisions)))
          (setq from-rev (alist-get 'revid prev-rev)
                from-content (alist-get '*
                              (alist-get 'main
                                (alist-get 'slots prev-rev))))))
      (if (and from-rev from-content)
          (if (string-equal from-content to-content)
              (message "No differences between last two revisions of %s" title)
            (pcase mediawiki-diff-function
              ('unified
               (let (from-file to-file diff-string)
                 (unwind-protect
                     (progn
                       (setq from-file
                             (mediawiki-diff--write-temp-file
                              from-content from-rev)
                             to-file
                             (mediawiki-diff--write-temp-file
                              to-content to-rev))
                       (setq diff-string
                             (mediawiki-diff--generate-unified
                              from-file to-file
                              (format "Revision %d" from-rev)
                              (format "Revision %d" to-rev)))
                       (if diff-string
                           (mediawiki-diff--show-unified
                            diff-string from-rev to-rev title)
                         (message "No differences between last two revisions of %s"
                                  title)))
                   (mediawiki-diff--cleanup-files
                    (list from-file to-file)))))
              ('ediff
               (mediawiki-diff-ediff from-content to-content
                                     from-rev to-rev title))
              (_
               (error "Unknown `mediawiki-diff-function': %S"
                      mediawiki-diff-function))))
        (message "Only one revision of %s exists" title)))))

;;; Diff Follow Mode

(defvar mediawiki-diff-follow--last-revid nil
  "Revision ID of the last displayed diff in follow mode.
Used to avoid re-displaying the same diff on cursor movement.")

(defvar mediawiki-diff-follow-mode nil
  "Non-nil when Diff-Follow mode is enabled.")

(defun mediawiki-diff-follow--entry-info ()
  "Extract revision pair and title from the current buffer context.
Detects the major mode and extracts (FROM-REV TO-REV TITLE).
Currently supports history, watchlist, and user-contributions modes.
Returns a list (FROM-REV TO-REV TITLE) or nil if not in a known mode."
  (cond
   ;; mediawiki-history-mode: use parentid and revid
   ((eq major-mode 'mediawiki-history-mode)
    (when-let* ((rev-id (get-text-property (point) 'rev-id))
                (parent-id (get-text-property (point) 'parent-id))
                (title (or (get-text-property (point) 'mw-title)
                           mediawiki-page-title)))
      (list parent-id rev-id title)))
   ;; mediawiki-watchlist-mode: use old_revid and revid
   ((eq major-mode 'mediawiki-watchlist-mode)
    (when-let* ((rev-id (get-text-property (point) 'rev-id))
                (old-revid (get-text-property (point) 'old_revid))
                (title (or (get-text-property (point) 'mw-title)
                           (get-text-property (point) 'title))))
      (list old-revid rev-id title)))
   ;; user-contributions-mode: use parentid and revid
   ((eq major-mode 'mediawiki-user-contributions-mode)
    (when-let* ((rev-id (get-text-property (point) 'rev-id))
                (parent-id (get-text-property (point) 'parent-id))
                (title (or (get-text-property (point) 'mw-title)
                           mediawiki-page-title)))
      (list parent-id rev-id title)))
   (t nil)))

(defun mediawiki-diff-follow--on-move ()
  "Hook for `post-command-hook' to show diff on cursor movement.
Only acts when `mediawiki-diff-follow-mode' is enabled and point has
moved to a different entry.  Extracts revision info from text properties
and displays the diff when appropriate."
  (when mediawiki-diff-follow-mode
    (let ((entry (mediawiki-diff-follow--entry-info)))
      (when entry
        (let ((from-rev (nth 0 entry))
              (to-rev (nth 1 entry))
              (title (nth 2 entry)))
          (when (and from-rev to-rev title
                     (not (equal to-rev mediawiki-diff-follow--last-revid)))
            (setq mediawiki-diff-follow--last-revid to-rev)
            (mediawiki-diff-show-diff from-rev to-rev title)))))))

;;;###autoload
(define-minor-mode mediawiki-diff-follow-mode
  "Toggle automatic diff display on cursor movement.
When enabled, moving point in history, watchlist, or user contributions
buffers will automatically show the diff for the entry at point.
Only works with unified diff (not ediff)."
  :lighter " Diff-Follow"
  :group 'mediawiki-diff
  (if mediawiki-diff-follow-mode
      (progn
        (add-hook 'post-command-hook #'mediawiki-diff-follow--on-move nil t)
        (setq mediawiki-diff-follow--last-revid nil)
        ;; Immediately show diff for the current entry
        (let ((entry (mediawiki-diff-follow--entry-info)))
          (when entry
            (let ((from-rev (nth 0 entry))
                  (to-rev (nth 1 entry))
                  (title (nth 2 entry)))
              (when (and from-rev to-rev title)
                (setq mediawiki-diff-follow--last-revid to-rev)
                (mediawiki-diff-show-diff from-rev to-rev title))))))
    (remove-hook 'post-command-hook #'mediawiki-diff-follow--on-move t)))

(provide 'mediawiki-diff)

;;; mediawiki-diff.el ends here
