;;; mediawiki-watchlist.el --- Watchlist browser for mediawiki.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el contributors

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

;; This module provides a watchlist browser for mediawiki.el.  It
;; displays the user's watchlist in a grouped, expandable
;; `tabulated-list-mode' table with unread tracking, diff integration,
;; and prefetch support.
;;
;; Key functions:
;;
;;   `mediawiki-watchlist'             - Open the watchlist browser
;;   `mediawiki-watchlist-open-page'   - Open page at point for editing
;;   `mediawiki-watchlist-show-diff'   - Show diff for entry at point
;;   `mediawiki-watchlist-show-history' - Show page history at point
;;   `mediawiki-watchlist-toggle-expand' - Toggle group expansion
;;   `mediawiki-watchlist-mark-all-read' - Mark all as read
;;
;; Entries are grouped by page title.  Groups with a single entry
;; are shown flat without expand/collapse.  Groups with multiple
;; entries show "▶"/"▼" toggles.  Unread entries are displayed in
;; bold.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-api)
(require 'mediawiki-utils)
(require 'mediawiki-cache)
(require 'mediawiki-diff)
(require 'tabulated-list)
(require 'cl-lib)

(declare-function mediawiki-history "mediawiki-history")
(declare-function mediawiki-site-url "mediawiki-site")
(declare-function mediawiki-prompt-for-site "mediawiki-site")
(declare-function mediawiki-edit "mediawiki-page")
(declare-function mediawiki-open "mediawiki-page")

;;; Keymap

(defvar mediawiki-watchlist-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'mediawiki-watchlist-open-page)
    (define-key map (kbd "o")   #'mediawiki-watchlist-open-page)
    (define-key map (kbd "v")   #'mediawiki-watchlist-show-diff)
    (define-key map (kbd "d")   #'mediawiki-watchlist-show-diff)
    (define-key map (kbd "h")   #'mediawiki-watchlist-show-history)
    (define-key map (kbd "b")   #'mediawiki-watchlist-browse)
    (define-key map (kbd "g")   #'mediawiki-watchlist-refresh)
    (define-key map (kbd "TAB") #'mediawiki-watchlist-toggle-expand)
    (define-key map (kbd "SPC") #'mediawiki-watchlist-toggle-expand)
    (define-key map (kbd "e")   #'mediawiki-watchlist-expand-all)
    (define-key map (kbd "c")   #'mediawiki-watchlist-collapse-all)
    (define-key map (kbd "m")   #'mediawiki-watchlist-mark-all-read)
    (define-key map (kbd "!")   #'mediawiki-watchlist-mark-all-read)
    (define-key map (kbd "w")   #'mediawiki-watchlist-toggle-watch)
    map)
  "Keymap for `mediawiki-watchlist-mode'.")

;;; Internal State

(defvar-local mediawiki-watchlist--entries nil
  "Raw watchlist entries for the current buffer.
Each entry is an alist with keys `title', `revid', `old_revid',
`timestamp', `user', `comment', `oldlen', `newlen'.")

(defvar-local mediawiki-watchlist--grouped nil
  "Grouped entries: alist of (TITLE . ENTRIES-LIST).
Each ENTRIES-LIST is sorted newest-first by timestamp.")

(defvar-local mediawiki-watchlist--read nil
  "Hash table mapping revision ID (integer) to t for read revisions.")

(defvar-local mediawiki-watchlist--collapsed nil
  "Hash table mapping group title (string) to t for collapsed groups.")

(defvar-local mediawiki-watchlist--sitename nil
  "Site name for this watchlist buffer.")

;;; Major Mode

(define-derived-mode mediawiki-watchlist-mode tabulated-list-mode
  "MediaWiki Watchlist"
  "Major mode for browsing the MediaWiki watchlist.

Entries are grouped by page title.  Groups with multiple revisions
show \"▶\"/\"▼\" expand/collapse toggles.  Single-entry groups are
shown flat.  Unread entries are displayed in bold.

\\{mediawiki-watchlist-mode-map}"
  (setq tabulated-list-format
        [("Icon" 2 nil)
         ("C" 3 nil)
         ("Page" 40 t)
         ("Time" 20 nil)
         ("User" 20 nil)
         ("Change" 8 nil)
         ("Summary" 0 nil)])
  (setq tabulated-list-sort-key nil)
  (setq tabulated-list-entries
        #'mediawiki-watchlist--build-display-entries)
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook
            #'mediawiki-watchlist--rebuild nil t)
  (setq mediawiki-watchlist--read (make-hash-table :test 'eql))
  (setq mediawiki-watchlist--collapsed (make-hash-table :test 'equal)))

;;; Grouping

(defun mediawiki-watchlist--group-entries (entries)
  "Group ENTRIES by page title.
Returns an alist of (TITLE . ENTRIES-LIST) where each
ENTRIES-LIST is sorted newest-first by timestamp.
Groups are sorted by the most recent timestamp, newest first."
  (let ((groups (make-hash-table :test 'equal)))
    ;; Collect entries by title
    (dolist (entry entries)
      (let ((title (alist-get 'title entry)))
        (push entry (gethash title groups))))
    ;; Sort each group by timestamp descending, then build output alist
    (let ((result nil))
      (maphash
       (lambda (title entries)
         (let ((sorted (cl-sort entries #'string>
                                :key (lambda (e) (or (alist-get 'timestamp e) "")))))
           (push (cons title (nreverse sorted)) result)))
       groups)
      ;; Sort groups by most recent timestamp, newest first
      (cl-sort result #'string>
:key (lambda (g)
                       (or (alist-get 'timestamp (car (cdr g))) ""))))))

(defun mediawiki-watchlist--make-group-entry (title entries)
  "Create a tabulated-list entry for group TITLE with ENTRIES.
Return a cons (ID . VECTOR) suitable for `tabulated-list-entries'."
  (let* ((count (length entries))
         (expanded (not (gethash title mediawiki-watchlist--collapsed)))
         (icon (if (= count 1) " "
                 (if expanded "▼" "▶")))
         (latest-ts (alist-get 'timestamp (car entries)))
         (users (cl-delete-duplicates
                 (mapcar (lambda (e) (alist-get 'user e)) entries)
                 :test #'string=))
         (user-str
          (cond
           ((null users) "")
           ((<= (length users) 2)
            (mapconcat #'identity users ", "))
           (t
            (format "%s, +%d more" (car users) (1- (length users))))))
         (total-change
          (cl-loop for e in entries
                   sum (- (or (alist-get 'newlen e) 0)
                          (or (alist-get 'oldlen e) 0))))
         (has-unread
          (cl-some (lambda (e)
                     (not (gethash (alist-get 'revid e)
                                  mediawiki-watchlist--read)))
                   entries)))
    (list (cons 'group title)
          (vector icon
                  (propertize (format "%d" count)
                              'face (and has-unread 'bold))
                  (propertize title
                              'face (and has-unread 'bold))
                  (mediawiki-format-timestamp latest-ts)
                  user-str
                  (mediawiki-format-size-change total-change)
                  ""))))

(defun mediawiki-watchlist--make-child-entry (entry)
  "Create a tabulated-list entry for a single revision ENTRY.
Return a cons (ID . VECTOR) suitable for `tabulated-list-entries'.
Adds text properties for `mediawiki-diff-follow-mode'."
  (let* ((title (alist-get 'title entry))
         (revid (alist-get 'revid entry))
         (old-revid (alist-get 'old_revid entry))
         (read-p (gethash revid mediawiki-watchlist--read))
         (face (and (not read-p) '(bold)))
         (timestamp (mediawiki-format-timestamp
                     (alist-get 'timestamp entry)))
         (user (or (alist-get 'user entry) ""))
         (sizediff (- (or (alist-get 'newlen entry) 0)
                      (or (alist-get 'oldlen entry) 0)))
         (page-col (propertize title
                               'face face
                               'rev-id revid
                               'old_revid old-revid
                               'mw-title title)))
    (list (cons 'child (cons title revid))
          (vector " "
                  " "
                  page-col
                  (propertize timestamp 'face face)
                  (propertize user 'face face)
                  (propertize (mediawiki-format-size-change sizediff)
                              'face face)
                  (or (alist-get 'comment entry) "")))))

(defun mediawiki-watchlist--build-display-entries ()
  "Build flat list of tabulated-list entries for current display.
Includes group headers and expanded children.
Called by `tabulated-list-print' when `tabulated-list-entries' is a function."
  (let ((entries '()))
    (dolist (group mediawiki-watchlist--grouped)
      (let ((title (car group))
            (children (cdr group)))
        ;; Add group header
        (push (mediawiki-watchlist--make-group-entry title children)
              entries)
        ;; Add children if group is expanded (not collapsed)
        (unless (gethash title mediawiki-watchlist--collapsed)
          (dolist (child children)
            (push (mediawiki-watchlist--make-child-entry child)
                  entries)))))
    (nreverse entries)))

(defun mediawiki-watchlist--rebuild ()
  "Rebuild internal state from `mediawiki-watchlist--entries'.
Re-groups entries and refreshes the tabulated list display.
Called as `tabulated-list-revert-hook'."
  (setq mediawiki-watchlist--grouped
        (mediawiki-watchlist--group-entries
         mediawiki-watchlist--entries)))

;;; Entry Point Helpers

(defun mediawiki-watchlist--entry-at-point ()
  "Return the entry alist at point, or nil.
Only meaningful on child rows (individual revisions).
For group headers, return nil."
  (let ((id (tabulated-list-get-id)))
    (when (and (consp id) (eq (car id) 'child))
      (let* ((title (car (cdr id)))
             (revid (cdr (cdr id))))
        (cl-find-if (lambda (e)
                      (and (string= (alist-get 'title e) title)
                           (= (alist-get 'revid e) revid)))
                    mediawiki-watchlist--entries)))))

(defun mediawiki-watchlist--title-at-point ()
  "Return the page title at point, or nil.
Works for both group headers and child entries."
  (let ((id (tabulated-list-get-id)))
    (pcase id
      ((pred consp)
       (if (eq (car id) 'group)
           (cdr id)
         (car (cdr id))))
      (_ nil))))

(defun mediawiki-watchlist--revid-at-point ()
  "Return the revision ID at point, or nil.
Only meaningful on child rows."
  (let ((id (tabulated-list-get-id)))
    (when (and (consp id) (eq (car id) 'child))
      (cdr (cdr id)))))

(defun mediawiki-watchlist--entries-for-group (title)
  "Return all entries for group TITLE, or nil."
  (cdr (assoc-string title mediawiki-watchlist--grouped)))

(defun mediawiki-watchlist--group-header-p ()
  "Return non-nil if point is on a group header row."
  (let ((id (tabulated-list-get-id)))
    (and (consp id) (eq (car id) 'group))))

(defun mediawiki-watchlist--mark-at-point-read ()
  "Mark the entry at point as read.
Updates the local read hash table and calls
`mediawiki-api-mark-page-seen' on the server."
  (let* ((title (mediawiki-watchlist--title-at-point))
         (entry (mediawiki-watchlist--entry-at-point))
         (revid (mediawiki-watchlist--revid-at-point)))
    (when (and entry revid)
      (puthash revid t mediawiki-watchlist--read)
      (when (and mediawiki-watchlist--sitename title)
        (mediawiki-api-mark-page-seen mediawiki-watchlist--sitename title))
      (tabulated-list-print t))))

;;; Interactive Commands

(defun mediawiki-watchlist-open-page ()
  "Open the page at point for editing.
Marks the entry as read before opening."
  (interactive)
  (let* ((title (or (mediawiki-watchlist--title-at-point)
                    (user-error "No page at point")))
         (sitename (or mediawiki-watchlist--sitename
                       (mediawiki-prompt-for-site))))
    ;; Mark as read
    (when-let* ((revid (mediawiki-watchlist--revid-at-point)))
      (puthash revid t mediawiki-watchlist--read)
      (mediawiki-api-mark-page-seen sitename title))
    (mediawiki-edit sitename title)))

(defun mediawiki-watchlist-show-diff ()
  "Show diff for the entry or group at point.
For individual entries, shows the diff between old_revid and revid.
For group headers, shows the diff from the oldest old_revid to the
newest revid across all entries in the group."
  (interactive)
  (if (mediawiki-watchlist--group-header-p)
        ;; Group header: diff oldest→newest across all entries
        (let* ((title (or (mediawiki-watchlist--title-at-point)
                          (user-error "No page at point")))
               (entries (or (mediawiki-watchlist--entries-for-group title)
                            (user-error "No entries for this group")))
               (all-revids (mapcar (lambda (e)
                                     (cons (alist-get 'revid e)
                                           (alist-get 'old_revid e)))
                                   entries))
               (newest-revid (apply #'max (mapcar #'car all-revids)))
               (oldest-old-revid (apply #'min (mapcar #'cdr all-revids))))
          (if (and oldest-old-revid (> oldest-old-revid 0))
              (mediawiki-diff-show-diff oldest-old-revid newest-revid title)
            (user-error "No previous revision to diff against")))
      ;; Single entry
      (let* ((entry (or (mediawiki-watchlist--entry-at-point)
                        (user-error "No entry at point")))
             (title (alist-get 'title entry))
             (revid (alist-get 'revid entry))
             (old-revid (alist-get 'old_revid entry)))
        (if (and old-revid (> old-revid 0))
            (mediawiki-diff-show-diff old-revid revid title)
          (user-error "No previous revision to diff against")))))

(defun mediawiki-watchlist-show-history ()
  "Show the page history for the page at point."
  (interactive)
  (let* ((title (or (mediawiki-watchlist--title-at-point)
                    (user-error "No page at point")))
         (sitename (or mediawiki-watchlist--sitename
                       (mediawiki-prompt-for-site))))
    (require 'mediawiki-history)
    (mediawiki-history title sitename)))

(defun mediawiki-watchlist-browse ()
  "Open the page URL at point in an external browser."
  (interactive)
  (let* ((title (or (mediawiki-watchlist--title-at-point)
                    (user-error "No page at point")))
         (sitename (or mediawiki-watchlist--sitename
                       (mediawiki-prompt-for-site)))
         (url (mediawiki-make-page-url sitename title)))
    (browse-url url)))

(defun mediawiki-watchlist-toggle-expand ()
  "Toggle expand/collapse for the group at point.
If point is on a child entry, expand/collapse the parent group."
  (interactive)
  (let ((title (or (mediawiki-watchlist--title-at-point)
                   (user-error "No group at point"))))
    (if (gethash title mediawiki-watchlist--collapsed)
        (remhash title mediawiki-watchlist--collapsed)
      (puthash title t mediawiki-watchlist--collapsed))
    (tabulated-list-print t)))

(defun mediawiki-watchlist-expand-all ()
  "Expand all collapsed groups."
  (interactive)
  (clrhash mediawiki-watchlist--collapsed)
  (tabulated-list-print t))

(defun mediawiki-watchlist-collapse-all ()
  "Collapse all expandable groups (groups with 2+ entries)."
  (interactive)
  (dolist (group mediawiki-watchlist--grouped)
    (let ((title (car group))
          (entries (cdr group)))
      (when (> (length entries) 1)
        (puthash title t mediawiki-watchlist--collapsed))))
  (tabulated-list-print t))

(defun mediawiki-watchlist-mark-all-read ()
  "Mark all entries in the watchlist as read.
Updates the local read hash table and calls
`mediawiki-api-mark-page-seen' for each unique page title.
Marks happen asynchronously."
  (interactive)
  (let ((sitename (or mediawiki-watchlist--sitename
                      (mediawiki-prompt-for-site))))
    ;; Mark all revisions as read locally
    (dolist (entry mediawiki-watchlist--entries)
      (let ((revid (alist-get 'revid entry)))
        (when revid
          (puthash revid t mediawiki-watchlist--read))))
    ;; Mark all unique pages as seen on the server (async)
    (let ((seen-titles (make-hash-table :test 'equal)))
      (dolist (entry mediawiki-watchlist--entries)
        (let ((title (alist-get 'title entry)))
          (unless (gethash title seen-titles)
            (puthash title t seen-titles)
            (mediawiki-api-mark-page-seen sitename title)))))
    (tabulated-list-print t)
    (message "Marked all watchlist entries as read")))

(defun mediawiki-watchlist-toggle-watch ()
  "Toggle watch/unwatch status for the page at point."
  (interactive)
  (let* ((title (or (mediawiki-watchlist--title-at-point)
                    (user-error "No page at point")))
         (sitename (or mediawiki-watchlist--sitename
                       (mediawiki-prompt-for-site))))
    ;; We toggle by setting unwatch=t (which means remove from watchlist).
    ;; To truly toggle, we'd need to know current watch status.
    ;; Default to unwatch (toggle off), prompt for direction.
    (if (y-or-n-p (format "Unwatch \"%s\"? " title))
        (progn
          (mediawiki-api-set-watch sitename title t)
          (message "Removed \"%s\" from watchlist" title))
      (mediawiki-api-set-watch sitename title nil)
      (message "Added \"%s\" to watchlist" title))))

;;; Refresh

(defun mediawiki-watchlist--refresh-internal ()
  "Fetch watchlist entries and rebuild the display.
Internal function that updates internal state and re-renders."
  (let* ((sitename (or mediawiki-watchlist--sitename
                       (mediawiki-prompt-for-site)))
         (entries (mediawiki-api-get-watchlist sitename 500 30)))
    (setq mediawiki-watchlist--entries entries
          mediawiki-watchlist--sitename sitename)
    ;; Prefetch diffs
    (mediawiki-cache-prefetch-watchlist-diffs sitename entries)
    ;; Rebuild grouped data
    (mediawiki-watchlist--rebuild)
    (tabulated-list-print t)
    (message "Watchlist updated: %d entries across %d pages"
             (length entries) (length mediawiki-watchlist--grouped))))

(defun mediawiki-watchlist-refresh ()
  "Refresh the watchlist by re-fetching from the server."
  (interactive)
  (mediawiki-watchlist--refresh-internal))

;;; Entry Point

;;;###autoload
(defun mediawiki-watchlist (&optional sitename)
  "Open the watchlist browser for SITENAME.
If SITENAME is nil, use the current `mediawiki-site' or prompt.
Displays grouped watchlist entries with expand/collapse support,
unread tracking, and diff integration."
  (interactive)
  (let* ((site (or sitename
                   (and (bound-and-true-p mediawiki-site)
                        mediawiki-site)
                   (mediawiki-prompt-for-site)))
         (buf (get-buffer-create "*MW Watchlist*")))
    (with-current-buffer buf
      (mediawiki-watchlist-mode)
      (setq mediawiki-watchlist--sitename site)
      (mediawiki-watchlist--refresh-internal))
    (display-buffer buf)
    buf))

(provide 'mediawiki-watchlist)

;;; mediawiki-watchlist.el ends here
