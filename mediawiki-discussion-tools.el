;;; mediawiki-discussion-tools.el --- DiscussionTools browser for mediawiki.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2026 Mark A. Hershberger

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

;; This module provides a generic discussion page browser for MediaWiki
;; sites, displayed as a `tabulated-list-mode' table with thread metadata
;; extracted from the DiscussionTools extension API.
;;
;; Key functions:
;;
;;   `mediawiki-discussion-tools'           - Open a discussion page thread list
;;   `mediawiki-discussion-tools-refresh'   - Re-fetch and redisplay threads
;;
;; User options:
;;
;;   `mediawiki-discussion-tools-stale-days'  - Days before thread is stale (default 14)
;;   `mediawiki-discussion-tools-max-threads' - Max threads to display (default nil)
;;   `mediawiki-discussion-tools-signature'   - Signature string (default "~~~~")
;;
;; API:
;;
;;   This module uses the DiscussionTools `discussiontoolspageinfo' API,
;;   which returns structured thread metadata including comment counts,
;;   author counts, and reply timestamps.  The API is provided by the
;;   DiscussionTools extension (mw:Extension:DiscussionTools).
;;
;; Example — opening the MediaWiki support desk:
;;
;;   (require 'mediawiki-discussion-tools)
;;   (mediawiki-discussion-tools "Project:Support_desk" "mediawiki.org")

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-site)
(require 'mediawiki-api)
(require 'tabulated-list)
(require 'cl-lib)

(declare-function mediawiki-prompt-for-site "mediawiki-site")

;;; Customization

(defgroup mediawiki-discussion-tools nil
  "Discussion page browser for mediawiki.el."
  :tag "MediaWiki Discussion Tools"
  :group 'mediawiki)

(defcustom mediawiki-discussion-tools-stale-days 14
  "Number of days before a thread is considered stale.
Should match the auto-archiving threshold on the wiki."
  :type 'integer
  :group 'mediawiki-discussion-tools)

(defcustom mediawiki-discussion-tools-max-threads nil
  "Maximum number of threads to display.  nil means no limit."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'mediawiki-discussion-tools)

(defcustom mediawiki-discussion-tools-signature "~~~~"
  "Signature string appended to replies and resolution comments.
The default \"~~~~\" expands to the user's standard MediaWiki
signature (timestamp + username link) when saved.  Set to a custom
string like \"--~~~~\" or a raw wikitext sig if preferred."
  :type 'string
  :group 'mediawiki-discussion-tools)

;;; Keymap

(defvar mediawiki-discussion-tools-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g")   #'mediawiki-discussion-tools-refresh)
    (define-key map (kbd "n")   #'mediawiki-discussion-tools-new-thread)
    (define-key map (kbd "r")   #'mediawiki-discussion-tools-reply)
    (define-key map (kbd "d")   #'mediawiki-discussion-tools-resolve)
    map)
  "Keymap for `mediawiki-discussion-tools-list-mode'.")

;;; Internal Variables

(defvar-local mediawiki-discussion-tools--threads nil
  "List of thread alists for the current discussion buffer.")

(defvar-local mediawiki-discussion-tools--sitename nil
  "The site name for the current discussion buffer.")

(defvar-local mediawiki-discussion-tools--page nil
  "The page name for the current discussion buffer.")

;;; Thread Parsing

(defun mediawiki-discussion-tools--parse-threads (json)
  "Parse the discussiontoolspageinfo JSON response into thread records.
Returns a list of thread alists, or signals an error if the response
is malformed."
  (let ((items (alist-get 'threaditemshtml
                          (alist-get 'discussiontoolspageinfo json))))
    (unless items
      (error "No thread data in API response"))
    (cl-loop for thread in items
             for heading = (alist-get 'headingLevel thread)
             when (and heading (= heading 2))
             collect (list
                      (cons 'id (alist-get 'id thread))
                      (cons 'title (mediawiki-discussion-tools--strip-tags
                                    (or (alist-get 'html thread) "")))
                      (cons 'author
                            (if-let* ((oldest (alist-get 'oldestReply thread)))
                                 (alist-get 'author oldest)
                               ""))
                      (cons 'reply-count
                            (or (alist-get 'commentCount thread) 0))
                      (cons 'author-count
                            (or (alist-get 'authorCount thread) 0))
                      (cons 'latest-reply
                            (alist-get 'latestReply thread))
                      (cons 'timestamp
                            (if-let* ((latest (alist-get 'latestReply thread)))
                                (alist-get 'timestamp latest)
                              nil))
                      (cons 'status
                            (mediawiki-discussion-tools--thread-status
                             thread))))))

(defun mediawiki-discussion-tools--strip-tags (html)
  "Strip HTML tags from HTML string."
  (replace-regexp-in-string "<[^>]+>" "" html))

(defun mediawiki-discussion-tools--thread-status (thread)
  "Determine the status of a THREAD record from the API.
Returns one of: \\='active, \\='stale, \\='unanswered.
Note: \\='resolved detection is deferred to Phase 2 (requires full HTML)."
  (let ((comment-count (or (alist-get 'commentCount thread) 0))
        (latest (alist-get 'latestReply thread)))
    (cond
     ((<= comment-count 1) 'unanswered)
     ((and latest
           (let* ((latest-ts (alist-get 'timestamp latest))
                  (latest-time (when latest-ts
                                 (mediawiki-discussion-tools--parse-iso8601
                                  latest-ts)))
                  (stale-time (time-subtract
                               (current-time)
                               (* mediawiki-discussion-tools-stale-days
                                  86400))))
             (and latest-time (time-less-p latest-time stale-time))))
      'stale)
     (t 'active))))

(defun mediawiki-discussion-tools--parse-iso8601 (timestamp)
  "Parse an ISO 8601 TIMESTAMP string into an Emacs time value."
  (condition-case nil
      (date-to-time timestamp)
    (error nil)))

;;; Priority Sorting

(defun mediawiki-discussion-tools--thread-priority (thread)
  "Return a numeric priority for THREAD (lower = higher priority).
Phase 1 ordering: unanswered first, then active, then stale."
  (pcase (alist-get 'status thread)
    ('unanswered 10)
    ('active 20)
    (_ 30)))

;;; Table Entries

(defun mediawiki-discussion-tools--status-icon (status)
  "Return a character icon for STATUS."
  (pcase status
    ('unanswered " ?")
    ('active     " \u00b7")   ; middle dot
    ('stale      " ~")
    ('resolved   " \u2713")   ; checkmark
    (_           "  ")))

(defun mediawiki-discussion-tools--format-relative-time (timestamp)
  "Format TIMESTAMP (ISO 8601 string) as a relative time string."
  (if (not (and timestamp (> (length timestamp) 0)))
      "unknown"
    (let* ((time (mediawiki-discussion-tools--parse-iso8601 timestamp))
           (diff (when time (float-time (time-subtract (current-time) time)))))
      (cond
       ((null time) "unknown")
       ((< diff 60) "just now")
       ((< diff 3600) (format "%dm ago" (/ (truncate diff) 60)))
       ((< diff 86400) (format "%dh ago" (/ (truncate diff) 3600)))
       ((< diff 604800) (format "%dd ago" (/ (truncate diff) 86400)))
       ((< diff 2592000) (format "%dw ago" (/ (truncate diff) 604800)))
       (t (format "%dmo ago" (/ (truncate diff) 2592000)))))))

(defun mediawiki-discussion-tools--make-entry (thread)
  "Create a `tabulated-list' entry vector from a THREAD alist."
  (let ((id (alist-get 'id thread)))
    (list id
          (vector
           (mediawiki-discussion-tools--status-icon
            (alist-get 'status thread))
           (propertize (or (alist-get 'title thread) "")
                       'thread-id id)
           (or (alist-get 'author thread) "")
           (format "%d" (or (alist-get 'reply-count thread) 0))
           (if-let* ((ts (alist-get 'timestamp thread)))
               (mediawiki-discussion-tools--format-relative-time ts)
             "")))))

;;; Thread Fetching

(defun mediawiki-discussion-tools--fetch-threads (sitename page &optional full)
  "Fetch thread data from PAGE on SITENAME using the DiscussionTools API.
If FULL is non-nil, fetch with full reply trees (no noreplies flag).
Otherwise uses threaditemsflags=activity|noreplies for lightweight list.
Returns a list of thread alists sorted by priority."
  (let* ((flags (if full
                    "activity|excludesignatures"
                  "activity|noreplies"))
         (result (mediawiki-api-call sitename "discussiontoolspageinfo"
                   `(("page" . ,page)
                     ("prop" . "threaditemshtml")
                     ("threaditemsflags" . ,flags)
                     ("formatversion" . "2"))))
         (threads (mediawiki-discussion-tools--parse-threads result)))
    (when mediawiki-discussion-tools-max-threads
      (setq threads (cl-subseq threads 0 mediawiki-discussion-tools-max-threads)))
    (sort threads
          (lambda (a b)
            (let ((pa (mediawiki-discussion-tools--thread-priority a))
                  (pb (mediawiki-discussion-tools--thread-priority b)))
              (< pa pb))))))

;;; Entry Point

;;;###autoload
(defun mediawiki-discussion-tools (page &optional sitename)
  "Open a discussion page thread list.
PAGE is the wiki page whose sections will be displayed as threads
(e.g., \"Project:Support_desk\" or \"Talk:Main Page\").
If SITENAME is nil, use the current `mediawiki-site' or prompt.
Displays threads sorted by priority: unanswered first, then active,
then stale."
  (interactive
   (list (read-string "Discussion page: ")
         (or mediawiki-site (mediawiki-prompt-for-site))))
  (let* ((site (or sitename mediawiki-site (mediawiki-prompt-for-site)))
         (buf-name (format "*MW Discussion: %s*" page))
         (threads (mediawiki-discussion-tools--fetch-threads site page)))
    (unless threads
      (error "No threads found on %s" page))
    (with-current-buffer (get-buffer-create buf-name)
      (mediawiki-discussion-tools-list-mode)
      (setq mediawiki-discussion-tools--threads threads
            mediawiki-discussion-tools--sitename site
            mediawiki-discussion-tools--page page)
      (mediawiki-discussion-tools--refresh-table)
      (pop-to-buffer (current-buffer)))))

;;; Interactive Commands

(defun mediawiki-discussion-tools-refresh ()
  "Re-fetch the thread list and refresh the table display."
  (interactive)
  (let ((site mediawiki-discussion-tools--sitename)
        (page mediawiki-discussion-tools--page))
    (unless (and site page)
      (user-error "No site or page set for this buffer"))
    (let ((threads (mediawiki-discussion-tools--fetch-threads site page)))
      (unless threads
        (error "No threads found on %s" page))
      (setq mediawiki-discussion-tools--threads threads)
      (mediawiki-discussion-tools--refresh-table)
      (message "Discussion refreshed (%d threads)" (length threads)))))

;;; Stub Commands (implemented in later phases)

(defun mediawiki-discussion-tools-new-thread ()
  "Create a new discussion thread.
Not yet implemented."
  (interactive)
  (user-error "New thread: not yet implemented (Phase 3)"))

(defun mediawiki-discussion-tools-reply ()
  "Reply to the thread at point.
Not yet implemented."
  (interactive)
  (user-error "Reply: not yet implemented (Phase 3)"))

(defun mediawiki-discussion-tools-resolve ()
  "Mark the thread at point as resolved.
Not yet implemented."
  (interactive)
  (user-error "Resolve: not yet implemented (Phase 4)"))

;;; Mode Definition

(defun mediawiki-discussion-tools--refresh-table ()
  "Rebuild the `tabulated-list' entries and refresh the display."
  (setq tabulated-list-entries
        (mapcar #'mediawiki-discussion-tools--make-entry
                mediawiki-discussion-tools--threads))
  (tabulated-list-print t))

(define-derived-mode mediawiki-discussion-tools-list-mode tabulated-list-mode
  "Discussion Tools"
  "Major mode for browsing a MediaWiki discussion page.
Displays a sortable table of discussion threads with metadata
extracted from the DiscussionTools API.

\\{mediawiki-discussion-tools-list-mode-map}"
  (setq tabulated-list-format
        [(""   3 t)
         ("Title"  40 t)
         ("Author" 16 t)
         ("#"       5 t :right-align t)
         ("Last"   20 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Last" nil))
  (add-hook 'tabulated-list-revert-hook #'mediawiki-discussion-tools-refresh nil t)
  (tabulated-list-init-header))

(provide 'mediawiki-discussion-tools)

;;; mediawiki-discussion-tools.el ends here
