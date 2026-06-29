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
    (define-key map (kbd "RET") #'mediawiki-discussion-tools-view-thread-at-point)
    (define-key map (kbd "n")   #'mediawiki-discussion-tools-view-next)
    (define-key map (kbd "p")   #'mediawiki-discussion-tools-view-prev)
    (define-key map (kbd "N")   #'mediawiki-discussion-tools-new-thread)
    (define-key map (kbd "r")   #'mediawiki-discussion-tools-reply)
    (define-key map (kbd "d")   #'mediawiki-discussion-tools-resolve)
    map)
  "Keymap for `mediawiki-discussion-tools-list-mode'.")

(defvar mediawiki-discussion-tools-view-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "q")   #'mediawiki-discussion-tools-view-close)
    (define-key map (kbd "g")   #'mediawiki-discussion-tools-view-refresh)
    (define-key map (kbd "RET") #'mediawiki-discussion-tools-view-close)
    (define-key map (kbd "r")   #'mediawiki-discussion-tools-reply)
    (define-key map (kbd "d")   #'mediawiki-discussion-tools-resolve)
    (define-key map (kbd "n")   #'mediawiki-discussion-tools-view-next)
    (define-key map (kbd "p")   #'mediawiki-discussion-tools-view-prev)
    map)
  "Keymap for `mediawiki-discussion-tools-view-mode'.")

(defconst mediawiki-discussion-tools--view-buffer-name "*MW Thread View*"
  "Name of the reusable thread view buffer.")

;;; Internal Variables

(defvar-local mediawiki-discussion-tools--threads nil
  "List of thread alists for the current discussion buffer.")

(defvar-local mediawiki-discussion-tools--sitename nil
  "The site name for the current discussion buffer.")

(defvar-local mediawiki-discussion-tools--page nil
  "The page name for the current discussion buffer.")

(defvar-local mediawiki-discussion-tools--view-thread nil
  "The thread currently displayed in view mode.")

(defvar-local mediawiki-discussion-tools--view-index nil
  "Index of the current thread in the thread list.")

(defvar-local mediawiki-discussion-tools--list-buffer nil
  "The list buffer associated with this view buffer.")

(defvar-local mediawiki-discussion-tools--last-viewed-id nil
  "Thread ID last shown in the view buffer, to avoid redundant updates.")

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
                      (cons 'replies
                            (alist-get 'replies thread))
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

;;; Thread Viewing

(defun mediawiki-discussion-tools--thread-at-point ()
  "Return the thread alist and its index at point in list mode."
  (let ((id (tabulated-list-get-id)))
    (when id
      (cl-loop for thread in mediawiki-discussion-tools--threads
               for i from 0
               when (string= (alist-get 'id thread) id)
                return (cons i thread)))))

(defun mediawiki-discussion-tools--show-thread (delta)
  "Show a thread in the reusable view buffer, split below the list.
DELTA: 0 for current thread, +1 for next, -1 for previous.
Always reads state from the list buffer — works when called from
either the list or view buffer."
  (let* ((list-buf (or mediawiki-discussion-tools--list-buffer
                       (current-buffer)))
         (threads (buffer-local-value 'mediawiki-discussion-tools--threads list-buf))
         (sitename (buffer-local-value 'mediawiki-discussion-tools--sitename list-buf))
         (page (buffer-local-value 'mediawiki-discussion-tools--page list-buf))
         (index (or (buffer-local-value 'mediawiki-discussion-tools--view-index list-buf)
                    0))
         (new-index (+ index delta))
         (max (1- (length threads))))
    (when (or (< new-index 0) (> new-index max))
      (user-error (if (< new-index 0) "First thread" "Last thread")))
    (let ((thread (nth new-index threads)))
      ;; Re-fetch with full reply tree if we only have the heading
      (when (not (alist-get 'replies thread))
        (let* ((fresh (mediawiki-discussion-tools--fetch-threads
                       sitename page t)))
          (setq threads fresh
                thread (nth new-index fresh))
          (with-current-buffer list-buf
            (setq mediawiki-discussion-tools--threads fresh))))
      (with-current-buffer list-buf
        (setq mediawiki-discussion-tools--view-index new-index
              mediawiki-discussion-tools--last-viewed-id
                (alist-get 'id thread)))
      (with-current-buffer (get-buffer-create
                            mediawiki-discussion-tools--view-buffer-name)
        (mediawiki-discussion-tools-view-mode)
        (setq mediawiki-discussion-tools--list-buffer list-buf)
        (mediawiki-discussion-tools--render-thread thread)
        (goto-char (point-min)))
      (display-buffer mediawiki-discussion-tools--view-buffer-name
                      '((display-buffer-reuse-window
                         display-buffer-below-selected)
                        (window-height . 0.4)))
      ;; Move point in the list buffer to the current thread row
      (when (buffer-live-p list-buf)
        (with-current-buffer list-buf
          (mediawiki-discussion-tools--move-to-row new-index))))))

(defun mediawiki-discussion-tools--move-to-row (row-index)
  "Move point to ROW-INDEX in the current tabulated-list buffer.
Ensure it is visible.  Row 0 is the first data row.
Skips any leading empty line inserted by `tabulated-list-print-entry`."
  (goto-char (point-min))
  ;; tabulated-list-print-entry may insert a leading newline
  (when (looking-at "^$")
    (forward-line 1))
  (forward-line row-index)
  ;; Move point in the list window and update hl-line overlay directly
  ;; so both the cursor and highlight follow navigation even when the
  ;; list window is not selected.
  (let ((win (get-buffer-window (current-buffer))))
    (when win
      (set-window-point win (point)))
    (when (and (boundp 'hl-line-overlay) (overlayp hl-line-overlay))
      (move-overlay hl-line-overlay
                    (line-beginning-position)
                    (line-end-position))
      (overlay-put hl-line-overlay 'window win))))

(defun mediawiki-discussion-tools--follow-point ()
  "If point moved to a different thread row, update the view buffer.
Intended for `post-command-hook'."
  (when-let* ((pair (mediawiki-discussion-tools--thread-at-point))
              (id (alist-get 'id (cdr pair))))
    (unless (equal id mediawiki-discussion-tools--last-viewed-id)
      (setq mediawiki-discussion-tools--last-viewed-id id)
      (setq mediawiki-discussion-tools--view-index (car pair))
      (ignore-errors (mediawiki-discussion-tools--show-thread 0)))))

(defun mediawiki-discussion-tools-view-thread-at-point ()
  "View the full thread at point in a dedicated buffer, split below."
  (interactive)
  (let ((pair (mediawiki-discussion-tools--thread-at-point)))
    (unless pair
      (user-error "No thread at point"))
    (setq mediawiki-discussion-tools--view-index (car pair))
    (mediawiki-discussion-tools--show-thread 0)))

(defun mediawiki-discussion-tools-view-next ()
  "View the next thread."
  (interactive)
  (mediawiki-discussion-tools--show-thread 1))

(defun mediawiki-discussion-tools-view-prev ()
  "View the previous thread."
  (interactive)
  (mediawiki-discussion-tools--show-thread -1))

(defun mediawiki-discussion-tools-view-close ()
  "Close the thread view window and return to the list."
  (interactive)
  (let ((win (get-buffer-window mediawiki-discussion-tools--view-buffer-name)))
    (when win
      (quit-window nil win))))

(defun mediawiki-discussion-tools--render-thread (thread)
  "Render THREAD content into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (alist-get 'title thread) 'face 'bold) "\n")
    (insert (make-string 72 ?─) "\n\n")
    (let ((replies (alist-get 'replies thread)))
      (when replies
        (mediawiki-discussion-tools--render-replies replies 0)))
    (set-buffer-modified-p nil)))

(defun mediawiki-discussion-tools--render-replies (replies depth)
  "Render REPLIES (a list) at indentation DEPTH into the current buffer."
  (dolist (reply replies)
    (let ((author (alist-get 'author reply))
          (timestamp (alist-get 'timestamp reply))
          (html (alist-get 'html reply))
          (children (alist-get 'replies reply)))
      (insert (make-string (* depth 2) ?\s))
      (when (stringp author)
        (insert (propertize author 'face 'font-lock-function-name-face)))
      (when (and timestamp (> (length timestamp) 0))
        (insert "  " (propertize (mediawiki-discussion-tools--format-timestamp timestamp)
                                  'face 'font-lock-comment-face)))
      (insert "\n")
      (let ((text (mediawiki-discussion-tools--strip-tags (or html ""))))
        (unless (string-empty-p text)
          (dolist (line (split-string text "\n"))
            (insert (make-string (* depth 2) ?\s))
            (insert line "\n"))))
      (insert "\n")
      (when children
        (mediawiki-discussion-tools--render-replies children (1+ depth))))))

(defun mediawiki-discussion-tools--format-timestamp (ts)
  "Format ISO 8601 TS as a human-readable date (UTC)."
  (let ((time (mediawiki-discussion-tools--parse-iso8601 ts)))
    (if time
        (format-time-string "%Y-%m-%d %H:%M" time t)
      ts)))

(defun mediawiki-discussion-tools-view-refresh ()
  "Re-fetch and re-render the current thread."
  (interactive)
  (let ((list-buf (or mediawiki-discussion-tools--list-buffer (current-buffer))))
    (with-current-buffer list-buf
      (let ((site mediawiki-discussion-tools--sitename)
            (page mediawiki-discussion-tools--page))
        (setq mediawiki-discussion-tools--threads
              (mediawiki-discussion-tools--fetch-threads site page t))))
    (mediawiki-discussion-tools--show-thread 0)))

;;; Posting

(defvar mediawiki-discussion-tools-reply-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mediawiki-discussion-tools-reply-submit)
    (define-key map (kbd "C-c C-k") #'mediawiki-discussion-tools-reply-cancel)
    map)
  "Keymap for `mediawiki-discussion-tools-reply-mode'.")

(defvar-local mediawiki-discussion-tools--reply-site nil)
(defvar-local mediawiki-discussion-tools--reply-page nil)
(defvar-local mediawiki-discussion-tools--reply-section nil)
(defvar-local mediawiki-discussion-tools--reply-summary nil)

(define-minor-mode mediawiki-discussion-tools-reply-mode
  "Minor mode for composing a reply.
\\{mediawiki-discussion-tools-reply-mode-map}"
  :lighter " MW-Reply")

(defun mediawiki-discussion-tools--section-numbers (sitename page)
  "Return a list of section numbers for discussion threads on PAGE.
Calls action=parse&prop=tocdata and filters out non-discussion
sections (those with empty index).  The resulting list maps 1:1
with the thread list order."
  (let* ((result (mediawiki-api-call sitename "parse"
                   `(("page" . ,page)
                     ("prop" . "tocdata")
                     ("formatversion" . "2"))))
         (sections (alist-get 'sections (alist-get 'parse result))))
    (cl-loop for s in sections
             for idx = (alist-get 'index s)
             for num = (alist-get 'number s)
             when (and idx (not (string= "" idx)))
             collect (string-to-number num))))

(defun mediawiki-discussion-tools--reply-section-number ()
  "Return the section number for the currently viewed thread."
  (let* ((site mediawiki-discussion-tools--sitename)
         (page mediawiki-discussion-tools--page)
         (index mediawiki-discussion-tools--view-index)
         (nums (mediawiki-discussion-tools--section-numbers site page)))
    (when (and nums (< index (length nums)))
      (nth index nums))))

(defun mediawiki-discussion-tools-reply ()
  "Open a buffer to compose a reply to the thread at point or in view.
Use \\[mediawiki-discussion-tools-reply-submit] to post,
\\[mediawiki-discussion-tools-reply-cancel] to abort."
  (interactive)
  (let* ((list-buf (or mediawiki-discussion-tools--list-buffer
                       (current-buffer)))
         (site (buffer-local-value 'mediawiki-discussion-tools--sitename list-buf))
         (page (buffer-local-value 'mediawiki-discussion-tools--page list-buf))
         (threads (buffer-local-value 'mediawiki-discussion-tools--threads list-buf))
         ;; Use view-index if set, otherwise use thread at point in list
         (index (or (buffer-local-value 'mediawiki-discussion-tools--view-index list-buf)
                    (car (with-current-buffer list-buf
                           (mediawiki-discussion-tools--thread-at-point)))))
         (thread (when (and index threads (< index (length threads)))
                   (nth index threads))))
    (unless thread
      (user-error "No thread selected — move point to a thread first"))
    (unless (and site page)
      (user-error "No site or page configured"))
    (let* ((title (alist-get 'title thread))
           (nums (condition-case err
                     (mediawiki-discussion-tools--section-numbers site page)
                   (error
                    (user-error "Failed to fetch section numbers: %S"
                                (error-message-string err)))))
           (section (when (and nums index (< index (length nums)))
                      (nth index nums))))
      (unless section
        (user-error "Cannot determine section number for thread %d (got %d sections)"
                    index (length nums)))
      (let ((buf (get-buffer-create "*MW Reply*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert "-- Reply to: " title " --\n")
          (insert "-- Press C-c C-c to post, C-c C-k to cancel --\n\n")
          (mediawiki-discussion-tools-reply-mode 1)
          (setq mediawiki-discussion-tools--reply-site site
                mediawiki-discussion-tools--reply-page page
                mediawiki-discussion-tools--reply-section section
                mediawiki-discussion-tools--reply-summary
                  (format "/* %s */ Reply" title)))
        (pop-to-buffer buf)
        (goto-char (point-max))))))

(defun mediawiki-discussion-tools-reply-submit ()
  "Post the reply in the current reply buffer."
  (interactive)
  (let* ((site mediawiki-discussion-tools--reply-site)
         (page mediawiki-discussion-tools--reply-page)
         (section mediawiki-discussion-tools--reply-section)
         (summary mediawiki-discussion-tools--reply-summary)
         (body (save-excursion
                 (goto-char (point-min))
                 (forward-paragraph 2)  ; skip the two header lines
                 (buffer-substring-no-properties (point) (point-max))))
         (reply-text (format ": %s %s\n"
                             (string-trim body)
                             mediawiki-discussion-tools-signature))
         (token (mediawiki-site-get-token site "csrf")))
    (unless token
      (error "No CSRF token — authenticate first"))
    (let ((result (mediawiki-api-call site "edit"
                    `(("title" . ,page)
                      ("section" . ,section)
                      ("appendtext" . ,reply-text)
                      ("summary" . ,summary)
                      ("token" . ,token)
                      ("formatversion" . "2")))))
      (if (alist-get 'edit result)
          (progn
            (message "Reply posted")
            (quit-window)
            (mediawiki-discussion-tools-view-refresh))
        (error "Reply failed: %S" result)))))

(defun mediawiki-discussion-tools-reply-cancel ()
  "Cancel the reply and close the buffer."
  (interactive)
  (quit-window))

(defun mediawiki-discussion-tools-new-thread ()
  "Create a new discussion thread on the current page."
  (interactive)
  (let* ((title (read-string "Thread title: "))
         (body (read-string "Body: "))
         (site mediawiki-discussion-tools--sitename)
         (page mediawiki-discussion-tools--page)
         (text (format "%s %s\n" body mediawiki-discussion-tools-signature))
         (token (mediawiki-site-get-token site "csrf")))
    (unless token
      (error "No CSRF token — authenticate first"))
    (let ((result (mediawiki-api-call site "edit"
                    `(("title" . ,page)
                      ("section" . "new")
                      ("sectiontitle" . ,title)
                      ("text" . ,text)
                      ("summary" . ,(format "/* %s */ new thread" title))
                      ("token" . ,token)
                      ("formatversion" . "2")))))
      (when (alist-get 'edit result)
        (message "Thread created")
        (mediawiki-discussion-tools-refresh)))))

;;; Stub (Phase 4)

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
  (setq tabulated-list-sort-key nil)   ; preserve our priority sort order
  (add-hook 'tabulated-list-revert-hook #'mediawiki-discussion-tools-refresh nil t)
  (tabulated-list-init-header)
  (hl-line-mode 1)
  (add-hook 'post-command-hook #'mediawiki-discussion-tools--follow-point nil t))

(define-derived-mode mediawiki-discussion-tools-view-mode special-mode
  "Thread"
  "Major mode for viewing a single discussion thread.
Displays the full reply tree with indentation.

\\{mediawiki-discussion-tools-view-mode-map}"
  (setq buffer-read-only t))

(provide 'mediawiki-discussion-tools)

;;; mediawiki-discussion-tools.el ends here
