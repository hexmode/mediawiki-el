;;; mediawiki-discussion-tools.el --- DiscussionTools browser for mediawiki.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mark A. Hershberger

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
(require 'mediawiki-utils)
(require 'tabulated-list)
(require 'cl-lib)
(require 'browse-url)
(require 'shr)
(require 'url-util)

(declare-function mediawiki-prompt-for-site "mediawiki-site")
(declare-function mediawiki-open "mediawiki-page")

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

(defcustom mediawiki-discussion-tools-reply-separator "--- Reply below this line ---"
  "Separator line inserted in the reply buffer.
Everything above and including this line is discarded when posting.
The user's reply text starts on the line after the separator."
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
    (define-key map (kbd "m")   #'mediawiki-discussion-tools-new-thread)
    (define-key map (kbd "C")   #'mediawiki-discussion-tools-new-thread)
    (define-key map (kbd "r")   #'mediawiki-discussion-tools-reply)
    (define-key map (kbd "d")   #'mediawiki-discussion-tools-resolve)
    (define-key map (kbd "s")   #'mediawiki-discussion-tools-search)
    map)
  "Keymap for `mediawiki-discussion-tools-list-mode'.")

(defvar mediawiki-discussion-tools-view-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "q")   #'mediawiki-discussion-tools-view-close)
    (define-key map (kbd "g")   #'mediawiki-discussion-tools-view-refresh)
    (define-key map (kbd "RET") #'mediawiki-discussion-tools-view-close)
    (define-key map (kbd "r")   #'mediawiki-discussion-tools-reply)
    (define-key map (kbd "m")   #'mediawiki-discussion-tools-new-thread)
    (define-key map (kbd "C")   #'mediawiki-discussion-tools-new-thread)
    (define-key map (kbd "d")   #'mediawiki-discussion-tools-resolve)
    (define-key map (kbd "n")   #'mediawiki-discussion-tools-view-next)
    (define-key map (kbd "p")   #'mediawiki-discussion-tools-view-prev)
    (define-key map (kbd "s")   #'mediawiki-discussion-tools-search)
    (define-key map (kbd "&")   #'mediawiki-discussion-tools-browse-comment-at-point)
    (define-key map (kbd "C-c C-o")
      #'mediawiki-discussion-tools-open-link-at-point)
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

(defvar-local mediawiki-discussion-tools--known-ids nil
  "Thread IDs seen at the last successful refresh of this buffer.
Used to notice threads that vanished, which usually means they were
archived.")

(defvar-local mediawiki-discussion-tools--vanished nil
  "Thread IDs that disappeared since the previous refresh of this buffer.")

(defvar mediawiki-discussion-tools--user-cache (make-hash-table :test #'equal)
  "Cache of sitename to logged-in user name, or nil when anonymous.")

(defvar mediawiki-discussion-tools--siteinfo-cache (make-hash-table :test #'equal)
  "Cache of sitename to the wiki's general siteinfo, for URL building.")

;;; Current User

(defun mediawiki-discussion-tools--userinfo (sitename)
  "Ask SITENAME who is logged in, or nil when anonymous."
  (ignore-errors
    (let* ((result (mediawiki-api-call sitename "query"
                     '(("meta" . "userinfo")
                       ("formatversion" . "2"))))
           (info (alist-get 'userinfo (alist-get 'query result))))
      (unless (alist-get 'anon info)
        (alist-get 'name info)))))

(defun mediawiki-discussion-tools--current-user (sitename)
  "Return the logged-in user name for SITENAME, or nil.
Uses the configured site user first, then `action=query&meta=userinfo'
once per session, which covers OAuth sessions with no stored name."
  (let ((name (ignore-errors (mediawiki-site-username sitename))))
    (if (and (stringp name) (not (string-empty-p name)))
        name
      (let ((cached (gethash sitename mediawiki-discussion-tools--user-cache
                             'unknown)))
        (if (not (eq cached 'unknown))
            (unless (eq cached 'anonymous)
              cached)
          (let ((name (mediawiki-discussion-tools--userinfo sitename)))
            (puthash sitename (or name 'anonymous)
                     mediawiki-discussion-tools--user-cache)
            name))))))

;;; Thread Parsing

(defun mediawiki-discussion-tools--parse-threads (json &optional username)
  "Parse the discussiontoolspageinfo JSON response into thread records.
USERNAME is the logged-in user, used to rank threads they took part in.
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
                             thread username))))))

(defun mediawiki-discussion-tools--strip-tags (html)
  "Strip HTML tags from HTML string."
  (replace-regexp-in-string "<[^>]+>" "" html))

(defun mediawiki-discussion-tools--flatten-replies (replies)
  "Return REPLIES and their nested replies as a flat, depth-first list."
  (cl-loop for reply in replies
           append (cons reply
                        (mediawiki-discussion-tools--flatten-replies
                         (alist-get 'replies reply)))))

(defun mediawiki-discussion-tools--thread-contains-username-p (thread username)
  "Return t if USERNAME appears in any reply in THREAD."
  (and username
       (cl-some (lambda (reply)
                  (equal (alist-get 'author reply) username))
                (mediawiki-discussion-tools--flatten-replies
                 (alist-get 'replies thread)))))

(defun mediawiki-discussion-tools--replied-after-username-p (thread username)
  "Return t if someone other than USERNAME posted after USERNAME did."
  (when username
    (let ((seen-user nil)
          (answered nil))
      (dolist (reply (mediawiki-discussion-tools--flatten-replies
                      (alist-get 'replies thread)))
        (let ((author (alist-get 'author reply)))
          (cond
           ((equal author username)
            (setq seen-user t
                  answered nil))
           ((and seen-user (not (equal author username)))
            (setq answered t)))))
      answered)))

(defun mediawiki-discussion-tools--thread-status (thread &optional username)
  "Determine the status of a THREAD record from the API.
USERNAME is the logged-in user; when it is given, threads they took
part in become \\='needs-attention (someone answered them) or
\\='watching (nobody has answered yet).  Otherwise returns one of
\\='active, \\='stale or \\='unanswered.  \\='resolved is applied later
by `mediawiki-discussion-tools--mark-resolved'."
  (let ((comment-count (or (alist-get 'commentCount thread) 0))
        (latest (alist-get 'latestReply thread))
        (mine (mediawiki-discussion-tools--thread-contains-username-p
               thread username)))
    (cond
     ((and mine
           (mediawiki-discussion-tools--replied-after-username-p
            thread username))
      'needs-attention)
     (mine 'watching)
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

;;; Resolved Markers

(defun mediawiki-discussion-tools--thread-anchor (thread)
  "Return the section anchor embedded in THREAD's DiscussionTools ID.
Thread IDs look like \"h-Abuse_filter_on_my_bot-20260616005000\" and
the anchor is the part between \"h-\" and the trailing timestamp."
  (when-let* ((id (alist-get 'id thread)))
    (when (string-match "\\`h-\\(.+\\)-[0-9]+\\'" id)
      (match-string 1 id))))

(defun mediawiki-discussion-tools--resolved-anchors (sitename page)
  "Return the anchors of PAGE sections that contain a resolved marker.
Reads the wikitext once with `action=parse&prop=wikitext|sections' and
matches `{{Resolved' inside each section's byte range.  Returns nil if
the page cannot be fetched, so a failure never blocks the thread list."
  (condition-case nil
      (let* ((result (mediawiki-api-call sitename "parse"
                       `(("page" . ,page)
                         ("prop" . "wikitext|sections")
                         ("formatversion" . "2"))))
             (parse (alist-get 'parse result))
             (wikitext (or (alist-get 'wikitext parse) ""))
             (length (length wikitext)))
        (cl-loop for (section . rest) on (alist-get 'sections parse)
                 for start = (or (alist-get 'byteoffset section) 0)
                 for end = (if rest
                               (or (alist-get 'byteoffset (car rest)) length)
                             length)
                 for anchor = (alist-get 'anchor section)
                 for text = (if (<= start length)
                                (substring wikitext start (min end length))
                              "")
                 when (and anchor
                           (string-match-p
                            "{{[[:space:]]*[Rr]esolved[[:space:]]*[|}]" text))
                 collect anchor))
    (error nil)))

(defun mediawiki-discussion-tools--mark-resolved (threads anchors)
  "Set THREADS whose anchor is in ANCHORS to `resolved' status."
  (dolist (thread threads threads)
    (when (member (mediawiki-discussion-tools--thread-anchor thread) anchors)
      (setf (alist-get 'status thread) 'resolved))))

;;; Priority Sorting

(defconst mediawiki-discussion-tools--priority-tiers
  '((needs-attention . 10)
    (watching . 20)
    (active . 30)
    (unanswered . 40)
    (resolved . 50)
    (stale . 60))
  "Ordered priority tiers for the thread list, lowest number first.
The first two tiers are only produced when the logged-in user name is
known; see `mediawiki-discussion-tools--thread-status'.")

(defun mediawiki-discussion-tools--thread-priority (thread)
  "Return a numeric priority for THREAD (lower = higher priority).
Order follows `mediawiki-discussion-tools--priority-tiers'.  Unknown
statuses sort last."
  (or (alist-get (alist-get 'status thread)
                 mediawiki-discussion-tools--priority-tiers)
      70))

;;; Table Entries

(defun mediawiki-discussion-tools--status-icon (status)
  "Return a character icon for STATUS."
  (pcase status
    ('needs-attention " !")
    ('watching        " *")
    ('unanswered      " ?")
    ('active          " \u00b7")   ; middle dot
    ('stale           " ~")
    ('resolved        " \u2713")   ; checkmark
    (_                "  ")))

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
Reply trees are also fetched when the logged-in user is known, because
the needs-attention and watching tiers need per-comment authors.
Otherwise uses threaditemsflags=activity|noreplies for a lightweight
list.  Returns a list of thread alists sorted by priority."
  (let* ((username (mediawiki-discussion-tools--current-user sitename))
         (flags (if (or full username)
                    "activity|excludesignatures"
                  "activity|noreplies"))
         (result (mediawiki-api-call sitename "discussiontoolspageinfo"
                   `(("page" . ,page)
                     ("prop" . "threaditemshtml")
                     ("threaditemsflags" . ,flags)
                     ("formatversion" . "2"))))
         (threads (mediawiki-discussion-tools--parse-threads result username)))
    (mediawiki-discussion-tools--mark-resolved
     threads (mediawiki-discussion-tools--resolved-anchors sitename page))
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
            mediawiki-discussion-tools--page page
            mediawiki-discussion-tools--known-ids
            (mapcar (lambda (thread) (alist-get 'id thread)) threads)
            mediawiki-discussion-tools--vanished nil)
      (mediawiki-discussion-tools--refresh-table)
      (pop-to-buffer (current-buffer)))))

;;; Interactive Commands

(defun mediawiki-discussion-tools--vanished-ids (old-ids threads)
  "Return the IDs in OLD-IDS that are absent from THREADS."
  (let ((new-ids (mapcar (lambda (thread) (alist-get 'id thread)) threads)))
    (cl-remove-if (lambda (id) (member id new-ids)) old-ids)))

(defun mediawiki-discussion-tools--report-refresh (threads vanished)
  "Message the result of a refresh of THREADS.
VANISHED lists threads that were in the list before and are not now,
which normally means they were archived."
  (message "Discussion refreshed (%d thread%s)%s"
           (length threads)
           (if (= 1 (length threads)) "" "s")
           (if vanished
               (format "; %d disappeared (probably archived — s to search archives)"
                       (length vanished))
             "")))

(defun mediawiki-discussion-tools-refresh ()
  "Re-fetch the thread list and refresh the table display.
Threads that vanished since the last refresh are noted, since that is
what archiving looks like from the client side."
  (interactive)
  (let ((site mediawiki-discussion-tools--sitename)
        (page mediawiki-discussion-tools--page))
    (unless (and site page)
      (user-error "No site or page set for this buffer"))
    (let* ((threads (mediawiki-discussion-tools--fetch-threads site page))
           (vanished (mediawiki-discussion-tools--vanished-ids
                      mediawiki-discussion-tools--known-ids threads)))
      (unless threads
        (error "No threads found on %s" page))
      (setq mediawiki-discussion-tools--threads threads
            mediawiki-discussion-tools--known-ids
            (mapcar (lambda (thread) (alist-get 'id thread)) threads)
            mediawiki-discussion-tools--vanished vanished)
      (mediawiki-discussion-tools--refresh-table)
      (mediawiki-discussion-tools--report-refresh threads vanished))))

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

;;; Page URLs

(defun mediawiki-discussion-tools--siteinfo (sitename)
  "Return the general siteinfo for SITENAME, or nil.
Cached per site: it is only needed to build canonical page URLs."
  (let ((cached (gethash sitename mediawiki-discussion-tools--siteinfo-cache
                         'unknown)))
    (if (not (eq cached 'unknown))
        (unless (eq cached 'none) cached)
      (let ((info (ignore-errors
                    (let* ((result (mediawiki-api-call sitename "query"
                                     '(("meta" . "siteinfo")
                                       ("siprop" . "general")
                                       ("formatversion" . "2"))))
                           (general (alist-get 'general
                                               (alist-get 'query result))))
                      (when general
                        (list :server (alist-get 'server general)
                              :articlepath (alist-get 'articlepath general)))))))
        (puthash sitename (or info 'none)
                 mediawiki-discussion-tools--siteinfo-cache)
        info))))

(defun mediawiki-discussion-tools--page-url (sitename page)
  "Return the canonical URL for PAGE on SITENAME.
Uses the wiki's own article path (for example /wiki/Title) so that the
relative links and protocol-relative image URLs MediaWiki returns
resolve the way they do in a browser.  Falls back to
`mediawiki-make-page-url' when siteinfo is unavailable."
  (let* ((info (mediawiki-discussion-tools--siteinfo sitename))
         (server (plist-get info :server))
         (path (plist-get info :articlepath)))
    (if (and server path)
        (replace-regexp-in-string
         "\\$1"
         (url-hexify-string (string-replace " " "_" page))
         (concat (if (string-prefix-p "//" server) (concat "https:" server)
                   server)
                 path)
         t t)
      (mediawiki-make-page-url sitename page))))

;;; Comment HTML

(defvar mediawiki-discussion-tools-link-map
  (let ((map (copy-keymap shr-map)))
    ;; RET closes the thread view, so a link must not swallow it.
    (define-key map (kbd "RET") nil)
    (define-key map (kbd "C-c C-o")
      #'mediawiki-discussion-tools-open-link-at-point)
    map)
  "Keymap placed on links rendered in the thread view.
It is `shr-map' without RET, plus `C-c C-o' to open a link.")

(defun mediawiki-discussion-tools--wiki-title-from-url (url sitename)
  "Return the page title URL points at on SITENAME, or nil.
Only links on SITENAME's own host are wiki links; an external URL
returns nil so the caller can hand it to `browse-url'."
  (when (and url sitename)
    (let* ((base (mediawiki-site-url sitename))
           (absolute (url-expand-file-name url base))
           (parsed (url-generic-parse-url absolute))
           (file (url-filename parsed))
           (host (downcase (or (url-host parsed) "")))
           (site-host (downcase (or (url-host (url-generic-parse-url base))
                                    ""))))
      (when (and file (string= host site-host))
        (let ((raw (cond
                    ((string-match "/wiki/\\([^?#]+\\)" file)
                     (match-string 1 file))
                    ((string-match "[?&]title=\\([^&#]+\\)" file)
                     (match-string 1 file)))))
          (when (and raw (not (string-empty-p raw)))
            (string-replace "_" " " (url-unhex-string raw))))))))

(defun mediawiki-discussion-tools-open-link-at-point ()
  "Open the link at point.
Links to pages on the current site open in an Emacs buffer through
`mediawiki-open'; anything else goes to `browse-url'."
  (interactive)
  (let ((url (or (get-text-property (point) 'shr-url)
                 (and (> (point) (point-min))
                      (get-text-property (1- (point)) 'shr-url)))))
    (unless url
      (user-error "No link at point"))
    (let* ((list-buf (or mediawiki-discussion-tools--list-buffer
                         (current-buffer)))
           (site (buffer-local-value 'mediawiki-discussion-tools--sitename
                                     list-buf))
           (title (mediawiki-discussion-tools--wiki-title-from-url url site)))
      (if title
          (let ((mediawiki-site site))
            (mediawiki-open title))
        (browse-url url)))))

(defun mediawiki-discussion-tools--retarget-links (start end)
  "Give links between START and END the thread view's own keymap.
`shr' binds RET on links, which would shadow the command that closes
the view."
  (let ((pos start))
    (while (< pos end)
      (let ((next (or (next-single-property-change pos 'shr-url nil end) end)))
        (when (get-text-property pos 'shr-url)
          (put-text-property pos next 'keymap
                             mediawiki-discussion-tools-link-map))
        (setq pos next)))))

(defun mediawiki-discussion-tools--render-html (html depth)
  "Insert comment HTML into the buffer at indentation DEPTH.
With libxml support, `shr' renders it: links stay clickable and
images are displayed.  Without it, the tags are stripped and the text
is shown plain."
  (let ((html (or html ""))
        (indent (make-string (* depth 2) ?\s)))
    (if (or (string-empty-p (string-trim html))
            (not (fboundp 'libxml-parse-html-region)))
        (dolist (line (split-string (mediawiki-discussion-tools--strip-tags
                                     html)
                                    "\n"))
          (if (string-empty-p line)
              (insert "\n")
            (insert indent line "\n")))
      (let* ((list-buf (or mediawiki-discussion-tools--list-buffer
                           (current-buffer)))
             (site (buffer-local-value 'mediawiki-discussion-tools--sitename
                                       list-buf))
             (page (buffer-local-value 'mediawiki-discussion-tools--page
                                       list-buf))
             (base (and site page (mediawiki-discussion-tools--page-url site page)))
             ;; shr binds shr-base to nil inside `shr-insert-document' and
             ;; only sets it from a <base> element, so the base URL has to
             ;; come in through the document.  Without it, MediaWiki's
             ;; protocol-relative image URLs and ./File: links stay
             ;; relative, the image fetch fails and shr's grey placeholder
             ;; is left behind.
             (dom (with-temp-buffer
                    (insert (if base
                                (format "<base href=\"%s\"/>" base)
                              "")
                            "<div>" html "</div>")
                    (libxml-parse-html-region (point-min) (point-max))))
             (start (point))
             (shr-width (max 40 (- (window-width) (* depth 2) 4)))
             ;; Pixel-based filling needs a real frame; in batch or a
             ;; terminal it splits every word onto its own line.
             (shr-use-fonts (and (display-graphic-p) shr-use-fonts)))
        (shr-insert-document dom)
        (when (> depth 0)
          (indent-rigidly start (point) (* depth 2)))
        (mediawiki-discussion-tools--retarget-links start (point))))))

(defun mediawiki-discussion-tools--render-replies (replies depth)
  "Render REPLIES (a list) at indentation DEPTH into the current buffer.
Each comment's text carries its DiscussionTools id in the
`mediawiki-comment-id' property, which is what `&' turns into a link."
  (dolist (reply replies)
    (let ((author (alist-get 'author reply))
          (timestamp (alist-get 'timestamp reply))
          (html (alist-get 'html reply))
          (children (alist-get 'replies reply))
          (id (alist-get 'id reply))
          (start (point)))
      (insert (make-string (* depth 2) ?\s))
      (when (stringp author)
        (insert (propertize author 'face 'font-lock-function-name-face)))
      (when (and timestamp (> (length timestamp) 0))
        (insert "  " (propertize (mediawiki-discussion-tools--format-timestamp timestamp)
                                  'face 'font-lock-comment-face)))
      (insert "\n")
      (mediawiki-discussion-tools--render-html html depth)
      (insert "\n")
      (when id
        (add-text-properties start (point)
                             (list 'mediawiki-comment-id id)))
      (when children
        (mediawiki-discussion-tools--render-replies children (1+ depth))))))

(defun mediawiki-discussion-tools--comment-id-at-point ()
  "Return the DiscussionTools id of the comment at point, or nil.
Point on the newline that ends a comment still belongs to it."
  (or (get-text-property (point) 'mediawiki-comment-id)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'mediawiki-comment-id))))

(defun mediawiki-discussion-tools-browse-comment-at-point ()
  "Open the comment at point in the web browser.
The DiscussionTools comment id doubles as the anchor MediaWiki gives
that comment, so the browser lands on it rather than the top of the
page."
  (interactive)
  (let* ((list-buf (or mediawiki-discussion-tools--list-buffer
                       (current-buffer)))
         (site (buffer-local-value 'mediawiki-discussion-tools--sitename
                                   list-buf))
         (page (buffer-local-value 'mediawiki-discussion-tools--page list-buf))
         (id (mediawiki-discussion-tools--comment-id-at-point)))
    (unless (and site page)
      (user-error "No site or page set for this buffer"))
    (unless id
      (user-error "No comment at point"))
    (browse-url (concat (mediawiki-make-page-url site page) "#" id))))

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

;;; Searching

(defun mediawiki-discussion-tools--decode-entities (text)
  "Replace the HTML entities MediaWiki search snippets contain."
  (let ((text (or text "")))
    (dolist (pair '(("&quot;" . "\"") ("&#039;" . "'") ("&lt;" . "<")
                    ("&gt;" . ">") ("&nbsp;" . " ") ("&amp;" . "&"))
                  text)
      (setq text (replace-regexp-in-string
                  (regexp-quote (car pair)) (cdr pair) text t t)))))

(defun mediawiki-discussion-tools--search-results (sitename page query)
  "Return search hits for QUERY under PAGE's prefix on SITENAME.
The prefix also matches archive subpages such as
\"Project:Support desk/Archive 24\", so one search covers both the
live page and its archives.  `srnamespace' is set to \"*\" because it
defaults to 0: without it the search would only look in the main
namespace and a Project- or Talk-namespace page would match nothing,
however well its prefix matches."
  (let* ((result (mediawiki-api-call sitename "query"
                   `(("list" . "search")
                     ("srsearch" .
                      ,(string-trim (concat query " prefix:" page)))
                     ("srnamespace" . "*")
                     ("srlimit" . "50")
                     ("formatversion" . "2")))))
    (alist-get 'search (alist-get 'query result))))

(defvar mediawiki-discussion-tools-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'mediawiki-discussion-tools-search-open)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `mediawiki-discussion-tools-search-mode'.")

(define-derived-mode mediawiki-discussion-tools-search-mode special-mode "MW Search"
  "Major mode for discussion page search results.
Press \\[mediawiki-discussion-tools-search-open] to open the page at point."
  (setq buffer-read-only t))

(defvar-local mediawiki-discussion-tools--search-site nil
  "Site name the search results belong to.")

(defun mediawiki-discussion-tools-search-open ()
  "Open the search result at point."
  (interactive)
  (let ((title (get-text-property (point) 'mediawiki-title)))
    (unless title
      (user-error "No search result at point"))
    (let ((mediawiki-site mediawiki-discussion-tools--search-site))
      (mediawiki-open title))))

(defun mediawiki-discussion-tools-search (query)
  "Search the current discussion page and its archives for QUERY.
With an empty QUERY this lists every page under the discussion page's
prefix, which is the quickest way to find the archive pages."
  (interactive
   (list (read-string "Search threads (empty lists subpages): ")))
  (let ((site mediawiki-discussion-tools--sitename)
        (page mediawiki-discussion-tools--page))
    (unless (and site page)
      (user-error "No site or page set for this buffer"))
    (let ((hits (mediawiki-discussion-tools--search-results site page query))
          (buf (get-buffer-create "*MW Discussion Search*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Search %S under %s on %s:\n\n" query page site))
          (if (null hits)
              (insert "No matches.\n")
            (dolist (hit hits)
              (insert (propertize (or (alist-get 'title hit) "")
                                  'face 'bold
                                  'mediawiki-title (alist-get 'title hit))
                      "\n")
              (when-let* ((snippet (mediawiki-discussion-tools--decode-entities
                                    (mediawiki-discussion-tools--strip-tags
                                     (or (alist-get 'snippet hit) "")))))
                (unless (string-empty-p (string-trim snippet))
                  (insert "  " (string-trim snippet) "\n")))
              (insert "\n")))
          (goto-char (point-min)))
        (mediawiki-discussion-tools-search-mode)
        (setq mediawiki-discussion-tools--search-site site))
      (pop-to-buffer buf))))

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
(defvar-local mediawiki-discussion-tools--reply-indent ":"
  "Indentation prefix for the pending reply, matching the comment it answers.")
(defvar-local mediawiki-discussion-tools--reply-summary nil)
(defvar-local mediawiki-discussion-tools--reply-kind 'reply
  "What the compose buffer is for: `reply' or `new-thread'.")
(defvar-local mediawiki-discussion-tools--reply-title nil
  "Thread title being created in a `new-thread' compose buffer.")

(define-minor-mode mediawiki-discussion-tools-reply-mode
  "Minor mode for composing a reply.
\\{mediawiki-discussion-tools-reply-mode-map}"
  :lighter " MW-Reply")

(defun mediawiki-discussion-tools--section-numbers (sitename page)
  "Return an alist mapping section anchors to section indices for PAGE.
Calls action=parse&prop=tocdata and filters out non-discussion
sections (those without an index field).  Each entry is
(ANCHOR . INDEX) where INDEX is the value for action=edit&section=."
  (let* ((result (mediawiki-api-call sitename "parse"
                   `(("page" . ,page)
                     ("prop" . "tocdata")
                     ("formatversion" . "2"))))
         (sections (alist-get 'sections
                              (alist-get 'tocdata
                                         (alist-get 'parse result)))))
    (cl-loop for s in sections
             for idx = (alist-get 'index s)
             for anchor = (alist-get 'anchor s)
             when (and idx (not (string= "" idx)) anchor)
             collect (cons anchor (string-to-number idx)))))

(defun mediawiki-discussion-tools--section-for-thread (thread sitename page)
  "Return the section number for THREAD on PAGE at SITENAME.
Matches the thread's ID to section anchors from the TOC."
  (when-let* ((anchor (mediawiki-discussion-tools--thread-anchor thread)))
    (alist-get anchor (mediawiki-discussion-tools--section-numbers sitename page)
               nil nil #'string=)))

;;; Reply Indentation

(defun mediawiki-discussion-tools--section-wikitext (sitename page section)
  "Return the raw wikitext of SECTION of PAGE on SITENAME."
  (let* ((result (mediawiki-api-call sitename "parse"
                    `(("page" . ,page)
                      ("section" . ,(number-to-string section))
                      ("prop" . "wikitext")
                      ("formatversion" . "2")))))
    (alist-get 'wikitext (alist-get 'parse result))))

(defun mediawiki-discussion-tools--reply-prefix (wikitext)
  "Return the indentation prefix a reply to WIKITEXT should use.
Uses the indentation of the last comment that has one and adds a
colon, so the reply nests under that comment.  Unindented prose
yields a plain \":\"."
  (let ((prefix ""))
    (dolist (line (split-string (or wikitext "") "\n"))
      (when (string-match "\\`\\([:*]+\\)" line)
        (setq prefix (match-string 1 line))))
    (concat prefix ":")))

(defun mediawiki-discussion-tools--reply-indent (sitename page section)
  "Return the indentation prefix for a reply in SECTION of PAGE on SITENAME.
Falls back to \":\" when the section cannot be fetched."
  (condition-case nil
      (mediawiki-discussion-tools--reply-prefix
       (mediawiki-discussion-tools--section-wikitext sitename page section))
    (error ":")))

(defun mediawiki-discussion-tools--indent-reply (body indent)
  "Return BODY as a wiki reply indented with INDENT.
Blank lines become INDENT alone so the reply stays a single block."
  (mapconcat (lambda (line)
               (if (string-empty-p line)
                   indent
                 (concat indent " " line)))
             (split-string (string-trim body) "\n")
             "\n"))

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
           (section (condition-case err
                       (mediawiki-discussion-tools--section-for-thread
                        thread site page)
                     (error
                      (user-error "Failed to fetch section numbers: %S"
                                  (error-message-string err))))))
      (unless section
        (user-error "Cannot determine section number for thread: %s"
                    (or (alist-get 'id thread) "unknown")))
      (let ((buf (get-buffer-create "*MW Reply*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert "Reply to: " title "\n")
          (insert "Press C-c C-c to post, C-c C-k to cancel.\n")
          (insert mediawiki-discussion-tools-reply-separator "\n\n")
          (mediawiki-discussion-tools-reply-mode 1)
          (setq mediawiki-discussion-tools--reply-kind 'reply
                mediawiki-discussion-tools--reply-site site
                mediawiki-discussion-tools--reply-page page
                mediawiki-discussion-tools--reply-section section
                mediawiki-discussion-tools--reply-title nil
                mediawiki-discussion-tools--reply-indent
                  (mediawiki-discussion-tools--reply-indent site page section)
                mediawiki-discussion-tools--reply-summary
                  (format "/* %s */ Reply" title)))
        (pop-to-buffer buf)
        (goto-char (point-max))))))

(defun mediawiki-discussion-tools--compose-body ()
  "Return the user text in the current compose buffer.
That is everything after the separator line, or the whole buffer when
the separator is missing."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward mediawiki-discussion-tools-reply-separator nil t)
        (progn
          (forward-line 1)
          (skip-chars-forward "\n")
          (buffer-substring-no-properties (point) (point-max)))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun mediawiki-discussion-tools--submit-reply ()
  "Post the reply described by the current compose buffer."
  (let* ((site mediawiki-discussion-tools--reply-site)
         (page mediawiki-discussion-tools--reply-page)
         (section mediawiki-discussion-tools--reply-section)
         (summary mediawiki-discussion-tools--reply-summary)
         (body (mediawiki-discussion-tools--compose-body))
         (indent mediawiki-discussion-tools--reply-indent)
         (indented (mediawiki-discussion-tools--indent-reply body indent))
         (reply-text (format "\n%s\n%s %s\n"
                             indented
                             indent
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

(defun mediawiki-discussion-tools--submit-new-thread ()
  "Post the new thread described by the current compose buffer."
  (let* ((site mediawiki-discussion-tools--reply-site)
         (page mediawiki-discussion-tools--reply-page)
         (title mediawiki-discussion-tools--reply-title)
         (summary mediawiki-discussion-tools--reply-summary)
         (body (string-trim (mediawiki-discussion-tools--compose-body)))
         (token (mediawiki-site-get-token site "csrf")))
    (when (string-empty-p body)
      (user-error "Thread body is empty"))
    (unless token
      (error "No CSRF token — authenticate first"))
    (let ((result (mediawiki-api-call site "edit"
                    `(("title" . ,page)
                      ("section" . "new")
                      ("sectiontitle" . ,title)
                      ("text" . ,(format "%s %s\n"
                                         body
                                         mediawiki-discussion-tools-signature))
                      ("summary" . ,summary)
                      ("token" . ,token)
                      ("formatversion" . "2")))))
      (if (alist-get 'edit result)
          (progn
            (message "Thread created")
            (quit-window)
            (mediawiki-discussion-tools--refresh-buffers page))
        (error "New thread failed: %S" result)))))

(defun mediawiki-discussion-tools-reply-submit ()
  "Post the text in the current compose buffer.
Posts a reply or a new thread, depending on the command that opened
the buffer."
  (interactive)
  (if (eq mediawiki-discussion-tools--reply-kind 'new-thread)
      (mediawiki-discussion-tools--submit-new-thread)
    (mediawiki-discussion-tools--submit-reply)))

(defun mediawiki-discussion-tools-reply-cancel ()
  "Cancel the reply and close the buffer."
  (interactive)
  (quit-window))

(defun mediawiki-discussion-tools-new-thread ()
  "Compose a new discussion thread on the current page.
Prompts for the title, then opens a buffer for the body.
Use \\[mediawiki-discussion-tools-reply-submit] to post,
\\[mediawiki-discussion-tools-reply-cancel] to abort."
  (interactive)
  (let ((title (string-trim (read-string "Thread title: ")))
        (site mediawiki-discussion-tools--sitename)
        (page mediawiki-discussion-tools--page))
    (when (string-empty-p title)
      (user-error "Thread title cannot be empty"))
    (unless (and site page)
      (user-error "No site or page configured"))
    (let ((buf (get-buffer-create "*MW New Thread*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert "New thread: " title "\n")
        (insert "Press C-c C-c to post, C-c C-k to cancel.\n")
        (insert mediawiki-discussion-tools-reply-separator "\n\n")
        (mediawiki-discussion-tools-reply-mode 1)
        (setq mediawiki-discussion-tools--reply-kind 'new-thread
              mediawiki-discussion-tools--reply-site site
              mediawiki-discussion-tools--reply-page page
              mediawiki-discussion-tools--reply-section nil
              mediawiki-discussion-tools--reply-title title
              mediawiki-discussion-tools--reply-summary
              (format "/* %s */ new thread" title)))
      (pop-to-buffer buf)
      (goto-char (point-max)))))

;;; Resolving

(defun mediawiki-discussion-tools--refresh-buffers (page)
  "Refresh the thread list for PAGE and the thread view if it is open."
  (when-let* ((list-buf (get-buffer (format "*MW Discussion: %s*" page))))
    (with-current-buffer list-buf
      (mediawiki-discussion-tools-refresh)))
  (when (get-buffer-window mediawiki-discussion-tools--view-buffer-name)
    (with-current-buffer mediawiki-discussion-tools--view-buffer-name
      (mediawiki-discussion-tools-view-refresh))))

(defun mediawiki-discussion-tools-resolve (&optional summary)
  "Mark the thread at point as resolved.
Prompts for an optional SUMMARY, which is placed before the signature
in the resolution comment, so it can point at the reply that answered
the question.  With no SUMMARY, only the marker and signature are
appended."
  (interactive
   (list (read-string "Resolution summary (optional): ")))
  (let* ((list-buf (or mediawiki-discussion-tools--list-buffer
                       (current-buffer)))
         (site (buffer-local-value 'mediawiki-discussion-tools--sitename
                                   list-buf))
         (page (buffer-local-value 'mediawiki-discussion-tools--page list-buf))
         (threads (buffer-local-value 'mediawiki-discussion-tools--threads
                                      list-buf))
         (index (or (buffer-local-value 'mediawiki-discussion-tools--view-index
                                        list-buf)
                    (car (with-current-buffer list-buf
                           (mediawiki-discussion-tools--thread-at-point)))))
         (thread (when (and index threads (< index (length threads)))
                   (nth index threads))))
    (unless thread
      (user-error "No thread selected — move point to a thread first"))
    (unless (and site page)
      (user-error "No site or page configured"))
    (let ((section (mediawiki-discussion-tools--section-for-thread
                    thread site page)))
      (unless section
        (user-error "Cannot determine section number for thread: %s"
                    (or (alist-get 'id thread) "unknown")))
      (let* ((summary (string-trim (or summary "")))
             (marker (if (string-empty-p summary)
                         (format "{{Resolved|%s}}"
                                 mediawiki-discussion-tools-signature)
                       (format "{{Resolved|%s|%s}}"
                               summary mediawiki-discussion-tools-signature)))
             (text (format "\n%s %s\n"
                           (mediawiki-discussion-tools--reply-indent
                            site page section)
                           marker))
             (token (mediawiki-site-get-token site "csrf")))
        (unless token
          (error "No CSRF token — authenticate first"))
        (let ((result (mediawiki-api-call site "edit"
                        `(("title" . ,page)
                          ("section" . ,section)
                          ("appendtext" . ,text)
                          ("summary" .
                           ,(format "/* %s */ Marking as resolved"
                                    (alist-get 'title thread)))
                          ("token" . ,token)
                          ("formatversion" . "2")))))
          (if (alist-get 'edit result)
              (progn
                (message "Thread marked resolved")
                (mediawiki-discussion-tools--refresh-buffers page))
            (error "Resolve failed: %S" result)))))))

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
