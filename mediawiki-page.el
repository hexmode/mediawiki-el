;;; mediawiki-page.el --- Page operations for MediaWiki -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module provides modern page retrieval functionality for MediaWiki.
;; It implements efficient caching and supports retrieving page metadata
;; and revision information.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-api)
(require 'mediawiki-session)

;;; Configuration Variables

(defcustom mediawiki-page-cache-ttl 300
  "Time-to-live in seconds for cached page content.
Set to 0 to disable caching."
  :type 'integer
  :tag "Page Cache TTL"
  :group 'mediawiki)

(defcustom mediawiki-page-cache-size 50
  "Maximum number of pages to keep in cache."
  :type 'integer
  :tag "Page Cache Size"
  :group 'mediawiki)

(defcustom mediawiki-page-retrieve-metadata t
  "Whether to retrieve page metadata by default."
  :type 'boolean
  :tag "Retrieve Page Metadata"
  :group 'mediawiki)

(defcustom mediawiki-page-retrieve-revisions 1
  "Number of revisions to retrieve by default.
Set to 0 to disable revision retrieval."
  :type 'integer
  :tag "Retrieve Revisions"
  :group 'mediawiki)

(defcustom mediawiki-page-save-retry-count 3
  "Number of automatic retries for failed page saves.
Set to 0 to disable automatic retries."
  :type 'integer
  :tag "Save Retry Count"
  :group 'mediawiki)

(defcustom mediawiki-page-save-retry-delay 2
  "Base delay in seconds between save retries.
Actual delay uses exponential backoff based on this value."
  :type 'number
  :tag "Save Retry Delay"
  :group 'mediawiki)

(defcustom mediawiki-page-save-draft-on-failure t
  "Whether to automatically save drafts when page saves fail.
When enabled, failed edits are saved to local draft files to prevent data loss."
  :type 'boolean
  :tag "Save Draft on Failure"
  :group 'mediawiki)

(defcustom mediawiki-page-draft-directory (expand-file-name "~/.emacs.d/mediawiki-drafts/")
  "Directory to store draft files for failed saves.
Drafts are stored with filenames based on site and page title."
  :type 'directory
  :tag "Draft Directory"
  :group 'mediawiki)

;;; Cache Implementation

(defvar mediawiki-page-draft-metadata nil
  "A plist of the draft metadata.
:title - of the page
:sitename - that the page is on
:params - other info")

(defvar mediawiki-page-draft-meta-file nil
  "The filename holding the draft.")

(defvar mediawiki-page-cache (make-hash-table :test 'equal)
  "Cache for page content and metadata.
Keys are (SITENAME . TITLE) cons cells.
Values are page data structures.")

(cl-defstruct mediawiki-page-data
  "Structure representing cached page data."
  title                   ; Page title
  content                 ; Page content
  metadata                ; Page metadata
  revisions               ; List of revisions
  timestamp               ; When the page was retrieved
  cache-key)              ; Cache key for this page

(defun mediawiki-page-cache-key (sitename title)
  "Create a cache key for SITENAME and TITLE."
  (cons sitename (mediawiki-normalize-title title)))

(defun mediawiki-normalize-title (title)
  "Normalize TITLE for consistent caching."
  (replace-regexp-in-string " " "_" title))

(defun mediawiki-page-cache-get (sitename title)
  "Get cached page data for SITENAME and TITLE.
Returns nil if not in cache or if cache entry has expired."
  (let* ((cache-key (mediawiki-page-cache-key sitename title))
         (cached-data (gethash cache-key mediawiki-page-cache)))
    (when cached-data
      ;; Check if cache has expired
      (if (and (> mediawiki-page-cache-ttl 0)
               (> (float-time (time-subtract (current-time)
                                            (mediawiki-page-data-timestamp cached-data)))
                  mediawiki-page-cache-ttl))
          ;; Cache expired, remove it
          (progn
            (remhash cache-key mediawiki-page-cache)
            nil)
        ;; Cache is valid
        cached-data))))

(defun mediawiki-page-cache-put (sitename title page-data)
  "Store PAGE-DATA in cache for SITENAME and TITLE."
  (let ((cache-key (mediawiki-page-cache-key sitename title)))
    ;; Set timestamp and cache key
    (setf (mediawiki-page-data-timestamp page-data) (current-time))
    (setf (mediawiki-page-data-cache-key page-data) cache-key)

    ;; Store in cache
    (puthash cache-key page-data mediawiki-page-cache)

    ;; Prune cache if needed
    (mediawiki-page-cache-prune)))

(defun mediawiki-page-cache-prune ()
  "Prune the page cache if it exceeds `mediawiki-page-cache-size'."
  (when (> (hash-table-count mediawiki-page-cache) mediawiki-page-cache-size)
    (let ((entries '()))
      ;; Collect all entries with timestamps
      (maphash (lambda (key value)
                 (push (cons key (mediawiki-page-data-timestamp value)) entries))
               mediawiki-page-cache)

      ;; Sort by timestamp (oldest first)
      (setq entries (sort entries
                          (lambda (a b)
                            (time-less-p (cdr a) (cdr b)))))

      ;; Remove oldest entries until we're under the limit
      (let ((to-remove (- (hash-table-count mediawiki-page-cache)
                          mediawiki-page-cache-size)))
        (dotimes (_i to-remove)
          (when entries
            (remhash (caar entries) mediawiki-page-cache)
            (setq entries (cdr entries))))))))

(defun mediawiki-page-cache-clear (sitename &optional title)
  "Clear cache entries for SITENAME.
If TITLE is provided, only clear that specific page."
  (if title
      (remhash (mediawiki-page-cache-key sitename title) mediawiki-page-cache)
    ;; Clear all entries for this site
    (let ((keys-to-remove '()))
      (maphash (lambda (key _value)
                 (when (string= (car key) sitename)
                   (push key keys-to-remove)))
               mediawiki-page-cache)
      (dolist (key keys-to-remove)
        (remhash key mediawiki-page-cache)))))

;;; Page Saving Functions

(defun mediawiki-page-save (sitename title content &optional options)
  "Save page CONTENT to SITENAME with TITLE.
OPTIONS is a plist with the following keys:
- :summary - Edit summary (defaults to empty string)
- :minor - Whether this is a minor edit (defaults to nil)
- :bot - Whether to mark as bot edit (defaults to nil)
- :section - Section number to edit (nil for whole page)
- :base-revision - Base revision ID for conflict detection
- :watch - Whether to add page to watchlist (nil, \='watch, \='unwatch, \='preferences)
- :tags - List of change tags to apply to the edit
- :captchaid - CAPTCHA ID if responding to a CAPTCHA challenge
- :captchaword - CAPTCHA response if responding to a CAPTCHA challenge
- :maxlag - Maximum lag in seconds (for busy wikis)
- :callback - Function to call with result (for async)
- :error-callback - Function to call on error (for async)
- :retry - Number of automatic retries on failure (defaults to mediawiki-page-save-retry-count)

Returns a plist with edit result information or signals an error."
  (let ((summary (or (plist-get options :summary) ""))
        (minor (plist-get options :minor))
        (bot (plist-get options :bot))
        (section (plist-get options :section))
        (base-revision (plist-get options :base-revision))
        (watch (plist-get options :watch))
        (tags (plist-get options :tags))
        (captchaid (plist-get options :captchaid))
        (captchaword (plist-get options :captchaword))
        (maxlag (plist-get options :maxlag))
        (callback (plist-get options :callback))
        (error-callback (plist-get options :error-callback))
        (retry-count (or (plist-get options :retry) mediawiki-page-save-retry-count)))

    ;; Clear cache for this page
    (mediawiki-page-cache-clear sitename title)

    ;; Build parameters for edit
    (let ((params (mediawiki-page-build-edit-params
                  title content summary minor bot section base-revision watch
                  tags captchaid captchaword maxlag)))

      ;; Save draft before attempting to save to server
      (when mediawiki-page-save-draft-on-failure
        (mediawiki-page-save-draft sitename params))

      (if callback
          ;; Async save
          (mediawiki-page-save-async sitename params callback error-callback retry-count)
        ;; Sync save
        (mediawiki-page-save-sync sitename params retry-count)))))

(defun mediawiki-page-save-sync (sitename params &optional retry-count)
  "Synchronously save page using PARAMS to SITENAME.
Uses proper edit tokens and conflict detection.  RETRY-COUNT is the
number of retries remaining.  Returns a plist with edit result
information or signals an error."
  (let* ((retry-count (or retry-count mediawiki-page-save-retry-count))
         (response (mediawiki-api-call-with-token sitename "edit" params "csrf")))
    (if (mediawiki-api-response-success response)
        (progn
          ;; remove draft if save is successful
          (when mediawiki-page-save-draft-on-failure
            (mediawiki-page-remove-draft sitename (plist-get params :title)))
          (mediawiki-page-parse-edit-response response))
      ;; handle error with potential retry
      (let* ((errors (mediawiki-api-response-errors response))
             (primary-error (car errors))
             (error-code (plist-get primary-error :code)))

        ;; check if this is a retryable error
        (if (and (> retry-count 0)
                 (member error-code '("ratelimited" "readonly" "timeout" "maxlag")))
            (progn
              ;; calculate delay with exponential backoff
              (let ((delay (* mediawiki-page-save-retry-delay
                              (expt 2 (- mediawiki-page-save-retry-count retry-count)))))
                (message "save failed (%s). retrying in %.1f seconds... (%d retries left)"
                         error-code delay retry-count)
                (sit-for delay)
                ;; retry the save
                (mediawiki-page-save-sync sitename params (1- retry-count))))

          ;; not retryable or out of retries
          (when (and (= retry-count 0) mediawiki-page-save-draft-on-failure)
            (mediawiki-page-save-draft sitename params))

          ;; handle the error normally
          (mediawiki-page-handle-edit-error response sitename params))))))

(defun mediawiki-page-save-async (sitename params callback error-callback &optional retry-count)
  "Asynchronously save page using PARAMS to SITENAME.
Uses proper edit tokens and conflict detection.  Callback is called with
edit result on success.  ERROR-CALLBACK is called with error information
on failure.  RETRY-COUNT is the number of retries remaining."
  (let ((retry-count (or retry-count mediawiki-page-save-retry-count))
        (title (plist-get params :title)))

    ;; save draft before attempting to save to server
    (when mediawiki-page-save-draft-on-failure
      (mediawiki-page-save-draft sitename params))

    ;; get csrf token first
    (condition-case err
        (let ((token (mediawiki-session-get-token sitename "csrf")))
          (if token
              (let ((params-with-token (cons (cons "token" token) params)))
                (mediawiki-api-call-async
                 sitename "edit" params-with-token
                 (lambda (response)
                   (if (mediawiki-api-response-success response)
                       (progn
                         ;; remove draft if save is successful
                         (when mediawiki-page-save-draft-on-failure
                           (mediawiki-page-remove-draft sitename title))
                         (message "draft for %s on %s successfully recovered and saved to wiki" title sitename)
                         (when callback
                           (funcall callback (mediawiki-page-parse-edit-response response))))
                     ;; handle error with potential retry
                     (let* ((errors (mediawiki-api-response-errors response))
                            (primary-error (car errors))
                            (error-code (plist-get primary-error :code)))

                       ;; check if this is a retryable error
                       (if (and (> retry-count 0)
                                (member error-code '("ratelimited" "readonly" "timeout" "maxlag")))
                           (progn
                             ;; calculate delay with exponential backoff
                             (let ((delay (* mediawiki-page-save-retry-delay
                                             (expt 2 (- mediawiki-page-save-retry-count retry-count)))))
                               (message "save failed (%s). retrying in %.1f seconds... (%d retries left)"
                                        error-code delay retry-count)
                               ;; use run-with-timer for async retry after delay
                               (run-with-timer delay nil
                                               #'mediawiki-page-save-async
                                               sitename params callback error-callback (1- retry-count))))

                         ;; not retryable or out of retries
                         ;; handle the error normally
                         (mediawiki-page-handle-edit-error response sitename params)))))))

                 ;; http error callback
                 (lambda (response)
                   (when error-callback
                     (funcall error-callback
                              (mediawiki-page-handle-edit-error response sitename params))))))

            ;; no token available
            (when error-callback
              (funcall error-callback
                       (list :error "token-error"
                             :message "failed to obtain csrf token")))))

      ;; handle any errors in the token retrieval process
      (error
       (when error-callback
         (funcall error-callback
                  (list :error "token-error"
                        :message (format "token error: %s" (error-message-string err)))))))

(defun mediawiki-page-parse-edit-response (response)
  "Parse edit RESPONSE into a structured result.
Returns a plist with edit information."
  (let* ((data (mediawiki-api-response-data response))
         (edit-data (cdr (assq 'edit data))))

    (list :success t
          :result (cdr (assq 'result edit-data))
          :pageid (cdr (assq 'pageid edit-data))
          :title (cdr (assq 'title edit-data))
          :contentmodel (cdr (assq 'contentmodel edit-data))
          :oldrevid (cdr (assq 'oldrevid edit-data))
          :newrevid (cdr (assq 'newrevid edit-data))
          :newtimestamp (cdr (assq 'newtimestamp edit-data))
          :watched (cdr (assq 'watched edit-data))
          :nochange (cdr (assq 'nochange edit-data)))))

(defun mediawiki-page-handle-edit-error (response sitename params)
  "Handle edit error from RESPONSE for SITENAME with PARAMS.
Implements edit conflict detection and handling.  Returns error
information or re-signals error."
  (let* ((errors (mediawiki-api-response-errors response))
         (primary-error (car errors))
         (error-code (plist-get primary-error :code))
         (error-info (plist-get primary-error :info)))

    (cond
     ;; edit conflict handling (requirement 3.3)
     ((string= error-code "editconflict")
      (mediawiki-page-handle-edit-conflict sitename params error-info))

     ;; page was deleted while editing
     ((string= error-code "pagedeleted")
      (mediawiki-page-handle-page-deleted sitename params error-info))

     ;; permission errors
     ((member error-code '("permissiondenied" "protectedpage" "cascadeprotected"))
      (error "Permission denied: %s" error-info))

     ;; rate limiting
     ((string= error-code "ratelimited")
      (error "Rate limited: %s" error-info))

     ;; token errors (should be handled by token refresh)
     ((member error-code '("badtoken" "notoken"))
      (error "Token error: %s" error-info))

     ;; generic error
     (t
      (error "Edit failed: %s (%s)" error-info error-code)))))

;;; edit conflict resolution

(defcustom mediawiki-page-edit-conflict-resolution 'prompt
  "How to handle edit conflicts.
- \='prompt: ask user what to do
- \='mine: use local changes (overwrite)
- \='theirs: use server version (discard local changes)
- \='merge: attempt automatic merge"
  :type '(choice (const :tag "prompt user" prompt)
                 (const :tag "use my changes" mine)
                 (const :tag "use server version" theirs)
                 (const :tag "attempt merge" merge))
  :group 'mediawiki)

(defcustom mediawiki-page-merge-conflict-marker-a "<<<<<<< local version"
  "Marker for the start of local version in conflict markers."
  :type 'string
  :group 'mediawiki)

(defcustom mediawiki-page-merge-conflict-marker-b "======="
  "Marker for the separator between versions in conflict markers."
  :type 'string
  :group 'mediawiki)

(defcustom mediawiki-page-merge-conflict-marker-c ">>>>>>> server version"
  "Marker for the end of server version in conflict markers."
  :type 'string
  :group 'mediawiki)

(defun mediawiki-page-handle-edit-conflict (sitename params error-info)
  "Handle edit conflict for SITENAME with PARAMS.
Implements clear conflict resolution options.  ERROR-INFO contains
information about the conflict."
  (let ((title (cdr (assoc "title" params)))
        (local-content (cdr (assoc "text" params)))
        (base-revision (cdr (assoc "baserevid" params))))

    (cond
     ((eq mediawiki-page-edit-conflict-resolution 'prompt)
      (mediawiki-page-prompt-conflict-resolution sitename title local-content base-revision params error-info))

     ((eq mediawiki-page-edit-conflict-resolution 'mine)
      (mediawiki-page-force-save sitename params))

     ((eq mediawiki-page-edit-conflict-resolution 'theirs)
      (error "Edit conflict: server version kept, local changes discarded"))

     ((eq mediawiki-page-edit-conflict-resolution 'merge)
      (mediawiki-page-attempt-merge sitename title local-content base-revision params))

     (t
      (error "Edit conflict: %s" error-info)))))

(defun mediawiki-page-prompt-conflict-resolution (sitename title local-content base-revision params error-info)
  "Prompt user for edit conflict resolution.
SITENAME, TITLE, LOCAL-CONTENT are the edit details.  BASE-REVISION is
the revision id the edit was based on.  PARAMS contains the original
edit parameters.  ERROR-INFO contains the conflict information."
  (let ((choice (read-char-choice
                (format "edit conflict on %s: %s\n(m)ine, (t)heirs, (d)iff, (3)way merge, (e)dit merged, (c)ancel: "
                        title error-info)
                '(?m ?t ?d ?3 ?e ?c))))
    (cond
     ((eq choice ?m)
      (when (y-or-n-p "Force save your changes (will overwrite server version)? ")
        (mediawiki-page-force-save sitename params)))

     ((eq choice ?t)
      (message "edit conflict resolved: keeping server version")
      (error "Edit cancelled: server version kept"))

     ((eq choice ?d)
      (mediawiki-page-show-conflict-diff sitename title local-content)
      ;; after showing diff, prompt again
      (mediawiki-page-prompt-conflict-resolution sitename title local-content base-revision params error-info))

     ((eq choice ?3)
      ;; attempt three-way merge and show result
      (mediawiki-page-attempt-merge sitename title local-content base-revision params))

     ((eq choice ?e)
      ;; edit merged content in a buffer
      (mediawiki-page-edit-merged-content sitename title local-content base-revision params))

     ((eq choice ?c)
      (error "Edit cancelled by user"))

     (t
      (error "Invalid choice")))))

(defun mediawiki-page-force-save (sitename params)
  "Force save by modifying the summary and removing baserevid parameter.
SITENAME is the wiki site name.  PARAMS are the edit parameters."
  (let ((params-no-base (remove (assoc "baserevid" params) params)))
    ;; find the association for "summary"
    (let ((summary-association (assoc "summary" params-no-base)))
      (when summary-association
        (setcdr summary-association
                (concat "edit conflict, choosing mine: "
                        (cdr summary-association)))))
    (mediawiki-page-save-sync sitename params-no-base)))

(defun mediawiki-page-show-conflict-diff (sitename title local-content)
  "Show diff between local content and server version.
SITENAME, TITLE identify the page, LOCAL-CONTENT is the local version."
  (let ((server-content (mediawiki-page-get-content sitename title '(:force-refresh t))))
    (if server-content
        (let ((diff-buffer (get-buffer-create "*mediawiki edit conflict*")))
          (with-current-buffer diff-buffer
            (erase-buffer)
            (insert "=== local version ===\n")
            (insert local-content)
            (insert "\n\n=== server version ===\n")
            (insert server-content)
            (insert "\n\n=== end ===\n")
            (goto-char (point-min))
            (diff-mode))
          (display-buffer diff-buffer))
      (message "could not retrieve server version for comparison"))))

(defun mediawiki-page-attempt-merge (sitename title local-content base-revision params)
  "Attempt automatic three-way merge of conflicting changes.
SITENAME and TITLE identify the page.  LOCAL-CONTENT is the local
version of the content.  BASE-REVISION is the revision id the edit was
based on.  PARAMS contains the original edit parameters."
  (let ((server-content (mediawiki-page-get-content sitename title '(:force-refresh t)))
        (base-content (when base-revision
                        (mediawiki-page-get-revision-content sitename title base-revision))))

    (if (not server-content)
        (error "Could not retrieve server version for merge")

      (if (not base-content)
          ;; fall back to two-way merge if base revision is not available
          (mediawiki-page-two-way-merge sitename title local-content server-content params)

        ;; perform three-way merge
        (mediawiki-page-three-way-merge sitename title local-content server-content base-content params)))))

(defun mediawiki-page-get-revision-content (sitename title revision-id)
  "Get content of a specific REVISION-ID for TITLE on SITENAME.
Returns the content as a string or nil if not found."
  (condition-case nil
      (let* ((params `(("prop" . "revisions")
                       ("titles" . ,title)
                       ("rvprop" . "content")
                       ("rvslots" . "main")
                       ("revids" . ,(if (numberp revision-id)
                                       (number-to-string revision-id)
                                     revision-id))))
             (response (mediawiki-api-call-sync sitename "query" params)))

        (when (mediawiki-api-response-success response)
          (let* ((data (mediawiki-api-response-data response))
                 (query (cdr (assq 'query data)))
                 (pages (cdr (assq 'pages query))))

            ;; find the page in the response
            (catch 'found
              (dolist (page pages)
                (let* ((revisions (cdr (assq 'revisions page)))
                       (revision (car revisions)))
                  (when revision
                    ;; extract content from revision
                    (let* ((slots (cdr (assq 'slots revision)))
                           (main-slot (cdr (assq 'main slots))))
                      (when main-slot
                        (throw 'found (cdr (assq 'content main-slot)))))))))))
    (error nil))))

(defun mediawiki-page-two-way-merge (sitename title local-content server-content params)
  "Perform a two-way merge between LOCAL-CONTENT and SERVER-CONTENT.
SITENAME and TITLE identify the page.  PARAMS contains the original edit
parameters.

This is used when base revision content is not available."
  (let ((merge-buffer (get-buffer-create "*mediawiki merge*")))
    (with-current-buffer merge-buffer
      (erase-buffer)
      (insert server-content)

      ;; create a temporary file for diff
      (let ((local-file (make-temp-file "mediawiki-local-"))
            (server-file (make-temp-file "mediawiki-server-")))
        (unwind-protect
            (progn
              ;; write content to temp files
              (with-temp-file local-file
                (insert local-content))
              (with-temp-file server-file
                (insert server-content))

              ;; use diff to create an ed script
              (let ((diff-output (with-temp-buffer
                                   (call-process "diff" nil t nil "-e" local-file server-file)
                                   (buffer-string))))

                ;; apply the ed script if non-empty
                (when (and diff-output (not (string-empty-p diff-output)))
                  (let ((ed-buffer (get-buffer-create "*mediawiki ed script*")))
                    (with-current-buffer ed-buffer
                      (erase-buffer)
                      (insert diff-output)
                      (goto-char (point-min)))

                    ;; apply the ed script
                    (with-current-buffer merge-buffer
                      (let ((ed-program (or (executable-find "ed") "ed")))
                        (call-process-region (point-min) (point-max)
                                            ed-program t t nil "-")
                        (goto-char (point-min))))))))

          ;; clean up temp files
          (when (file-exists-p local-file)
            (delete-file local-file))
          (when (file-exists-p server-file)
            (delete-file server-file))))

      ;; show the merged result
      (goto-char (point-min))
      (mediawiki-page-edit-mode)
      (rename-buffer (format "*mediawiki merge: %s*" title)))

    ;; display the merge buffer
    (switch-to-buffer merge-buffer)
    (message "review merged content. use c-c c-c to save or c-c c-k to cancel.")

    ;; set up local variables for saving
    (setq-local mediawiki-page-merge-sitename sitename)
    (setq-local mediawiki-page-merge-title title)
    (setq-local mediawiki-page-merge-params params)))

(defun mediawiki-page-three-way-merge (sitename title local-content server-content base-content params)
  "Perform a three-way merge between versions.
SITENAME and TITLE identify the page.  LOCAL-CONTENT is the local
version.  SERVER-CONTENT is the server version.  BASE-CONTENT is the
common ancestor version.  PARAMS contains the original edit parameters."
  (let ((merge-buffer (get-buffer-create "*mediawiki merge*")))
    (with-current-buffer merge-buffer
      (erase-buffer)

      ;; create temporary files for the three versions
      (let ((base-file (make-temp-file "mediawiki-base-"))
            (local-file (make-temp-file "mediawiki-local-"))
            (server-file (make-temp-file "mediawiki-server-")))
        (unwind-protect
            (progn
              ;; write content to temp files
              (with-temp-file base-file
                (insert base-content))
              (with-temp-file local-file
                (insert local-content))
              (with-temp-file server-file
                (insert server-content))

              ;; use diff3 to create a merged file with conflict markers
              (call-process "diff3" nil t nil "-m"
                           "-l" "local version"
                           "-l" "base version"
                           "-l" "server version"
                           local-file base-file server-file))

          ;; clean up temp files
          (when (file-exists-p base-file)
            (delete-file base-file))
          (when (file-exists-p local-file)
            (delete-file local-file))
          (when (file-exists-p server-file)
            (delete-file server-file))))

      ;; set up the buffer for editing
      (goto-char (point-min))
      (mediawiki-page-edit-mode)
      (smerge-mode 1)
      (rename-buffer (format "*mediawiki merge: %s*" title)))

    ;; display the merge buffer
    (switch-to-buffer merge-buffer)
    (message "review merged content. resolve conflicts with smerge commands. use c-c c-c to save or c-c c-k to cancel.")

    ;; set up local variables for saving
    (setq-local mediawiki-page-merge-sitename sitename)
    (setq-local mediawiki-page-merge-title title)
    (setq-local mediawiki-page-merge-params params)))

(defun mediawiki-page-edit-merged-content (sitename title local-content base-revision params)
  "Edit merged content in a buffer for manual conflict resolution.
SITENAME and TITLE identify the page.  LOCAL-CONTENT is the local
version.  BASE-REVISION is the revision id the edit was based on.
PARAMS contains the original edit parameters."
  (let ((server-content (mediawiki-page-get-content sitename title '(:force-refresh t)))
        (merge-buffer (get-buffer-create "*mediawiki manual merge*")))

    (with-current-buffer merge-buffer
      (erase-buffer)
      (insert mediawiki-page-merge-conflict-marker-a "\n")
      (insert local-content)
      (insert "\n" mediawiki-page-merge-conflict-marker-b "\n")
      (insert server-content)
      (insert "\n" mediawiki-page-merge-conflict-marker-c "\n")

      ;; set up the buffer for editing
      (goto-char (point-min))
      (mediawiki-page-edit-mode)
      (rename-buffer (format "*mediawiki manual merge: %s*" title)))

    ;; display the merge buffer
    (switch-to-buffer merge-buffer)
    (message "edit to resolve conflicts. remove conflict markers when done. use c-c c-c to save or c-c c-k to cancel.")

    ;; set up local variables for saving
    (setq-local mediawiki-page-merge-sitename sitename)
    (setq-local mediawiki-page-merge-title title)
    (setq-local mediawiki-page-merge-params params)))

;;; page retrieval functions

(defun mediawiki-page-get (sitename title &optional options)
  "Get page content and metadata from SITENAME for TITLE.
OPTIONS is a plist with the following keys:
- :force-refresh - if non-nil, bypass cache
- :metadata - if non-nil, retrieve page metadata
- :revisions - number of revisions to retrieve (0 for none)
- :section - section number to retrieve (nil for whole page)
- :redirect - whether to follow redirects (defaults to t)
- :callback - function to call with page data (for async)

Returns a mediawiki-page-data structure or nil if page doesn't exist."
  (let ((force-refresh (plist-get options :force-refresh))
        (get-metadata (if (plist-member options :metadata)
                         (plist-get options :metadata)
                       mediawiki-page-retrieve-metadata))
        (get-revisions (if (plist-member options :revisions)
                          (plist-get options :revisions)
                        mediawiki-page-retrieve-revisions))
        (section (plist-get options :section))
        (follow-redirect (if (plist-member options :redirect)
                            (plist-get options :redirect)
                          t))
        (callback (plist-get options :callback)))

    ;; Check cache first unless force refresh
    (let ((cached-data (unless force-refresh
                         (mediawiki-page-cache-get sitename title))))
      (if cached-data
          ;; Use cached data
          (progn
            (mediawiki-debug-log "Using cached page data for %s on %s"
                                title sitename)
            (if callback
                (funcall callback cached-data)
              cached-data))

        ;; Need to fetch from API
        (if callback
            ;; Async retrieval
            (mediawiki-page-get-async sitename title get-metadata get-revisions
                                     section follow-redirect callback)
          ;; Sync retrieval
          (mediawiki-page-get-sync sitename title get-metadata get-revisions
                                  section follow-redirect))))))

(defun mediawiki-page-get-sync (sitename title get-metadata get-revisions section follow-redirect)
  "Synchronously get page from SITENAME with TITLE.
GET-METADATA, GET-REVISIONS, SECTION, and FOLLOW-REDIRECT control retrieval options."
  (let* ((params (mediawiki-page-build-query-params
                 title get-metadata get-revisions section follow-redirect))
         (response (mediawiki-api-call-sync sitename "query" params)))

    (if (mediawiki-api-response-success response)
        (let ((page-data (mediawiki-page-parse-response response title)))
          ;; Cache the result if we got valid data
          (when (and page-data
                     (mediawiki-page-data-content page-data))
            (mediawiki-page-cache-put sitename title page-data))
          page-data)

      ;; Handle error
      (mediawiki-debug-log "Failed to retrieve page %s: %s"
                          title
                          (mediawiki-api-format-error-summary response))
      nil)))

(defun mediawiki-page-get-async (sitename title get-metadata get-revisions
                                        section follow-redirect callback)
  "Asynchronously get page from SITENAME with TITLE.
GET-METADATA, GET-REVISIONS, SECTION, and FOLLOW-REDIRECT control
retrieval options.  CALLBACK is called with the page data."
  (let ((params (mediawiki-page-build-query-params
                title get-metadata get-revisions section follow-redirect)))

    (mediawiki-api-call-async
     sitename "query" params
     (lambda (response)
       (let ((page-data (mediawiki-page-parse-response response title)))
         ;; Cache the result if we got valid data
         (when (and page-data
                    (mediawiki-page-data-content page-data))
           (mediawiki-page-cache-put sitename title page-data))

         ;; Call the callback with the result
         (funcall callback page-data)))

     ;; Error callback
     (lambda (response)
       (mediawiki-debug-log "Failed to retrieve page %s: %s"
                           title
                           (mediawiki-api-format-error-summary response))
       (funcall callback nil)))))

(defun mediawiki-page-build-edit-params (title content summary minor &optional bot section base-revision watch
                                                tags captchaid captchaword maxlag)
  "Build API query parameters for page editing.
TITLE is the page title to edit.
CONTENT is the content for this edit.
SUMMARY is the summary for this edit.
MINOR indicates this is a minor edit if t.
BOT indicates this is a bot edit if t.
SECTION integer for the section being edited, if any.
BASE-REVISION the base revision we're working against.
WATCH indicates this page should be added to our watchlist if t.
TAGS is a list of change tags to apply to the edit.
CAPTCHAID is the CAPTCHA ID if responding to a CAPTCHA challenge.
CAPTCHAWORD is the CAPTCHA response if responding to a CAPTCHA challenge.
MAXLAG is the maximum lag in seconds (for busy wikis)."
  (let ((params '()))
    (push (cons "title" title) params)
    (push (cons "text" content) params)  ; Use 'text' parameter for edit API
    (push (cons "summary" summary) params)  ; Fix: use summary, not content
    (when minor
      (push (cons "minor" "1") params))  ; Use string values for API
    (when bot
      (push (cons "bot" "1") params))
    (when section
      (push (cons "section" (if (numberp section)
                               (number-to-string section)
                               section)) params))
    (when base-revision
      (push (cons "baserevid" (if (numberp base-revision)
                                 (number-to-string base-revision)
                                 base-revision)) params))
    (when watch
      (cond
       ((eq watch 'watch) (push (cons "watchlist" "watch") params))
       ((eq watch 'unwatch) (push (cons "watchlist" "unwatch") params))
       ((eq watch 'preferences) (push (cons "watchlist" "preferences") params))
       (t (push (cons "watchlist" "watch") params))))  ; Default to watch if t

    ;; Add modern MediaWiki API parameters
    (when tags
      (push (cons "tags" (if (listp tags)
                            (mapconcat 'identity tags "|")
                          tags)) params))
    (when captchaid
      (push (cons "captchaid" captchaid) params))
    (when captchaword
      (push (cons "captchaword" captchaword) params))
    (when maxlag
      (push (cons "maxlag" (if (numberp maxlag)
                              (number-to-string maxlag)
                              maxlag)) params))

    params))

(defun mediawiki-page-build-query-params (title get-metadata get-revisions section follow-redirect)
  "Build api query parameters for page retrieval.
TITLE is the page title to retrieve.  GET-METADATA controls whether to
retrieve page metadata.  GET-REVISIONS is the number of revisions to
retrieve.  SECTION is the section number to retrieve (nil for whole
page).  FOLLOW-REDIRECT controls whether to follow redirects."
  (let ((params '()))
    ;; basic query parameters
    (push (cons "prop" "revisions") params)
    (push (cons "titles" title) params)
    (push (cons "rvslots" "main") params)

    ;; content retrieval
    (push (cons "rvprop" "content") params)

    ;; section retrieval
    (when section
      (push (cons "rvsection" (number-to-string section)) params))

    ;; redirect handling
    (unless follow-redirect
      (push (cons "redirects" "0") params))

    ;; metadata retrieval
    (when get-metadata
      (setcar (cdr (assoc "prop" params))
              (concat (cdr (assoc "prop" params)) "|info"))
      (push (cons "inprop" "url|displaytitle|protection|talkid|watched|watchers|notificationtimestamp|subjectid|associatedpage|url|readable|preload|displaytitle") params))

    ;; revision retrieval
    (when (> get-revisions 0)
      (push (cons "rvlimit" (number-to-string get-revisions)) params)
      (setcar (cdr (assoc "rvprop" params))
              (concat (cdr (assoc "rvprop" params)) "|ids|timestamp|flags|comment|user")))

    ;; return the parameters
    params))

(defun mediawiki-page-parse-response (response title)
  "Parse page data from api RESPONSE for TITLE.
Returns a `mediawiki-page-data' structure or nil if page doesn't exist."
  (let* ((data (mediawiki-api-response-data response))
         (query (cdr (assq 'query data)))
         (pages (cdr (assq 'pages query)))
         (redirects (cdr (assq 'redirects query)))
         (normalized (cdr (assq 'normalized query)))
         (page-data nil)
         (page-title title)
         (page-content nil)
         (page-metadata nil)
         (page-revisions nil))

    ;; Handle title normalization
    (when normalized
      (let ((norm (cl-find-if (lambda (norm)
                               (string= (cdr (assq 'from norm)) title))
                             normalized)))
        (when norm
          (setq page-title (cdr (assq 'to norm))))))

    ;; Handle redirects
    (when redirects
      (let ((redirect (cl-find-if (lambda (redir)
                                   (string= (cdr (assq 'from redir)) page-title))
                                 redirects)))
        (when redirect
          (setq page-title (cdr (assq 'to redirect))))))

    ;; Find the page in the response
    (let ((page nil))
      (dolist (p pages)
        (when (string= (cdr (assq 'title p)) page-title)
          (setq page p)))

      (when page
        ;; Check if page exists
        (if (assq 'missing page)
            (mediawiki-debug-log "Page %s does not exist" title)

          ;; Extract content
          (let* ((revisions (cdr (assq 'revisions page)))
                 (revision (car revisions)))
            (when revision
              ;; Extract content from revision
              (let* ((slots (cdr (assq 'slots revision)))
                     (main-slot (cdr (assq 'main slots))))
                (when main-slot
                  (setq page-content (cdr (assq 'content main-slot))))))

            ;; Extract revisions
            (when revisions
              (setq page-revisions
                    (mapcar (lambda (rev)
                              (let* ((slots (cdr (assq 'slots rev)))
                                     (main-slot (cdr (assq 'main slots)))
                                     (content (when main-slot
                                               (cdr (assq 'content main-slot)))))
                                (list :revid (cdr (assq 'revid rev))
                                      :parentid (cdr (assq 'parentid rev))
                                      :user (cdr (assq 'user rev))
                                      :timestamp (cdr (assq 'timestamp rev))
                                      :comment (cdr (assq 'comment rev))
                                      :content content)))
                            revisions)))

            ;; Extract metadata (remove revisions to avoid duplication)
            (let ((metadata-page (copy-alist page)))
              (setq metadata-page (assq-delete-all 'revisions metadata-page))
              (setq page-metadata metadata-page))

            ;; Create page data structure
            (setq page-data (make-mediawiki-page-data
                             :title page-title
                             :content page-content
                             :metadata page-metadata
                             :revisions page-revisions))))))

    page-data))

;;; Convenience Functions

(defun mediawiki-page-get-content (sitename title &optional options)
  "Get page content from SITENAME for TITLE.
OPTIONS is passed to `mediawiki-page-get'.
Returns the page content as a string or nil if page doesn't exist."
  (let ((page-data (mediawiki-page-get sitename title options)))
    (when page-data
      (mediawiki-page-data-content page-data))))

(defun mediawiki-page-get-metadata (sitename title &optional options)
  "Get page metadata from SITENAME for TITLE.
OPTIONS is passed to `mediawiki-page-get'.
Returns the page metadata or nil if page doesn't exist."
  (let ((page-data (mediawiki-page-get
                   sitename title
                   (plist-put options :metadata t))))
    (when page-data
      (mediawiki-page-data-metadata page-data))))

(defun mediawiki-page-get-revisions (sitename title count &optional options)
  "Get COUNT revisions for page on SITENAME with TITLE.
OPTIONS is passed to `mediawiki-page-get'.  Returns a list of revisions
or nil if page doesn't exist."
  (let ((page-data (mediawiki-page-get
                   sitename title
                   (plist-put options :revisions count))))
    (when page-data
      (mediawiki-page-data-revisions page-data))))

(defun mediawiki-page-exists-p (sitename title)
  "Check if page with TITLE exists on SITENAME."
  (let ((page-data (mediawiki-page-get sitename title '(:metadata t :revisions 0))))
    (and page-data (mediawiki-page-data-content page-data) t)))

(defun mediawiki-page-get-property (sitename title property &optional options)
  "Get specific PROPERTY from page metadata on SITENAME with TITLE.
OPTIONS is passed to `mediawiki-page-get'.
Returns the property value or nil if not found."
  (let ((metadata (mediawiki-page-get-metadata sitename title options)))
    (when metadata
      (cdr (assq property metadata)))))

(defun mediawiki-page-get-url (sitename title)
  "Get the full URL for page with TITLE on SITENAME."
  (mediawiki-page-get-property sitename title 'fullurl '(:metadata t :revisions 0)))

(defun mediawiki-page-get-last-editor (sitename title)
  "Get the username of the last editor of page with TITLE on SITENAME."
  (let ((revisions (mediawiki-page-get-revisions sitename title 1)))
    (when revisions
      (plist-get (car revisions) :user))))

(defun mediawiki-page-get-last-edit-time (sitename title)
  "Get the timestamp of the last edit to page with TITLE on SITENAME."
  (let ((revisions (mediawiki-page-get-revisions sitename title 1)))
    (when revisions
      (plist-get (car revisions) :timestamp))))

(defun mediawiki-page-get-section (sitename title section &optional options)
  "Get specific SECTION from page with TITLE on SITENAME.
OPTIONS is passed to `mediawiki-page-get'.
Returns the section content as a string or nil if not found."
  (let ((options (plist-put options :section section)))
    (mediawiki-page-get-content sitename title options)))

;;; Page Saving Convenience Functions

(defun mediawiki-page-save-section (sitename title section content &optional summary minor)
  "Save CONTENT to SECTION of page TITLE on SITENAME.
SUMMARY is the edit summary, MINOR indicates if this is a minor edit.
Returns edit result information."
  (mediawiki-page-save sitename title content
                      (list :section section
                            :summary (or summary "")
                            :minor minor)))

(defun mediawiki-page-save-minor (sitename title content &optional summary)
  "Save CONTENT to page TITLE on SITENAME as a minor edit.
SUMMARY is the edit summary.
Returns edit result information."
  (mediawiki-page-save sitename title content
                      (list :summary (or summary "")
                            :minor t)))

(defun mediawiki-page-save-with-base (sitename title content base-revision &optional summary)
  "Save CONTENT to page TITLE on SITENAME with BASE-REVISION for conflict detection.
SUMMARY is the edit summary.
Returns edit result information."
  (mediawiki-page-save sitename title content
                      (list :base-revision base-revision
                            :summary (or summary ""))))

(defun mediawiki-page-save-and-watch (sitename title content &optional summary)
  "Save CONTENT to page TITLE on SITENAME and add to watchlist.
SUMMARY is the edit summary.
Returns edit result information."
  (mediawiki-page-save sitename title content
                      (list :summary (or summary "")
                            :watch 'watch)))

(defun mediawiki-page-create (sitename title content &optional summary)
  "Create new page TITLE on SITENAME with CONTENT.
SUMMARY is the edit summary.
Returns edit result information."
  (mediawiki-page-save sitename title content
                      (list :summary (or summary "Created page"))))

(defun mediawiki-page-append (sitename title content &optional summary)
  "Append CONTENT to existing page TITLE on SITENAME.
SUMMARY is the edit summary.
Returns edit result information."
  (let ((existing-content (mediawiki-page-get-content sitename title)))
    (if existing-content
        (mediawiki-page-save sitename title
                            (concat existing-content "\n" content)
                            (list :summary (or summary "Appended content")))
      (error "Page %s does not exist on %s" title sitename))))

(defun mediawiki-page-prepend (sitename title content &optional summary)
  "Prepend CONTENT to existing page TITLE on SITENAME.
SUMMARY is the edit summary.
Returns edit result information."
  (let ((existing-content (mediawiki-page-get-content sitename title)))
    (if existing-content
        (mediawiki-page-save sitename title
                            (concat content "\n" existing-content)
                            (list :summary (or summary "Prepended content")))
      (error "Page %s does not exist on %s" title sitename))))

(defun mediawiki-page-replace (sitename title old-content content &optional summary)
  "Replace TITLE on SITENAME with CONTENT.
SUMMARY is the edit summary.
Returns edit result information."
  (let ((existing-content (mediawiki-page-get-content sitename title)))
    (if (string= existing-content old-content)
        (mediawiki-page-save sitename title content
                             (list :summary (or summary "Replaced content")))
      (error "The content to replace on %s is not what we expect" title))))

;;; Draft Saving and Recovery

(defun mediawiki-page-open-draft (draft-file meta-file)
  "Open a draft file for editing.
DRAFT-FILE is the path to the saved draft content.
META-FILE is the path to the metadata file."
  (let ((draft-buffer (find-file-noselect draft-file)))
    (with-current-buffer draft-buffer
      ;; Load metadata
      (let ((metadata (mediawiki-page-load-draft-metadata meta-file)))
        ;; Set local variables for recovery
        (setq-local mediawiki-page-draft-metadata metadata)
        (setq-local mediawiki-page-draft-meta-file meta-file)

        ;; Set up keymap for recovery
        (use-local-map (copy-keymap (current-local-map)))
        (local-set-key (kbd "C-c C-c") 'mediawiki-page-recover-draft)
        (local-set-key (kbd "C-c C-k") 'mediawiki-page-discard-draft)))

    ;; Switch to the buffer
    (switch-to-buffer draft-buffer)
    (message "Edit draft and use C-c C-c to attempt recovery or C-c C-k to discard.")))

(defun mediawiki-page-load-draft-metadata (meta-file)
  "Load metadata from META-FILE for a saved draft.
Returns a plist with the metadata."
  (let ((metadata '())
        (title nil)
        (sitename nil)
        (params '()))

    (when (file-exists-p meta-file)
      (with-temp-buffer
        (insert-file-contents meta-file)
        (goto-char (point-min))

        ;; parse metadata lines
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (cond
             ;; title
             ((string-match "^title: \\(.*\\)$" line)
              (setq title (match-string 1 line)))

             ;; site
             ((string-match "^site: \\(.*\\)$" line)
              (setq sitename (match-string 1 line)))

             ;; summary
             ((string-match "^summary: \\(.*\\)$" line)
              (push (cons "summary" (match-string 1 line)) params))

             ;; other parameters
             ((string-match "^param-\\([^:]+\\): \\(.*\\)$" line)
              (push (cons (match-string 1 line) (match-string 2 line)) params))))

          (forward-line 1))))

    ;; return as plist
    (list :title title :sitename sitename :params params)))

(defun mediawiki-page-discard-draft ()
  "Discard the current draft without saving to wiki."
  (interactive)
  (when (y-or-n-p "Discard this draft without saving to wiki?")
    (let ((draft-file buffer-file-name)
          (meta-file mediawiki-page-draft-meta-file))

      ;; close buffer
      (set-buffer-modified-p nil)
      (kill-buffer)

      ;; offer to delete draft files
      (when (and draft-file meta-file
                 (file-exists-p draft-file)
                 (file-exists-p meta-file)
                 (y-or-n-p "Delete draft files?"))
        (delete-file draft-file)
        (delete-file meta-file)
        (message "Draft files deleted")))))

(defun mediawiki-page-list-drafts (&optional sitename)
  "List all available drafts, optionally filtered by SITENAME.
Shows a buffer with drafts that can be recovered."
  (interactive)
  (let ((draft-dir mediawiki-page-draft-directory)
        (drafts '()))

    ;; create draft directory if it doesn't exist
    (unless (file-directory-p draft-dir)
      (make-directory draft-dir t))

    ;; collect all drafts
    (if sitename
        ;; filter by sitename
        (let ((site-dir (expand-file-name
                         (concat (replace-regexp-in-string "[^a-za-z0-9_-]" "_" sitename) "/")
                         draft-dir)))
          (when (file-directory-p site-dir)
            (dolist (file (directory-files site-dir t "\\.wiki$"))
              (let ((meta-file (concat (file-name-sans-extension file) ".meta")))
                (when (file-exists-p meta-file)
                  (push (cons file meta-file) drafts))))))

      ;; all sites
      (dolist (site-dir (directory-files draft-dir t))
        (when (and (file-directory-p site-dir)
                   (not (member (file-name-nondirectory site-dir) '("." ".."))))
          (dolist (file (directory-files site-dir t "\\.wiki$"))
            (let ((meta-file (concat (file-name-sans-extension file) ".meta")))
              (when (file-exists-p meta-file)
                (push (cons file meta-file) drafts)))))))

    ;; display drafts
    (if (null drafts)
        (message "No drafts found%s"
                 (if sitename (format " for %s" sitename) ""))

      ;; create buffer to display drafts
      (let ((buffer (get-buffer-create "*mediawiki drafts*")))
        (with-current-buffer buffer
          (erase-buffer)
          (insert "mediawiki saved drafts\n")
          (insert "====================\n\n")

          ;; sort drafts by modification time (newest first)
          (setq drafts
                (sort drafts
                      (lambda (a b)
                        (time-less-p
                         (nth 5 (file-attributes (car b)))
                         (nth 5 (file-attributes (car a)))))))

          ;; List each draft with metadata
          (let ((index 1))
            (dolist (draft drafts)
              (let* ((draft-file (car draft))
                     (meta-file (cdr draft))
                     (metadata (mediawiki-page-load-draft-metadata meta-file))
                     (title (plist-get metadata :title))
                     (site (plist-get metadata :sitename))
                     (mtime (format-time-string "%Y-%m-%d %H:%M:%S"
                                               (nth 5 (file-attributes draft-file)))))

                (insert (format "[%d] %s\n" index title))
                (insert (format "    Site: %s\n" site))
                (insert (format "    Date: %s\n" mtime))
                (insert (format "    File: %s\n\n" draft-file))
                (setq index (1+ index)))))

          ;; Add instructions
          (insert "\nPress a number key to open the corresponding draft, or q to quit.\n")

          ;; Set up keymap for selection
          (use-local-map (make-sparse-keymap))
          (dotimes (i (min 9 (length drafts)))
            (define-key (current-local-map) (kbd (format "%d" (1+ i)))
                        (lambda ()
                          (interactive)
                          (let* ((draft (nth i drafts))
                                 (draft-file (car draft))
                                 (meta-file (cdr draft)))
                            (quit-window)
                            (mediawiki-page-open-draft draft-file meta-file)))))

          (define-key (current-local-map) (kbd "q") 'quit-window)

          ;; set up the buffer
          (setq buffer-read-only t)
          (goto-char (point-min)))

        ;; display the buffer
        (switch-to-buffer buffer)))))

(defun mediawiki-page-handle-page-deleted (sitename params error-info)
  "Handle case where page was deleted while editing.
SITENAME and PARAMS identify the edit attempt.  ERROR-INFO contains
information about the error.

Offers to save as draft or create new page."
  (let ((title (cdr (assoc "title" params)))
        (content (cdr (assoc "text" params))))

    (if (not (y-or-n-p (format "Page %s was deleted.  Save as draft? " title)))
        (error "Edit canceled: %s" error-info)

      ;; save as draft
      (mediawiki-page-save-draft sitename params)

      ;; offer to create new page
      (when (y-or-n-p "Would you like to create the page as new?")
        ;; remove baserevid parameter if present and force save without retry
        (let ((new-params (remove (assoc "baserevid" params) params)))
          ;; use direct api call to avoid recursion through error handler
          (condition-case err
              (let ((response (mediawiki-api-call-with-token sitename "edit" new-params "csrf")))
                (if (mediawiki-api-response-success response)
                    (message "Page created successfully.")
                  (message "Failed to create page: %s"
                          (plist-get (car (mediawiki-api-response-errors response)) :info))))
            (error
             (message "Failed to create page: %s" (error-message-string err)))))))))

(defun mediawiki-page-save-draft (sitename params)
  "Save draft for failed edit to prevent data loss.
SITENAME is the wiki site name.  PARAMS are the edit parameters."
  (when mediawiki-page-save-draft-on-failure
    (let* ((title (cdr (assoc "title" params)))
           (content (cdr (assoc "text" params)))
           (summary (cdr (assoc "summary" params)))
           (site-dir (expand-file-name
                      (concat (replace-regexp-in-string "[^a-za-z0-9_-]" "_" sitename) "/")
                      mediawiki-page-draft-directory))
           (timestamp (format-time-string "%y%m%d-%h%m%s"))
           (safe-title (replace-regexp-in-string "[^a-za-z0-9_-]" "_" title))
           (draft-file (expand-file-name (format "%s-%s.wiki" safe-title timestamp) site-dir))
           (meta-file (concat (file-name-sans-extension draft-file) ".meta")))

      ;; create directory if it doesn't exist
      (unless (file-directory-p site-dir)
        (make-directory site-dir t))

      ;; save draft content
      (with-temp-file draft-file
        (insert content))

      ;; save metadata
      (with-temp-file meta-file
        (insert (format "title: %s\n" title))
        (insert (format "site: %s\n" sitename))
        (insert (format "timestamp: %s\n" (current-time-string)))
        (when summary
          (insert (format "summary: %s\n" summary)))

        ;; save other parameters
        (dolist (param params)
          (unless (member (car param) '("title" "text" "summary"))
            (insert (format "param-%s: %s\n" (car param) (cdr param))))))

      (message "Draft saved to %s." draft-file)
      draft-file)))

(defun mediawiki-page-remove-draft (sitename title)
  "Remove the draft for SITENAME and TITLE."
  (let* ((safe-title (replace-regexp-in-string "[^a-za-z0-9_-]" "_" title))
         (site-dir (expand-file-name
                    (concat (replace-regexp-in-string "[^a-za-z0-9_-]" "_" sitename) "/")
                    mediawiki-page-draft-directory))
         (draft-file (expand-file-name (format "%s.wiki" safe-title) site-dir))
         (meta-file (concat (file-name-sans-extension draft-file) ".meta")))

    (when (and (file-exists-p draft-file)
               (file-exists-p meta-file))
      (delete-file draft-file)
      (delete-file meta-file)
      (message "Draft for %s on %s removed." title sitename))))

(defun mediawiki-page-recover-draft (draft-file meta-file)
  "Attempt to recover and submit a draft edit.
DRAFT-FILE is the path to the saved draft content.  META-FILE is the
path to the metadata file."
  (let ((metadata (mediawiki-page-load-draft-metadata meta-file))
        (content (with-temp-buffer
                   (insert-file-contents draft-file)
                   (buffer-string))))

    (if (not metadata)
        (message "No draft metadata available for recovery.")

      (let ((title (plist-get metadata :title))
            (sitename (plist-get metadata :sitename))
            (params (plist-get metadata :params)))

        (if (or (not title) (not sitename))
            (message "Incomplete draft metadata, cannot recover.")

          ;; confirm recovery
          (when (y-or-n-p (format "Attempt to save draft to %s on %s?" title sitename))
            ;; add content to params
            (setq params (remove (assoc "text" params) params))
            (push (cons "text" content) params)

            ;; update summary to indicate recovery
            (let ((summary-assoc (assoc "summary" params)))
              (when summary-assoc
                (setcdr summary-assoc
                        (concat "[recovered draft] " (cdr summary-assoc)))))

            ;; attempt to save
            (message "Attempting to recover draft...")
            (condition-case err
                (progn
                  (mediawiki-page-save-sync sitename params)
                  (message "Draft successfully recovered and saved to wiki.")

                  ;; mark buffer as saved and offer to close
                  (when (get-buffer (file-name-nondirectory draft-file))
                    (with-current-buffer (file-name-nondirectory draft-file)
                      (set-buffer-modified-p nil)
                      (when (y-or-n-p "Draft recovered successfully.  Close buffer?")
                        (kill-buffer)
                        ;; offer to delete draft files
                        (when (and (file-exists-p draft-file)
                                   (file-exists-p meta-file)
                                   (y-or-n-p "Delete draft files?"))
                          (delete-file draft-file)
                          (delete-file meta-file)
                          (message "Draft files deleted")))))

              (error
               (message "Recovery failed: %s" (error-message-string err))
               (when (y-or-n-p "Would you like to edit the draft and try again? ")
                 (message "Edit draft and use C-c C-c to retry recovery")))))))))))

;;; page edit mode

(defvar mediawiki-page-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c-c c-c") 'mediawiki-page-merge-save)
    (define-key map (kbd "c-c c-k") 'mediawiki-page-merge-cancel)
    map)
  "Keymap for mediawiki page edit mode.")

(define-derived-mode mediawiki-page-edit-mode text-mode "mw-edit"
  "Major mode for editing mediawiki page content during conflict resolution."
  :group 'mediawiki
  (setq-local font-lock-defaults '(wiki-font-lock-keywords t))
  (when (fboundp 'wiki-mode)
    (set (make-local-variable 'font-lock-defaults)
         '(wiki-font-lock-keywords t))))

(defun mediawiki-page-merge-save ()
  "Save the merged content from the merge buffer."
  (interactive)
  (let ((sitename (buffer-local-value 'mediawiki-page-merge-sitename (current-buffer)))
        (title (buffer-local-value 'mediawiki-page-merge-title (current-buffer)))
        (params (buffer-local-value 'mediawiki-page-merge-params (current-buffer)))
        (content (buffer-string)))

    (when (and sitename title params)
      ;; Update the content parameter
      (let ((text-param (assoc "text" params)))
        (if text-param
            (setcdr text-param content)
          (push (cons "text" content) params)))

      ;; Remove baserevid to avoid conflict
      (setq params (remove (assoc "baserevid" params) params))

      ;; Update summary to indicate merge
      (let ((summary-param (assoc "summary" params)))
        (if summary-param
            (setcdr summary-param (concat "Merged edit conflict: " (cdr summary-param)))
          (push (cons "summary" "Merged edit conflict") params)))

      ;; Save the merged content
      (condition-case err
          (progn
            (mediawiki-page-save-sync sitename params)
            (message "Merged content saved successfully")
            (kill-buffer (current-buffer)))
        (error
         (message "Failed to save merged content: %s" (error-message-string err)))))))

(defun mediawiki-page-merge-cancel ()
  "Cancel the merge operation."
  (interactive)
  (when (y-or-n-p "Cancel merge? Unsaved changes will be lost.")
    (kill-buffer (current-buffer))
    (message "Merge cancelled")))

(defun mediawiki-page-remove-conflict-markers (content)
  "Remove conflict markers from CONTENT.
This is a utility function for cleaning up merged content."
  (let ((lines (split-string content "\n"))
        (result '())
        (in-conflict nil))

    (dolist (line lines)
      (cond
       ;; Start of conflict
       ((string-match-p "^<<<<<<<" line)
        (setq in-conflict 'local))

       ;; Separator
       ((string-match-p "^=======" line)
        (setq in-conflict 'server))

       ;; End of conflict
       ((string-match-p "^>>>>>>>" line)
        (setq in-conflict nil))

       ;; Regular line - only include if not in conflict section
       ((not in-conflict)
        (push line result))))

    (mapconcat 'identity (nreverse result) "\n")))

(provide 'mediawiki-page)

;;; mediawiki-page.el ends here
