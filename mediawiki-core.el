;;; mediawiki-core.el --- Core data structures and configuration for MediaWiki -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module defines the core data structures and configuration
;; variables for the modernized MediaWiki package.

;;; Code:

(require 'cl-lib)

;;; Configuration Variables

(defgroup mediawiki nil
  "A mode for editing pages on MediaWiki sites."
  :tag "MediaWiki"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/hexmode/mediawiki-el")
  :prefix "mediawiki-")

(defgroup mediawiki-async nil
  "Asynchronous operation settings for MediaWiki."
  :tag "MediaWiki Async"
  :group 'mediawiki
  :prefix "mediawiki-async-")

(defgroup mediawiki-session nil
  "Session management settings for MediaWiki."
  :tag "MediaWiki Session"
  :group 'mediawiki
  :prefix "mediawiki-session-")

(defgroup mediawiki-auth nil
  "Authentication settings for MediaWiki."
  :tag "MediaWiki Auth"
  :group 'mediawiki
  :prefix "mediawiki-auth-")

(defgroup mediawiki-api nil
  "API communication settings for MediaWiki."
  :tag "MediaWiki API"
  :group 'mediawiki
  :prefix "mediawiki-api-")

(defcustom mediawiki-site-default "Wikipedia"
  "The default mediawiki site to point to.
Set here for the default and use `mediawiki-site' to set it
per-session later."
  :type 'string
  :tag "MediaWiki Site Default"
  :group 'mediawiki)

(defcustom mediawiki-debug nil
  "Turn on debugging (non-nil)."
  :type 'boolean
  :tag "MediaWiki Debugging"
  :group 'mediawiki)

(defcustom mediawiki-request-timeout 30
  "Timeout in seconds for HTTP requests."
  :type 'integer
  :tag "Request Timeout"
  :group 'mediawiki)

(defcustom mediawiki-max-retries 3
  "Maximum number of retries for failed requests."
  :type 'integer
  :tag "Maximum Retries"
  :group 'mediawiki)

(defcustom mediawiki-retry-delay 1.0
  "Initial delay in seconds between retries."
  :type 'float
  :tag "Retry Delay"
  :group 'mediawiki)

(defcustom mediawiki-debug-verbose nil
  "Enable verbose debugging output including response bodies."
  :type 'boolean
  :tag "Verbose Debugging"
  :group 'mediawiki)

(defcustom mediawiki-debug-max-body-length 1000
  "Maximum length of response body to log in debug output."
  :type 'integer
  :tag "Debug Max Body Length"
  :group 'mediawiki)

(defcustom mediawiki-large-response-threshold 100000
  "Threshold in bytes for considering a response `large'."
  :type 'integer
  :tag "Large Response Threshold"
  :group 'mediawiki)

;;; Core Data Structures

(cl-defstruct mediawiki-site-config
  "Structure representing a MediaWiki site configuration."
  name                    ; Site display name
  url                     ; Base URL
  api-url                 ; API endpoint URL
  username                ; Username (optional, can use auth-source)
  auth-method             ; Authentication method (basic, oauth, etc.)
  auth-config             ; Method-specific configuration
  capabilities            ; Cached site capabilities
  session-info)           ; Current session information

(cl-defstruct mediawiki-session
  "Structure representing a MediaWiki session."
  site-name               ; Associated site name
  tokens                  ; Hash table of token types and values
  token-expiry            ; Token expiration times
  user-info               ; Current user information
  login-time              ; When login occurred
  last-activity)          ; Last API activity timestamp

(cl-defstruct mediawiki-api-response
  "Structure representing a MediaWiki API response."
  success                 ; Boolean success flag
  data                    ; Parsed response data
  warnings                ; API warnings
  errors                  ; API errors
  raw-response            ; Original response for debugging
  request-info)           ; Original request information

;;; Global Variables

(defvar mediawiki-site nil
  "The current mediawiki site from `mediawiki-site-alist'.
If not set, defaults to `mediawiki-site-default'.")

(defvar mediawiki-site-alist '()
  "Alist of configured MediaWiki sites.
Each entry is (SITE-NAME . MEDIAWIKI-SITE-STRUCT).")

(defvar mediawiki-sessions (make-hash-table :test 'equal)
  "Hash table storing active sessions by site name.")

(defvar mediawiki-debug-buffer "*MediaWiki Debug*"
  "Buffer name for debug output.")

;;; Utility Functions

(defun mediawiki-get-url (sitename)
  "Get the base URL for SITENAME."
  (let ((site (cdr (assoc sitename mediawiki-site-alist))))
    (if site
        (mediawiki-site-config-url site)
      (error "Unknown site: %s" sitename))))

(defun mediawiki-get-site (sitename)
  "Get the site structure for SITENAME."
  (cdr (assoc sitename mediawiki-site-alist)))

(defun mediawiki-add-site (site)
  "Add or update a SITE in the site alist."
  (let ((name (mediawiki-site-config-name site)))
    (setq mediawiki-site-alist
          (cons (cons name site)
                (assoc-delete-all name mediawiki-site-alist)))))

(defun mediawiki-remove-site (site)
  "Add or update a SITE in the site alist."
  (setq mediawiki-site-alist
        (assoc-delete-all site mediawiki-site-alist)))

(defun mediawiki-get-session (sitename)
  "Get the session for SITENAME."
  (gethash sitename mediawiki-sessions))

(defun mediawiki-set-session (sitename session)
  "Set the SESSION for SITENAME."
  (puthash sitename session mediawiki-sessions))

(defun mediawiki-remove-session (sitename)
  "Remove the session for SITENAME."
  (remhash sitename mediawiki-sessions))

;;; Debug Functions

(defun mediawiki-debug-enabled-p ()
  "Check if debugging is enabled."
  mediawiki-debug)

(defun mediawiki-debug-verbose-p ()
  "Check if verbose debugging is enabled."
  (and mediawiki-debug mediawiki-debug-verbose))

(defun mediawiki-debug-log (format-string &rest args)
  "Log a formatted message to debug buffer.
FORMAT-STRING and ARGS are passed to `format'."
  (when mediawiki-debug
    (let ((message (apply 'format format-string args))
          (timestamp (format-time-string "%H:%M:%S")))
      (with-current-buffer (get-buffer-create mediawiki-debug-buffer)
        (goto-char (point-max))
        (insert (format "[%s] %s\n" timestamp message))
        (mediawiki-debug-manage-buffer-size)))))

(defun mediawiki-debug-line (line)
  "Log a LINE to debug buffer."
  (when mediawiki-debug
    (with-current-buffer (get-buffer-create mediawiki-debug-buffer)
      (goto-char (point-max))
      (insert "\n")
      (insert line)
      (mediawiki-debug-manage-buffer-size))))

(defun mediawiki-debug-request (url method data)
  "Log URL request details for debugging with METHOD and DATA."
  (when mediawiki-debug
    (mediawiki-debug-line
     (format "REQUEST: %s %s\nDATA: %s"
             method url (prin1-to-string data)))))

(defun mediawiki-debug-response (response)
  "Log RESPONSE details for debugging."
  (when mediawiki-debug
    (mediawiki-debug-line
     (format "RESPONSE: %s" (prin1-to-string response)))))

;;; Log Filtering and Export Functions

(defcustom mediawiki-debug-max-buffer-size 100000
  "Maximum size of debug buffer in characters.
When buffer exceeds this size, oldest entries are removed."
  :type 'integer
  :tag "Debug Buffer Max Size"
  :group 'mediawiki)

(defun mediawiki-debug-clear ()
  "Clear the debug buffer."
  (interactive)
  (when (get-buffer mediawiki-debug-buffer)
    (with-current-buffer mediawiki-debug-buffer
      (erase-buffer)
      (message "MediaWiki debug buffer cleared"))))

;;;###autoload
(defun mediawiki-debug-view ()
  "View the debug buffer in a window."
  (interactive)
  (let ((buffer (get-buffer mediawiki-debug-buffer)))
    (if buffer
        (progn
          (switch-to-buffer-other-window buffer)
          (goto-char (point-max)))
      (message "No debug buffer exists"))))

(defun mediawiki-debug-filter-by-time (start-time end-time)
  "Filter debug log entries between START-TIME and END-TIME.
Returns filtered content as a string."
  (let ((buffer (get-buffer mediawiki-debug-buffer))
        (filtered-lines '()))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties 
                        (line-beginning-position) 
                        (line-end-position))))
              (when (string-match "^\\[\\([0-9:]+\\)\\]" line)
                (let ((time-str (match-string 1 line)))
                  (when (and (not (string< time-str start-time))
                            (not (string> time-str end-time)))
                    (push line filtered-lines))))
              (forward-line 1))))))
    (mapconcat 'identity (nreverse filtered-lines) "\n")))

(defun mediawiki-debug-filter-by-pattern (pattern)
  "Filter debug log entries matching PATTERN (regex).
Returns filtered content as a string."
  (let ((buffer (get-buffer mediawiki-debug-buffer))
        (filtered-lines '()))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties 
                        (line-beginning-position) 
                        (line-end-position))))
              (when (string-match-p pattern line)
                (push line filtered-lines))
              (forward-line 1))))))
    (mapconcat 'identity (nreverse filtered-lines) "\n")))

(defun mediawiki-debug-filter-by-module (module)
  "Filter debug log entries from specific MODULE.
MODULE can be 'auth', 'session', 'api', 'http', 'page', etc."
  (let ((pattern (cond
                  ((string= module "auth") "\\(login\\|auth\\|credential\\|token\\)")
                  ((string= module "session") "\\(session\\|token\\|logout\\)")
                  ((string= module "api") "\\(API\\|api\\|REQUEST\\|RESPONSE\\)")
                  ((string= module "http") "\\(HTTP\\|REQUEST\\|RESPONSE\\|status\\)")
                  ((string= module "page") "\\(page\\|edit\\|save\\|conflict\\)")
                  ((string= module "error") "\\(error\\|failed\\|Error\\)")
                  (t module))))
    (mediawiki-debug-filter-by-pattern pattern)))

(defun mediawiki-debug-export-to-file (filename &optional filter-func filter-arg)
  "Export debug log to FILENAME.
Optional FILTER-FUNC can be used to filter content.
FILTER-ARG is passed to the filter function."
  (interactive "FExport debug log to file: ")
  (let ((buffer (get-buffer mediawiki-debug-buffer))
        (content ""))
    (if (not buffer)
        (message "No debug buffer exists")
      (setq content 
            (if filter-func
                (funcall filter-func filter-arg)
              (with-current-buffer buffer
                (buffer-string))))
      (if (string-empty-p content)
          (message "No content to export")
        (with-temp-file filename
          (insert "MediaWiki Debug Log Export\n")
          (insert (format "Generated: %s\n" (current-time-string)))
          (insert (format "Emacs Version: %s\n" emacs-version))
          (insert "=====================================\n\n")
          (insert content))
        (message "Debug log exported to %s (%d bytes)" 
                filename (length content))))))

(defun mediawiki-debug-export-filtered (filename module)
  "Export filtered debug log for MODULE to FILENAME."
  (interactive 
   (list (read-file-name "Export debug log to file: ")
         (completing-read "Filter by module: " 
                         '("auth" "session" "api" "http" "page" "error" "all")
                         nil t)))
  (if (string= module "all")
      (mediawiki-debug-export-to-file filename)
    (mediawiki-debug-export-to-file filename 'mediawiki-debug-filter-by-module module)))

(defun mediawiki-debug-export-time-range (filename start-time end-time)
  "Export debug log entries between START-TIME and END-TIME to FILENAME.
Time format should be HH:MM:SS."
  (interactive
   (list (read-file-name "Export debug log to file: ")
         (read-string "Start time (HH:MM:SS): ")
         (read-string "End time (HH:MM:SS): ")))
  (mediawiki-debug-export-to-file filename 'mediawiki-debug-filter-by-time 
                                 (list start-time end-time)))

(defun mediawiki-debug-manage-buffer-size ()
  "Manage debug buffer size, removing old entries if necessary."
  (let ((buffer (get-buffer mediawiki-debug-buffer)))
    (when buffer
      (with-current-buffer buffer
        (when (> (buffer-size) mediawiki-debug-max-buffer-size)
          (let ((excess (- (buffer-size) (round (* 0.8 mediawiki-debug-max-buffer-size)))))
            (goto-char (point-min))
            (forward-char excess)
            (forward-line 1)  ; Move to start of next line
            (delete-region (point-min) (point))
            (goto-char (point-min))
            (insert (format "[%s] Debug buffer trimmed (removed %d chars)\n" 
                           (format-time-string "%H:%M:%S") excess))))))))

(defun mediawiki-debug-log-with-module (module format-string &rest args)
  "Log a message with MODULE prefix to debug buffer.
MODULE should be a short string like 'auth', 'api', 'http', etc."
  (when mediawiki-debug
    (let ((message (apply 'format format-string args))
          (timestamp (format-time-string "%H:%M:%S")))
      (with-current-buffer (get-buffer-create mediawiki-debug-buffer)
        (goto-char (point-max))
        (insert (format "[%s] [%s] %s\n" timestamp (upcase module) message))
        (mediawiki-debug-manage-buffer-size)))))

(defun mediawiki-debug-search (pattern)
  "Search for PATTERN in debug buffer and highlight matches."
  (interactive "sSearch debug log for: ")
  (let ((buffer (get-buffer mediawiki-debug-buffer)))
    (if (not buffer)
        (message "No debug buffer exists")
      (switch-to-buffer-other-window buffer)
      (goto-char (point-min))
      (if (search-forward pattern nil t)
          (progn
            (highlight-regexp pattern)
            (message "Found pattern: %s" pattern))
        (message "Pattern not found: %s" pattern)))))

;;; Async Operation Queue System

(defcustom mediawiki-async-max-concurrent-operations 5
  "Maximum number of concurrent async operations."
  :type 'integer
  :tag "Max Concurrent Operations"
  :group 'mediawiki-async)

(defcustom mediawiki-async-queue-enabled t
  "Enable operation queuing for async operations."
  :type 'boolean
  :tag "Async Queue Enabled"
  :group 'mediawiki-async)

(defcustom mediawiki-async-operation-timeout 300
  "Timeout in seconds for async operations."
  :type 'integer
  :tag "Async Operation Timeout"
  :group 'mediawiki-async)

(defvar mediawiki-async-operations (make-hash-table :test 'equal)
  "Hash table tracking all async operations.")

(defvar mediawiki-async-operation-queue '()
  "Queue of pending async operations.")

(defvar mediawiki-async-active-operations '()
  "List of currently active operation IDs.")

(defvar mediawiki-async-next-id 1
  "Next available async operation ID.")

(cl-defstruct mediawiki-async-operation
  "Structure representing an async operation."
  id                          ; Unique operation ID
  type                        ; Operation type (api-call, page-save, etc.)
  description                 ; Human-readable description
  sitename                    ; Associated site
  function                    ; Function to execute
  args                        ; Arguments for function
  callback                    ; Success callback
  error-callback              ; Error callback
  priority                    ; Operation priority (1-10, higher = more priority)
  created                     ; Creation timestamp
  started                     ; Start timestamp
  timeout                     ; Operation timeout
  progress-id                 ; Associated progress tracking ID
  retryable                   ; Whether operation can be retried
  retry-count                 ; Current retry count
  max-retries)                ; Maximum retry attempts

(defun mediawiki-async-create-operation (type description sitename function args callback error-callback &optional options)
  "Create a new async operation.
TYPE is the operation type (e.g., 'api-call', 'page-save').
DESCRIPTION is a human-readable description.
SITENAME is the associated MediaWiki site.
FUNCTION is the async function to execute.
ARGS are the arguments for the function.
CALLBACK is called on success.
ERROR-CALLBACK is called on error.
OPTIONS is a plist of additional options."
  (let* ((id (format "async-%d" mediawiki-async-next-id))
         (priority (or (plist-get options :priority) 5))
         (timeout (or (plist-get options :timeout) mediawiki-async-operation-timeout))
         (retryable (plist-get options :retryable))
         (max-retries (or (plist-get options :max-retries) 3))
         (operation (make-mediawiki-async-operation
                    :id id
                    :type type
                    :description description
                    :sitename sitename
                    :function function
                    :args args
                    :callback callback
                    :error-callback error-callback
                    :priority priority
                    :created (current-time)
                    :timeout timeout
                    :retryable retryable
                    :retry-count 0
                    :max-retries max-retries)))
    (setq mediawiki-async-next-id (1+ mediawiki-async-next-id))
    (puthash id operation mediawiki-async-operations)
    id))

(defun mediawiki-async-queue-operation (operation-id)
  "Add operation to the queue."
  (let ((operation (gethash operation-id mediawiki-async-operations)))
    (when operation
      (setq mediawiki-async-operation-queue
            (append mediawiki-async-operation-queue (list operation-id)))
      (mediawiki-debug-log-with-module "async" "Queued operation %s: %s" 
                                      operation-id 
                                      (mediawiki-async-operation-description operation))
      (mediawiki-async-process-queue))))

(defun mediawiki-async-process-queue ()
  "Process the operation queue, starting operations up to the concurrent limit."
  (when (and mediawiki-async-queue-enabled
             (< (length mediawiki-async-active-operations) 
                mediawiki-async-max-concurrent-operations)
             mediawiki-async-operation-queue)
    
    ;; Sort queue by priority (higher priority first)
    (setq mediawiki-async-operation-queue
          (sort mediawiki-async-operation-queue
                (lambda (a b)
                  (let ((op-a (gethash a mediawiki-async-operations))
                        (op-b (gethash b mediawiki-async-operations)))
                    (> (mediawiki-async-operation-priority op-a)
                       (mediawiki-async-operation-priority op-b))))))
    
    ;; Start operations up to concurrent limit
    (while (and (< (length mediawiki-async-active-operations) 
                   mediawiki-async-max-concurrent-operations)
                mediawiki-async-operation-queue)
      (let ((operation-id (pop mediawiki-async-operation-queue)))
        (mediawiki-async-start-operation operation-id)))))

(defun mediawiki-async-start-operation (operation-id)
  "Start executing an async operation."
  (let ((operation (gethash operation-id mediawiki-async-operations)))
    (when operation
      (setf (mediawiki-async-operation-started operation) (current-time))
      (push operation-id mediawiki-async-active-operations)
      
      ;; Start progress tracking if enabled (without cancellation to avoid recursion)
      (when mediawiki-progress-feedback-enabled
        (let ((progress-id (mediawiki-progress-start 
                           (mediawiki-async-operation-description operation)
                           nil nil))) ; Don't make it cancellable to avoid recursion
          (setf (mediawiki-async-operation-progress-id operation) progress-id)))
      
      (mediawiki-debug-log-with-module "async" "Starting operation %s: %s" 
                                      operation-id 
                                      (mediawiki-async-operation-description operation))
      
      ;; Set up timeout
      (when (mediawiki-async-operation-timeout operation)
        (run-with-timer (mediawiki-async-operation-timeout operation) 
                        nil
                        #'mediawiki-async-timeout-operation operation-id))
      
      ;; Execute the operation with wrapped callbacks
      (let ((wrapped-callback (mediawiki-async-wrap-callback operation-id 'success))
            (wrapped-error-callback (mediawiki-async-wrap-callback operation-id 'error)))
        (condition-case err
            (apply (mediawiki-async-operation-function operation)
                   (append (mediawiki-async-operation-args operation)
                           (list wrapped-callback wrapped-error-callback)))
          (error 
           (mediawiki-debug-log-with-module "async" "Operation %s failed to start: %s" 
                                           operation-id (error-message-string err))
           (funcall wrapped-error-callback err)))))))

(defun mediawiki-async-wrap-callback (operation-id callback-type)
  "Create a wrapped callback that handles operation completion."
  (lambda (&rest args)
    (let ((operation (gethash operation-id mediawiki-async-operations)))
      (when operation
        ;; Remove from active operations
        (setq mediawiki-async-active-operations
              (remove operation-id mediawiki-async-active-operations))
        
        ;; Clear progress tracking
        (when (mediawiki-async-operation-progress-id operation)
          (mediawiki-progress-finish (mediawiki-async-operation-progress-id operation)))
        
        (cond
         ((eq callback-type 'success)
          (mediawiki-debug-log-with-module "async" "Operation %s completed successfully" operation-id)
          ;; Update statistics
          (let ((completion-time (when (mediawiki-async-operation-started operation)
                                  (float-time (time-subtract (current-time)
                                                           (mediawiki-async-operation-started operation))))))
            (mediawiki-async-update-statistics operation 'completed completion-time))
          (when (mediawiki-async-operation-callback operation)
            (condition-case err
                (apply (mediawiki-async-operation-callback operation) args)
              (error
               (mediawiki-debug-log-with-module "async" "Callback error for %s: %s" 
                                               operation-id (error-message-string err)))))
          (remhash operation-id mediawiki-async-operations))
         
         ((eq callback-type 'error)
          (mediawiki-debug-log-with-module "async" "Operation %s failed: %s" operation-id args)
          ;; Try retry if operation is retryable
          (if (and (mediawiki-async-operation-retryable operation)
                   (< (mediawiki-async-operation-retry-count operation)
                      (mediawiki-async-operation-max-retries operation)))
              (progn
                (setf (mediawiki-async-operation-retry-count operation)
                      (1+ (mediawiki-async-operation-retry-count operation)))
                (mediawiki-debug-log-with-module "async" "Retrying operation %s (attempt %d/%d)" 
                                                operation-id
                                                (mediawiki-async-operation-retry-count operation)
                                                (mediawiki-async-operation-max-retries operation))
                (mediawiki-async-queue-operation operation-id))
            ;; No retry, call error callback and clean up
            (mediawiki-async-update-statistics operation 'failed nil)
            (when (mediawiki-async-operation-error-callback operation)
              (condition-case err
                  (apply (mediawiki-async-operation-error-callback operation) args)
                (error
                 (mediawiki-debug-log-with-module "async" "Error callback error for %s: %s" 
                                                 operation-id (error-message-string err)))))
            (remhash operation-id mediawiki-async-operations))))
        
        ;; Process next operations in queue
        (mediawiki-async-process-queue)))))

(defun mediawiki-async-timeout-operation (operation-id)
  "Handle operation timeout."
  (let ((operation (gethash operation-id mediawiki-async-operations)))
    (when (and operation (member operation-id mediawiki-async-active-operations))
      (mediawiki-debug-log-with-module "async" "Operation %s timed out" operation-id)
      (mediawiki-async-cancel-operation operation-id)
      (when (mediawiki-async-operation-error-callback operation)
        (funcall (mediawiki-async-operation-error-callback operation)
                 (list :code "timeout" :info "Operation timed out"))))))

(defun mediawiki-async-cancel-operation (operation-id)
  "Cancel an async operation."
  (interactive (list (completing-read "Cancel operation: " 
                                     (mediawiki-async-list-operation-ids))))
  (let ((operation (gethash operation-id mediawiki-async-operations)))
    (when operation
      ;; Remove from queue if queued
      (setq mediawiki-async-operation-queue
            (remove operation-id mediawiki-async-operation-queue))
      
      ;; Remove from active operations if active
      (setq mediawiki-async-active-operations
            (remove operation-id mediawiki-async-active-operations))
      
      ;; Clear progress tracking
      (when (mediawiki-async-operation-progress-id operation)
        (mediawiki-progress-cancel (mediawiki-async-operation-progress-id operation)))
      
      (mediawiki-debug-log-with-module "async" "Operation %s cancelled" operation-id)
      (mediawiki-async-update-statistics operation 'cancelled nil)
      (remhash operation-id mediawiki-async-operations)
      
      ;; Process next operations in queue
      (mediawiki-async-process-queue)
      
      (when (called-interactively-p 'interactive)
        (message "Cancelled operation: %s" operation-id))
      
      t)))

(defun mediawiki-async-cancel-all-operations ()
  "Cancel all async operations."
  (interactive)
  (let ((cancelled-count 0))
    (maphash (lambda (id operation)
               (when (mediawiki-async-cancel-operation id)
                 (setq cancelled-count (1+ cancelled-count))))
             mediawiki-async-operations)
    (setq mediawiki-async-operation-queue '())
    (setq mediawiki-async-active-operations '())
    (message "Cancelled %d async operation%s" 
             cancelled-count 
             (if (= cancelled-count 1) "" "s"))))

(defun mediawiki-async-list-operation-ids ()
  "Get list of all operation IDs."
  (let ((ids '()))
    (maphash (lambda (id operation) (push id ids)) mediawiki-async-operations)
    ids))

;;;###autoload
(defun mediawiki-async-list-operations ()
  "List all async operations with status."
  (interactive)
  (let ((operations '())
        (buffer-name "*MediaWiki Async Operations*"))
    
    ;; Collect operation info
    (maphash (lambda (id operation)
               (let* ((status (cond
                              ((member id mediawiki-async-active-operations) "ACTIVE")
                              ((member id mediawiki-async-operation-queue) "QUEUED")
                              (t "UNKNOWN")))
                      (elapsed (if (mediawiki-async-operation-started operation)
                                  (mediawiki-progress-format-time-elapsed 
                                   (mediawiki-async-operation-started operation))
                                "Not started"))
                      (retry-info (if (> (mediawiki-async-operation-retry-count operation) 0)
                                     (format " (retry %d/%d)"
                                            (mediawiki-async-operation-retry-count operation)
                                            (mediawiki-async-operation-max-retries operation))
                                   "")))
                 (push (list id
                            (mediawiki-async-operation-type operation)
                            (mediawiki-async-operation-description operation)
                            (mediawiki-async-operation-sitename operation)
                            status
                            (mediawiki-async-operation-priority operation)
                            elapsed
                            retry-info)
                       operations)))
             mediawiki-async-operations)
    
    ;; Display operations
    (if operations
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (erase-buffer)
            (insert "MediaWiki Async Operations\n")
            (insert "=============================\n\n")
            (insert (format "Active operations: %d\n" (length mediawiki-async-active-operations)))
            (insert (format "Queued operations: %d\n" (length mediawiki-async-operation-queue)))
            (insert (format "Max concurrent: %d\n\n" mediawiki-async-max-concurrent-operations))
            
            ;; Sort by status (active first, then queued)
            (setq operations 
                  (sort operations 
                        (lambda (a b)
                          (let ((status-a (nth 4 a))
                                (status-b (nth 4 b)))
                            (cond
                             ((and (string= status-a "ACTIVE") (not (string= status-b "ACTIVE"))) t)
                             ((and (string= status-b "ACTIVE") (not (string= status-a "ACTIVE"))) nil)
                             ((and (string= status-a "QUEUED") (string= status-b "QUEUED"))
                              (> (nth 5 a) (nth 5 b))) ; Sort queued by priority
                             (t nil))))))
            
            (dolist (op operations)
              (let ((id (nth 0 op))
                    (type (nth 1 op))
                    (description (nth 2 op))
                    (sitename (nth 3 op))
                    (status (nth 4 op))
                    (priority (nth 5 op))
                    (elapsed (nth 6 op))
                    (retry-info (nth 7 op)))
                (insert (format "[%s] %s%s\n" status id retry-info))
                (insert (format "  Type: %s | Site: %s | Priority: %d\n" type sitename priority))
                (insert (format "  Description: %s\n" description))
                (insert (format "  Elapsed: %s\n\n" elapsed))))
            
            (goto-char (point-min)))
          (switch-to-buffer-other-window buffer))
      (message "No async operations active"))))

(defun mediawiki-async-show-queue-status ()
  "Show current queue status in minibuffer."
  (interactive)
  (message "Async operations - Active: %d, Queued: %d, Max concurrent: %d"
           (length mediawiki-async-active-operations)
           (length mediawiki-async-operation-queue)
           mediawiki-async-max-concurrent-operations))

(defun mediawiki-async-set-max-concurrent (max)
  "Set maximum concurrent operations to MAX."
  (interactive "nMax concurrent operations: ")
  (when (> max 0)
    (setq mediawiki-async-max-concurrent-operations max)
    (message "Set max concurrent operations to %d" max)
    ;; Process queue in case we can start more operations now
    (mediawiki-async-process-queue)))

(defun mediawiki-async-pause-queue ()
  "Pause processing of the operation queue."
  (interactive)
  (setq mediawiki-async-queue-enabled nil)
  (message "Async operation queue paused"))

(defun mediawiki-async-resume-queue ()
  "Resume processing of the operation queue."
  (interactive)
  (setq mediawiki-async-queue-enabled t)
  (message "Async operation queue resumed")
  (mediawiki-async-process-queue))

(defun mediawiki-async-clear-completed ()
  "Clear completed operations from tracking."
  (interactive)
  (let ((removed-count 0))
    (maphash (lambda (id operation)
               (unless (or (member id mediawiki-async-active-operations)
                          (member id mediawiki-async-operation-queue))
                 (remhash id mediawiki-async-operations)
                 (setq removed-count (1+ removed-count))))
             mediawiki-async-operations)
    (message "Cleared %d completed operation%s from tracking"
             removed-count
             (if (= removed-count 1) "" "s"))))

(defun mediawiki-async-prioritize-operation (operation-id new-priority)
  "Change the priority of OPERATION-ID to NEW-PRIORITY."
  (interactive 
   (list (completing-read "Prioritize operation: " (mediawiki-async-list-operation-ids))
         (read-number "New priority (1-10, higher = more priority): " 5)))
  (let ((operation (gethash operation-id mediawiki-async-operations)))
    (when operation
      (setf (mediawiki-async-operation-priority operation) new-priority)
      (message "Set priority of %s to %d" operation-id new-priority)
      ;; Re-sort queue if operation is queued
      (when (member operation-id mediawiki-async-operation-queue)
        (mediawiki-async-process-queue)))))

;;; Non-blocking Operation Modes

(defcustom mediawiki-non-blocking-mode nil
  "Enable non-blocking mode for MediaWiki operations.
When enabled, operations are queued and processed asynchronously."
  :type 'boolean
  :tag "Non-blocking Mode"
  :group 'mediawiki)

;;;###autoload
(defun mediawiki-toggle-non-blocking-mode ()
  "Toggle non-blocking mode for MediaWiki operations."
  (interactive)
  (setq mediawiki-non-blocking-mode (not mediawiki-non-blocking-mode))
  (message "MediaWiki non-blocking mode %s" 
           (if mediawiki-non-blocking-mode "enabled" "disabled")))

(defun mediawiki-async-api-call (sitename action params callback &optional error-callback options)
  "Make an async API call using the operation queue system.
This is a high-level wrapper that creates and queues an async operation."
  (let* ((description (format "API call: %s on %s" action sitename))
         (operation-id (mediawiki-async-create-operation
                       'api-call description sitename
                       #'mediawiki-api-call-async
                       (list sitename action params)
                       callback error-callback options)))
    (mediawiki-async-queue-operation operation-id)
    operation-id))

(defun mediawiki-async-page-save (sitename title content summary &optional callback error-callback options)
  "Save a page asynchronously using the operation queue system."
  (let* ((description (format "Save page: %s on %s" title sitename))
         (params (list :title title :text content :summary summary))
         (operation-id (mediawiki-async-create-operation
                       'page-save description sitename
                       #'mediawiki-page-save-async
                       (list sitename params)
                       callback error-callback options)))
    (mediawiki-async-queue-operation operation-id)
    operation-id))

(defun mediawiki-async-page-get (sitename title &optional callback error-callback options)
  "Get a page asynchronously using the operation queue system."
  (let* ((description (format "Get page: %s from %s" title sitename))
         (operation-id (mediawiki-async-create-operation
                       'page-get description sitename
                       #'mediawiki-page-get-async
                       (list sitename title t t) ; get metadata and revisions
                       callback error-callback options)))
    (mediawiki-async-queue-operation operation-id)
    operation-id))

(defun mediawiki-maybe-async-operation (sync-func async-func &rest args)
  "Execute SYNC-FUNC or ASYNC-FUNC based on non-blocking mode.
If non-blocking mode is enabled, uses ASYNC-FUNC with queuing.
Otherwise uses SYNC-FUNC directly."
  (if mediawiki-non-blocking-mode
      (apply async-func args)
    (apply sync-func (butlast args 2)))) ; Remove callback args for sync

(defmacro mediawiki-with-async-fallback (sync-form async-form)
  "Execute SYNC-FORM or ASYNC-FORM based on non-blocking mode."
  `(if mediawiki-non-blocking-mode
       ,async-form
     ,sync-form))

(defun mediawiki-async-batch-operations (operations)
  "Queue multiple operations as a batch with shared progress tracking.
OPERATIONS is a list of operation specs: (type description sitename function args callback error-callback options)."
  (let ((batch-id (format "batch-%d" (length operations)))
        (operation-ids '())
        (progress-id (when mediawiki-progress-feedback-enabled
                      (mediawiki-progress-start 
                       (format "Batch operation (%d items)" (length operations))
                       (length operations)))))
    
    (dolist (op-spec operations)
      (let* ((type (nth 0 op-spec))
             (description (nth 1 op-spec))
             (sitename (nth 2 op-spec))
             (function (nth 3 op-spec))
             (args (nth 4 op-spec))
             (callback (nth 5 op-spec))
             (error-callback (nth 6 op-spec))
             (options (or (nth 7 op-spec) '()))
             (batch-options (plist-put options :batch-id batch-id))
             (operation-id (mediawiki-async-create-operation
                           type description sitename function args
                           callback error-callback batch-options)))
        (push operation-id operation-ids)
        (mediawiki-async-queue-operation operation-id)))
    
    (when progress-id
      (mediawiki-progress-finish progress-id (format "Queued %d operations" (length operations))))
    
    (nreverse operation-ids)))

;;; Enhanced Async Operation Status Tracking

(defvar mediawiki-async-operation-statistics (make-hash-table :test 'equal)
  "Hash table storing operation statistics by type.")

(cl-defstruct mediawiki-async-stats
  "Statistics for async operations."
  total-operations            ; Total operations created
  completed-operations        ; Successfully completed operations
  failed-operations           ; Failed operations
  cancelled-operations        ; Cancelled operations
  average-completion-time     ; Average completion time
  total-execution-time        ; Total execution time across all operations
  last-updated)               ; When stats were last updated

(defun mediawiki-async-update-statistics (operation result-type completion-time)
  "Update statistics for OPERATION with RESULT-TYPE and COMPLETION-TIME."
  (let* ((op-type (mediawiki-async-operation-type operation))
         (stats (gethash op-type mediawiki-async-operation-statistics)))
    
    ;; Initialize stats if not exists
    (unless stats
      (setq stats (make-mediawiki-async-stats
                   :total-operations 0
                   :completed-operations 0
                   :failed-operations 0
                   :cancelled-operations 0
                   :average-completion-time 0.0
                   :total-execution-time 0.0
                   :last-updated (current-time)))
      (puthash op-type stats mediawiki-async-operation-statistics))
    
    ;; Update counters
    (setf (mediawiki-async-stats-total-operations stats)
          (1+ (mediawiki-async-stats-total-operations stats)))
    
    (cond
     ((eq result-type 'completed)
      (setf (mediawiki-async-stats-completed-operations stats)
            (1+ (mediawiki-async-stats-completed-operations stats))))
     ((eq result-type 'failed)
      (setf (mediawiki-async-stats-failed-operations stats)
            (1+ (mediawiki-async-stats-failed-operations stats))))
     ((eq result-type 'cancelled)
      (setf (mediawiki-async-stats-cancelled-operations stats)
            (1+ (mediawiki-async-stats-cancelled-operations stats)))))
    
    ;; Update timing stats if completion time provided
    (when completion-time
      (let ((total-time (+ (mediawiki-async-stats-total-execution-time stats) completion-time))
            (completed-count (mediawiki-async-stats-completed-operations stats)))
        (setf (mediawiki-async-stats-total-execution-time stats) total-time)
        (when (> completed-count 0)
          (setf (mediawiki-async-stats-average-completion-time stats)
                (/ total-time completed-count)))))
    
    (setf (mediawiki-async-stats-last-updated stats) (current-time))))

(defun mediawiki-async-get-operation-status (operation-id)
  "Get detailed status information for OPERATION-ID."
  (let ((operation (gethash operation-id mediawiki-async-operations)))
    (when operation
      (let* ((status (cond
                     ((member operation-id mediawiki-async-active-operations) 'active)
                     ((member operation-id mediawiki-async-operation-queue) 'queued)
                     (t 'unknown)))
             (created (mediawiki-async-operation-created operation))
             (started (mediawiki-async-operation-started operation))
             (current-time (current-time))
             (age (float-time (time-subtract current-time created)))
             (execution-time (when started
                              (float-time (time-subtract current-time started)))))
        
        (list :id operation-id
              :type (mediawiki-async-operation-type operation)
              :description (mediawiki-async-operation-description operation)
              :status status
              :sitename (mediawiki-async-operation-sitename operation)
              :priority (mediawiki-async-operation-priority operation)
              :age age
              :execution-time execution-time
              :retry-count (mediawiki-async-operation-retry-count operation)
              :max-retries (mediawiki-async-operation-max-retries operation)
              :retryable (mediawiki-async-operation-retryable operation)
              :progress-id (mediawiki-async-operation-progress-id operation))))))

;;;###autoload
(defun mediawiki-async-show-statistics ()
  "Display async operation statistics."
  (interactive)
  (let ((buffer-name "*MediaWiki Async Statistics*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert "MediaWiki Async Operation Statistics\n")
      (insert "=====================================\n\n")
      
      (if (= (hash-table-count mediawiki-async-operation-statistics) 0)
          (insert "No operation statistics available.\n")
        
        (insert (format "Current queue status:\n"))
        (insert (format "  Active operations: %d\n" (length mediawiki-async-active-operations)))
        (insert (format "  Queued operations: %d\n" (length mediawiki-async-operation-queue)))
        (insert (format "  Total tracked operations: %d\n\n" (hash-table-count mediawiki-async-operations)))
        
        (maphash (lambda (op-type stats)
                   (let ((total (mediawiki-async-stats-total-operations stats))
                         (completed (mediawiki-async-stats-completed-operations stats))
                         (failed (mediawiki-async-stats-failed-operations stats))
                         (cancelled (mediawiki-async-stats-cancelled-operations stats))
                         (avg-time (mediawiki-async-stats-average-completion-time stats))
                         (last-updated (mediawiki-async-stats-last-updated stats)))
                     
                     (insert (format "Operation Type: %s\n" op-type))
                     (insert (format "  Total: %d | Completed: %d | Failed: %d | Cancelled: %d\n"
                                    total completed failed cancelled))
                     (when (> completed 0)
                       (insert (format "  Success Rate: %.1f%%\n" 
                                      (* 100.0 (/ (float completed) total))))
                       (insert (format "  Average Completion Time: %.2fs\n" avg-time)))
                     (insert (format "  Last Updated: %s\n\n" 
                                    (format-time-string "%Y-%m-%d %H:%M:%S" last-updated)))))
                 mediawiki-async-operation-statistics))
      
      (goto-char (point-min)))
    (switch-to-buffer-other-window buffer-name)))

(defun mediawiki-async-reset-statistics ()
  "Reset all async operation statistics."
  (interactive)
  (when (yes-or-no-p "Reset all async operation statistics? ")
    (clrhash mediawiki-async-operation-statistics)
    (message "Async operation statistics reset")))

(defun mediawiki-async-export-statistics (filename)
  "Export async operation statistics to FILENAME."
  (interactive "FExport statistics to file: ")
  (let ((stats-data '()))
    (maphash (lambda (op-type stats)
               (push (list op-type
                          (mediawiki-async-stats-total-operations stats)
                          (mediawiki-async-stats-completed-operations stats)
                          (mediawiki-async-stats-failed-operations stats)
                          (mediawiki-async-stats-cancelled-operations stats)
                          (mediawiki-async-stats-average-completion-time stats)
                          (mediawiki-async-stats-total-execution-time stats)
                          (format-time-string "%Y-%m-%d %H:%M:%S" 
                                             (mediawiki-async-stats-last-updated stats)))
                     stats-data))
             mediawiki-async-operation-statistics)
    
    (with-temp-file filename
      (insert "MediaWiki Async Operation Statistics Export\n")
      (insert (format "Generated: %s\n" (current-time-string)))
      (insert "==========================================\n\n")
      (insert "Format: Type | Total | Completed | Failed | Cancelled | Avg Time | Total Time | Last Updated\n\n")
      
      (dolist (stat stats-data)
        (insert (format "%s | %d | %d | %d | %d | %.2f | %.2f | %s\n"
                       (nth 0 stat) (nth 1 stat) (nth 2 stat) (nth 3 stat)
                       (nth 4 stat) (nth 5 stat) (nth 6 stat) (nth 7 stat)))))
    
    (message "Statistics exported to %s" filename)))

;;; Progress Feedback System

(defcustom mediawiki-progress-feedback-enabled t
  "Enable progress feedback for long-running operations."
  :type 'boolean
  :tag "Progress Feedback Enabled"
  :group 'mediawiki)

(defcustom mediawiki-progress-update-interval 1.0
  "Interval in seconds between progress updates."
  :type 'float
  :tag "Progress Update Interval"
  :group 'mediawiki)

(defcustom mediawiki-progress-use-modeline t
  "Show progress in the mode line instead of messages."
  :type 'boolean
  :tag "Use Mode Line for Progress"
  :group 'mediawiki)

(defvar mediawiki-progress-operations (make-hash-table :test 'equal)
  "Hash table tracking active progress operations.")

(defvar mediawiki-progress-next-id 1
  "Next available progress operation ID.")

(cl-defstruct mediawiki-progress-operation
  "Structure representing a progress tracking operation."
  id                          ; Unique operation ID
  description                 ; Human-readable description
  total                       ; Total units of work (nil if indeterminate)
  completed                   ; Completed units of work
  start-time                  ; When operation started
  last-update                 ; Last update time
  status                      ; Current status message
  cancellable                 ; Whether operation can be cancelled
  cancel-function             ; Function to call for cancellation
  update-callback             ; Callback for progress updates
  completion-callback)        ; Callback for completion

(defun mediawiki-progress-start (description &optional total cancellable cancel-function)
  "Start tracking progress for an operation.
DESCRIPTION is a human-readable description of the operation.
TOTAL is the total number of units (nil for indeterminate progress).
CANCELLABLE indicates if the operation can be cancelled.
CANCEL-FUNCTION is called to cancel the operation.
Returns a progress operation ID."
  (when mediawiki-progress-feedback-enabled
    (let* ((id (format "progress-%d" mediawiki-progress-next-id))
           (operation (make-mediawiki-progress-operation
                      :id id
                      :description description
                      :total total
                      :completed 0
                      :start-time (current-time)
                      :last-update (current-time)
                      :cancellable cancellable
                      :cancel-function cancel-function)))
      (setq mediawiki-progress-next-id (1+ mediawiki-progress-next-id))
      (puthash id operation mediawiki-progress-operations)
      (mediawiki-progress-update-display id)
      id)))

(defun mediawiki-progress-update (id completed &optional status)
  "Update progress for operation ID.
COMPLETED is the number of completed units.
STATUS is an optional status message."
  (when mediawiki-progress-feedback-enabled
    (let ((operation (gethash id mediawiki-progress-operations)))
      (when operation
        (setf (mediawiki-progress-operation-completed operation) completed)
        (setf (mediawiki-progress-operation-last-update operation) (current-time))
        (when status
          (setf (mediawiki-progress-operation-status operation) status))
        (mediawiki-progress-update-display id)))))

(defun mediawiki-progress-increment (id &optional amount status)
  "Increment progress for operation ID by AMOUNT (default 1).
STATUS is an optional status message."
  (when mediawiki-progress-feedback-enabled
    (let ((operation (gethash id mediawiki-progress-operations)))
      (when operation
        (let ((new-completed (+ (mediawiki-progress-operation-completed operation)
                               (or amount 1))))
          (mediawiki-progress-update id new-completed status))))))

(defun mediawiki-progress-finish (id &optional final-message)
  "Finish tracking progress for operation ID.
FINAL-MESSAGE is an optional completion message."
  (when mediawiki-progress-feedback-enabled
    (let ((operation (gethash id mediawiki-progress-operations)))
      (when operation
        (when (mediawiki-progress-operation-completion-callback operation)
          (funcall (mediawiki-progress-operation-completion-callback operation) operation))
        (remhash id mediawiki-progress-operations)
        (mediawiki-progress-clear-display)
        (when final-message
          (message "%s" final-message))))))

(defun mediawiki-progress-cancel (id)
  "Cancel operation ID if it's cancellable."
  (when mediawiki-progress-feedback-enabled
    (let ((operation (gethash id mediawiki-progress-operations)))
      (when (and operation (mediawiki-progress-operation-cancellable operation))
        (let ((cancel-fn (mediawiki-progress-operation-cancel-function operation)))
          (when cancel-fn
            (funcall cancel-fn))
          (mediawiki-progress-finish id "Operation cancelled")
          t)))))

(defun mediawiki-progress-cancel-all ()
  "Cancel all active cancellable operations."
  (interactive)
  (let ((cancelled-count 0))
    (maphash (lambda (id operation)
               (when (mediawiki-progress-cancel id)
                 (setq cancelled-count (1+ cancelled-count))))
             mediawiki-progress-operations)
    (when (> cancelled-count 0)
      (message "Cancelled %d MediaWiki operation%s"
               cancelled-count
               (if (= cancelled-count 1) "" "s")))))

(defun mediawiki-progress-list-active ()
  "List all active progress operations."
  (interactive)
  (let ((operations '()))
    (maphash (lambda (id operation)
               (push (list id
                          (mediawiki-progress-operation-description operation)
                          (mediawiki-progress-operation-completed operation)
                          (mediawiki-progress-operation-total operation)
                          (mediawiki-progress-operation-cancellable operation))
                     operations))
             mediawiki-progress-operations)
    (if operations
        (let ((buffer (get-buffer-create "*MediaWiki Progress*")))
          (with-current-buffer buffer
            (erase-buffer)
            (insert "Active MediaWiki Operations:\n")
            (insert "==============================\n\n")
            (dolist (op operations)
              (let ((id (nth 0 op))
                    (desc (nth 1 op))
                    (completed (nth 2 op))
                    (total (nth 3 op))
                    (cancellable (nth 4 op)))
                (insert (format "%s: %s\n" id desc))
                (if total
                    (insert (format "  Progress: %d/%d (%.1f%%)\n"
                                   completed total
                                   (* 100.0 (/ (float completed) total))))
                  (insert (format "  Progress: %d units completed\n" completed)))
                (when cancellable
                  (insert "  [Can be cancelled]\n"))
                (insert "\n")))
            (goto-char (point-min)))
          (switch-to-buffer-other-window buffer))
      (message "No active MediaWiki operations"))))

(defun mediawiki-progress-update-display (id)
  "Update progress display for operation ID."
  (let ((operation (gethash id mediawiki-progress-operations)))
    (when operation
      (let* ((desc (mediawiki-progress-operation-description operation))
             (completed (mediawiki-progress-operation-completed operation))
             (total (mediawiki-progress-operation-total operation))
             (status (mediawiki-progress-operation-status operation))
             (progress-text (if total
                               (format "%s: %d/%d (%.1f%%)"
                                      desc completed total
                                      (* 100.0 (/ (float completed) total)))
                             (format "%s: %d units" desc completed))))
        (when status
          (setq progress-text (format "%s - %s" progress-text status)))
        
        (if mediawiki-progress-use-modeline
            (mediawiki-progress-update-modeline progress-text)
          (message "%s" progress-text))))))

(defvar mediawiki-progress-modeline-string ""
  "Current progress string for mode line display.")

(defun mediawiki-progress-update-modeline (text)
  "Update mode line with progress TEXT."
  (setq mediawiki-progress-modeline-string text)
  (force-mode-line-update t))

(defun mediawiki-progress-clear-display ()
  "Clear progress display."
  (when mediawiki-progress-use-modeline
    (setq mediawiki-progress-modeline-string "")
    (force-mode-line-update t)))

(defun mediawiki-progress-format-time-elapsed (start-time)
  "Format elapsed time since START-TIME."
  (let ((elapsed (float-time (time-subtract (current-time) start-time))))
    (cond
     ((< elapsed 60) (format "%.1fs" elapsed))
     ((< elapsed 3600) (format "%dm %.0fs" (/ elapsed 60) (mod elapsed 60)))
     (t (format "%dh %dm" (/ elapsed 3600) (/ (mod elapsed 3600) 60))))))

(defun mediawiki-progress-with-feedback (description total-work work-function)
  "Execute WORK-FUNCTION with progress feedback.
DESCRIPTION describes the operation.
TOTAL-WORK is the expected number of work units.
WORK-FUNCTION should be a lambda that accepts a progress-update function."
  (let* ((progress-id (mediawiki-progress-start description total-work))
         (update-fn (lambda (completed &optional status)
                     (mediawiki-progress-update progress-id completed status))))
    (unwind-protect
        (funcall work-function update-fn)
      (mediawiki-progress-finish progress-id))))

(provide 'mediawiki-core)

;;; mediawiki-core.el ends here
