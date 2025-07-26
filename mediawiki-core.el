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
  :group 'applications)

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

(provide 'mediawiki-core)

;;; mediawiki-core.el ends here
