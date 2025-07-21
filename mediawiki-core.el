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
        (insert (format "[%s] %s\n" timestamp message))))))

(defun mediawiki-debug-line (line)
  "Log a LINE to debug buffer."
  (when mediawiki-debug
    (with-current-buffer (get-buffer-create mediawiki-debug-buffer)
      (goto-char (point-max))
      (insert "\n")
      (insert line))))

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

(provide 'mediawiki-core)

;;; mediawiki-core.el ends here
