;;; mediawiki-errors.el --- Error handling for MediaWiki -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module provides comprehensive error handling for MediaWiki API operations.
;; It implements error classification, mapping from MediaWiki API error codes,
;; and user-friendly error message generation.

;;; Code:

(require 'mediawiki-core)

;;; Error Classification System

(defconst mediawiki-error-categories
  '((network     . "Network errors")
    (auth        . "Authentication errors")
    (permission  . "Permission errors")
    (rate-limit  . "Rate limiting errors")
    (conflict    . "Edit conflicts")
    (validation  . "Input validation errors")
    (server      . "Server errors")
    (client      . "Client errors")
    (api         . "API errors")
    (parsing     . "Parsing errors")
    (unknown     . "Unknown errors"))
  "Categories of errors with human-readable descriptions.")

(defconst mediawiki-error-severity-levels
  '((critical . 4)  ;; System cannot continue, requires immediate attention
    (error    . 3)  ;; Operation failed, but system can continue
    (warning  . 2)  ;; Operation succeeded with issues, or might fail soon
    (info     . 1)) ;; Informational message about potential issues
  "Error severity levels with numeric values for comparison.")

(defconst mediawiki-error-recovery-types
  '((automatic . "Can be automatically recovered")
    (retry     . "Can be retried")
    (user      . "Requires user intervention")
    (fatal     . "Cannot be recovered"))
  "Types of error recovery strategies.")

;;; MediaWiki API Error Code Mapping

(defconst mediawiki-api-error-map
  '(
    ;; Authentication errors
    ("badtoken"           . (:category auth       :severity error    :recovery retry     :message "Invalid token"))
    ("notoken"            . (:category auth       :severity error    :recovery retry     :message "Missing token"))
    ("badlogin"           . (:category auth       :severity error    :recovery user      :message "Invalid username or password"))
    ("wrongtoken"         . (:category auth       :severity error    :recovery retry     :message "Wrong token type"))
    ("mustbeloggedin"     . (:category auth       :severity error    :recovery user      :message "You must be logged in"))
    ("sessionfailure"     . (:category auth       :severity error    :recovery retry     :message "Session failure, please log in again"))
    
    ;; Permission errors
    ("permissiondenied"   . (:category permission :severity error    :recovery user      :message "Permission denied"))
    ("protectedpage"      . (:category permission :severity error    :recovery user      :message "Page is protected"))
    ("cascadeprotected"   . (:category permission :severity error    :recovery user      :message "Page is cascade-protected"))
    ("titleblacklisted"   . (:category permission :severity error    :recovery user      :message "Title is blacklisted"))
    ("autoblocked"        . (:category permission :severity error    :recovery user      :message "Your IP address is blocked"))
    ("blocked"            . (:category permission :severity error    :recovery user      :message "User is blocked"))
    
    ;; Rate limiting errors
    ("ratelimited"        . (:category rate-limit :severity warning  :recovery retry     :message "Rate limited, please wait"))
    ("maxlag"             . (:category rate-limit :severity warning  :recovery retry     :message "Server is lagged, please wait"))
    
    ;; Edit conflicts
    ("editconflict"       . (:category conflict   :severity warning  :recovery user      :message "Edit conflict detected"))
    ("pagedeleted"        . (:category conflict   :severity warning  :recovery user      :message "Page was deleted since you started editing"))
    ("articleexists"      . (:category conflict   :severity warning  :recovery user      :message "Article already exists"))
    ("missingtitle"       . (:category conflict   :severity warning  :recovery user      :message "Page does not exist"))
    
    ;; Validation errors
    ("invalidtitle"       . (:category validation :severity error    :recovery user      :message "Invalid title"))
    ("emptypage"          . (:category validation :severity error    :recovery user      :message "Empty page content not allowed"))
    ("contenttoobig"      . (:category validation :severity error    :recovery user      :message "Content too large"))
    ("spamdetected"       . (:category validation :severity error    :recovery user      :message "Spam detected in content"))
    ("filtered"           . (:category validation :severity error    :recovery user      :message "Content was filtered"))
    
    ;; Server errors
    ("readonly"           . (:category server     :severity warning  :recovery retry     :message "Wiki is in read-only mode"))
    ("internal_api_error" . (:category server     :severity error    :recovery retry     :message "Internal API error"))
    ("unsupportednamespace" . (:category server   :severity error    :recovery fatal     :message "Unsupported namespace"))
    
    ;; Network errors
    ("timeout"            . (:category network    :severity warning  :recovery retry     :message "Request timed out"))
    ("connectfailed"      . (:category network    :severity error    :recovery retry     :message "Connection failed"))
    ("dnsfailure"         . (:category network    :severity error    :recovery retry     :message "DNS resolution failed"))
    
    ;; Client errors
    ("invalidparameter"   . (:category client     :severity error    :recovery user      :message "Invalid parameter"))
    ("missingparam"       . (:category client     :severity error    :recovery user      :message "Missing required parameter"))
    ("badformat"          . (:category client     :severity error    :recovery user      :message "Bad format"))
    
    ;; Parsing errors
    ("parseerror"         . (:category parsing    :severity error    :recovery user      :message "Parse error"))
    ("invalidjson"        . (:category parsing    :severity error    :recovery retry     :message "Invalid JSON response"))
    
    ;; Generic/unknown errors
    ("unknownerror"       . (:category unknown    :severity error    :recovery user      :message "Unknown error"))
    ("unclassified"       . (:category unknown    :severity error    :recovery user      :message "Unclassified error")))
  "Mapping of MediaWiki API error codes to error classifications.
Each error code maps to a plist with:
- :category - The error category symbol
- :severity - The error severity symbol
- :recovery - The recovery type symbol
- :message - A default human-readable message")

;;; HTTP Status Code Mapping

(defconst mediawiki-http-status-error-map
  '((400 . (:category client    :severity error   :recovery user  :message "Bad Request - Check your request parameters"))
    (401 . (:category auth      :severity error   :recovery user  :message "Unauthorized - Authentication required"))
    (403 . (:category permission :severity error  :recovery user  :message "Forbidden - You don't have permission"))
    (404 . (:category client    :severity error   :recovery user  :message "Not Found - Resource doesn't exist"))
    (405 . (:category client    :severity error   :recovery user  :message "Method Not Allowed"))
    (408 . (:category network   :severity warning :recovery retry :message "Request Timeout"))
    (409 . (:category conflict  :severity warning :recovery user  :message "Conflict - Resource state conflict"))
    (413 . (:category validation :severity error  :recovery user  :message "Payload Too Large"))
    (414 . (:category validation :severity error  :recovery user  :message "URI Too Long"))
    (429 . (:category rate-limit :severity warning :recovery retry :message "Too Many Requests - Rate limited"))
    (500 . (:category server    :severity error   :recovery retry :message "Internal Server Error"))
    (501 . (:category server    :severity error   :recovery fatal :message "Not Implemented"))
    (502 . (:category network   :severity warning :recovery retry :message "Bad Gateway"))
    (503 . (:category server    :severity warning :recovery retry :message "Service Unavailable - Try again later"))
    (504 . (:category network   :severity warning :recovery retry :message "Gateway Timeout")))
  "Mapping of HTTP status codes to error classifications.")

;;; Error Structure

(cl-defstruct mediawiki-error
  "Structure representing a MediaWiki error."
  category                ; Error category symbol (network, auth, etc.)
  severity                ; Error severity symbol (critical, error, warning, info)
  recovery                ; Recovery type symbol (automatic, retry, user, fatal)
  code                    ; Original error code (string or number)
  message                 ; Human-readable error message
  details                 ; Additional error details (plist)
  source                  ; Error source ('api, 'http, 'client, etc.)
  timestamp               ; When the error occurred
  context)                ; Context information about the operation

;;; Error Creation Functions

(defun mediawiki-error-create-from-api (error-code error-info &optional context)
  "Create a structured error from MediaWiki API error code and info.
ERROR-CODE is the API error code string.
ERROR-INFO is the error info message from the API.
CONTEXT is optional additional context about the operation."
  (let* ((error-mapping (or (cdr (assoc error-code mediawiki-api-error-map))
                           (cdr (assoc "unclassified" mediawiki-api-error-map))))
         (category (plist-get error-mapping :category))
         (severity (plist-get error-mapping :severity))
         (recovery (plist-get error-mapping :recovery))
         (default-message (plist-get error-mapping :message)))
    
    (make-mediawiki-error
     :category category
     :severity severity
     :recovery recovery
     :code error-code
     :message (if error-info
                 (format "%s: %s" default-message error-info)
               default-message)
     :details (list :info error-info)
     :source 'api
     :timestamp (current-time)
     :context context)))

(defun mediawiki-error-create-from-http (status-code message &optional context)
  "Create a structured error from HTTP status code.
STATUS-CODE is the HTTP status code.
MESSAGE is an optional error message.
CONTEXT is optional additional context about the operation."
  (let* ((error-mapping (or (cdr (assoc status-code mediawiki-http-status-error-map))
                           ;; Default mappings based on status code ranges
                           (cond
                            ((and (>= status-code 400) (< status-code 500))
                             '(:category client :severity error :recovery user :message "Client Error"))
                            ((and (>= status-code 500) (< status-code 600))
                             '(:category server :severity error :recovery retry :message "Server Error"))
                            (t
                             '(:category unknown :severity error :recovery user :message "Unknown HTTP Error")))))
         (category (plist-get error-mapping :category))
         (severity (plist-get error-mapping :severity))
         (recovery (plist-get error-mapping :recovery))
         (default-message (plist-get error-mapping :message)))
    
    (make-mediawiki-error
     :category category
     :severity severity
     :recovery recovery
     :code status-code
     :message (if message
                 (format "%s: %s" default-message message)
               default-message)
     :details (list :status-code status-code)
     :source 'http
     :timestamp (current-time)
     :context context)))

(defun mediawiki-error-create-from-exception (error-symbol error-message &optional context)
  "Create a structured error from an Emacs error.
ERROR-SYMBOL is the error symbol.
ERROR-MESSAGE is the error message string.
CONTEXT is optional additional context about the operation."
  (let ((category (cond
                   ((memq error-symbol '(file-error file-missing file-notify-error))
                    'client)
                   ((memq error-symbol '(network-error url-http-error timeout-error))
                    'network)
                   ((memq error-symbol '(json-parse-error json-readtable-error))
                    'parsing)
                   (t 'client))))
    
    (make-mediawiki-error
     :category category
     :severity 'error
     :recovery 'user
     :code (symbol-name error-symbol)
     :message (format "Error: %s" error-message)
     :details (list :error-symbol error-symbol)
     :source 'client
     :timestamp (current-time)
     :context context)))

(defun mediawiki-error-create (category severity message &optional code recovery details source context)
  "Create a custom structured error.
CATEGORY is the error category symbol.
SEVERITY is the error severity symbol.
MESSAGE is the human-readable error message.
CODE is an optional error code.
RECOVERY is an optional recovery type symbol.
DETAILS is an optional plist with additional details.
SOURCE is an optional error source symbol.
CONTEXT is optional additional context about the operation."
  (make-mediawiki-error
   :category (or category 'unknown)
   :severity (or severity 'error)
   :recovery (or recovery 'user)
   :code code
   :message message
   :details details
   :source (or source 'client)
   :timestamp (current-time)
   :context context))

;;; Error Processing Functions

(defun mediawiki-error-from-response (response &optional context)
  "Extract structured error information from API RESPONSE.
CONTEXT is optional additional context about the operation."
  (let* ((errors (mediawiki-api-response-errors response))
         (primary-error (car errors)))
    
    (if primary-error
        ;; API error
        (let ((error-code (plist-get primary-error :code))
              (error-info (plist-get primary-error :info)))
          (mediawiki-error-create-from-api error-code error-info context))
      
      ;; HTTP error
      (let ((status-code (plist-get response :status-code))
            (error-message (plist-get response :error)))
        (mediawiki-error-create-from-http status-code error-message context)))))

(defun mediawiki-error-retryable-p (error)
  "Check if ERROR is retryable."
  (memq (mediawiki-error-recovery error) '(automatic retry)))

(defun mediawiki-error-requires-user-intervention-p (error)
  "Check if ERROR requires user intervention."
  (eq (mediawiki-error-recovery error) 'user))

(defun mediawiki-error-fatal-p (error)
  "Check if ERROR is fatal."
  (eq (mediawiki-error-recovery error) 'fatal))

(defun mediawiki-error-get-severity-level (error)
  "Get numeric severity level for ERROR."
  (or (cdr (assq (mediawiki-error-severity error) mediawiki-error-severity-levels)) 0))

(defun mediawiki-error-get-category-name (error)
  "Get human-readable category name for ERROR."
  (or (cdr (assq (mediawiki-error-category error) mediawiki-error-categories))
      "Unknown error category"))

;;; User-Friendly Error Message Generation

(defun mediawiki-error-format-message (error &optional verbose)
  "Format a user-friendly message for ERROR.
When VERBOSE is non-nil, include additional details."
  (let ((base-message (mediawiki-error-message error))
        (category-name (mediawiki-error-get-category-name error))
        (context (mediawiki-error-context error))
        (details (mediawiki-error-details error)))
    
    (if verbose
        (format "%s\n\nCategory: %s\nCode: %s\nSource: %s%s%s"
                base-message
                category-name
                (or (mediawiki-error-code error) "unknown")
                (or (mediawiki-error-source error) "unknown")
                (if context (format "\nContext: %s" context) "")
                (if details
                    (format "\nDetails: %s"
                            (mapconcat (lambda (kv)
                                         (format "%s: %s"
                                                 (substring (symbol-name (car kv)) 1)
                                                 (cdr kv)))
                                       details
                                       ", "))
                  ""))
      base-message)))

(defun mediawiki-error-suggest-solution (error)
  "Suggest a solution for ERROR based on its category and code."
  (let ((category (mediawiki-error-category error))
        (code (mediawiki-error-code error))
        (recovery (mediawiki-error-recovery error)))
    
    (cond
     ;; Authentication errors
     ((eq category 'auth)
      (cond
       ((member code '("badtoken" "notoken" "wrongtoken"))
        "Try logging in again to refresh your authentication tokens.")
       ((string= code "badlogin")
        "Check your username and password and try again.")
       (t
        "You may need to log in again to continue.")))
     
     ;; Permission errors
     ((eq category 'permission)
      (cond
       ((string= code "protectedpage")
        "This page is protected. You need higher permissions to edit it.")
       ((string= code "blocked")
        "Your account is blocked from editing. Contact a wiki administrator.")
       (t
        "You don't have permission to perform this action.")))
     
     ;; Rate limiting errors
     ((eq category 'rate-limit)
      (cond
       ((string= code "ratelimited")
        "You've made too many requests. Wait a few minutes and try again.")
       ((string= code "maxlag")
        "The wiki server is experiencing lag. Wait a few minutes and try again.")
       (t
        "Try again later when the server is less busy.")))
     
     ;; Edit conflicts
     ((eq category 'conflict)
      (cond
       ((string= code "editconflict")
        "Someone else edited this page while you were editing. Review both versions and merge your changes.")
       ((string= code "pagedeleted")
        "This page was deleted since you started editing. Check if it should be recreated.")
       ((string= code "articleexists")
        "An article with this title already exists. Choose a different title.")
       (t
        "There was a conflict with your edit. Review the current page state.")))
     
     ;; Network errors
     ((eq category 'network)
      (cond
       ((string= code "timeout")
        "The request timed out. Check your internet connection and try again.")
       (t
        "Check your internet connection and try again.")))
     
     ;; Server errors
     ((eq category 'server)
      (cond
       ((string= code "readonly")
        "The wiki is currently in read-only mode. Try again later.")
       (t
        "This is a server error. Try again later or contact the wiki administrators.")))
     
     ;; Default suggestion based on recovery type
     ((eq recovery 'retry)
      "Try the operation again.")
     
     ((eq recovery 'automatic)
      "The system will attempt to recover automatically.")
     
     ((eq recovery 'user)
      "You need to take action to resolve this issue.")
     
     ((eq recovery 'fatal)
      "This operation cannot be completed. Try a different approach.")
     
     (t
      "No specific solution available for this error."))))

;;; Error Handling Functions

(defun mediawiki-error-handle (error &optional options)
  "Handle ERROR according to its category and severity.
OPTIONS is a plist of handling options:
- :retry-function - Function to call for retry
- :retry-args - Arguments for retry function
- :max-retries - Maximum number of retries
- :current-retry - Current retry count
- :on-user-intervention - Function to call when user intervention is needed
- :on-fatal - Function to call for fatal errors
- :quiet - If non-nil, suppress messages

Returns t if handled, nil otherwise."
  (let ((retry-function (plist-get options :retry-function))
        (retry-args (plist-get options :retry-args))
        (max-retries (or (plist-get options :max-retries) 3))
        (current-retry (or (plist-get options :current-retry) 0))
        (on-user-intervention (plist-get options :on-user-intervention))
        (on-fatal (plist-get options :on-fatal))
        (quiet (plist-get options :quiet)))
    
    (cond
     ;; Automatic retry for retryable errors
     ((and (mediawiki-error-retryable-p error)
           retry-function
           (< current-retry max-retries))
      (unless quiet
        (message "Error: %s. Retrying (%d/%d)..."
                 (mediawiki-error-message error)
                 (1+ current-retry)
                 max-retries))
      
      ;; Calculate exponential backoff delay
      (let ((delay (* 1.0 (expt 2 current-retry))))
        (sit-for delay)
        (apply retry-function
               (append retry-args
                       (list :current-retry (1+ current-retry)))))
      t)
     
     ;; User intervention required
     ((mediawiki-error-requires-user-intervention-p error)
      (unless quiet
        (message "Error: %s\nSuggestion: %s"
                 (mediawiki-error-message error)
                 (mediawiki-error-suggest-solution error)))
      
      (when on-user-intervention
        (funcall on-user-intervention error))
      t)
     
     ;; Fatal errors
     ((mediawiki-error-fatal-p error)
      (unless quiet
        (message "Fatal error: %s" (mediawiki-error-message error)))
      
      (when on-fatal
        (funcall on-fatal error))
      t)
     
     ;; Default handling
     (t
      (unless quiet
        (message "Error: %s" (mediawiki-error-message error)))
      nil))))

(defun mediawiki-error-display (error &optional verbose)
  "Display ERROR to the user in a user-friendly format.
When VERBOSE is non-nil, include additional details."
  (let ((message (mediawiki-error-format-message error verbose))
        (suggestion (mediawiki-error-suggest-solution error)))
    
    (if verbose
        ;; Show in a dedicated buffer for verbose output
        (with-current-buffer (get-buffer-create "*MediaWiki Error*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "MediaWiki Error\n\n%s\n\nSuggested solution:\n%s\n" 
                            message suggestion))
            (goto-char (point-min))
            (special-mode)
            (display-buffer (current-buffer))))
      
      ;; Simple message for non-verbose
      (message "MediaWiki error: %s\nSuggestion: %s" message suggestion))))

;;; Integration with API Response Processing

(defun mediawiki-error-process-api-response (response &optional context options)
  "Process errors in API RESPONSE with CONTEXT and handling OPTIONS.
Returns the response if successful, or handles the error according to options."
  (if (mediawiki-api-response-success response)
      response
    (let ((error-obj (mediawiki-error-from-response response context)))
      (if (mediawiki-error-handle error-obj options)
          nil  ; Error was handled
        (signal 'mediawiki-error (list error-obj))))))

(provide 'mediawiki-errors)

;;; mediawiki-errors.el ends here