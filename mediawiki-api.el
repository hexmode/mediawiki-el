;;; mediawiki-api.el --- MediaWiki API communication framework -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module provides the JSON-based MediaWiki API communication framework.
;; It handles API request building, response parsing, and error handling.

;;; Code:

(require 'json)
(require 'mediawiki-core)
(require 'mediawiki-http)
(require 'mediawiki-errors)

;;; API Communication Functions

(defun mediawiki-api-call-async (sitename action params callback &optional error-callback)
  "Make an asynchronous API call to SITENAME with ACTION and PARAMS.
CALLBACK is called with parsed response on success.
ERROR-CALLBACK is called with error information on failure."
  (let ((url (mediawiki-api-make-url sitename))
        (data (mediawiki-api-build-request-data action params)))

    (mediawiki-http-request-async
     url "POST" data
     (lambda (response)
       (let ((parsed (mediawiki-api-parse-response response)))
         (if (mediawiki-api-response-success parsed)
             (when callback
               (funcall callback parsed))
           (when error-callback
             (funcall error-callback parsed)))))
     (lambda (response)
       (when error-callback
         (funcall error-callback
                 (make-mediawiki-api-response
                  :success nil
                  :errors (list (list :code "http-error"
                                     :info (plist-get response :error)))
                  :raw-response response)))))))

(defun mediawiki-api-call-sync (sitename action params)
  "Make a synchronous API call to SITENAME with ACTION and PARAMS.
Returns parsed API response or signals an error."
  (let ((url (mediawiki-api-make-url sitename))
        (data (mediawiki-api-build-request-data action params)))

    (let ((response (mediawiki-http-request-sync url "POST" data)))
      (mediawiki-api-parse-response response))))

(defun mediawiki-api-call-with-token (sitename action params token-type &optional retry-count)
  "Make API call with automatic token handling and refresh.
Implements automatic token refresh as required by requirement 2.3.
SITENAME, ACTION, and PARAMS are standard API call parameters.
TOKEN-TYPE specifies the type of token needed (e.g., 'csrf', 'login').
RETRY-COUNT is used internally for retry logic."
  (let ((retry-count (or retry-count 0))
        (max-retries 2))

    (when (> retry-count max-retries)
      (error "Maximum token refresh retries exceeded for %s" sitename))

    ;; Get token (will refresh automatically if needed)
    (let* ((token (condition-case err
                      (progn
                        (require 'mediawiki-session)
                        (mediawiki-session-get-token sitename token-type))
                    (error
                     (mediawiki-debug-log "Failed to get %s token for %s: %s"
                                        token-type sitename (error-message-string err))
                     nil)))
           ;; Add token to parameters
           (token-param (concat token-type "token"))
           (params-with-token (if token
                                 (cons (cons token-param token) params)
                               params)))

      ;; Make the API call
      (let ((response (mediawiki-api-call-sync sitename action params-with-token)))

        ;; Check if we got a token error and should retry
        (if (and (not (mediawiki-api-response-success response))
                 (mediawiki-api-has-token-error-p response)
                 (< retry-count max-retries))
            (progn
              (mediawiki-debug-log "Token error detected, forcing refresh and retrying (attempt %d)"
                                 (1+ retry-count))
              ;; Force token refresh by clearing cache
              (mediawiki-session-clear-token sitename token-type)
              ;; Retry the call
              (mediawiki-api-call-with-token sitename action params token-type (1+ retry-count)))

          ;; Return the response (success or final failure)
          response)))))

(defun mediawiki-api-has-token-error-p (response)
  "Check if RESPONSE contains a token-related error."
  (let ((error-codes (mediawiki-api-get-all-error-codes response)))
    (cl-some (lambda (code)
               (member code '("badtoken" "notoken" "sessionfailure" "assertuserfailed")))
             error-codes)))

;;; Request Building

(defun mediawiki-api-make-url (sitename)
  "Build API URL for SITENAME."
  (let ((site (mediawiki-get-site sitename)))
    (if site
        (or (mediawiki-site-config-api-url site)
            (concat (mediawiki-site-config-url site) "api.php"))
      (error "Unknown site: %s" sitename))))

(defun mediawiki-api-build-request-data (action params)
  "Build request data hash table for ACTION with PARAMS."
  (let ((data (make-hash-table :test 'equal)))
    (puthash "format" "json" data)
    (puthash "action" action data)

    ;; Add parameters
    (dolist (param params)
      (when (consp param)
        (puthash (car param) (cdr param) data)))

    data))

;;; Response Parsing

(defun mediawiki-api-parse-response (http-response)
  "Parse HTTP-RESPONSE into a mediawiki-api-response structure.
Implements comprehensive error detection and structured error reporting
as required by requirements 1.4 and 5.1."
  (let ((body (plist-get http-response :body))
        (success (plist-get http-response :success))
        (status-code (plist-get http-response :status-code)))

    (if (not success)
        (make-mediawiki-api-response
         :success nil
         :errors (list (mediawiki-api-create-error
                       "http-error"
                       (plist-get http-response :error)
                       :http-status status-code
                       :error-type (plist-get http-response :error-type)))
         :raw-response http-response)

      (condition-case err
          (let ((json-data (mediawiki-api-parse-json-safely body)))
            (if json-data
                (mediawiki-api-parse-json-response json-data http-response)
              (make-mediawiki-api-response
               :success nil
               :errors (list (mediawiki-api-create-error
                             "json-parse-error"
                             "Response body is not valid JSON"
                             :response-body (substring body 0 (min 200 (length body)))))
               :raw-response http-response)))
        (error
         (make-mediawiki-api-response
          :success nil
          :errors (list (mediawiki-api-create-error
                        "json-parse-error"
                        (format "Failed to parse JSON: %s" (error-message-string err))
                        :original-error err))
          :raw-response http-response))))))

(defun mediawiki-api-parse-json-safely (body)
  "Safely parse JSON from BODY string.
Returns parsed data or nil if parsing fails."
  (when (and body (stringp body) (> (length body) 0))
    (condition-case nil
        (if (fboundp 'json-parse-string)
            ;; Use modern json-parse-string if available (Emacs 27+)
            (json-parse-string body :object-type 'alist :array-type 'list)
          ;; Fall back to json.el for older Emacs versions
          (let ((json-object-type 'alist)
                (json-array-type 'list))
            (json-read-from-string body)))
      (error nil))))

(defun mediawiki-api-parse-json-response (json-data http-response)
  "Parse JSON-DATA from HTTP-RESPONSE into API response structure.
Implements comprehensive MediaWiki API error code handling."
  (let* ((api-errors (cdr (assq 'error json-data)))
         (api-warnings (cdr (assq 'warnings json-data)))
         (servedby (cdr (assq 'servedby json-data)))
         (data (mediawiki-api-extract-action-data json-data))
         (parsed-errors (mediawiki-api-parse-api-errors api-errors))
         (parsed-warnings (mediawiki-api-parse-api-warnings api-warnings)))

    ;; Check for additional error indicators
    (let ((additional-errors (mediawiki-api-detect-implicit-errors json-data)))
      (when additional-errors
        (setq parsed-errors (append parsed-errors additional-errors))))

    (make-mediawiki-api-response
     :success (null parsed-errors)
     :data data
     :warnings parsed-warnings
     :errors parsed-errors
     :raw-response http-response
     :request-info (list :server servedby))))

(defun mediawiki-api-extract-action-data (json-data)
  "Extract action-specific data from JSON-DATA.
Removes standard API wrapper elements to return clean action data."
  (let ((filtered-data (copy-alist json-data)))
    ;; Remove standard MediaWiki API metadata
    (setq filtered-data (assq-delete-all 'error filtered-data))
    (setq filtered-data (assq-delete-all 'warnings filtered-data))
    (setq filtered-data (assq-delete-all 'servedby filtered-data))
    (setq filtered-data (assq-delete-all 'requestid filtered-data))
    (setq filtered-data (assq-delete-all 'uselang filtered-data))
    filtered-data))

;;; Error Parsing and Classification

(defun mediawiki-api-parse-api-errors (errors)
  "Parse MediaWiki API ERRORS into structured error list.
Handles both single errors and arrays of errors."
  (when errors
    (let ((error-list (if (listp errors) errors (list errors))))
      (mapcar 'mediawiki-api-parse-single-error error-list))))

(defun mediawiki-api-parse-single-error (error)
  "Parse a single MediaWiki API ERROR into structured format."
  (let ((code (cond
               ;; If error is a simple cons cell like (code . "value")
               ((and (consp error) (not (listp (cdr error))))
                (cdr error))
               ;; If error is an alist with code field
               ((listp error)
                (cdr (assq 'code error)))
               ;; Fallback
               (t "unknown")))
        (info (if (listp error)
                  (cdr (assq 'info error))
                "API error"))
        (data (if (listp error)
                  (cdr (assq 'data error))
                nil)))

    (mediawiki-api-create-error
     code
     info
     :api-data data
     :error-category (mediawiki-api-classify-error-code code)
     :severity (mediawiki-api-get-error-severity code)
     :recoverable (mediawiki-api-is-error-recoverable code))))

(defun mediawiki-api-parse-api-warnings (warnings)
  "Parse MediaWiki API WARNINGS into structured warning list."
  (when warnings
    (let ((warning-list '()))
      ;; Warnings can be in different formats depending on the API
      (cond
       ;; Simple list format
       ((listp warnings)
        (dolist (warning warnings)
          (push (mediawiki-api-parse-single-warning warning) warning-list)))
       ;; Hash/alist format with modules
       ((and (consp warnings) (not (listp (cdr warnings))))
        (dolist (module-warning warnings)
          (let ((module (car module-warning))
                (warning-data (cdr module-warning)))
            (push (mediawiki-api-parse-module-warning module warning-data) warning-list))))
       ;; Single warning
       (t
        (push (mediawiki-api-parse-single-warning warnings) warning-list)))
      (nreverse warning-list))))

(defun mediawiki-api-parse-single-warning (warning)
  "Parse a single MediaWiki API WARNING."
  (if (stringp warning)
      (list :message warning :module "unknown")
    (list :message (cdr (assq 'info warning))
          :module (cdr (assq 'module warning))
          :code (cdr (assq 'code warning)))))

(defun mediawiki-api-parse-module-warning (module warning-data)
  "Parse a module-specific WARNING-DATA for MODULE."
  (list :message (if (stringp warning-data)
                     warning-data
                   (cdr (assq 'info warning-data)))
        :module (symbol-name module)
        :code (when (consp warning-data)
                (cdr (assq 'code warning-data)))))

(defun mediawiki-api-detect-implicit-errors (json-data)
  "Detect implicit errors in JSON-DATA that aren't in the error field.
Some MediaWiki API responses indicate errors through other means."
  (let ((errors '()))

    ;; Check for login failures in login responses
    (let ((login-result (cdr (assq 'login json-data))))
      (when login-result
        (let ((result-code (cdr (assq 'result login-result))))
          (when (and result-code (not (string= result-code "Success")))
            (push (mediawiki-api-create-error
                   (format "login-%s" (downcase result-code))
                   (or (cdr (assq 'reason login-result))
                       (format "Login failed: %s" result-code))
                   :login-result result-code
                   :error-category 'authentication)
                  errors)))))

    ;; Check for edit failures in edit responses
    (let ((edit-result (cdr (assq 'edit json-data))))
      (when edit-result
        (let ((result-code (cdr (assq 'result edit-result))))
          (when (and result-code (not (string= result-code "Success")))
            (push (mediawiki-api-create-error
                   (format "edit-%s" (downcase result-code))
                   (or (cdr (assq 'info edit-result))
                       (format "Edit failed: %s" result-code))
                   :edit-result result-code
                   :error-category 'operation)
                  errors)))))

    ;; Check for query continuation issues
    (let ((query-continue (cdr (assq 'query-continue json-data))))
      (when (and query-continue (not (cdr (assq 'query json-data))))
        (push (mediawiki-api-create-error
               "incomplete-query"
               "Query was truncated and requires continuation"
               :continue-data query-continue
               :error-category 'data
               :severity 'warning)
              errors)))

    errors))

;;; Error Classification and Utilities

(defun mediawiki-api-create-error (code info &rest properties)
  "Create a structured error with CODE and INFO message.
PROPERTIES are additional key-value pairs to include in the error."
  (let ((error (list :code code :info info)))
    (while properties
      (setq error (plist-put error (car properties) (cadr properties)))
      (setq properties (cddr properties)))
    error))

(defun mediawiki-api-classify-error-code (code)
  "Classify MediaWiki API error CODE into categories.
Returns a symbol indicating the error category."
  (cond
   ;; Authentication and permission errors
   ((member code '("badtoken" "notoken" "mustbeloggedin" "permissiondenied"
                   "login-failed" "login-blocked" "login-throttled"))
    'authentication)

   ;; Rate limiting and throttling
   ((member code '("ratelimited" "actionthrottledtext" "login-throttled"))
    'rate-limit)

   ;; Data validation errors
   ((member code '("badtitle" "invalidtitle" "missingtitle" "nosuchpageid"
                   "badrevision" "nosuchrevid" "badformat"))
    'validation)

   ;; Edit conflicts and concurrency
   ((member code '("editconflict" "pagedeleted" "articleexists"))
    'conflict)

   ;; Server and system errors
   ((member code '("readonly" "blocked" "autoblocked" "systemblocked"))
    'system)

   ;; Network and communication errors
   ((member code '("http-error" "json-parse-error" "timeout"))
    'network)

   ;; Default category
   (t 'unknown)))

(defun mediawiki-api-get-error-severity (code)
  "Get severity level for error CODE.
Returns 'error, 'warning, or 'info."
  (cond
   ;; Critical errors that prevent operation
   ((member code '("badtoken" "notoken" "mustbeloggedin" "permissiondenied"
                   "readonly" "blocked" "http-error"))
    'error)

   ;; Warnings that may affect operation
   ((member code '("ratelimited" "editconflict" "incomplete-query"))
    'warning)

   ;; Informational messages
   (t 'info)))

(defun mediawiki-api-is-error-recoverable (code)
  "Check if error with CODE is potentially recoverable.
Returns t if the error might be resolved by retrying or user action."
  (member code '("ratelimited" "actionthrottledtext" "login-throttled"
                 "editconflict" "readonly" "timeout" "http-error")))

;;; Warning Parsing

(defun mediawiki-api-parse-warnings (warnings)
  "Parse WARNINGS from API response.
Maintained for backward compatibility."
  (mediawiki-api-parse-api-warnings warnings))

;;; Response Validation

(defun mediawiki-api-response-success (response)
  "Check if API RESPONSE indicates success."
  (mediawiki-api-response-success response))

(defun mediawiki-api-validate-response (response)
  "Validate API RESPONSE structure and content.
Returns a list of validation issues or nil if valid."
  (let ((issues '()))

    ;; Check response structure
    (unless (mediawiki-api-response-p response)
      (push "Response is not a valid mediawiki-api-response structure" issues))

    ;; Check for required fields
    (unless (mediawiki-api-response-success response)
      (unless (mediawiki-api-response-errors response)
        (push "Failed response missing error information" issues)))

    ;; Validate error structure if present
    (let ((errors (mediawiki-api-response-errors response)))
      (when errors
        (dolist (error errors)
          (unless (plist-get error :code)
            (push "Error missing code field" issues))
          (unless (plist-get error :info)
            (push "Error missing info field" issues)))))

    ;; Validate warning structure if present
    (let ((warnings (mediawiki-api-response-warnings response)))
      (when warnings
        (dolist (warning warnings)
          (unless (plist-get warning :message)
            (push "Warning missing message field" issues)))))

    ;; Check for data consistency
    (when (and (mediawiki-api-response-success response)
               (not (mediawiki-api-response-data response)))
      (push "Successful response has no data" issues))

    issues))

(defun mediawiki-api-get-error-code (response)
  "Get primary error code from API RESPONSE."
  (let ((errors (mediawiki-api-response-errors response)))
    (when errors
      (plist-get (car errors) :code))))

(defun mediawiki-api-get-error-info (response)
  "Get primary error information from API RESPONSE."
  (let ((errors (mediawiki-api-response-errors response)))
    (when errors
      (plist-get (car errors) :info))))

(defun mediawiki-api-get-all-error-codes (response)
  "Get all error codes from API RESPONSE."
  (let ((errors (mediawiki-api-response-errors response)))
    (mapcar (lambda (error) (plist-get error :code)) errors)))

(defun mediawiki-api-has-error-code (response code)
  "Check if API RESPONSE contains specific error CODE."
  (member code (mediawiki-api-get-all-error-codes response)))

(defun mediawiki-api-get-errors-by-category (response category)
  "Get all errors from RESPONSE matching CATEGORY."
  (let ((errors (mediawiki-api-response-errors response)))
    (cl-remove-if-not
     (lambda (error) (eq (plist-get error :error-category) category))
     errors)))

(defun mediawiki-api-has-recoverable-errors (response)
  "Check if RESPONSE contains only recoverable errors."
  (let ((errors (mediawiki-api-response-errors response)))
    (and errors
         (cl-every (lambda (error) (plist-get error :recoverable)) errors))))

(defun mediawiki-api-get-error-severity (response)
  "Get the highest error severity from RESPONSE.
Returns 'error, 'warning, 'info, or nil if no errors."
  (let ((errors (mediawiki-api-response-errors response))
        (max-severity nil))
    (dolist (error errors)
      (let ((severity (plist-get error :severity)))
        (cond
         ((eq severity 'error) (setq max-severity 'error))
         ((and (eq severity 'warning) (not (eq max-severity 'error)))
          (setq max-severity 'warning))
         ((and (eq severity 'info) (not max-severity))
          (setq max-severity 'info)))))
    max-severity))

(defun mediawiki-api-format-error-summary (response)
  "Format a human-readable error summary from RESPONSE."
  (let ((errors (mediawiki-api-response-errors response)))
    (if (not errors)
        "No errors"
      (let ((primary-error (car errors))
            (error-count (length errors)))
        (format "%s%s"
                (plist-get primary-error :info)
                (if (> error-count 1)
                    (format " (and %d more error%s)"
                            (1- error-count)
                            (if (> error-count 2) "s" ""))
                  ""))))))

(defun mediawiki-api-format-warning-summary (response)
  "Format a human-readable warning summary from RESPONSE."
  (let ((warnings (mediawiki-api-response-warnings response)))
    (if (not warnings)
        "No warnings"
      (let ((warning-count (length warnings)))
        (format "%d warning%s: %s"
                warning-count
                (if (> warning-count 1) "s" "")
                (mapconcat (lambda (w) (plist-get w :message))
                          warnings "; "))))))

;;; Response Content Validation

(defun mediawiki-api-validate-query-response (response expected-props)
  "Validate that query RESPONSE contains EXPECTED-PROPS.
EXPECTED-PROPS is a list of property names that should be present."
  (let ((data (mediawiki-api-response-data response))
        (missing-props '()))

    (when (mediawiki-api-response-success response)
      (let ((query-data (cdr (assq 'query data))))
        (dolist (prop expected-props)
          (unless (assq prop query-data)
            (push prop missing-props)))))

    (when missing-props
      (format "Missing expected properties: %s"
              (mapconcat 'symbol-name missing-props ", ")))))

(defun mediawiki-api-validate-edit-response (response)
  "Validate edit RESPONSE for common issues."
  (let ((issues '()))

    (when (mediawiki-api-response-success response)
      (let* ((data (mediawiki-api-response-data response))
             (edit-data (cdr (assq 'edit data))))

        ;; Check for edit result
        (unless (cdr (assq 'result edit-data))
          (push "Edit response missing result field" issues))

        ;; Check for new revision ID on successful edits
        (when (string= (cdr (assq 'result edit-data)) "Success")
          (unless (cdr (assq 'newrevid edit-data))
            (push "Successful edit missing new revision ID" issues)))

        ;; Check for edit conflicts
        (when (cdr (assq 'nochange edit-data))
          (push "Edit resulted in no change" issues))))

    issues))

(defun mediawiki-api-validate-login-response (response)
  "Validate login RESPONSE for authentication status."
  (let ((issues '()))

    (let* ((data (mediawiki-api-response-data response))
           (login-data (cdr (assq 'login data))))

      (when login-data
        ;; Check login result
        (let ((result (cdr (assq 'result login-data))))
          (cond
           ((string= result "Success")
            ;; Successful login should have user info
            (unless (cdr (assq 'lgusername login-data))
              (push "Successful login missing username" issues)))

           ((string= result "NeedToken")
            ;; Token-based login should provide token
            (unless (cdr (assq 'token login-data))
              (push "Token-based login missing token" issues)))

           (t
            ;; Failed login should have reason
            (unless (cdr (assq 'reason login-data))
              (push "Failed login missing reason" issues)))))))

    issues))

;;; Parameter Validation

(defun mediawiki-api-validate-params (action params)
  "Validate PARAMS for ACTION.
Returns validated params or signals an error."
  ;; Basic validation - can be extended
  (dolist (param params)
    (unless (consp param)
      (error "Invalid parameter format: %s" param))
    (unless (stringp (car param))
      (error "Parameter name must be string: %s" (car param))))
  params)

;;; API Endpoint Discovery and Validation

(defvar mediawiki-api-capabilities-cache (make-hash-table :test 'equal)
  "Cache for MediaWiki site capabilities to avoid repeated API calls.")

(defvar mediawiki-api-version-cache (make-hash-table :test 'equal)
  "Cache for MediaWiki version information.")

(defun mediawiki-api-discover-capabilities (sitename &optional force-refresh)
  "Discover and cache API capabilities for SITENAME.
If FORCE-REFRESH is non-nil, bypass cache and fetch fresh data.
Implements requirement 1.1 for API capability checking."
  (let ((cache-key sitename))
    (if (and (not force-refresh)
             (gethash cache-key mediawiki-api-capabilities-cache))
        (gethash cache-key mediawiki-api-capabilities-cache)

      ;; Fetch capabilities from API
      (let ((capabilities (mediawiki-api-fetch-site-info sitename)))
        (when capabilities
          (puthash cache-key capabilities mediawiki-api-capabilities-cache))
        capabilities))))

(defun mediawiki-api-fetch-site-info (sitename)
  "Fetch site information and capabilities from SITENAME.
Returns a plist with version, capabilities, and other site metadata."
  (condition-case err
      (let ((response (mediawiki-api-call-sync
                      sitename "query"
                      (mediawiki-api-build-params
                       "meta" "siteinfo"
                       "siprop" "general|namespaces|extensions|rightsinfo"))))

        (if (mediawiki-api-response-success response)
            (mediawiki-api-parse-site-info response)
          (progn
            (mediawiki-debug-log "Failed to fetch site info for %s: %s"
                                sitename
                                (mediawiki-api-get-error-info response))
            nil)))
    (error
     (mediawiki-debug-log "Error fetching site info for %s: %s"
                         sitename (error-message-string err))
     nil)))

(defun mediawiki-api-parse-site-info (response)
  "Parse site information from API RESPONSE.
Returns a plist with structured site capabilities."
  (let* ((data (mediawiki-api-response-data response))
         (query (cdr (assq 'query data)))
         (general (cdr (assq 'general query)))
         (namespaces (cdr (assq 'namespaces query)))
         (extensions (cdr (assq 'extensions query)))
         (rights (cdr (assq 'rightsinfo query))))

    (list
     ;; Version information
     :version (cdr (assq 'generator general))
     :api-version (cdr (assq 'version general))
     :mediawiki-version (mediawiki-api-extract-version-number
                        (cdr (assq 'generator general)))

     ;; Basic site info
     :sitename (cdr (assq 'sitename general))
     :base-url (cdr (assq 'base general))
     :server-url (cdr (assq 'server general))
     :script-path (cdr (assq 'scriptpath general))

     ;; Capabilities
     :namespaces (mediawiki-api-parse-namespaces namespaces)
     :extensions (mediawiki-api-parse-extensions extensions)
     :rights (mediawiki-api-parse-rights rights)

     ;; Feature detection
     :supports-json (t)  ; We're using JSON API
     :supports-oauth (mediawiki-api-detect-oauth-support extensions)
     :supports-flow (mediawiki-api-detect-extension extensions "Flow")
     :supports-visual-editor (mediawiki-api-detect-extension extensions "VisualEditor")
     :supports-mobile (mediawiki-api-detect-extension extensions "MobileFrontend")

     ;; API-specific capabilities
     :max-upload-size (cdr (assq 'maxuploadsize general))
     :upload-enabled (not (null (cdr (assq 'uploadsenabled general))))
     :edit-token-required (mediawiki-api-version-requires-edit-tokens
                          (cdr (assq 'generator general))))))

(defun mediawiki-api-extract-version-number (version-string)
  "Extract numeric version from VERSION-STRING.
Returns a list of version components (major minor patch) or nil."
  (when (and version-string (stringp version-string))
    (if (string-match "MediaWiki \\([0-9]+\\)\\.\\([0-9]+\\)\\(?:\\.\\([0-9]+\\)\\)?" version-string)
        (list (string-to-number (match-string 1 version-string))
              (string-to-number (match-string 2 version-string))
              (if (match-string 3 version-string)
                  (string-to-number (match-string 3 version-string))
                0))
      nil)))

(defun mediawiki-api-parse-namespaces (namespaces)
  "Parse NAMESPACES data from site info."
  (when namespaces
    (mapcar (lambda (ns)
              (list :id (cdr (assq 'id ns))
                    :name (cdr (assq 'name ns))
                    :canonical (cdr (assq 'canonical ns))
                    :content (not (null (cdr (assq 'content ns))))))
            namespaces)))

(defun mediawiki-api-parse-extensions (extensions)
  "Parse EXTENSIONS data from site info."
  (when extensions
    (mapcar (lambda (ext)
              (list :name (cdr (assq 'name ext))
                    :version (cdr (assq 'version ext))
                    :description (cdr (assq 'description ext))))
            extensions)))

(defun mediawiki-api-parse-rights (rights)
  "Parse RIGHTS information from site info."
  (when rights
    (list :url (cdr (assq 'url rights))
          :text (cdr (assq 'text rights)))))

(defun mediawiki-api-detect-extension (extensions extension-name)
  "Check if EXTENSION-NAME is present in EXTENSIONS list."
  (cl-some (lambda (ext)
             (string= (plist-get ext :name) extension-name))
           extensions))

(defun mediawiki-api-detect-oauth-support (extensions)
  "Detect OAuth support from EXTENSIONS list."
  (or (mediawiki-api-detect-extension extensions "OAuth")
      (mediawiki-api-detect-extension extensions "OAuthAuthentication")))

;;; Version Compatibility Functions

(defun mediawiki-api-get-version (sitename)
  "Get MediaWiki version for SITENAME.
Returns version info or nil if unavailable."
  (let ((capabilities (mediawiki-api-discover-capabilities sitename)))
    (when capabilities
      (plist-get capabilities :mediawiki-version))))

(defun mediawiki-api-version-compare (version1 version2)
  "Compare two version lists VERSION1 and VERSION2.
Returns -1 if version1 < version2, 0 if equal, 1 if version1 > version2."
  (cond
   ((null version1) (if (null version2) 0 -1))
   ((null version2) 1)
   (t
    (let ((v1-major (or (nth 0 version1) 0))
          (v1-minor (or (nth 1 version1) 0))
          (v1-patch (or (nth 2 version1) 0))
          (v2-major (or (nth 0 version2) 0))
          (v2-minor (or (nth 1 version2) 0))
          (v2-patch (or (nth 2 version2) 0)))
      (cond
       ((< v1-major v2-major) -1)
       ((> v1-major v2-major) 1)
       ((< v1-minor v2-minor) -1)
       ((> v1-minor v2-minor) 1)
       ((< v1-patch v2-patch) -1)
       ((> v1-patch v2-patch) 1)
       (t 0))))))

(defun mediawiki-api-version-at-least (sitename min-version)
  "Check if SITENAME has MediaWiki version at least MIN-VERSION.
MIN-VERSION should be a list like (1 35 0)."
  (let ((site-version (mediawiki-api-get-version sitename)))
    (and site-version
         (>= (mediawiki-api-version-compare site-version min-version) 0))))

(defun mediawiki-api-version-requires-edit-tokens (version-string)
  "Check if VERSION-STRING indicates edit tokens are required.
Edit tokens became mandatory in MediaWiki 1.24."
  (let ((version (mediawiki-api-extract-version-number version-string)))
    (if version
        (mediawiki-api-version-compare version '(1 24 0))
      t))) ; Assume modern version if we can't parse

;;; Capability Checking Functions

(defun mediawiki-api-supports-feature (sitename feature)
  "Check if SITENAME supports FEATURE.
FEATURE can be :oauth, :flow, :visual-editor, :mobile, :upload, etc."
  (let ((capabilities (mediawiki-api-discover-capabilities sitename)))
    (when capabilities
      (plist-get capabilities feature))))

(defun mediawiki-api-get-namespace-info (sitename namespace-id)
  "Get namespace information for NAMESPACE-ID on SITENAME."
  (let ((capabilities (mediawiki-api-discover-capabilities sitename)))
    (when capabilities
      (let ((namespaces (plist-get capabilities :namespaces)))
        (cl-find-if (lambda (ns) (= (plist-get ns :id) namespace-id))
                    namespaces)))))

(defun mediawiki-api-validate-endpoint (sitename)
  "Validate that SITENAME has a working MediaWiki API endpoint.
Returns validation result with status and details."
  (condition-case err
      (let ((capabilities (mediawiki-api-discover-capabilities sitename t)))
        (if capabilities
            (list :status 'success
                  :version (plist-get capabilities :mediawiki-version)
                  :api-version (plist-get capabilities :api-version)
                  :features (mediawiki-api-summarize-features capabilities))
          (list :status 'error
                :error "Failed to retrieve site information")))
    (error
     (list :status 'error
           :error (error-message-string err)))))

(defun mediawiki-api-summarize-features (capabilities)
  "Summarize key features from CAPABILITIES for display."
  (let ((features '()))
    (when (plist-get capabilities :supports-oauth)
      (push "OAuth" features))
    (when (plist-get capabilities :supports-flow)
      (push "Flow" features))
    (when (plist-get capabilities :supports-visual-editor)
      (push "VisualEditor" features))
    (when (plist-get capabilities :upload-enabled)
      (push "File Upload" features))
    features))

;;; Fallback Mechanisms

(defun mediawiki-api-call-with-fallback (sitename action params callback &optional error-callback)
  "Make API call with automatic fallback for older MediaWiki versions.
Implements requirement 1.1 for fallback mechanisms."
  (let ((version (mediawiki-api-get-version sitename)))

    ;; Apply version-specific parameter adjustments
    (setq params (mediawiki-api-adjust-params-for-version params version))

    ;; Make the API call
    (mediawiki-api-call-async
     sitename action params
     callback
     (lambda (error-response)
       ;; Try fallback if the error suggests version incompatibility
       (if (mediawiki-api-should-try-fallback error-response)
           (mediawiki-api-try-fallback-call
            sitename action params callback error-callback error-response)
         (when error-callback
           (funcall error-callback error-response)))))))

(defun mediawiki-api-adjust-params-for-version (params version)
  "Adjust PARAMS based on MediaWiki VERSION compatibility.
Returns modified parameter list."
  (let ((adjusted-params (copy-sequence params)))

    ;; For older versions, some parameters may need different names or formats
    (when (and version (mediawiki-api-version-compare version '(1 25 0)) < 0)
      ;; Adjust for pre-1.25 versions
      (setq adjusted-params
            (mediawiki-api-replace-param adjusted-params "formatversion" nil)))

    ;; Add other version-specific adjustments as needed
    adjusted-params))

(defun mediawiki-api-replace-param (params old-name new-name)
  "Replace parameter OLD-NAME with NEW-NAME in PARAMS list.
If NEW-NAME is nil, removes the parameter."
  (let ((result '()))
    (dolist (param params)
      (if (string= (car param) old-name)
          (when new-name
            (push (cons new-name (cdr param)) result))
        (push param result)))
    (nreverse result)))

(defun mediawiki-api-should-try-fallback (error-response)
  "Check if ERROR-RESPONSE suggests trying a fallback approach."
  (let ((error-code (mediawiki-api-get-error-code error-response)))
    (member error-code '("badformat" "unrecognizedparams" "invalidparammix"))))

(defun mediawiki-api-try-fallback-call (sitename action params callback error-callback original-error)
  "Try fallback API call for older MediaWiki versions.
If fallback also fails, calls error-callback with ORIGINAL-ERROR."
  (let ((fallback-params (mediawiki-api-create-fallback-params params)))
    (mediawiki-api-call-async
     sitename action fallback-params
     callback
     (lambda (fallback-error)
       ;; Fallback failed, return original error
       (when error-callback
         (funcall error-callback original-error))))))

(defun mediawiki-api-create-fallback-params (params)
  "Create fallback parameter list from PARAMS for older MediaWiki versions."
  (let ((fallback-params '()))
    (dolist (param params)
      (let ((name (car param))
            (value (cdr param)))
        ;; Remove parameters that might not be supported in older versions
        (unless (member name '("formatversion" "errorformat"))
          (push param fallback-params))))
    (nreverse fallback-params)))

;;; Utility Functions

(defun mediawiki-api-build-params (&rest args)
  "Build parameter list from ARGS.
ARGS should be alternating parameter names and values."
  (let ((params '())
        (i 0))
    (while (< i (length args))
      (let ((name (nth i args))
            (value (nth (1+ i) args)))
        (when value
          (push (cons name value) params))
        (setq i (+ i 2))))
    (nreverse params)))

(defun mediawiki-api-clear-capabilities-cache (&optional sitename)
  "Clear capabilities cache for SITENAME or all sites if nil."
  (if sitename
      (remhash sitename mediawiki-api-capabilities-cache)
    (clrhash mediawiki-api-capabilities-cache)))

(provide 'mediawiki-api)

;;; mediawiki-api.el ends here

;;; Error Handling with New Error System

(defun mediawiki-api-handle-error (response &optional context options)
  "Handle errors in API RESPONSE with CONTEXT and handling OPTIONS.
CONTEXT is a plist with additional context information.
OPTIONS is a plist with handling options (see `mediawiki-error-handle').

Returns t if the error was handled, nil otherwise."
  (when (not (mediawiki-api-response-success response))
    (let* ((error-obj (or (plist-get response :error-object)
                         (mediawiki-error-from-response response context))))
      (mediawiki-error-handle error-obj options))))

(defun mediawiki-api-call-with-error-handling (sitename action params &optional options)
  "Make API call to SITENAME with ACTION and PARAMS with error handling.
OPTIONS is a plist with the following keys:
- :token-type - Token type to use (defaults to nil)
- :max-retries - Maximum number of retries (defaults to 2)
- :context - Additional context information
- :on-success - Function to call on success
- :on-error - Function to call on error
- :quiet - If non-nil, suppress messages

Returns the API response or nil if an error occurred and was handled."
  (let* ((token-type (plist-get options :token-type))
         (max-retries (or (plist-get options :max-retries) 2))
         (context (or (plist-get options :context)
                     (list :sitename sitename :action action)))
         (on-success (plist-get options :on-success))
         (on-error (plist-get options :on-error))
         (quiet (plist-get options :quiet))
         (response (if token-type
                      (mediawiki-api-call-with-token sitename action params token-type)
                    (mediawiki-api-call-sync sitename action params))))

    (if (mediawiki-api-response-success response)
        (progn
          (when on-success
            (funcall on-success response))
          response)

      ;; Handle error
      (let* ((error-obj (mediawiki-error-from-response response context))
             (handled (mediawiki-error-handle error-obj
                                            (list :retry-function #'mediawiki-api-call-with-error-handling
                                                  :retry-args (list sitename action params options)
                                                  :max-retries max-retries
                                                  :on-user-intervention on-error
                                                  :on-fatal on-error
                                                  :quiet quiet))))
        (unless handled
          (when on-error
            (funcall on-error error-obj response)))

        nil))))  ; Return nil to indicate error

(defun mediawiki-api-call-async-with-error-handling (sitename action params callback &optional options)
  "Make async API call to SITENAME with ACTION and PARAMS with error handling.
CALLBACK is called with the response on success.
OPTIONS is a plist with the following keys:
- :token-type - Token type to use (defaults to nil)
- :error-callback - Function to call on error
- :max-retries - Maximum number of retries (defaults to 2)
- :context - Additional context information
- :quiet - If non-nil, suppress messages"
  (let* ((token-type (plist-get options :token-type))
         (error-callback (plist-get options :error-callback))
         (max-retries (or (plist-get options :max-retries) 2))
         (context (or (plist-get options :context)
                     (list :sitename sitename :action action)))
         (quiet (plist-get options :quiet))
         (current-retry (or (plist-get options :current-retry) 0)))

    (if token-type
        ;; With token
        (condition-case err
            (let ((token (mediawiki-session-get-token sitename token-type)))
              (if token
                  (let ((params-with-token (cons (cons (concat token-type "token") token) params)))
                    (mediawiki-api-call-async
                     sitename action params-with-token
                     ;; Success callback
                     callback
                     ;; Error callback with retry logic
                     (lambda (response)
                       (let* ((error-obj (mediawiki-error-from-response response context))
                              (retryable (mediawiki-error-retryable-p error-obj))
                              (should-retry (and retryable (< current-retry max-retries))))

                         (if should-retry
                             ;; Retry the request
                             (progn
                               (unless quiet
                                 (message "API error: %s. Retrying (%d/%d)..."
                                          (mediawiki-error-message error-obj)
                                          (1+ current-retry)
                                          max-retries))

                               ;; Calculate exponential backoff delay
                               (let ((delay (* 1.0 (expt 2 current-retry))))
                                 (run-with-timer delay nil
                                                #'mediawiki-api-call-async-with-error-handling
                                                sitename action params callback
                                                (plist-put options :current-retry (1+ current-retry)))))

                           ;; No retry, call error callback
                           (when error-callback
                             (funcall error-callback error-obj response)))))))

                ;; No token available
                (when error-callback
                  (let ((error-obj (mediawiki-error-create
                                   'auth 'error
                                   (format "Failed to obtain %s token" token-type)
                                   "token-error" 'user nil 'client context)))
                    (funcall error-callback error-obj nil)))))

          ;; Handle any errors in the token retrieval process
          (error
           (when error-callback
             (let ((error-obj (mediawiki-error-create-from-exception
                              (car err) (error-message-string err) context)))
               (funcall error-callback error-obj nil)))))

      ;; Without token (simple async call)
      (mediawiki-api-call-async
       sitename action params
       callback
       (lambda (response)
         (let* ((error-obj (mediawiki-error-from-response response context))
                (retryable (mediawiki-error-retryable-p error-obj))
                (should-retry (and retryable (< current-retry max-retries))))

           (if should-retry
               ;; Retry the request
               (progn
                 (unless quiet
                   (message "API error: %s. Retrying (%d/%d)..."
                            (mediawiki-error-message error-obj)
                            (1+ current-retry)
                            max-retries))

                 ;; Calculate exponential backoff delay
                 (let ((delay (* 1.0 (expt 2 current-retry))))
                   (run-with-timer delay nil
                                  #'mediawiki-api-call-async-with-error-handling
                                  sitename action params callback
                                  (plist-put options :current-retry (1+ current-retry)))))

             ;; No retry, call error callback
             (when error-callback
               (funcall error-callback error-obj response)))))))))
