;;; mediawiki-http.el --- HTTP communication layer for MediaWiki -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module provides the HTTP communication layer for MediaWiki API calls.
;; It handles both asynchronous and synchronous HTTP requests with proper
;; error handling and response processing.

;;; Code:

(require 'url)
(require 'url-http)
(require 'json)
(require 'mediawiki-core)

;;; HTTP Request Functions

(defun mediawiki-http-request-async (url method data callback &optional error-callback)
  "Make an asynchronous HTTP request to URL using METHOD with DATA.
CALLBACK is called with the response on success.
ERROR-CALLBACK is called with error information on failure.

This function implements requirement 4.1 by using asynchronous operations
to prevent blocking the Emacs interface."
  (mediawiki-debug-request url method data)

  (let ((url-request-method method)
        (url-request-data (when data
                           (if (hash-table-p data)
                               (mediawiki-http-encode-form-data data)
                             data)))
        (url-request-extra-headers
         (append
          (when data
            '(("Content-Type" . "application/x-www-form-urlencoded")))
          '(("User-Agent" . "MediaWiki.el/2.0 (Emacs)")))))

    (condition-case err
        (url-retrieve url
                      (lambda (status)
                        (mediawiki-http-handle-async-response
                         status callback error-callback url method))
                      nil t)
      (error
       (when error-callback
         (funcall error-callback
                  (mediawiki-http-create-error-response
                   'network-error
                   (format "Failed to initiate request: %s" (error-message-string err))
                   nil)))))))

(defun mediawiki-http-request-sync (url method data &optional timeout)
  "Make a synchronous HTTP request to URL using METHOD with DATA.
This is a wrapper around the async function that blocks until completion.
TIMEOUT specifies the maximum time to wait (defaults to `mediawiki-request-timeout').
Returns the response data or signals an error.

This function provides backward compatibility while leveraging the async
implementation for consistent behavior and error handling.
Supports cancellation via keyboard interrupt (\\[keyboard-quit])."
  (let ((timeout (or timeout mediawiki-request-timeout))
        (result nil)
        (error-result nil)
        (completed nil)
        (cancelled nil)
        (start-time (current-time))
        (timer nil))

    ;; Set up completion callbacks
    (let ((success-callback
           (lambda (response)
             (setq result response
                   completed t)))
          (error-callback
           (lambda (error-response)
             (setq error-result error-response
                   completed t))))

      ;; Start the async request
      (mediawiki-http-request-async url method data success-callback error-callback)

      ;; Set up timeout timer
      (when (> timeout 0)
        (setq timer
              (run-at-time timeout nil
                           (lambda ()
                             (unless completed
                               (setq error-result
                                     (mediawiki-http-create-error-response
                                      'timeout-error
                                      (format "Request timed out after %d seconds" timeout)
                                      nil)
                                     completed t))))))

      ;; Wait for completion with periodic checks and cancellation support
      (condition-case err
          (while (not completed)
            ;; Check if we've exceeded timeout manually (fallback)
            (when (and (> timeout 0)
                       (> (float-time (time-subtract (current-time) start-time)) timeout))
              (setq error-result
                    (mediawiki-http-create-error-response
                     'timeout-error
                     (format "Request timed out after %d seconds" timeout)
                     nil)
                    completed t))

            ;; Allow Emacs to process other events (including keyboard interrupts)
            (unless completed
              (accept-process-output nil 0.1)))
        (quit
         ;; Handle cancellation (C-g)
         (setq cancelled t
               completed t)))

      ;; Clean up timer
      (when timer
        (cancel-timer timer))

      ;; Return result, signal error, or handle cancellation
      (cond
       (cancelled
        (signal 'quit nil))  ; Re-signal the quit to maintain standard behavior
       (result result)
       (error-result
        (error "HTTP request failed: %s" (plist-get error-result :error)))
       (t
        (error "HTTP request failed: Unknown error"))))))

;;; Response Processing Utilities

(defun mediawiki-http-validate-response (response)
  "Validate RESPONSE structure and content.
Returns t if response is valid, nil otherwise.
Logs validation errors when debugging is enabled."
  (let ((valid t)
        (errors '()))

    ;; Check required fields
    (unless (plist-member response :success)
      (push "Missing :success field" errors)
      (setq valid nil))

    (unless (plist-member response :status-code)
      (push "Missing :status-code field" errors)
      (setq valid nil))

    ;; Validate status code if present
    (let ((status-code (plist-get response :status-code)))
      (when (and status-code (not (integerp status-code)))
        (push "Invalid status code type" errors)
        (setq valid nil)))

    ;; Validate headers if present
    (let ((headers (plist-get response :headers)))
      (when (and headers (not (listp headers)))
        (push "Invalid headers format" errors)
        (setq valid nil)))

    ;; Log validation errors
    (when (and errors (mediawiki-debug-enabled-p))
      (mediawiki-debug-log "Response validation failed: %s"
                          (mapconcat 'identity errors "; ")))

    valid))

(defun mediawiki-http-parse-json-response (response)
  "Parse JSON content from RESPONSE body.
Returns parsed JSON data or nil if parsing fails.
Logs parsing errors when debugging is enabled."
  (let ((body (plist-get response :body)))
    (when (and body (stringp body) (> (length body) 0))
      (condition-case err
          (if (fboundp 'json-parse-string)
              ;; Use modern json-parse-string if available (Emacs 27+)
              (json-parse-string body :object-type 'plist :array-type 'list)
            ;; Fall back to json.el for older Emacs versions
            (let ((json-object-type 'plist)
                  (json-array-type 'vector))
              (json-read-from-string body)))
        (json-parse-error
         (when (mediawiki-debug-enabled-p)
           (mediawiki-debug-log "JSON parsing failed: %s\nBody: %s"
                               (error-message-string err) body))
         nil)
        (json-error
         (when (mediawiki-debug-enabled-p)
           (mediawiki-debug-log "JSON parsing failed: %s\nBody: %s"
                               (error-message-string err) body))
         nil)
        (error
         (when (mediawiki-debug-enabled-p)
           (mediawiki-debug-log "Unexpected error parsing JSON: %s"
                               (error-message-string err)))
         nil)))))

(defun mediawiki-http-extract-response-info (response)
  "Extract useful information from RESPONSE for logging and debugging.
Returns a plist with extracted information."
  (let ((status-code (plist-get response :status-code))
        (content-type (mediawiki-http-get-content-type response))
        (content-length (mediawiki-http-get-header response "content-length"))
        (server (mediawiki-http-get-header response "server"))
        (body-length (length (or (plist-get response :body) ""))))

    (list :status-code status-code
          :content-type content-type
          :content-length content-length
          :actual-body-length body-length
          :server server
          :success (plist-get response :success)
          :error-type (plist-get response :error-type))))

(defun mediawiki-http-log-response (response &optional request-info)
  "Log RESPONSE details for debugging purposes.
REQUEST-INFO is optional additional context about the original request."
  (when (mediawiki-debug-enabled-p)
    (let ((info (mediawiki-http-extract-response-info response))
          (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))

      (mediawiki-debug-log "=== HTTP Response [%s] ===" timestamp)

      ;; Log request context if available
      (when request-info
        (mediawiki-debug-log "Request: %s %s"
                            (plist-get request-info :method)
                            (plist-get request-info :url)))

      ;; Log response summary
      (mediawiki-debug-log "Status: %s (%s)"
                          (plist-get info :status-code)
                          (if (plist-get info :success) "SUCCESS" "ERROR"))

      (when (plist-get info :content-type)
        (mediawiki-debug-log "Content-Type: %s" (plist-get info :content-type)))

      (when (plist-get info :server)
        (mediawiki-debug-log "Server: %s" (plist-get info :server)))

      (mediawiki-debug-log "Body Length: %d bytes" (plist-get info :actual-body-length))

      ;; Log error information if present
      (when (not (plist-get info :success))
        (mediawiki-debug-log "Error Type: %s" (plist-get info :error-type))
        (let ((error-msg (plist-get response :error)))
          (when error-msg
            (mediawiki-debug-log "Error Message: %s" error-msg))))

      ;; Log response body if it's small enough and in debug mode
      (let ((body (plist-get response :body)))
        (when (and body
                   (< (length body) mediawiki-debug-max-body-length)
                   (mediawiki-debug-verbose-p))
          (mediawiki-debug-log "Response Body:\n%s" body)))

      (mediawiki-debug-log "=== End Response ==="))))

(defun mediawiki-http-check-response-health (response)
  "Check RESPONSE for common issues and return health status.
Returns a plist with health information and warnings."
  (let ((warnings '())
        (status :healthy))

    ;; Check for missing content-type
    (unless (mediawiki-http-get-content-type response)
      (push "Missing Content-Type header" warnings))

    ;; Check for suspicious content-type for API responses
    (let ((content-type (mediawiki-http-get-content-type response)))
      (when (and content-type
                 (not (string-match-p "application/json\\|text/plain" content-type)))
        (push (format "Unexpected Content-Type: %s" content-type) warnings)))

    ;; Check for empty body on successful responses
    (when (and (plist-get response :success)
               (or (not (plist-get response :body))
                   (string-empty-p (plist-get response :body))))
      (push "Empty response body on successful request" warnings)
      (setq status :warning))

    ;; Check for very large responses
    (let ((body-length (length (or (plist-get response :body) ""))))
      (when (> body-length mediawiki-large-response-threshold)
        (push (format "Large response body: %d bytes" body-length) warnings)
        (when (eq status :healthy)
          (setq status :warning))))

    ;; Check for rate limiting indicators
    (let ((status-code (plist-get response :status-code)))
      (when (= status-code 429)
        (push "Rate limiting detected" warnings)
        (setq status :rate-limited)))

    (list :status status
          :warnings warnings
          :warning-count (length warnings))))

;;; Response Handling

(defun mediawiki-http-handle-async-response (status callback error-callback url method)
  "Handle asynchronous HTTP response with STATUS.
Call CALLBACK on success or ERROR-CALLBACK on failure.
URL and METHOD are used for error reporting.

This function implements requirement 5.3 by distinguishing between
different types of errors (network, authentication, server errors)."
  (condition-case err
      (let* ((response (mediawiki-http-parse-response status url method))
             (request-info (list :url url :method method)))

        ;; Validate response structure
        (unless (mediawiki-http-validate-response response)
          (mediawiki-debug-log "Response validation failed for %s %s" method url))

        ;; Log response details
        (mediawiki-http-log-response response request-info)

        ;; Check response health and log warnings
        (let ((health (mediawiki-http-check-response-health response)))
          (when (plist-get health :warnings)
            (mediawiki-debug-log "Response health warnings: %s"
                                (mapconcat 'identity (plist-get health :warnings) "; "))))

        ;; Call appropriate callback
        (if (mediawiki-http-response-success-p response)
            (when callback
              (funcall callback response))
          (when error-callback
            (funcall error-callback response))))
    (error
     (let ((error-response (mediawiki-http-create-error-response
                           'parsing-error
                           (format "Failed to parse response: %s" (error-message-string err))
                           nil)))
       (mediawiki-debug-log "Response parsing error: %s" (error-message-string err))
       (when error-callback
         (funcall error-callback error-response))))))



(defun mediawiki-http-parse-response (status &optional url method)
  "Parse HTTP response from current buffer with STATUS.
URL and METHOD are optional parameters for enhanced error reporting.
Returns a plist with response information.

This function implements requirement 5.3 by classifying different
types of errors (network, authentication, server errors)."
  (goto-char (point-min))

  ;; Check for URL library errors first
  (if (and status (plist-get status :error))
      (mediawiki-http-create-error-response
       'network-error
       (format "Network error: %s" (plist-get status :error))
       nil)

    ;; Parse HTTP status line
    (let ((status-code nil)
          (headers nil)
          (body nil))

      (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
        (setq status-code (string-to-number (match-string 1))))

      ;; Parse headers
      (goto-char (point-min))
      (when (re-search-forward "^$" nil t)
        (setq headers (mediawiki-http-parse-headers (point-min) (point)))
        (setq body (buffer-substring-no-properties (1+ (point)) (point-max))))

      ;; Determine success and error classification
      (let ((success (and status-code (>= status-code 200) (< status-code 300))))
        (if success
            (list :success t
                  :status-code status-code
                  :headers headers
                  :body body
                  :error nil
                  :error-type nil)
          ;; Classify error types based on HTTP status codes
          (let ((error-type (mediawiki-http-classify-error status-code))
                (error-message (mediawiki-http-format-error-message status-code url method)))
            (list :success nil
                  :status-code status-code
                  :headers headers
                  :body body
                  :error error-message
                  :error-type error-type)))))))

(defun mediawiki-http-parse-headers (start end)
  "Parse HTTP headers between START and END positions.
Returns an alist of header name/value pairs."
  (let ((headers '()))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^\\([^:]+\\):\\s-*\\(.*\\)$" end t)
        (push (cons (downcase (match-string 1)) (match-string 2)) headers)))
    (nreverse headers)))

(defun mediawiki-http-response-success-p (response)
  "Check if RESPONSE indicates success."
  (plist-get response :success))

;;; Error Handling Functions

(defun mediawiki-http-create-error-response (error-type error-message status-code)
  "Create a standardized error response.
ERROR-TYPE classifies the type of error (network-error, auth-error, etc.).
ERROR-MESSAGE is a human-readable description.
STATUS-CODE is the HTTP status code if available."
  (list :success nil
        :status-code status-code
        :headers nil
        :body nil
        :error error-message
        :error-type error-type))

(defun mediawiki-http-classify-error (status-code)
  "Classify error type based on HTTP STATUS-CODE.
Returns a symbol indicating the error category for requirement 5.3."
  (cond
   ((not status-code) 'network-error)
   ((= status-code 401) 'auth-error)
   ((= status-code 403) 'auth-error)
   ((= status-code 404) 'not-found-error)
   ((= status-code 429) 'rate-limit-error)
   ((and (>= status-code 400) (< status-code 500)) 'client-error)
   ((and (>= status-code 500) (< status-code 600)) 'server-error)
   (t 'unknown-error)))

(defun mediawiki-http-format-error-message (status-code url method)
  "Format a user-friendly error message based on STATUS-CODE.
URL and METHOD provide context for the error message."
  (let ((base-message
         (cond
          ((not status-code) "Network connection failed")
          ((= status-code 401) "Authentication required")
          ((= status-code 403) "Access forbidden - check permissions")
          ((= status-code 404) "Resource not found")
          ((= status-code 429) "Rate limit exceeded - please wait before retrying")
          ((= status-code 500) "Internal server error")
          ((= status-code 502) "Bad gateway - server temporarily unavailable")
          ((= status-code 503) "Service unavailable - server temporarily down")
          ((= status-code 504) "Gateway timeout - server took too long to respond")
          ((and (>= status-code 400) (< status-code 500))
           (format "Client error (HTTP %d)" status-code))
          ((and (>= status-code 500) (< status-code 600))
           (format "Server error (HTTP %d)" status-code))
          (t (format "HTTP error %d" status-code)))))
    (if (and url method)
        (format "%s when %s %s" base-message method url)
      base-message)))

;;; Utility Functions

(defun mediawiki-http-encode-form-data (data)
  "Encode hash table DATA as application/x-www-form-urlencoded."
  (mapconcat
   (lambda (key)
     (let ((value (gethash key data)))
       (format "%s=%s"
               (url-hexify-string (format "%s" key))
               (url-hexify-string (format "%s" value)))))
   (hash-table-keys data)
   "&"))

(defun mediawiki-http-get-header (response header-name)
  "Get HEADER-NAME from RESPONSE headers."
  (cdr (assoc (downcase header-name) (plist-get response :headers))))

(defun mediawiki-http-get-content-type (response)
  "Get content type from RESPONSE."
  (mediawiki-http-get-header response "content-type"))

(defun mediawiki-http-is-json-response (response)
  "Check if RESPONSE contains JSON content."
  (let ((content-type (mediawiki-http-get-content-type response)))
    (and content-type
         (string-match-p "application/json" content-type))))

(defun mediawiki-http-is-success-status (status-code)
  "Check if STATUS-CODE indicates success (2xx range)."
  (and status-code (>= status-code 200) (< status-code 300)))

(defun mediawiki-http-is-client-error (status-code)
  "Check if STATUS-CODE indicates client error (4xx range)."
  (and status-code (>= status-code 400) (< status-code 500)))

(defun mediawiki-http-is-server-error (status-code)
  "Check if STATUS-CODE indicates server error (5xx range)."
  (and status-code (>= status-code 500) (< status-code 600)))

(defun mediawiki-http-is-redirect-status (status-code)
  "Check if STATUS-CODE indicates redirect (3xx range)."
  (and status-code (>= status-code 300) (< status-code 400)))

(defun mediawiki-http-get-response-size (response)
  "Get the size of RESPONSE body in bytes."
  (length (or (plist-get response :body) "")))

(defun mediawiki-http-response-has-body (response)
  "Check if RESPONSE has a non-empty body."
  (let ((body (plist-get response :body)))
    (and body (stringp body) (> (length body) 0))))

(defun mediawiki-http-get-status-description (status-code)
  "Get a human-readable description for STATUS-CODE."
  (cond
   ((= status-code 200) "OK")
   ((= status-code 201) "Created")
   ((= status-code 204) "No Content")
   ((= status-code 301) "Moved Permanently")
   ((= status-code 302) "Found")
   ((= status-code 304) "Not Modified")
   ((= status-code 400) "Bad Request")
   ((= status-code 401) "Unauthorized")
   ((= status-code 403) "Forbidden")
   ((= status-code 404) "Not Found")
   ((= status-code 405) "Method Not Allowed")
   ((= status-code 429) "Too Many Requests")
   ((= status-code 500) "Internal Server Error")
   ((= status-code 502) "Bad Gateway")
   ((= status-code 503) "Service Unavailable")
   ((= status-code 504) "Gateway Timeout")
   (t (format "HTTP %d" status-code))))

(provide 'mediawiki-http)

;;; mediawiki-http.el ends here
