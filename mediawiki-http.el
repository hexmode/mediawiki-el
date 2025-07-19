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

(defun mediawiki-http-request-sync (url method data)
  "Make a synchronous HTTP request to URL using METHOD with DATA.
Returns the response data or signals an error.

This function provides backward compatibility while using the same
enhanced error handling as the async version."
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
        (with-current-buffer (url-retrieve-synchronously url t nil mediawiki-request-timeout)
          (mediawiki-http-handle-sync-response url method))
      (error
       (error "HTTP request failed: %s" (error-message-string err))))))

;;; Response Handling

(defun mediawiki-http-handle-async-response (status callback error-callback url method)
  "Handle asynchronous HTTP response with STATUS.
Call CALLBACK on success or ERROR-CALLBACK on failure.
URL and METHOD are used for error reporting.

This function implements requirement 5.3 by distinguishing between
different types of errors (network, authentication, server errors)."
  (condition-case err
      (let ((response (mediawiki-http-parse-response status url method)))
        (mediawiki-debug-response response)
        (if (mediawiki-http-response-success-p response)
            (when callback
              (funcall callback response))
          (when error-callback
            (funcall error-callback response))))
    (error
     (when error-callback
       (funcall error-callback
        (mediawiki-http-create-error-response
         'parsing-error
         (format "Failed to parse response: %s" (error-message-string err))
         nil))))))

(defun mediawiki-http-handle-sync-response (url method)
  "Handle synchronous HTTP response in current buffer.
URL and METHOD are used for enhanced error reporting.
Returns parsed response or signals an error."
  (let ((response (mediawiki-http-parse-response nil url method)))
    (mediawiki-debug-response response)
    (if (mediawiki-http-response-success-p response)
        response
      (error "HTTP request failed: %s" (plist-get response :error)))))

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

(provide 'mediawiki-http)

;;; mediawiki-http.el ends here
