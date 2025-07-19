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
ERROR-CALLBACK is called with error information on failure."
  (mediawiki-debug-request url method data)
  
  (let ((url-request-method method)
        (url-request-data (when data
                           (if (hash-table-p data)
                               (mediawiki-http-encode-form-data data)
                             data)))
        (url-request-extra-headers
         (when data
           '(("Content-Type" . "application/x-www-form-urlencoded")))))
    
    (url-retrieve url
                  (lambda (status)
                    (mediawiki-http-handle-async-response
                     status callback error-callback))
                  nil t)))

(defun mediawiki-http-request-sync (url method data)
  "Make a synchronous HTTP request to URL using METHOD with DATA.
Returns the response data or signals an error."
  (mediawiki-debug-request url method data)
  
  (let ((url-request-method method)
        (url-request-data (when data
                           (if (hash-table-p data)
                               (mediawiki-http-encode-form-data data)
                             data)))
        (url-request-extra-headers
         (when data
           '(("Content-Type" . "application/x-www-form-urlencoded")))))
    
    (with-current-buffer (url-retrieve-synchronously url t nil mediawiki-request-timeout)
      (mediawiki-http-handle-sync-response))))

;;; Response Handling

(defun mediawiki-http-handle-async-response (status callback error-callback)
  "Handle asynchronous HTTP response with STATUS.
Call CALLBACK on success or ERROR-CALLBACK on failure."
  (condition-case err
      (let ((response (mediawiki-http-parse-response status)))
        (mediawiki-debug-response response)
        (if (mediawiki-http-response-success-p response)
            (when callback
              (funcall callback response))
          (when error-callback
            (funcall error-callback response))))
    (error
     (when error-callback
       (funcall error-callback (list :error err))))))

(defun mediawiki-http-handle-sync-response ()
  "Handle synchronous HTTP response in current buffer.
Returns parsed response or signals an error."
  (let ((response (mediawiki-http-parse-response nil)))
    (mediawiki-debug-response response)
    (if (mediawiki-http-response-success-p response)
        response
      (error "HTTP request failed: %s" (plist-get response :error)))))

(defun mediawiki-http-parse-response (status)
  "Parse HTTP response from current buffer with STATUS.
Returns a plist with response information."
  (goto-char (point-min))
  
  ;; Check for URL library errors
  (when status
    (let ((error-status (plist-get status :error)))
      (when error-status
        (return (list :success nil
                     :error (format "Network error: %s" error-status)
                     :status-code nil
                     :headers nil
                     :body nil)))))
  
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
    
    (list :success (and status-code (>= status-code 200) (< status-code 300))
          :status-code status-code
          :headers headers
          :body body
          :error (when (or (not status-code) (>= status-code 400))
                  (format "HTTP %s" (or status-code "unknown"))))))

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