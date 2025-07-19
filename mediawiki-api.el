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

;;; Request Building

(defun mediawiki-api-make-url (sitename)
  "Build API URL for SITENAME."
  (let ((site (mediawiki-get-site sitename)))
    (if site
        (or (mediawiki-site-api-url site)
            (concat (mediawiki-site-url site) "api.php"))
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
  "Parse HTTP-RESPONSE into a mediawiki-api-response structure."
  (let ((body (plist-get http-response :body))
        (success (plist-get http-response :success)))
    
    (if (not success)
        (make-mediawiki-api-response
         :success nil
         :errors (list (list :code "http-error"
                            :info (plist-get http-response :error)))
         :raw-response http-response)
      
      (condition-case err
          (let ((json-data (json-read-from-string body)))
            (mediawiki-api-parse-json-response json-data http-response))
        (error
         (make-mediawiki-api-response
          :success nil
          :errors (list (list :code "json-parse-error"
                             :info (format "Failed to parse JSON: %s" err)))
          :raw-response http-response))))))

(defun mediawiki-api-parse-json-response (json-data http-response)
  "Parse JSON-DATA from HTTP-RESPONSE into API response structure."
  (let ((errors (cdr (assq 'error json-data)))
        (warnings (cdr (assq 'warnings json-data)))
        (data (mediawiki-api-extract-action-data json-data)))
    
    (make-mediawiki-api-response
     :success (not errors)
     :data data
     :warnings (mediawiki-api-parse-warnings warnings)
     :errors (when errors (list errors))
     :raw-response http-response)))

(defun mediawiki-api-extract-action-data (json-data)
  "Extract action-specific data from JSON-DATA."
  ;; Remove standard API wrapper elements
  (let ((filtered-data (copy-alist json-data)))
    (setq filtered-data (assq-delete-all 'error filtered-data))
    (setq filtered-data (assq-delete-all 'warnings filtered-data))
    (setq filtered-data (assq-delete-all 'servedby filtered-data))
    filtered-data))

(defun mediawiki-api-parse-warnings (warnings)
  "Parse WARNINGS from API response."
  (when warnings
    (if (listp warnings)
        warnings
      (list warnings))))

;;; Response Validation

(defun mediawiki-api-response-success (response)
  "Check if API RESPONSE indicates success."
  (mediawiki-api-response-success response))

(defun mediawiki-api-get-error-code (response)
  "Get error code from API RESPONSE."
  (let ((errors (mediawiki-api-response-errors response)))
    (when errors
      (cdr (assq 'code (car errors))))))

(defun mediawiki-api-get-error-info (response)
  "Get error information from API RESPONSE."
  (let ((errors (mediawiki-api-response-errors response)))
    (when errors
      (cdr (assq 'info (car errors))))))

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

(provide 'mediawiki-api)

;;; mediawiki-api.el ends here