;;; mediawiki-compat.el --- Backward compatibility layer for MediaWiki.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module provides backward compatibility shims for deprecated functions
;; and configuration migration utilities to help users upgrade from older
;; versions of MediaWiki.el.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-api)
(require 'mediawiki-session)

;;; Configuration Migration

(defcustom mediawiki-compat-enable-shims t
  "Enable backward compatibility shims for deprecated functions.
When non-nil, deprecated functions will work with warnings.
When nil, deprecated functions will throw errors."
  :type 'boolean
  :tag "Enable Compatibility Shims"
  :group 'mediawiki)

(defcustom mediawiki-compat-warn-deprecated t
  "Warn when deprecated functions are used.
When non-nil, using deprecated functions will display warnings."
  :type 'boolean
  :tag "Warn About Deprecated Functions"
  :group 'mediawiki)

;;; Legacy Function Shims

(defun mediawiki-compat-warn (function-name replacement)
  "Warn about deprecated FUNCTION-NAME and suggest REPLACEMENT."
  (when mediawiki-compat-warn-deprecated
    (display-warning 'mediawiki-deprecated
                     (format "%s is deprecated. Use %s instead."
                             function-name replacement)
                     :warning)))

;;;###autoload
(defun mediawiki-api-call (sitename action &optional args)
  "Legacy API call function - DEPRECATED.
Use `mediawiki-api-call-sync' or `mediawiki-api-call-async' instead.
SITENAME is the site name, ACTION is the API action, ARGS are parameters."
  (mediawiki-compat-warn 'mediawiki-api-call 
                         'mediawiki-api-call-sync)
  
  (unless mediawiki-compat-enable-shims
    (error "mediawiki-api-call is deprecated. Use mediawiki-api-call-sync instead"))
  
  ;; Convert legacy args format to modern format
  (let ((params '()))
    (dolist (arg (delq nil args))
      (when (consp arg)
        ;; Filter out "action" parameter since it's passed separately
        (unless (string= (car arg) "action")
          (push (cons (car arg) (cdr arg)) params))))
    
    ;; Make synchronous API call using modern system
    (let ((response (mediawiki-api-call-sync sitename action (nreverse params))))
      (if (mediawiki-api-response-success response)
          ;; Convert modern response back to legacy format
          (let ((data (mediawiki-api-response-data response)))
            ;; Legacy format expected: (api (action-name . result))
            (list 'api (cons (intern action) data)))
        ;; Handle error in legacy format  
        (error "API call failed: %s" 
               (or (car (mediawiki-api-response-errors response))
                   "Unknown error"))))))

;;; Legacy Configuration Migration

(defvar mediawiki-compat-legacy-config-detected nil
  "Whether legacy configuration has been detected.")

(defvar mediawiki-compat-migration-log '()
  "Log of configuration migrations performed.")

(defun mediawiki-compat-detect-legacy-config ()
  "Detect if legacy configuration variables are set."
  (let ((legacy-vars '()))
    
    ;; Check for old site configuration format
    (when (boundp 'mediawiki-site-alist)
      (dolist (site mediawiki-site-alist)
        (let ((site-config (cdr site)))
          ;; Check if using old format (list instead of struct)
          (when (and (listp site-config) 
                     (not (mediawiki-site-config-p site-config)))
            (push 'old-site-format legacy-vars)))))
    
    ;; Check for deprecated variables
    (when (boundp 'mediawiki-site-url)
      (push 'mediawiki-site-url legacy-vars))
    (when (boundp 'mediawiki-username)
      (push 'mediawiki-username legacy-vars))
    (when (boundp 'mediawiki-password)
      (push 'mediawiki-password legacy-vars))
    
    (setq mediawiki-compat-legacy-config-detected legacy-vars)
    legacy-vars))

(defun mediawiki-compat-migrate-site-config (old-site-entry)
  "Migrate OLD-SITE-ENTRY from legacy format to modern format.
Returns a modern mediawiki-site-config struct."
  (let* ((name (car old-site-entry))
         (old-config (cdr old-site-entry))
         (url (if (listp old-config)
                  (nth 0 old-config)
                old-config))
         (username (when (listp old-config) (nth 1 old-config)))
         (password (when (listp old-config) (nth 2 old-config)))
         (domain (when (listp old-config) (nth 3 old-config))))
    
    ;; Create auth-config with legacy credentials
    (let ((auth-config (when (or username password domain)
                         (list :username username
                               :password password  
                               :domain domain))))
      
      (make-mediawiki-site-config
       :name name
       :url url
       :username username
       :auth-method (if username 'basic 'none)
       :auth-config auth-config))))

(defun mediawiki-compat-migrate-legacy-config ()
  "Migrate legacy configuration to modern format."
  (interactive)
  (let ((legacy-vars (mediawiki-compat-detect-legacy-config))
        (migrated-count 0))
    
    (when legacy-vars
      (message "Migrating legacy MediaWiki configuration...")
      
      ;; Migrate site configurations
      (when (memq 'old-site-format legacy-vars)
        (let ((new-sites '()))
          (dolist (site-entry mediawiki-site-alist)
            (let ((site-config (cdr site-entry)))
              (if (and (listp site-config) 
                       (not (mediawiki-site-config-p site-config)))
                  (progn
                    (push (cons (car site-entry)
                                (mediawiki-compat-migrate-site-config site-entry))
                          new-sites)
                    (cl-incf migrated-count)
                    (push (format "Migrated site: %s" (car site-entry))
                          mediawiki-compat-migration-log))
                (push site-entry new-sites))))
          (setq mediawiki-site-alist (nreverse new-sites))))
      
      ;; Migrate individual variables
      (when (boundp 'mediawiki-site-url)
        (unless (mediawiki-get-site "default")
          (mediawiki-add-site 
           (make-mediawiki-site-config
            :name "default"
            :url mediawiki-site-url
            :auth-method 'basic))
          (cl-incf migrated-count)
          (push "Created default site from mediawiki-site-url"
                mediawiki-compat-migration-log)))
      
      (when migrated-count
        (message "Migrated %d configuration items. See migration log for details."
                 migrated-count))
      
      migrated-count)))

;;;###autoload
(defun mediawiki-compat-check-and-migrate ()
  "Check for legacy configuration and offer to migrate."
  (interactive)
  (let ((legacy-vars (mediawiki-compat-detect-legacy-config)))
    (when legacy-vars
      (when (yes-or-no-p 
             (format "Legacy MediaWiki configuration detected (%s). Migrate now? "
                     (mapconcat #'symbol-name legacy-vars ", ")))
        (mediawiki-compat-migrate-legacy-config)))))

(defun mediawiki-compat-show-migration-log ()
  "Show the configuration migration log."
  (interactive)
  (if mediawiki-compat-migration-log
      (let ((buffer (get-buffer-create "*MediaWiki Migration Log*")))
        (with-current-buffer buffer
          (erase-buffer)
          (insert "MediaWiki Configuration Migration Log\n")
          (insert "==========================================\n\n")
          (dolist (entry (reverse mediawiki-compat-migration-log))
            (insert "• " entry "\n"))
          (goto-char (point-min))
          (special-mode))
        (display-buffer buffer))
    (message "No migration log available")))

;;; Legacy Variable Aliases

;; Create aliases for commonly used legacy variables
(defvaralias 'mediawiki-site-url 'mediawiki-site-default
  "Legacy alias for mediawiki-site-default.")

;; Mark as obsolete
(make-obsolete-variable 'mediawiki-site-url 'mediawiki-site-default "2.5.0")

;;; Legacy Function Aliases

(defalias 'mediawiki-login 'mediawiki-do-login
  "Legacy alias for mediawiki-do-login.")
(make-obsolete 'mediawiki-login 'mediawiki-do-login "2.5.0")

(defalias 'mediawiki-logout 'mediawiki-do-logout  
  "Legacy alias for mediawiki-do-logout.")
(make-obsolete 'mediawiki-logout 'mediawiki-do-logout "2.5.0")

;;; Initialization

(defun mediawiki-compat-initialize ()
  "Initialize the compatibility layer."
  ;; Check for legacy config on load
  (when (mediawiki-compat-detect-legacy-config)
    (run-with-idle-timer 1 nil #'mediawiki-compat-check-and-migrate)))

;; Initialize compatibility layer when loaded
(mediawiki-compat-initialize)

(provide 'mediawiki-compat)

;;; mediawiki-compat.el ends here