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

(defcustom mediawiki-check-compat t
  "Whether to automatically check for and offer to migrate legacy configuration.
When non-nil, the compatibility layer will check for legacy configuration
on startup and offer to migrate it. Set to nil to disable automatic checking."
  :type 'boolean
  :tag "Check for Legacy Configuration"
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

(defvar mediawiki-compat-validation-errors '()
  "List of validation errors found during migration.")

(defvar mediawiki-compat-migration-report nil
  "Detailed report of the last migration performed.")

(defun mediawiki-compat-detect-legacy-config ()
  "Detect if legacy configuration variables are set.
Returns an alist with detailed information about each legacy configuration found."
  (let ((legacy-configs '()))

    ;; Check for old site configuration format
    (when (boundp 'mediawiki-site-alist)
      (dolist (site mediawiki-site-alist)
        (let ((site-config (cdr site)))
          ;; Check if using old format (list instead of struct)
          (when (and (listp site-config)
                     (not (mediawiki-site-config-p site-config)))
            (push `(old-site-format
                    :name ,(car site)
                    :url ,(if (listp site-config) (car site-config) site-config)
                    :username ,(when (listp site-config) (nth 1 site-config))
                    :has-password ,(when (and (listp site-config) (nth 2 site-config)) t)
                    :domain ,(when (listp site-config) (nth 3 site-config)))
                  legacy-configs)))))

    ;; Check for deprecated variables
    (when (boundp 'mediawiki-site-url)
      (push `(legacy-site-url
              :value ,mediawiki-site-url
              :type individual-variable)
            legacy-configs))
    (when (boundp 'mediawiki-username)
      (push `(legacy-username
              :value ,mediawiki-username
              :type individual-variable)
            legacy-configs))
    (when (boundp 'mediawiki-password)
      (push `(legacy-password
              :has-value ,(not (null mediawiki-password))
              :type individual-variable)
            legacy-configs))

    ;; Check for other legacy patterns
    (when (boundp 'mediawiki-api-url)
      (push `(legacy-api-url
              :value ,mediawiki-api-url
              :type individual-variable)
            legacy-configs))
    (when (boundp 'mediawiki-site-domain)
      (push `(legacy-domain
              :value ,mediawiki-site-domain
              :type individual-variable)
            legacy-configs))

    (setq mediawiki-compat-legacy-config-detected legacy-configs)
    legacy-configs))

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
         (domain (when (listp old-config) (nth 3 old-config)))
         (first-page (when (listp old-config) (nth 4 old-config))))

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
       :auth-config auth-config
       :first-page (or first-page "Main Page")))))

(defun mediawiki-compat-migrate-legacy-config ()
  "Migrate legacy configuration to modern format."
  (interactive)
  (let ((legacy-configs (mediawiki-compat-detect-legacy-config))
        (migrated-count 0))
    (let ((legacy-vars (mapcar #'car legacy-configs)))

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

      migrated-count))))

;;;###autoload
(defun mediawiki-compat-check-and-migrate ()
  "Check for legacy configuration and offer to migrate."
  (interactive)
  (let* ((legacy-configs (mediawiki-compat-detect-legacy-config))
         (legacy-vars (mapcar #'car legacy-configs)))
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

;;; Enhanced Migration Tools (Task 10.2)

(defun mediawiki-compat-validate-site-config (site-config)
  "Validate a migrated SITE-CONFIG and return list of issues found."
  (let ((issues '()))

    ;; Check required fields
    (unless (mediawiki-site-config-name site-config)
      (push "Site name is missing" issues))

    (unless (mediawiki-site-config-url site-config)
      (push "Site URL is missing" issues))

    ;; Validate URL format
    (let ((url (mediawiki-site-config-url site-config)))
      (when url
        (unless (string-match-p "^https?://" url)
          (push "Site URL should start with http:// or https://" issues))
        (when (string-match-p " " url)
          (push "Site URL contains spaces" issues))))

    ;; Check authentication configuration
    (let ((auth-method (mediawiki-site-config-auth-method site-config))
          (username (mediawiki-site-config-username site-config))
          (auth-config (mediawiki-site-config-auth-config site-config)))

      (when (eq auth-method 'basic)
        (unless username
          (push "Basic auth method requires username" issues))
        (when (and auth-config (not (plist-get auth-config :password)))
          (push "Basic auth method requires password in auth-config" issues)))

      (when (eq auth-method 'oauth)
        (unless auth-config
          (push "OAuth method requires auth-config" issues))
        (when auth-config
          (unless (plist-get auth-config :consumer-key)
            (push "OAuth requires consumer-key in auth-config" issues))
          (unless (plist-get auth-config :consumer-secret)
            (push "OAuth requires consumer-secret in auth-config" issues)))))

    issues))

(defun mediawiki-compat-validate-migrated-config ()
  "Validate all migrated site configurations."
  (interactive)
  (setq mediawiki-compat-validation-errors '())

  (dolist (site-entry mediawiki-site-alist)
    (let* ((site-name (car site-entry))
           (site-config (cdr site-entry))
           (issues (when (mediawiki-site-config-p site-config)
                     (mediawiki-compat-validate-site-config site-config))))
      (when issues
        (push `(:site ,site-name :issues ,issues)
              mediawiki-compat-validation-errors))))

  (if mediawiki-compat-validation-errors
      (progn
        (message "Validation found %d issues. Use M-x mediawiki-compat-show-validation-report to see details."
                 (length mediawiki-compat-validation-errors))
        nil)
    (message "All migrated configurations are valid.")
    t))

(defun mediawiki-compat-show-validation-report ()
  "Show detailed validation report for migrated configurations."
  (interactive)
  (let ((buffer (get-buffer-create "*MediaWiki Validation Report*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "MediaWiki Configuration Validation Report\n")
      (insert "=============================================\n\n")

      (if mediawiki-compat-validation-errors
          (progn
            (insert (format "Found %d configuration issues:\n\n"
                           (length mediawiki-compat-validation-errors)))
            (dolist (error-entry mediawiki-compat-validation-errors)
              (let ((site-name (plist-get error-entry :site))
                    (issues (plist-get error-entry :issues)))
                (insert (format "Site: %s\n" site-name))
                (dolist (issue issues)
                  (insert (format "  • %s\n" issue)))
                (insert "\n"))))
        (insert "All configurations are valid.\n"))

      (goto-char (point-min))
      (special-mode))
    (display-buffer buffer)))

;;;###autoload
(defun mediawiki-compat-migration-wizard ()
  "Interactive wizard to guide users through configuration migration."
  (interactive)
  (let ((legacy-configs (mediawiki-compat-detect-legacy-config)))
    (if (not legacy-configs)
        (message "No legacy configuration detected. Migration not needed.")

      ;; Start wizard
      (let ((buffer (get-buffer-create "*MediaWiki Migration Wizard*")))
        (with-current-buffer buffer
          (erase-buffer)
          (mediawiki-compat-wizard-mode)
          (setq buffer-read-only nil)

          (insert "MediaWiki Configuration Migration Wizard\n")
          (insert "==========================================\n\n")

          (insert "This wizard will help you migrate your legacy MediaWiki configuration\n")
          (insert "to the modern format used by the current version.\n\n")

          (insert (format "Found %d legacy configuration items:\n\n"
                         (length legacy-configs)))

          ;; Display detected legacy configs
          (dolist (config legacy-configs)
            (let ((type (car config))
                  (props (cdr config)))
              (cond
               ((eq type 'old-site-format)
                (insert (format "• Legacy site: %s\n" (plist-get props :name)))
                (insert (format "  URL: %s\n" (plist-get props :url)))
                (when (plist-get props :username)
                  (insert (format "  Username: %s\n" (plist-get props :username))))
                (when (plist-get props :has-password)
                  (insert "  Has stored password\n"))
                (when (plist-get props :domain)
                  (insert (format "  Domain: %s\n" (plist-get props :domain)))))

               ((eq type 'legacy-site-url)
                (insert (format "• Legacy site URL: %s\n" (plist-get props :value))))

               ((eq type 'legacy-username)
                (insert (format "• Legacy username: %s\n" (plist-get props :value))))

               ((eq type 'legacy-password)
                (insert "• Legacy password (stored)\n"))

               ((eq type 'legacy-api-url)
                (insert (format "• Legacy API URL: %s\n" (plist-get props :value))))

               ((eq type 'legacy-domain)
                (insert (format "• Legacy domain: %s\n" (plist-get props :value)))))

              (insert "\n")))

          (insert "Migration Options:\n")
          (insert "==================\n\n")
          (insert "[1] Automatic migration (recommended)\n")
          (insert "    Let the wizard migrate all configurations automatically\n\n")
          (insert "[2] Interactive migration\n")
          (insert "    Review and customize each migration step\n\n")
          (insert "[3] Show migration preview\n")
          (insert "    See what changes will be made without applying them\n\n")
          (insert "[4] Cancel migration\n")
          (insert "    Exit without making changes\n\n")

          (insert "Choose an option (1-4): ")
          (setq buffer-read-only t))

        (display-buffer buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (mediawiki-compat-wizard-get-user-choice legacy-configs)))))

(defun mediawiki-compat-wizard-get-user-choice (legacy-configs)
  "Get user choice in migration wizard."
  (let ((choice (read-char-choice "Choose option (1-4): " '(?1 ?2 ?3 ?4))))
    (cond
     ((eq choice ?1)
      (mediawiki-compat-wizard-automatic-migration legacy-configs))
     ((eq choice ?2)
      (mediawiki-compat-wizard-interactive-migration legacy-configs))
     ((eq choice ?3)
      (mediawiki-compat-wizard-show-preview legacy-configs))
     ((eq choice ?4)
      (message "Migration cancelled.")))))

(defun mediawiki-compat-wizard-automatic-migration (legacy-configs)
  "Perform automatic migration with wizard guidance."
  (message "Starting automatic migration...")

  ;; Create migration report
  (setq mediawiki-compat-migration-report
        `(:timestamp ,(current-time)
          :legacy-configs ,legacy-configs
          :migration-type automatic
          :results nil))

  ;; Perform migration
  (let ((results '()))
    (condition-case err
        (progn
          (let ((migrated-count (mediawiki-compat-migrate-legacy-config)))
            (push `(:status success :migrated-count ,migrated-count) results)

            ;; Validate migrated configuration
            (if (mediawiki-compat-validate-migrated-config)
                (push `(:validation success) results)
              (push `(:validation warning :errors ,mediawiki-compat-validation-errors) results))

            (setf (plist-get mediawiki-compat-migration-report :results) results)

            (message "Migration completed successfully! Use M-x mediawiki-compat-show-migration-report for details.")))
      (error
       (push `(:status error :message ,(error-message-string err)) results)
       (setf (plist-get mediawiki-compat-migration-report :results) results)
       (message "Migration failed: %s" (error-message-string err))))))

(defun mediawiki-compat-wizard-interactive-migration (legacy-configs)
  "Perform interactive migration with user confirmation for each step."
  (message "Starting interactive migration...")

  (setq mediawiki-compat-migration-report
        `(:timestamp ,(current-time)
          :legacy-configs ,legacy-configs
          :migration-type interactive
          :results nil))

  (let ((results '())
        (migrated-count 0))

    ;; Process each legacy config individually
    (dolist (config legacy-configs)
      (let ((type (car config))
            (props (cdr config)))
        (when (y-or-n-p (format "Migrate %s? " type))
          (condition-case err
              (progn
                (cond
                 ((eq type 'old-site-format)
                  (let* ((name (plist-get props :name))
                         (old-entry (assoc name mediawiki-site-alist)))
                    (when old-entry
                      (let ((new-config (mediawiki-compat-migrate-site-config old-entry)))
                        (setcdr old-entry new-config)
                        (cl-incf migrated-count)
                        (push (format "Migrated site: %s" name) mediawiki-compat-migration-log)))))

                 ((eq type 'legacy-site-url)
                  (unless (mediawiki-get-site "default")
                    (mediawiki-add-site
                     (make-mediawiki-site-config
                      :name "default"
                      :url (plist-get props :value)
                      :auth-method 'basic))
                    (cl-incf migrated-count)
                    (push "Created default site from mediawiki-site-url" mediawiki-compat-migration-log))))

                (push `(:config ,type :status success) results))
            (error
             (push `(:config ,type :status error :message ,(error-message-string err)) results))))))

    (setf (plist-get mediawiki-compat-migration-report :results) results)

    (if (> migrated-count 0)
        (progn
          (mediawiki-compat-validate-migrated-config)
          (message "Interactive migration completed. Migrated %d items." migrated-count))
      (message "No items were migrated."))))

;;;###autoload
(defun mediawiki-compat-wizard-show-preview (legacy-configs)
  "Show preview of what migration would do without applying changes."
  (let ((buffer (get-buffer-create "*MediaWiki Migration Preview*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "MediaWiki Migration Preview\n")
      (insert "===========================\n\n")

      (insert "The following changes would be made:\n\n")

      (dolist (config legacy-configs)
        (let ((type (car config))
              (props (cdr config)))
          (cond
           ((eq type 'old-site-format)
            (insert (format "• Migrate site '%s':\n" (plist-get props :name)))
            (insert (format "  - Convert from old list format to modern struct\n"))
            (insert (format "  - URL: %s\n" (plist-get props :url)))
            (when (plist-get props :username)
              (insert (format "  - Set username: %s\n" (plist-get props :username))))
            (when (plist-get props :has-password)
              (insert "  - Migrate stored password to auth-config\n"))
            (when (plist-get props :domain)
              (insert (format "  - Set domain: %s\n" (plist-get props :domain)))))

           ((eq type 'legacy-site-url)
            (insert (format "• Create default site from legacy URL:\n"))
            (insert (format "  - URL: %s\n" (plist-get props :value)))
            (insert "  - Set auth-method to 'basic\n"))

           ((eq type 'legacy-username)
            (insert (format "• Legacy username '%s' will be noted but may need manual integration\n"
                           (plist-get props :value))))

           ((eq type 'legacy-password)
            (insert "• Legacy password will be noted but may need manual integration\n")))

          (insert "\n")))

      (insert "No changes have been applied yet.\n")
      (insert "Use M-x mediawiki-compat-migration-wizard to perform the actual migration.\n")

      (goto-char (point-min))
      (special-mode))
    (display-buffer buffer)))

(defun mediawiki-compat-show-migration-report ()
  "Show detailed report of the last migration performed."
  (interactive)
  (if (not mediawiki-compat-migration-report)
      (message "No migration report available.")

    (let ((buffer (get-buffer-create "*MediaWiki Migration Report*")))
      (with-current-buffer buffer
        (erase-buffer)
        (insert "MediaWiki Migration Report\n")
        (insert "==========================\n\n")

        (let ((timestamp (plist-get mediawiki-compat-migration-report :timestamp))
              (migration-type (plist-get mediawiki-compat-migration-report :migration-type))
              (legacy-configs (plist-get mediawiki-compat-migration-report :legacy-configs))
              (results (plist-get mediawiki-compat-migration-report :results)))

          (insert (format "Migration performed: %s\n"
                         (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)))
          (insert (format "Migration type: %s\n\n" migration-type))

          (insert (format "Legacy configurations found: %d\n" (length legacy-configs)))
          (dolist (config legacy-configs)
            (insert (format "  • %s\n" (car config))))
          (insert "\n")

          (insert "Migration Results:\n")
          (dolist (result results)
            (let ((status (plist-get result :status)))
              (insert (format "  • Status: %s\n" status))
              (when (plist-get result :migrated-count)
                (insert (format "    Migrated items: %d\n" (plist-get result :migrated-count))))
              (when (plist-get result :validation)
                (insert (format "    Validation: %s\n" (plist-get result :validation))))
              (when (plist-get result :message)
                (insert (format "    Message: %s\n" (plist-get result :message))))))

          (insert "\nFor detailed migration log, use M-x mediawiki-compat-show-migration-log\n")
          (when mediawiki-compat-validation-errors
            (insert "For validation details, use M-x mediawiki-compat-show-validation-report\n")))

        (goto-char (point-min))
        (special-mode))
      (display-buffer buffer))))

(define-derived-mode mediawiki-compat-wizard-mode fundamental-mode "MW-Migration"
  "Major mode for MediaWiki migration wizard."
  (setq buffer-read-only t)))

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

;; Initialize compatibility layer when loaded (if enabled)
(when mediawiki-check-compat
  (mediawiki-compat-initialize))

(provide 'mediawiki-compat)

;;; mediawiki-compat.el ends here
