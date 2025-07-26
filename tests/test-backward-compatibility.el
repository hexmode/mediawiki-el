;;; test-backward-compatibility.el --- Test backward compatibility layer -*- lexical-binding: t; -*-

;; Test implementation of task 10.1: Create compatibility layer
;; Tests shims for deprecated functions, configuration migration, and compatibility

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-compat)
(require 'mediawiki-api)

;;; Test Setup

(defvar test-compat-warning-count 0
  "Count of deprecation warnings during tests.")

(defvar test-compat-original-warn-setting nil
  "Original setting for deprecation warnings.")

(defun test-compat-setup ()
  "Set up test environment."
  ;; Clear any existing sites
  (setq mediawiki-site-alist '())
  
  ;; Reset warning count
  (setq test-compat-warning-count 0)
  
  ;; Save original warning setting
  (setq test-compat-original-warn-setting mediawiki-compat-warn-deprecated)
  
  ;; Enable warnings for testing
  (setq mediawiki-compat-warn-deprecated t)
  
  ;; Temporarily override display-warning to count warnings
  (advice-add 'display-warning :after
              (lambda (&rest args)
                (when (eq (car args) 'mediawiki-deprecated)
                  (setq test-compat-warning-count (1+ test-compat-warning-count)))))
  
  ;; Set up mock API call response
  (advice-add 'mediawiki-api-call-sync :override #'test-compat-mock-api-call-sync))

(defun test-compat-teardown ()
  "Clean up test environment."
  ;; Restore original warning setting
  (setq mediawiki-compat-warn-deprecated test-compat-original-warn-setting)
  
  ;; Remove advice
  (advice-remove 'display-warning #'test-compat-advice-display-warning)
  (advice-remove 'mediawiki-api-call-sync #'test-compat-mock-api-call-sync)
  
  ;; Clean up sites and variables
  (setq mediawiki-site-alist '())
  (setq mediawiki-compat-migration-log '())
  (setq mediawiki-compat-legacy-config-detected nil)
  (when (boundp 'mediawiki-site-url)
    (makunbound 'mediawiki-site-url))
  (when (boundp 'mediawiki-username)
    (makunbound 'mediawiki-username))
  (when (boundp 'mediawiki-password)
    (makunbound 'mediawiki-password)))

(defun test-compat-mock-api-call-sync (sitename action params)
  "Mock API call function for testing compatibility layer."
  (cond
   ((string= action "query")
    (make-mediawiki-api-response
     :success t
     :data '((query (pages ((123 (title . "Test Page") (pageid . 123))))))))
   ((string= action "login")
    (make-mediawiki-api-response
     :success t
     :data '((login (result . "Success") (lgusername . "testuser")))))
   ((string= action "logout")
    (make-mediawiki-api-response
     :success t
     :data '((logout))))
   ((string= action "edit")
    (make-mediawiki-api-response
     :success t
     :data '((edit (result . "Success") (pageid . 123)))))
   (t
    (make-mediawiki-api-response
     :success nil
     :errors (list "Unknown action")))))

;;; Test Legacy Function Shims

(ert-deftest test-compat-legacy-api-call ()
  "Test that legacy mediawiki-api-call function works with warning."
  (test-compat-setup)
  (unwind-protect
      (progn
        ;; Test basic API call
        (let ((result (mediawiki-api-call "test-site" "query" 
                                          '(("prop" . "info") ("titles" . "Test Page")))))
          
          ;; Should return data in legacy format
          (should result)
          (should (eq (car result) 'api))
          (should (assq 'query (cdr result)))
          
          ;; Should have generated a deprecation warning
          (should (> test-compat-warning-count 0))))
    
    (test-compat-teardown)))

(ert-deftest test-compat-legacy-api-call-disabled ()
  "Test that legacy API call can be disabled."
  (test-compat-setup)
  (unwind-protect
      (progn
        ;; Disable compatibility shims
        (setq mediawiki-compat-enable-shims nil)
        
        ;; Should throw error when disabled
        (should-error
         (mediawiki-api-call "test-site" "query" '(("prop" . "info")))
         :type 'error))
    
    ;; Restore setting
    (setq mediawiki-compat-enable-shims t)
    (test-compat-teardown)))

(ert-deftest test-compat-legacy-function-aliases ()
  "Test that legacy function aliases work."
  (test-compat-setup)
  (unwind-protect
      (progn
        ;; Test function aliases exist
        (should (fboundp 'mediawiki-login))
        (should (fboundp 'mediawiki-logout))
        
        ;; Test aliases point to correct functions  
        (should (equal (indirect-function 'mediawiki-login)
                       (indirect-function 'mediawiki-do-login)))
        (should (equal (indirect-function 'mediawiki-logout)
                       (indirect-function 'mediawiki-do-logout))))
    
    (test-compat-teardown)))

;;; Test Configuration Migration

(ert-deftest test-compat-legacy-config-detection ()
  "Test detection of legacy configuration."
  (test-compat-setup)
  (unwind-protect
      (progn
        ;; Set up legacy site configuration
        (setq mediawiki-site-alist 
              '(("OldSite" . ("https://old.example.com" "olduser" "oldpass"))))
        
        ;; Should detect legacy format
        (let ((legacy-configs (mediawiki-compat-detect-legacy-config)))
          (should legacy-configs)
          (let ((legacy-types (mapcar #'car legacy-configs)))
            (should (memq 'old-site-format legacy-types)))))
    
    (test-compat-teardown)))

(ert-deftest test-compat-site-config-migration ()
  "Test migration of legacy site configuration."
  (test-compat-setup)
  (unwind-protect
      (progn
        ;; Set up legacy site configuration
        (setq mediawiki-site-alist 
              '(("TestSite" . ("https://test.example.com" "testuser" "testpass" "testdomain"))))
        
        ;; Migrate configuration
        (let ((migrated-count (mediawiki-compat-migrate-legacy-config)))
          (should (> migrated-count 0))
          
          ;; Check that site was migrated to modern format
          (let ((migrated-site (mediawiki-get-site "TestSite")))
            (should migrated-site)
            (should (mediawiki-site-config-p migrated-site))
            (should (string= (mediawiki-site-config-url migrated-site) 
                            "https://test.example.com"))
            (should (string= (mediawiki-site-config-username migrated-site) 
                            "testuser"))
            ;; Check auth-config contains legacy credentials
            (let ((auth-config (mediawiki-site-config-auth-config migrated-site)))
              (should auth-config)
              (should (string= (plist-get auth-config :password) "testpass"))
              (should (string= (plist-get auth-config :domain) "testdomain")))
            (should (eq (mediawiki-site-config-auth-method migrated-site) 
                       'basic)))))
    
    (test-compat-teardown)))

(ert-deftest test-compat-individual-variable-migration ()
  "Test migration of individual legacy variables."
  (test-compat-setup)
  (unwind-protect
      (progn
        ;; Set up legacy variable
        (setq mediawiki-site-url "https://legacy.example.com")
        
        ;; Migrate configuration
        (let ((migrated-count (mediawiki-compat-migrate-legacy-config)))
          (should (> migrated-count 0))
          
          ;; Check that default site was created
          (let ((default-site (mediawiki-get-site "default")))
            (should default-site)
            (should (string= (mediawiki-site-config-url default-site) 
                            "https://legacy.example.com")))))
    
    (test-compat-teardown)))

(ert-deftest test-compat-migration-log ()
  "Test that migration creates a proper log."
  (test-compat-setup)
  (unwind-protect
      (progn
        ;; Set up legacy configuration
        (setq mediawiki-site-alist 
              '(("TestSite" . ("https://test.example.com" "user" "pass"))))
        
        ;; Migrate and check log
        (mediawiki-compat-migrate-legacy-config)
        
        (should mediawiki-compat-migration-log)
        (should (> (length mediawiki-compat-migration-log) 0))
        (should (string-match-p "Migrated site: TestSite" (car mediawiki-compat-migration-log))))
    
    (test-compat-teardown)))

;;; Test Compatibility Features

(ert-deftest test-compat-warning-system ()
  "Test the deprecation warning system."
  (test-compat-setup)
  (unwind-protect
      (progn
        ;; Test warning enabled
        (setq mediawiki-compat-warn-deprecated t)
        (setq test-compat-warning-count 0)
        
        (mediawiki-api-call "test-site" "query" '(("prop" . "info")))
        (should (> test-compat-warning-count 0))
        
        ;; Test warning disabled
        (setq mediawiki-compat-warn-deprecated nil)
        (setq test-compat-warning-count 0)
        
        (mediawiki-api-call "test-site" "query" '(("prop" . "info")))
        (should (= test-compat-warning-count 0)))
    
    (test-compat-teardown)))

(ert-deftest test-compat-variable-aliases ()
  "Test that variable aliases work correctly."
  (test-compat-setup)
  (unwind-protect
      (progn
        ;; Test that the alias works functionally
        (setq mediawiki-site-url "test-value")
        (should (string= mediawiki-site-default "test-value"))
        
        ;; Test obsolete marking
        (should (get 'mediawiki-site-url 'byte-obsolete-variable)))
    
    (test-compat-teardown)))

;;; Test Integration

(ert-deftest test-compat-integration-with-modern-system ()
  "Test that compatibility layer integrates with modern system."
  (test-compat-setup)
  (unwind-protect
      (progn
        ;; Add a modern site configuration
        (mediawiki-add-site 
         (make-mediawiki-site-config
          :name "modern-site"
          :url "https://modern.example.com"
          :auth-method 'oauth))
        
        ;; Legacy API call should work with modern site
        (let ((result (mediawiki-api-call "modern-site" "query" 
                                          '(("prop" . "info") ("titles" . "Test")))))
          (should result)
          (should (eq (car result) 'api))))
    
    (test-compat-teardown)))

(ert-deftest test-compat-error-handling ()
  "Test error handling in compatibility layer."
  (test-compat-setup)
  (unwind-protect
      (progn
        ;; Override mock to return error
        (advice-add 'mediawiki-api-call-sync :override
                    (lambda (&rest args)
                      (make-mediawiki-api-response
                       :success nil
                       :errors (list "Test error"))))
        
        ;; Legacy call should propagate error
        (should-error
         (mediawiki-api-call "test-site" "invalid" '())
         :type 'error))
    
    (advice-remove 'mediawiki-api-call-sync #'test-compat-mock-api-call-sync)
    (test-compat-teardown)))

(ert-deftest test-compat-parameter-conversion ()
  "Test conversion between legacy and modern parameter formats."
  (test-compat-setup)
  (unwind-protect
      (progn
        ;; Test parameter format conversion
        (let ((legacy-args '(("prop" . "info") ("titles" . "Test Page") ("action" . "query"))))
          
          ;; Mock API call to capture converted parameters
          (advice-add 'mediawiki-api-call-sync :override
                      (lambda (sitename action params)
                        ;; Check that parameters are properly converted
                        (should (string= sitename "test-site"))
                        (should (string= action "query"))
                        (should (listp params))
                        (should (assoc "prop" params))
                        (should (assoc "titles" params))
                        ;; Should not include action in params (it's separate)
                        (should-not (assoc "action" params))
                        
                        (make-mediawiki-api-response :success t :data '((query)))))
          
          (mediawiki-api-call "test-site" "query" legacy-args)))
    
    (test-compat-teardown)))

(ert-deftest test-compat-initialization ()
  "Test that compatibility layer initializes correctly."
  (test-compat-setup)
  (unwind-protect
      (progn
        ;; Test that compatibility functions are available
        (should (fboundp 'mediawiki-api-call))
        (should (fboundp 'mediawiki-compat-check-and-migrate))
        (should (fboundp 'mediawiki-compat-show-migration-log))
        
        ;; Test configuration variables exist
        (should (boundp 'mediawiki-compat-enable-shims))
        (should (boundp 'mediawiki-compat-warn-deprecated)))
    
    (test-compat-teardown)))

(provide 'test-backward-compatibility)

;;; test-backward-compatibility.el ends here