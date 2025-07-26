;;; test-migration-tools.el --- Test enhanced migration tools (Task 10.2) -*- lexical-binding: t; -*-

;; Test implementation of task 10.2: Add configuration migration tools
;; Tests automatic config detection, migration wizard, and validation

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-compat)
(require 'mediawiki-api)

;;; Test Setup

(defvar test-migration-original-site-alist nil
  "Original site alist for restoration.")

(defun test-migration-setup ()
  "Set up test environment for migration tools."
  ;; Save original state
  (setq test-migration-original-site-alist mediawiki-site-alist)
  
  ;; Clear state
  (setq mediawiki-site-alist '())
  (setq mediawiki-compat-migration-log '())
  (setq mediawiki-compat-validation-errors '())
  (setq mediawiki-compat-migration-report nil)
  (setq mediawiki-compat-legacy-config-detected nil)
  
  ;; Clear legacy variables
  (when (boundp 'mediawiki-site-url)
    (makunbound 'mediawiki-site-url))
  (when (boundp 'mediawiki-username)
    (makunbound 'mediawiki-username))
  (when (boundp 'mediawiki-password)
    (makunbound 'mediawiki-password))
  (when (boundp 'mediawiki-api-url)
    (makunbound 'mediawiki-api-url))
  (when (boundp 'mediawiki-site-domain)
    (makunbound 'mediawiki-site-domain)))

(defun test-migration-teardown ()
  "Clean up test environment."
  ;; Restore original state
  (setq mediawiki-site-alist test-migration-original-site-alist)
  
  ;; Clear test state
  (setq mediawiki-compat-migration-log '())
  (setq mediawiki-compat-validation-errors '())
  (setq mediawiki-compat-migration-report nil)
  (setq mediawiki-compat-legacy-config-detected nil)
  
  ;; Clear legacy variables
  (when (boundp 'mediawiki-site-url)
    (makunbound 'mediawiki-site-url))
  (when (boundp 'mediawiki-username)
    (makunbound 'mediawiki-username))
  (when (boundp 'mediawiki-password)
    (makunbound 'mediawiki-password))
  (when (boundp 'mediawiki-api-url)
    (makunbound 'mediawiki-api-url))
  (when (boundp 'mediawiki-site-domain)
    (makunbound 'mediawiki-site-domain)))

;;; Test Enhanced Legacy Detection

(ert-deftest test-migration-enhanced-detection-old-site-format ()
  "Test enhanced detection of old site format configurations."
  (test-migration-setup)
  (unwind-protect
      (progn
        ;; Set up legacy site configuration
        (setq mediawiki-site-alist 
              '(("TestSite" . ("https://test.example.com" "testuser" "testpass" "testdomain"))))
        
        ;; Test enhanced detection
        (let ((detected (mediawiki-compat-detect-legacy-config)))
          (should detected)
          (should (= (length detected) 1))
          
          ;; Check detailed information
          (let ((config (car detected)))
            (should (eq (car config) 'old-site-format))
            (let ((props (cdr config)))
              (should (string= (plist-get props :name) "TestSite"))
              (should (string= (plist-get props :url) "https://test.example.com"))
              (should (string= (plist-get props :username) "testuser"))
              (should (plist-get props :has-password))
              (should (string= (plist-get props :domain) "testdomain"))))))
    
    (test-migration-teardown)))

(ert-deftest test-migration-enhanced-detection-individual-variables ()
  "Test enhanced detection of individual legacy variables."
  (test-migration-setup)
  (unwind-protect
      (progn
        ;; Set up individual legacy variables
        (setq mediawiki-site-url "https://legacy.example.com")
        (setq mediawiki-username "legacyuser")
        (setq mediawiki-password "legacypass")
        (setq mediawiki-api-url "https://legacy.example.com/api.php")
        (setq mediawiki-site-domain "legacydomain")
        
        ;; Test enhanced detection
        (let ((detected (mediawiki-compat-detect-legacy-config)))
          (should detected)
          (should (= (length detected) 5))
          
          ;; Check for all expected types
          (let ((types (mapcar #'car detected)))
            (should (memq 'legacy-site-url types))
            (should (memq 'legacy-username types))
            (should (memq 'legacy-password types))
            (should (memq 'legacy-api-url types))
            (should (memq 'legacy-domain types)))
          
          ;; Check detailed information for site URL
          (let ((site-url-config (cl-find 'legacy-site-url detected :key #'car)))
            (should site-url-config)
            (let ((props (cdr site-url-config)))
              (should (string= (plist-get props :value) "https://legacy.example.com"))
              (should (eq (plist-get props :type) 'individual-variable))))))
    
    (test-migration-teardown)))

;;; Test Configuration Validation

(ert-deftest test-migration-validate-site-config-valid ()
  "Test validation of a valid site configuration."
  (test-migration-setup)
  (unwind-protect
      (progn
        ;; Create valid site config
        (let ((site-config (make-mediawiki-site-config
                           :name "ValidSite"
                           :url "https://valid.example.com"
                           :username "validuser"
                           :auth-method 'basic
                           :auth-config '(:password "validpass"))))
          
          ;; Test validation
          (let ((issues (mediawiki-compat-validate-site-config site-config)))
            (should (null issues)))))
    
    (test-migration-teardown)))

(ert-deftest test-migration-validate-site-config-invalid ()
  "Test validation of invalid site configurations."
  (test-migration-setup)
  (unwind-protect
      (progn
        ;; Create invalid site config - missing name and bad URL
        (let ((site-config (make-mediawiki-site-config
                           :name nil
                           :url "not-a-url"
                           :username "user"
                           :auth-method 'basic
                           :auth-config '(:no-password t))))
          
          ;; Test validation
          (let ((issues (mediawiki-compat-validate-site-config site-config)))
            (should issues)
            (should (> (length issues) 0))
            
            ;; Check for expected issues
            (should (cl-find-if (lambda (issue) (string-match-p "name is missing" issue)) issues))
            (should (cl-find-if (lambda (issue) (string-match-p "should start with http" issue)) issues))
            ;; Since auth-config exists but doesn't have :password, should get password error
            (should (cl-find-if (lambda (issue) (string-match-p "password" issue)) issues)))))
    
    (test-migration-teardown)))

(ert-deftest test-migration-validate-oauth-config ()
  "Test validation of OAuth configuration."
  (test-migration-setup)
  (unwind-protect
      (progn
        ;; Create OAuth config missing required fields
        (let ((site-config (make-mediawiki-site-config
                           :name "OAuthSite"
                           :url "https://oauth.example.com"
                           :auth-method 'oauth
                           :auth-config '(:consumer-key "key"))))
          
          ;; Test validation
          (let ((issues (mediawiki-compat-validate-site-config site-config)))
            (should issues)
            (should (cl-find-if (lambda (issue) (string-match-p "consumer-secret" issue)) issues)))))
    
    (test-migration-teardown)))

(ert-deftest test-migration-validate-migrated-config ()
  "Test validation of entire migrated configuration."
  (test-migration-setup)
  (unwind-protect
      (progn
        ;; Set up mixed valid/invalid configurations
        (setq mediawiki-site-alist
              `(("ValidSite" . ,(make-mediawiki-site-config
                                :name "ValidSite"
                                :url "https://valid.example.com"
                                :auth-method 'none))
                ("InvalidSite" . ,(make-mediawiki-site-config
                                  :name nil
                                  :url "bad-url"
                                  :auth-method 'basic))))
        
        ;; Test validation
        (let ((is-valid (mediawiki-compat-validate-migrated-config)))
          (should (not is-valid))
          (should mediawiki-compat-validation-errors)
          (should (= (length mediawiki-compat-validation-errors) 1))
          
          ;; Check error details
          (let ((error-entry (car mediawiki-compat-validation-errors)))
            (should (string= (plist-get error-entry :site) "InvalidSite"))
            (should (plist-get error-entry :issues)))))
    
    (test-migration-teardown)))

;;; Test Migration Wizard Functions

(ert-deftest test-migration-wizard-automatic-migration ()
  "Test automatic migration through wizard."
  (test-migration-setup)
  (unwind-protect
      (progn
        ;; Set up legacy configuration
        (setq mediawiki-site-alist 
              '(("TestSite" . ("https://test.example.com" "testuser" "testpass"))))
        (setq mediawiki-site-url "https://legacy.example.com")
        
        ;; Get legacy configs
        (let ((legacy-configs (mediawiki-compat-detect-legacy-config)))
          
          ;; Test automatic migration
          (mediawiki-compat-wizard-automatic-migration legacy-configs)
          
          ;; Check that migration report was created
          (should mediawiki-compat-migration-report)
          (should (eq (plist-get mediawiki-compat-migration-report :migration-type) 'automatic))
          (should (plist-get mediawiki-compat-migration-report :timestamp))
          (should (plist-get mediawiki-compat-migration-report :results))))
    
    (test-migration-teardown)))

(ert-deftest test-migration-wizard-preview ()
  "Test migration preview functionality."
  (test-migration-setup)
  (unwind-protect
      (progn
        ;; Set up legacy configuration
        (setq mediawiki-site-alist 
              '(("TestSite" . ("https://test.example.com" "testuser" "testpass"))))
        (setq mediawiki-site-url "https://legacy.example.com")
        
        ;; Get legacy configs
        (let ((legacy-configs (mediawiki-compat-detect-legacy-config)))
          
          ;; Test preview (should not modify anything)
          (let ((original-alist (copy-tree mediawiki-site-alist)))
            (mediawiki-compat-wizard-show-preview legacy-configs)
            
            ;; Verify no changes were made
            (should (equal mediawiki-site-alist original-alist))
            
            ;; Check that preview buffer was created
            (should (get-buffer "*MediaWiki Migration Preview*")))))
    
    (test-migration-teardown)))

;;; Test Migration Report System

(ert-deftest test-migration-report-creation ()
  "Test creation and content of migration reports."
  (test-migration-setup)
  (unwind-protect
      (progn
        ;; Set up test report
        (setq mediawiki-compat-migration-report
              `(:timestamp ,(current-time)
                :legacy-configs ((old-site-format :name "TestSite"))
                :migration-type automatic
                :results ((:status success :migrated-count 1)
                         (:validation success))))
        
        ;; Test report display (should not error)
        (mediawiki-compat-show-migration-report)
        
        ;; Check that report buffer was created
        (should (get-buffer "*MediaWiki Migration Report*"))
        
        ;; Check buffer contents
        (with-current-buffer "*MediaWiki Migration Report*"
          (should (string-match-p "Migration Report" (buffer-string)))
          (should (string-match-p "automatic" (buffer-string)))
          (should (string-match-p "success" (buffer-string)))))
    
    (test-migration-teardown)))

;;; Test Interactive Migration

(ert-deftest test-migration-interactive-site-migration ()
  "Test interactive migration of site configurations."
  (test-migration-setup)
  (unwind-protect
      (progn
        ;; Set up legacy site
        (setq mediawiki-site-alist 
              '(("TestSite" . ("https://test.example.com" "testuser" "testpass"))))
        
        ;; Mock y-or-n-p to always return yes
        (advice-add 'y-or-n-p :override (lambda (&rest args) t))
        
        ;; Get legacy configs and run interactive migration
        (let ((legacy-configs (mediawiki-compat-detect-legacy-config)))
          (mediawiki-compat-wizard-interactive-migration legacy-configs)
          
          ;; Check that site was migrated
          (let ((migrated-site (cdr (assoc "TestSite" mediawiki-site-alist))))
            (should (mediawiki-site-config-p migrated-site))
            (should (string= (mediawiki-site-config-name migrated-site) "TestSite"))
            (should (string= (mediawiki-site-config-url migrated-site) "https://test.example.com"))
            (should (string= (mediawiki-site-config-username migrated-site) "testuser")))
          
          ;; Check migration report
          (should mediawiki-compat-migration-report)
          (should (eq (plist-get mediawiki-compat-migration-report :migration-type) 'interactive))))
    
    ;; Cleanup
    (advice-remove 'y-or-n-p (lambda (&rest args) t))
    (test-migration-teardown)))

;;; Test Error Handling

(ert-deftest test-migration-error-handling ()
  "Test error handling in migration tools."
  (test-migration-setup)
  (unwind-protect
      (progn
        ;; Test validation with no configurations
        (setq mediawiki-site-alist '())
        (let ((is-valid (mediawiki-compat-validate-migrated-config)))
          (should is-valid)
          (should (null mediawiki-compat-validation-errors)))
        
        ;; Test migration report when no report exists
        (setq mediawiki-compat-migration-report nil)
        (mediawiki-compat-show-migration-report)
        ;; Should not error, just show message
        
        ;; Test wizard with no legacy config
        (let ((legacy-configs '()))
          (mediawiki-compat-migration-wizard)
          ;; Should show "no legacy configuration" message
          ))
    
    (test-migration-teardown)))

;;; Test Integration with Existing System

(ert-deftest test-migration-integration-with-existing-compat ()
  "Test that enhanced tools integrate with existing compatibility layer."
  (test-migration-setup)
  (unwind-protect
      (progn
        ;; Set up legacy configuration
        (setq mediawiki-site-alist 
              '(("TestSite" . ("https://test.example.com" "testuser" "testpass"))))
        
        ;; Use existing migration function
        (let ((migrated-count (mediawiki-compat-migrate-legacy-config)))
          (should (> migrated-count 0))
          
          ;; Validate the migrated configuration
          (let ((is-valid (mediawiki-compat-validate-migrated-config)))
            (should is-valid))
          
          ;; Check that site was properly migrated
          (let ((migrated-site (cdr (assoc "TestSite" mediawiki-site-alist))))
            (should (mediawiki-site-config-p migrated-site)))))
    
    (test-migration-teardown)))

(ert-deftest test-migration-validation-report-display ()
  "Test validation report display functionality."
  (test-migration-setup)
  (unwind-protect
      (progn
        ;; Set up validation errors
        (setq mediawiki-compat-validation-errors
              '((:site "TestSite" :issues ("URL is missing" "Auth method invalid"))))
        
        ;; Test report display
        (mediawiki-compat-show-validation-report)
        
        ;; Check that report buffer was created
        (should (get-buffer "*MediaWiki Validation Report*"))
        
        ;; Check buffer contents
        (with-current-buffer "*MediaWiki Validation Report*"
          (should (string-match-p "Validation Report" (buffer-string)))
          (should (string-match-p "TestSite" (buffer-string)))
          (should (string-match-p "URL is missing" (buffer-string)))
          (should (string-match-p "Auth method invalid" (buffer-string)))))
    
    (test-migration-teardown)))

(provide 'test-migration-tools)

;;; test-migration-tools.el ends here