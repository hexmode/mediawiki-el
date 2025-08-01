;;; test-migration-tools-basic.el --- Basic tests for enhanced migration tools -*- lexical-binding: t; -*-

;; Test implementation of task 10.2: Add configuration migration tools
;; Basic tests for the core functionality that is working

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-compat)
(require 'mediawiki-api)

;;; Test Setup

(defvar test-migration-original-site-alist nil)

(defun test-migration-setup ()
  "Set up test environment."
  (setq test-migration-original-site-alist mediawiki-site-alist)
  (setq mediawiki-site-alist '())
  (setq mediawiki-compat-migration-log '())
  (setq mediawiki-compat-validation-errors '())
  (when (boundp 'mediawiki-site-url)
    (makunbound 'mediawiki-site-url)))

(defun test-migration-teardown ()
  "Clean up test."
  (setq mediawiki-site-alist test-migration-original-site-alist)
  (setq mediawiki-compat-migration-log '())
  (setq mediawiki-compat-validation-errors '())
  (when (boundp 'mediawiki-site-url)
    (makunbound 'mediawiki-site-url)))

;;; Test Enhanced Detection

(ert-deftest test-enhanced-detection-old-site-format ()
  "Test enhanced detection returns detailed information."
  (test-migration-setup)
  (unwind-protect
      (progn
        (setq mediawiki-site-alist
              '(("TestSite" . ("https://test.example.com" "testuser" "testpass" "testdomain"))))

        (let ((detected (mediawiki-compat-detect-legacy-config)))
          (should detected)
          (should (>= (length detected) 1))

          (let ((config (cl-find 'old-site-format detected :key #'car)))
            (should config)
            (let ((props (cdr config)))
              (should (string= (plist-get props :name) "TestSite"))
              (should (string= (plist-get props :url) "https://test.example.com"))
              (should (string= (plist-get props :username) "testuser"))
              (should (plist-get props :has-password))
              (should (string= (plist-get props :domain) "testdomain"))))))

    (test-migration-teardown)))

(ert-deftest test-enhanced-detection-individual-variables ()
  "Test enhanced detection of individual legacy variables."
  (test-migration-setup)
  (unwind-protect
      (progn
        (setq mediawiki-site-url "https://legacy.example.com")
        (setq mediawiki-username "legacyuser")

        (let ((detected (mediawiki-compat-detect-legacy-config)))
          (should detected)
          (should (>= (length detected) 2))

          (let ((types (mapcar #'car detected)))
            (should (memq 'legacy-site-url types))
            (should (memq 'legacy-username types)))

          (let ((site-url-config (cl-find 'legacy-site-url detected :key #'car)))
            (should site-url-config)
            (let ((props (cdr site-url-config)))
              (should (string= (plist-get props :value) "https://legacy.example.com"))
              (should (eq (plist-get props :type) 'individual-variable))))))

    (test-migration-teardown)))

;;; Test Validation System

(ert-deftest test-validate-site-config-valid ()
  "Test validation of valid site configuration."
  (test-migration-setup)
  (unwind-protect
      (progn
        (let ((site-config (make-mediawiki-site-config
                           :name "ValidSite"
                           :url "https://valid.example.com"
                           :username "validuser"
                           :auth-method 'basic
                           :auth-config '(:password "validpass"))))

          (let ((issues (mediawiki-compat-validate-site-config site-config)))
            (should (null issues)))))

    (test-migration-teardown)))

(ert-deftest test-validate-site-config-invalid ()
  "Test validation detects issues in invalid configurations."
  (test-migration-setup)
  (unwind-protect
      (progn
        (let ((site-config (make-mediawiki-site-config
                           :name nil
                           :url "not-a-url"
                           :auth-method 'basic)))

          (let ((issues (mediawiki-compat-validate-site-config site-config)))
            (should issues)
            (should (> (length issues) 0))
            (should (cl-find-if (lambda (issue) (string-match-p "name is missing" issue)) issues))
            (should (cl-find-if (lambda (issue) (string-match-p "should start with http" issue)) issues)))))

    (test-migration-teardown)))

(ert-deftest test-validate-migrated-config ()
  "Test validation of entire migrated configuration."
  (test-migration-setup)
  (unwind-protect
      (progn
        (setq mediawiki-site-alist
              `(("ValidSite" . ,(make-mediawiki-site-config
                                :name "ValidSite"
                                :url "https://valid.example.com"
                                :auth-method 'none))
                ("InvalidSite" . ,(make-mediawiki-site-config
                                  :name nil
                                  :url "bad-url"))))

        (let ((is-valid (mediawiki-compat-validate-migrated-config)))
          (should (not is-valid))
          (should mediawiki-compat-validation-errors)
          (should (= (length mediawiki-compat-validation-errors) 1))))

    (test-migration-teardown)))

;;; Test Integration

(ert-deftest test-integration-enhanced-with-existing ()
  "Test that enhanced tools work with existing migration functions."
  (test-migration-setup)
  (unwind-protect
      (progn
        (setq mediawiki-site-alist
              '(("TestSite" . ("https://test.example.com" "testuser" "testpass"))))

        ;; Use existing migration function
        (let ((migrated-count (mediawiki-compat-migrate-legacy-config)))
          (should (> migrated-count 0))

          ;; Use enhanced validation
          (let ((is-valid (mediawiki-compat-validate-migrated-config)))
            (should is-valid))

          ;; Check that site was properly migrated and validated
          (let ((migrated-site (cdr (assoc "TestSite" mediawiki-site-alist))))
            (should (mediawiki-site-config-p migrated-site)))))

    (test-migration-teardown)))

(ert-deftest test-enhanced-detection-comprehensive ()
  "Test comprehensive detection of various legacy patterns."
  (test-migration-setup)
  (unwind-protect
      (progn
        ;; Set up multiple legacy configurations
        (setq mediawiki-site-alist
              '(("OldSite" . ("https://old.example.com" "olduser" "oldpass"))))
        (setq mediawiki-site-url "https://legacy.example.com")
        (setq mediawiki-username "legacyuser")
        (setq mediawiki-api-url "https://api.example.com")

        ;; Test enhanced detection
        (let ((detected (mediawiki-compat-detect-legacy-config)))
          (should detected)
          (should (>= (length detected) 4))

          ;; Check that all types are detected
          (let ((types (mapcar #'car detected)))
            (should (memq 'old-site-format types))
            (should (memq 'legacy-site-url types))
            (should (memq 'legacy-username types))
            (should (memq 'legacy-api-url types)))))

    (test-migration-teardown)))

(provide 'test-migration-tools-basic)

;;; test-migration-tools-basic.el ends here
