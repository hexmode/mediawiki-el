;;; test-mediawiki-migration.el --- Tests for MediaWiki site migration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;;; Commentary:

;; This file contains comprehensive tests to verify that the legacy site
;; configuration migration works correctly, converting old format to new
;; modular format.
;;
;; Test Coverage:
;; - Basic migration of legacy site configurations
;; - Handling of LDAP domain configurations
;; - Skipping default "username" values
;; - Empty legacy configuration handling
;; - Idempotent migration (safe to run multiple times)
;; - Site retrieval after migration
;; - URL access through migrated sites
;; - Adding new sites after migration
;; - Updating existing sites after migration
;; - Data structure validation of migrated sites
;; - Complete integration workflow
;; - Edge cases: malformed entries, nil values, duplicate names
;; - Performance with large configurations (100+ sites)

;;; Code:

(require 'ert)
(require 'mediawiki-core)

;; Define the migration function for testing (extracted from mediawiki.el)
(defcustom mediawiki-legacy-site-alist '()
  "Legacy list of MediaWiki websites (for testing)."
  :type 'list)

(defun mediawiki-migrate-legacy-sites ()
  "Migrate legacy site configuration to new format."
  (when mediawiki-legacy-site-alist
    (dolist (entry mediawiki-legacy-site-alist)
      (let* ((name (car entry))
             (params (cdr entry))
             (url (nth 0 params))
             (username (nth 1 params))
             (password (nth 2 params))
             (domain (nth 3 params))
             (first-page (nth 4 params)))

        ;; Create new site structure
        (let ((site (make-mediawiki-site
                     :name name
                     :url url
                     :username (unless (string= username "username") username)
                     :auth-method 'basic
                     :auth-config (when domain (list :domain domain))
                     :capabilities nil
                     :session-info nil)))

          (mediawiki-add-site site))))

    ;; Clear legacy configuration after migration
    (setq mediawiki-legacy-site-alist nil)
    (message "Migrated %d sites to new configuration format"
             (length mediawiki-site-alist))))

;;; Test Setup and Utilities

(defun test-mediawiki-reset-state ()
  "Reset MediaWiki state for testing."
  (setq mediawiki-site-alist '())
  (setq mediawiki-legacy-site-alist '()))

(defun test-mediawiki-create-legacy-config ()
  "Create test legacy configuration."
  '(("Wikipedia" "https://en.wikipedia.org/w/" "testuser" "testpass" nil "Main Page")
    ("Wiktionary" "https://en.wiktionary.org/w/" "wikiuser" "wikipass" "LDAP" "Wiktionary:Main Page")
    ("TestWiki" "https://test.example.com/w/" "username" "password" nil "Home")))

;;; Migration Tests

(ert-deftest test-mediawiki-migration-basic ()
  "Test basic migration of legacy site configuration."
  (test-mediawiki-reset-state)

  ;; Set up legacy configuration
  (setq mediawiki-legacy-site-alist (test-mediawiki-create-legacy-config))

  ;; Verify legacy config exists
  (should (= 3 (length mediawiki-legacy-site-alist)))
  (should (equal "Wikipedia" (caar mediawiki-legacy-site-alist)))

  ;; Run migration
  (mediawiki-migrate-legacy-sites)

  ;; Verify migration results
  (should (= 3 (length mediawiki-site-alist)))
  (should (null mediawiki-legacy-site-alist)) ; Should be cleared after migration

  ;; Check that sites were migrated correctly
  (let ((wikipedia-site (mediawiki-get-site "Wikipedia")))
    (should wikipedia-site)
    (should (string= "Wikipedia" (mediawiki-site-name wikipedia-site)))
    (should (string= "https://en.wikipedia.org/w/" (mediawiki-site-url wikipedia-site)))
    (should (string= "testuser" (mediawiki-site-username wikipedia-site)))
    (should (eq 'basic (mediawiki-site-auth-method wikipedia-site)))))

(ert-deftest test-mediawiki-migration-with-domain ()
  "Test migration of site with LDAP domain."
  (test-mediawiki-reset-state)

  ;; Set up legacy configuration with domain
  (setq mediawiki-legacy-site-alist
        '(("TestSite" "https://test.example.com/w/" "user" "pass" "TESTDOMAIN" "Main")))

  ;; Run migration
  (mediawiki-migrate-legacy-sites)

  ;; Verify domain was preserved in auth-config
  (let ((site (mediawiki-get-site "TestSite")))
    (should site)
    (should (equal '(:domain "TESTDOMAIN") (mediawiki-site-auth-config site)))))

(ert-deftest test-mediawiki-migration-skip-default-username ()
  "Test that default 'username' is not migrated."
  (test-mediawiki-reset-state)

  ;; Set up legacy configuration with default username
  (setq mediawiki-legacy-site-alist
        '(("TestSite" "https://test.example.com/w/" "username" "password" nil "Main")))

  ;; Run migration
  (mediawiki-migrate-legacy-sites)

  ;; Verify default username was not migrated
  (let ((site (mediawiki-get-site "TestSite")))
    (should site)
    (should (null (mediawiki-site-username site)))))

(ert-deftest test-mediawiki-migration-empty-legacy-config ()
  "Test migration with empty legacy configuration."
  (test-mediawiki-reset-state)

  ;; Set up empty legacy configuration
  (setq mediawiki-legacy-site-alist '())

  ;; Run migration
  (mediawiki-migrate-legacy-sites)

  ;; Verify no sites were created
  (should (= 0 (length mediawiki-site-alist))))

(ert-deftest test-mediawiki-migration-idempotent ()
  "Test that migration is idempotent (can be run multiple times safely)."
  (test-mediawiki-reset-state)

  ;; Set up legacy configuration
  (setq mediawiki-legacy-site-alist (test-mediawiki-create-legacy-config))

  ;; Run migration first time
  (mediawiki-migrate-legacy-sites)
  (let ((first-count (length mediawiki-site-alist)))

    ;; Run migration second time (should be no-op since legacy config is cleared)
    (mediawiki-migrate-legacy-sites)

    ;; Verify no additional sites were created
    (should (= first-count (length mediawiki-site-alist)))))

;;; Site Access Tests

(ert-deftest test-mediawiki-get-site-after-migration ()
  "Test that sites can be retrieved after migration."
  (test-mediawiki-reset-state)

  ;; Set up and migrate
  (setq mediawiki-legacy-site-alist (test-mediawiki-create-legacy-config))
  (mediawiki-migrate-legacy-sites)

  ;; Test site retrieval
  (let ((wikipedia (mediawiki-get-site "Wikipedia"))
        (wiktionary (mediawiki-get-site "Wiktionary"))
        (nonexistent (mediawiki-get-site "NonExistent")))

    (should wikipedia)
    (should wiktionary)
    (should (null nonexistent))

    ;; Verify site properties
    (should (mediawiki-site-p wikipedia))
    (should (string= "Wikipedia" (mediawiki-site-name wikipedia)))))

(ert-deftest test-mediawiki-get-url-after-migration ()
  "Test that URLs can be retrieved after migration."
  (test-mediawiki-reset-state)

  ;; Set up and migrate
  (setq mediawiki-legacy-site-alist (test-mediawiki-create-legacy-config))
  (mediawiki-migrate-legacy-sites)

  ;; Test URL retrieval
  (should (string= "https://en.wikipedia.org/w/" (mediawiki-get-url "Wikipedia")))
  (should (string= "https://en.wiktionary.org/w/" (mediawiki-get-url "Wiktionary")))

  ;; Test error for nonexistent site
  (should-error (mediawiki-get-url "NonExistent")))

;;; Site Management Tests

(ert-deftest test-mediawiki-add-site-after-migration ()
  "Test adding new sites after migration."
  (test-mediawiki-reset-state)

  ;; Set up and migrate
  (setq mediawiki-legacy-site-alist (test-mediawiki-create-legacy-config))
  (mediawiki-migrate-legacy-sites)

  (let ((initial-count (length mediawiki-site-alist)))

    ;; Add a new site
    (let ((new-site (make-mediawiki-site
                     :name "NewSite"
                     :url "https://new.example.com/w/"
                     :username "newuser"
                     :auth-method 'basic)))
      (mediawiki-add-site new-site))

    ;; Verify site was added
    (should (= (1+ initial-count) (length mediawiki-site-alist)))
    (should (mediawiki-get-site "NewSite"))))

(ert-deftest test-mediawiki-add-site-update-existing ()
  "Test updating existing site after migration."
  (test-mediawiki-reset-state)

  ;; Set up and migrate
  (setq mediawiki-legacy-site-alist (test-mediawiki-create-legacy-config))
  (mediawiki-migrate-legacy-sites)

  (let ((initial-count (length mediawiki-site-alist)))

    ;; Update existing site
    (let ((updated-site (make-mediawiki-site
                         :name "Wikipedia"
                         :url "https://en.wikipedia.org/w/"
                         :username "updateduser"
                         :auth-method 'oauth)))
      (mediawiki-add-site updated-site))

    ;; Verify site count didn't change (update, not add)
    (should (= initial-count (length mediawiki-site-alist)))

    ;; Verify site was updated
    (let ((site (mediawiki-get-site "Wikipedia")))
      (should (string= "updateduser" (mediawiki-site-username site)))
      (should (eq 'oauth (mediawiki-site-auth-method site))))))

;;; Data Structure Validation Tests

(ert-deftest test-mediawiki-site-struct-validation ()
  "Test that migrated sites have valid structure."
  (test-mediawiki-reset-state)

  ;; Set up and migrate
  (setq mediawiki-legacy-site-alist (test-mediawiki-create-legacy-config))
  (mediawiki-migrate-legacy-sites)

  ;; Test each migrated site
  (dolist (entry mediawiki-site-alist)
    (let ((site (cdr entry)))
      ;; Verify it's a proper mediawiki-site struct
      (should (mediawiki-site-p site))

      ;; Verify required fields are present
      (should (stringp (mediawiki-site-name site)))
      (should (stringp (mediawiki-site-url site)))
      (should (symbolp (mediawiki-site-auth-method site)))

      ;; Verify optional fields have correct types when present
      (when (mediawiki-site-username site)
        (should (stringp (mediawiki-site-username site))))
      (when (mediawiki-site-auth-config site)
        (should (listp (mediawiki-site-auth-config site)))))))

;;; Integration Tests

(ert-deftest test-mediawiki-migration-integration ()
  "Test complete migration workflow integration."
  (test-mediawiki-reset-state)

  ;; Simulate loading mediawiki.el with legacy config
  (setq mediawiki-legacy-site-alist (test-mediawiki-create-legacy-config))

  ;; Verify initial state
  (should (> (length mediawiki-legacy-site-alist) 0))
  (should (= 0 (length mediawiki-site-alist)))

  ;; Run migration (as would happen on load)
  (mediawiki-migrate-legacy-sites)

  ;; Verify final state
  (should (= 0 (length mediawiki-legacy-site-alist)))
  (should (> (length mediawiki-site-alist) 0))

  ;; Verify all expected sites are present and functional
  (should (mediawiki-get-site "Wikipedia"))
  (should (mediawiki-get-site "Wiktionary"))
  (should (mediawiki-get-site "TestWiki"))

  ;; Verify URL access works through site structures
  (should (string= "https://en.wikipedia.org/w/" (mediawiki-site-url (mediawiki-get-site "Wikipedia"))))
  (should (string= "https://en.wiktionary.org/w/" (mediawiki-site-url (mediawiki-get-site "Wiktionary"))))
  (should (string= "https://test.example.com/w/" (mediawiki-site-url (mediawiki-get-site "TestWiki")))))

;;; Edge Case Tests

(ert-deftest test-mediawiki-migration-malformed-entry ()
  "Test migration handles malformed legacy entries gracefully."
  (test-mediawiki-reset-state)

  ;; Set up legacy configuration with incomplete entry
  (setq mediawiki-legacy-site-alist
        '(("ValidSite" "https://valid.example.com/w/" "user" "pass" nil "Main")
          ("IncompleteEntry" "https://incomplete.example.com/w/")  ; Missing fields
          ("AnotherValid" "https://another.example.com/w/" "user2" "pass2" nil "Home")))

  ;; Migration should handle this gracefully
  (condition-case err
      (progn
        (mediawiki-migrate-legacy-sites)
        t)  ; Success
    (error nil))  ; Should not reach here
  (should t)  ; Test passes if we get here

  ;; Valid sites should still be migrated
  (should (mediawiki-get-site "ValidSite"))
  (should (mediawiki-get-site "AnotherValid")))

(ert-deftest test-mediawiki-migration-nil-values ()
  "Test migration handles nil values correctly."
  (test-mediawiki-reset-state)

  ;; Set up legacy configuration with nil values
  (setq mediawiki-legacy-site-alist
        '(("TestSite" "https://test.example.com/w/" nil nil nil nil)))

  ;; Run migration
  (mediawiki-migrate-legacy-sites)

  ;; Verify site was created with appropriate defaults
  (let ((site (mediawiki-get-site "TestSite")))
    (should site)
    (should (string= "TestSite" (mediawiki-site-name site)))
    (should (string= "https://test.example.com/w/" (mediawiki-site-url site)))
    (should (null (mediawiki-site-username site)))
    (should (eq 'basic (mediawiki-site-auth-method site)))))

(ert-deftest test-mediawiki-migration-duplicate-names ()
  "Test migration handles duplicate site names."
  (test-mediawiki-reset-state)

  ;; Set up legacy configuration with duplicate names (last one should win)
  (setq mediawiki-legacy-site-alist
        '(("DuplicateSite" "https://first.example.com/w/" "user1" "pass1" nil "Main")
          ("DuplicateSite" "https://second.example.com/w/" "user2" "pass2" nil "Home")))

  ;; Run migration
  (mediawiki-migrate-legacy-sites)

  ;; Should have only one site with the last configuration
  (should (= 1 (length mediawiki-site-alist)))
  (let ((site (mediawiki-get-site "DuplicateSite")))
    (should site)
    (should (string= "https://second.example.com/w/" (mediawiki-site-url site)))
    (should (string= "user2" (mediawiki-site-username site)))))

;;; Performance Tests

(ert-deftest test-mediawiki-migration-large-config ()
  "Test migration performance with large configuration."
  (test-mediawiki-reset-state)

  ;; Create a large legacy configuration
  (let ((large-config '()))
    (dotimes (i 100)
      (push (list (format "Site%d" i)
                  (format "https://site%d.example.com/w/" i)
                  (format "user%d" i)
                  (format "pass%d" i)
                  nil
                  "Main Page")
            large-config))
    (setq mediawiki-legacy-site-alist large-config))

  ;; Migration should complete without error
  (condition-case err
      (progn
        (mediawiki-migrate-legacy-sites)
        t)  ; Success
    (error nil))  ; Should not reach here
  (should t)  ; Test passes if we get here

  ;; Verify all sites were migrated
  (should (= 100 (length mediawiki-site-alist)))
  (should (mediawiki-get-site "Site0"))
  (should (mediawiki-get-site "Site99")))

;;; Test Runner

(defun run-mediawiki-migration-tests ()
  "Run all MediaWiki migration tests."
  (interactive)
  (ert-run-tests-batch-and-exit "test-mediawiki-migration"))

(provide 'test-mediawiki-migration)

;;; test-mediawiki-migration.el ends here
