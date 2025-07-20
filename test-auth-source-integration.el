;;; test-auth-source-integration.el --- Tests for MediaWiki auth-source integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;;; Commentary:

;; This file contains tests for the auth-source integration functionality
;; in the MediaWiki authentication module.

;;; Code:

(require 'ert)
(require 'mediawiki-auth)
(require 'mediawiki-core)

;;; Test Setup

(defvar test-mediawiki-site-config-name "test-wiki"
  "Test site name for auth-source integration tests.")

(defvar test-mediawiki-site-config-url "https://test.example.com/wiki/"
  "Test site URL for auth-source integration tests.")

(defun test-mediawiki-setup-test-site ()
  "Set up a test MediaWiki site for testing."
  (let ((test-site (make-mediawiki-site-config
                    :name test-mediawiki-site-config-name
                    :url test-mediawiki-site-config-url
                    :api-url (concat test-mediawiki-site-config-url "api.php")
                    :username "testuser"
                    :auth-method 'basic)))
    (mediawiki-add-site test-site)))

(defun test-mediawiki-cleanup-test-site ()
  "Clean up test site and cached credentials."
  (mediawiki-auth-clear-cached-credentials test-mediawiki-site-config-name)
  (mediawiki-remove-session test-mediawiki-site-config-name)
  (setq mediawiki-site-alist
        (assoc-delete-all test-mediawiki-site-config-name mediawiki-site-alist)))

;;; Credential Cache Tests

(ert-deftest test-mediawiki-auth-cache-key-generation ()
  "Test cache key generation for credentials."
  (should (string= (mediawiki-auth-make-cache-key "test-site") "test-site"))
  (should (string= (mediawiki-auth-make-cache-key "another-site") "another-site")))

(ert-deftest test-mediawiki-auth-credential-caching ()
  "Test credential caching functionality."
  (test-mediawiki-setup-test-site)
  (unwind-protect
      (let ((cache-key (mediawiki-auth-make-cache-key test-mediawiki-site-config-name))
            (test-credentials '(:username "testuser" :password "testpass")))
        
        ;; Test caching credentials
        (mediawiki-auth-cache-credentials cache-key test-credentials)
        
        ;; Test retrieving cached credentials
        (let ((cached-creds (mediawiki-auth-get-cached-credentials cache-key)))
          (should cached-creds)
          (should (string= (plist-get cached-creds :username) "testuser"))
          (should (string= (plist-get cached-creds :password) "testpass")))
        
        ;; Test cache expiration (simulate expired cache)
        (let ((expired-entry (list :username "testuser" 
                                  :password "testpass"
                                  :expiry (time-subtract (current-time) 10))))
          (puthash cache-key expired-entry mediawiki-auth-credential-cache)
          (should-not (mediawiki-auth-get-cached-credentials cache-key))))
    
    (test-mediawiki-cleanup-test-site)))

(ert-deftest test-mediawiki-auth-cache-cleanup ()
  "Test credential cache cleanup functionality."
  (test-mediawiki-setup-test-site)
  (unwind-protect
      (let ((cache-key (mediawiki-auth-make-cache-key test-mediawiki-site-config-name)))
        
        ;; Add expired entry
        (let ((expired-entry (list :username "testuser" 
                                  :password "testpass"
                                  :expiry (time-subtract (current-time) 10))))
          (puthash cache-key expired-entry mediawiki-auth-credential-cache))
        
        ;; Add valid entry
        (let ((valid-key "valid-site")
              (valid-entry (list :username "validuser"
                                :password "validpass"
                                :expiry (time-add (current-time) 3600))))
          (puthash valid-key valid-entry mediawiki-auth-credential-cache))
        
        ;; Test cleanup
        (mediawiki-auth-cleanup-expired-credentials)
        
        ;; Expired entry should be removed
        (should-not (gethash cache-key mediawiki-auth-credential-cache))
        
        ;; Valid entry should remain
        (should (gethash "valid-site" mediawiki-auth-credential-cache)))
    
    (test-mediawiki-cleanup-test-site)))

;;; URL Parsing Tests

(ert-deftest test-mediawiki-auth-url-parsing ()
  "Test URL parsing utilities for auth-source integration."
  (should (string= (mediawiki-auth-extract-host "https://example.com/wiki/")
                   "example.com"))
  (should (string= (mediawiki-auth-extract-host "http://test.org:8080/")
                   "test.org:8080"))
  (should (string= (mediawiki-auth-extract-host "https://en.wikipedia.org/w/")
                   "en.wikipedia.org"))
  
  (should (string= (mediawiki-auth-extract-port "https://example.com/") "https"))
  (should (string= (mediawiki-auth-extract-port "http://example.com/") "http"))
  (should-not (mediawiki-auth-extract-port "ftp://example.com/")))

;;; Cache Status Tests

(ert-deftest test-mediawiki-auth-cache-status ()
  "Test cache status reporting functionality."
  (test-mediawiki-setup-test-site)
  (unwind-protect
      (progn
        ;; Clear cache first
        (mediawiki-auth-clear-all-cached-credentials)
        
        ;; Add some test entries
        (let ((active-key "active-site")
              (expired-key "expired-site"))
          
          ;; Add active entry
          (puthash active-key
                   (list :username "activeuser"
                         :password "activepass"
                         :expiry (time-add (current-time) 3600))
                   mediawiki-auth-credential-cache)
          
          ;; Add expired entry
          (puthash expired-key
                   (list :username "expireduser"
                         :password "expiredpass"
                         :expiry (time-subtract (current-time) 10))
                   mediawiki-auth-credential-cache)
          
          ;; Test status
          (let ((status (mediawiki-auth-get-cache-status)))
            (should (= (plist-get status :total-entries) 2))
            (should (= (plist-get status :expired-entries) 1))
            (should (= (plist-get status :active-entries) 1)))))
    
    (test-mediawiki-cleanup-test-site)))

;;; Security Tests

(ert-deftest test-mediawiki-auth-credential-invalidation ()
  "Test credential invalidation functionality."
  (test-mediawiki-setup-test-site)
  (unwind-protect
      (let ((cache-key (mediawiki-auth-make-cache-key test-mediawiki-site-config-name))
            (test-credentials '(:username "testuser" :password "testpass")))
        
        ;; Cache credentials and create session
        (mediawiki-auth-cache-credentials cache-key test-credentials)
        (let ((session (make-mediawiki-session
                        :site-name test-mediawiki-site-config-name
                        :tokens (make-hash-table :test 'equal)
                        :login-time (current-time))))
          (mediawiki-set-session test-mediawiki-site-config-name session))
        
        ;; Verify credentials and session exist
        (should (mediawiki-auth-get-cached-credentials cache-key))
        (should (mediawiki-get-session test-mediawiki-site-config-name))
        
        ;; Invalidate credentials
        (mediawiki-auth-invalidate-credentials test-mediawiki-site-config-name)
        
        ;; Verify credentials and session are cleared
        (should-not (mediawiki-auth-get-cached-credentials cache-key))
        (should-not (mediawiki-get-session test-mediawiki-site-config-name)))
    
    (test-mediawiki-cleanup-test-site)))

(ert-deftest test-mediawiki-auth-logout-clears-cache ()
  "Test that logout clears cached credentials."
  (test-mediawiki-setup-test-site)
  (unwind-protect
      (let ((cache-key (mediawiki-auth-make-cache-key test-mediawiki-site-config-name))
            (test-credentials '(:username "testuser" :password "testpass")))
        
        ;; Cache credentials and create session
        (mediawiki-auth-cache-credentials cache-key test-credentials)
        (let ((session (make-mediawiki-session
                        :site-name test-mediawiki-site-config-name
                        :tokens (make-hash-table :test 'equal)
                        :login-time (current-time))))
          (mediawiki-set-session test-mediawiki-site-config-name session))
        
        ;; Verify credentials exist
        (should (mediawiki-auth-get-cached-credentials cache-key))
        
        ;; Mock the API call to avoid actual network request
        (cl-letf (((symbol-function 'mediawiki-api-call-sync)
                   (lambda (&rest _args) nil)))
          (mediawiki-auth-logout test-mediawiki-site-config-name))
        
        ;; Verify credentials are cleared
        (should-not (mediawiki-auth-get-cached-credentials cache-key)))
    
    (test-mediawiki-cleanup-test-site)))

;;; Integration Tests

(ert-deftest test-mediawiki-auth-manual-backend ()
  "Test manual credential backend (non-auth-source)."
  (test-mediawiki-setup-test-site)
  (unwind-protect
      (let ((mediawiki-auth-source-backend 'manual))
        
        ;; Mock user input
        (cl-letf (((symbol-function 'read-string)
                   (lambda (_prompt) "manual-user"))
                  ((symbol-function 'read-passwd)
                   (lambda (_prompt) "manual-pass")))
          
          (let ((credentials (mediawiki-auth-get-credentials test-mediawiki-site-config-name)))
            (should (string= (plist-get credentials :username) "manual-user"))
            (should (string= (plist-get credentials :password) "manual-pass")))))
    
    (test-mediawiki-cleanup-test-site)))

;;; Test Runner

(defun test-mediawiki-run-auth-source-tests ()
  "Run all auth-source integration tests."
  (interactive)
  (ert-run-tests-batch-and-exit "test-mediawiki-auth"))

;;; Test Utilities

(defun test-mediawiki-auth-debug-cache ()
  "Debug function to inspect credential cache state."
  (interactive)
  (let ((status (mediawiki-auth-get-cache-status)))
    (message "Cache Status: %s" status)
    (maphash (lambda (key entry)
               (message "Cache Entry: %s -> %s" key 
                       (list :username (plist-get entry :username)
                             :expiry (format-time-string "%H:%M:%S" 
                                                       (plist-get entry :expiry)))))
             mediawiki-auth-credential-cache)))

(provide 'test-auth-source-integration)

;;; test-auth-source-integration.el ends here