;;; test-mediawiki-test-utils.el --- Core testing infrastructure for MediaWiki -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module provides core testing infrastructure for MediaWiki interactive
;; commands. It includes utilities for mocking, state management, and test
;; environment isolation as required by requirements 6.1 and 6.5.

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; Test Environment State Management

(defvar mediawiki-test-saved-state nil
  "Saved MediaWiki state for test isolation.")

(defun mediawiki-test-save-state ()
  "Save current MediaWiki state for restoration after tests.
Saves all relevant global variables that tests might modify."
  (setq mediawiki-test-saved-state
        `((mediawiki-site . ,mediawiki-site)
          (mediawiki-site-alist . ,(copy-alist mediawiki-site-alist))
          (mediawiki-page-history . ,(copy-alist mediawiki-page-history))
          (mediawiki-site-info . ,mediawiki-site-info)
          (mediawiki-debug . ,mediawiki-debug)
          ;; Session state if available
          ,@(when (boundp 'mediawiki-sessions)
              `((mediawiki-sessions . ,(copy-hash-table mediawiki-sessions))))
          ;; UI state if available
          ,@(when (and (featurep 'mediawiki-ui)
                       (boundp 'mediawiki-ui-recent-pages))
              `((mediawiki-ui-recent-pages . ,(copy-hash-table mediawiki-ui-recent-pages)))))))

(defun mediawiki-test-restore-state ()
  "Restore MediaWiki state from saved state.
Restores all global variables to their pre-test values."
  (when mediawiki-test-saved-state
    (dolist (state-pair mediawiki-test-saved-state)
      (let ((var (car state-pair))
            (value (cdr state-pair)))
        (set var value)))
    (setq mediawiki-test-saved-state nil)))

(defun mediawiki-test-clean-state ()
  "Clean up test state and kill test buffers.
Removes all test-related buffers and resets state variables."
  ;; Kill all MediaWiki buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (or (eq major-mode 'mediawiki-mode)
                (string-match-p "^\\*MediaWiki" (buffer-name))
                (string-match-p "TestWiki\\|DevWiki\\|LocalWiki" (buffer-name)))
        (kill-buffer buffer))))

  ;; Clear session data if available
  (when (boundp 'mediawiki-sessions)
    (clrhash mediawiki-sessions))

  ;; Clear UI state if available
  (when (and (featurep 'mediawiki-ui)
             (boundp 'mediawiki-ui-recent-pages))
    (clrhash mediawiki-ui-recent-pages))

  ;; Reset core variables
  (setq mediawiki-site nil
        mediawiki-site-alist nil
        mediawiki-page-history nil
        mediawiki-site-info nil))

;;; Test Environment Isolation Macro

(defmacro mediawiki-test-with-clean-environment (&rest body)
  "Execute BODY with a clean MediaWiki test environment.
Saves current state, provides isolated test environment, and restores
state afterwards. Ensures tests don't interfere with each other."
  `(let ((mediawiki-site nil)
         (mediawiki-site-alist nil)
         (mediawiki-page-history nil)
         (mediawiki-site-info nil)
         (mediawiki-debug nil)
         ;; Create clean session storage if module is loaded
         ,@(when (featurep 'mediawiki-session)
             '((mediawiki-sessions (make-hash-table :test 'equal))))
         ;; Create clean UI state if module is loaded
         ,@(when (featurep 'mediawiki-ui)
             '((mediawiki-ui-recent-pages (make-hash-table :test 'equal)))))
     (unwind-protect
         (progn
           ;; Save original state before test execution
           (mediawiki-test-save-state)
           ,@body)
       ;; Always clean up, even if test fails
       (mediawiki-test-clean-state)
       (mediawiki-test-restore-state))))

;;; Mock Site Management

(defconst mediawiki-test-mock-sites
  '(("TestWiki" . (:name "TestWiki"
                   :url "https://test.wiki.example.com"
                   :api-url "https://test.wiki.example.com/w/api.php"
                   :auth-method basic
                   :first-page "Main Page"))
    ("DevWiki" . (:name "DevWiki"
                  :url "https://dev.wiki.example.com"
                  :api-url "https://dev.wiki.example.com/w/api.php"
                  :auth-method oauth
                  :first-page "Development"))
    ("LocalWiki" . (:name "LocalWiki"
                    :url "http://localhost:8080"
                    :api-url "http://localhost:8080/w/api.php"
                    :auth-method basic
                    :first-page "Home")))
  "Predefined mock site configurations for testing.")

(defun mediawiki-test-create-mock-site (name &optional overrides)
  "Create a mock site configuration for NAME with optional OVERRIDES.
Returns a properly structured site configuration for testing."
  (let* ((base-config (or (cdr (assoc name mediawiki-test-mock-sites))
                          `(:name ,name
                            :url ,(format "https://%s.example.com" (downcase name))
                            :api-url ,(format "https://%s.example.com/w/api.php" (downcase name))
                            :auth-method basic
                            :first-page "Main Page")))
         (merged-config (if overrides
                           (append overrides base-config)
                         base-config)))
    ;; Convert plist to site config structure
    (make-mediawiki-site-config
     :name (plist-get merged-config :name)
     :url (plist-get merged-config :url)
     :api-url (plist-get merged-config :api-url)
     :username (plist-get merged-config :username)
     :auth-method (plist-get merged-config :auth-method)
     :auth-config (plist-get merged-config :auth-config)
     :first-page (plist-get merged-config :first-page))))

(defun mediawiki-test-setup-site-alist (&optional site-names)
  "Set up mediawiki-site-alist with mock sites.
If SITE-NAMES is provided, only set up those sites.
Otherwise, set up all predefined mock sites."
  (let ((sites-to-setup (or site-names
                           (mapcar #'car mediawiki-test-mock-sites))))
    (setq mediawiki-site-alist
          (mapcar (lambda (site-name)
                    (cons site-name (mediawiki-test-create-mock-site site-name)))
                  sites-to-setup))))

(defun mediawiki-test-cleanup-sites ()
  "Clean up all mock sites and reset site-related variables.
Removes all sites from mediawiki-site-alist and clears current site."
  (setq mediawiki-site-alist nil
        mediawiki-site nil
        mediawiki-site-info nil))

(defmacro mediawiki-test-with-mock-sites (site-names &rest body)
  "Execute BODY with mock sites SITE-NAMES set up.
SITE-NAMES can be a single site name or a list of site names."
  `(let ((mediawiki-site-alist nil)
         (mediawiki-site nil))
     (unwind-protect
         (progn
           (mediawiki-test-setup-site-alist ,(if (listp site-names)
                                                site-names
                                              (list site-names)))
           ,@body)
       (mediawiki-test-cleanup-sites))))

(defmacro mediawiki-test-with-mock-site (site-name &rest body)
  "Execute BODY with a single mock site SITE-NAME set up and selected."
  `(mediawiki-test-with-mock-sites (,site-name)
     (setq mediawiki-site ,site-name)
     ,@body))

;;; Mock Authentication System

(defvar mediawiki-test-mock-login-results (make-hash-table :test 'equal)
  "Hash table storing mock login results by site name.")

(defun mediawiki-test-mock-login-success (sitename &optional username)
  "Set up mock successful login for SITENAME with optional USERNAME."
  (let ((username (or username "testuser")))
    (puthash sitename
             `(:success t
               :username ,username
               :login-time ,(current-time))
             mediawiki-test-mock-login-results)))

(defun mediawiki-test-mock-login-failure (sitename &optional error-message)
  "Set up mock login failure for SITENAME with optional ERROR-MESSAGE."
  (let ((error-message (or error-message "Invalid credentials")))
    (puthash sitename
             `(:success nil
               :error-message ,error-message)
             mediawiki-test-mock-login-results)))

(defun mediawiki-test-mock-session-state (sitename state-plist)
  "Set up mock session state for SITENAME with STATE-PLIST."
  (puthash sitename state-plist mediawiki-test-mock-login-results))

(defun mediawiki-test-get-mock-login-result (sitename)
  "Get the mock login result configured for SITENAME."
  (gethash sitename mediawiki-test-mock-login-results))

(defun mediawiki-test-clear-mock-auth ()
  "Clear all mock authentication configurations."
  (clrhash mediawiki-test-mock-login-results))

;;; Input Simulation Framework

(defvar mediawiki-test-input-responses nil
  "Queue of responses for simulated user input.")

(defun mediawiki-test-simulate-user-input (input-type value)
  "Add a simulated user input of INPUT-TYPE with VALUE to the response queue."
  (push `(:type ,input-type :value ,value) mediawiki-test-input-responses))

(defun mediawiki-test-get-next-input (expected-type)
  "Get the next input response of EXPECTED-TYPE from the queue."
  (let ((response (pop mediawiki-test-input-responses)))
    (if response
        (if (eq (plist-get response :type) expected-type)
            (plist-get response :value)
          (error "Expected input type %s but got %s" expected-type (plist-get response :type)))
      (error "No more simulated input responses available for type %s" expected-type))))

(defun mediawiki-test-mock-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Mock implementation of completing-read that uses simulated input."
  (mediawiki-test-get-next-input 'completing-read))

(defun mediawiki-test-mock-read-string (prompt &optional initial-input history default-value inherit-input-method)
  "Mock implementation of read-string that uses simulated input."
  (mediawiki-test-get-next-input 'read-string))

(defmacro mediawiki-test-with-completing-read (response &rest body)
  "Execute BODY with completing-read mocked to return RESPONSE."
  `(let ((original-completing-read (symbol-function 'completing-read)))
     (unwind-protect
         (progn
           (fset 'completing-read #'mediawiki-test-mock-completing-read)
           (mediawiki-test-simulate-user-input 'completing-read ,response)
           ,@body)
       (fset 'completing-read original-completing-read))))

(defmacro mediawiki-test-with-read-string (response &rest body)
  "Execute BODY with read-string mocked to return RESPONSE."
  `(let ((original-read-string (symbol-function 'read-string)))
     (unwind-protect
         (progn
           (fset 'read-string #'mediawiki-test-mock-read-string)
           (mediawiki-test-simulate-user-input 'read-string ,response)
           ,@body)
       (fset 'read-string original-read-string))))

(provide 'test-mediawiki-test-utils)

;;; test-mediawiki-test-utils.el ends here
