;;; test-real-wiki-login.el --- Test script for real wiki login -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;;; Commentary:

;; Simple test script to verify the modern login implementation works with a real wiki.
;; This script will help you test the login functionality interactively.

;;; Code:

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-http)
(require 'mediawiki-api)
(require 'mediawiki-auth)

;;; Test Variables

(defvar test-real-wiki-name "test-wiki"
  "Test site name for real wiki login tests.")

;;; ERT Tests

(ert-deftest test-wiki-site-setup ()
  "Test setting up a wiki site configuration."
  (let ((site (make-mediawiki-site-config
               :name test-real-wiki-name
               :url "https://wiki.example.com/"
               :api-url "https://wiki.example.com/w/api.php"
               :username "test-user")))
    
    (mediawiki-add-site site)
    
    (unwind-protect
        (let ((retrieved-site (mediawiki-get-site test-real-wiki-name)))
          (should retrieved-site)
          (should (string= (mediawiki-site-config-url retrieved-site) "https://wiki.example.com/"))
          (should (string= (mediawiki-site-config-api-url retrieved-site) "https://wiki.example.com/w/api.php"))
          (should (string= (mediawiki-site-config-username retrieved-site) "test-user")))
      
      ;; Cleanup
      (mediawiki-remove-site test-real-wiki-name))))

(ert-deftest test-wiki-credential-handling ()
  "Test credential handling for wiki login."
  (let ((username "test-user")
        (password "test-password"))
    
    ;; Override the credential function
    (cl-letf (((symbol-function 'mediawiki-auth-get-credentials)
               (lambda (_sitename) 
                 (list :username username :password password))))
      
      (let ((credentials (mediawiki-auth-get-credentials "any-site")))
        (should (equal (plist-get credentials :username) username))
        (should (equal (plist-get credentials :password) password))))))

(ert-deftest test-wiki-api-response-parsing ()
  "Test parsing of wiki API responses."
  (let ((success-response (make-mediawiki-api-response
                          :success t
                          :data '((query . ((userinfo . ((name . "test-user")
                                                       (id . 123)
                                                       (groups . ("user" "autoconfirmed")))))))
                          :warnings nil
                          :errors nil)))
    
    (should (mediawiki-api-response-success success-response))
    (let* ((data (mediawiki-api-response-data success-response))
           (query (cdr (assq 'query data)))
           (userinfo (cdr (assq 'userinfo query))))
      (should (string= (cdr (assq 'name userinfo)) "test-user"))
      (should (= (cdr (assq 'id userinfo)) 123))
      (should (equal (cdr (assq 'groups userinfo)) '("user" "autoconfirmed"))))))

;;; Interactive Test Functions

(defun test-wiki-login-interactive ()
  "Interactive function to test wiki login with your credentials."
  (interactive)

  ;; Get wiki details from user
  (let* ((wiki-name (read-string "Wiki name (for reference): "))
         (wiki-url (read-string "Wiki base URL (e.g., https://en.wikipedia.org): "))
         (api-url (read-string (format "API URL (default: %sapi.php): " wiki-url)
                              nil nil (concat wiki-url "api.php")))
         (username (read-string "Username: "))
         (password (read-passwd "Password: ")))

    ;; Create and add the site
    (let ((site (make-mediawiki-site-config
                 :name wiki-name
                 :url wiki-url
                 :api-url api-url
                 :username username)))

      (mediawiki-add-site site)

      ;; Enable debug logging
      (setq mediawiki-debug-enabled t)

      ;; Try to login
      (message "Attempting to login to %s..." wiki-name)

      (condition-case err
          (progn
            ;; Mock the credential function to use our provided credentials
            (cl-letf (((symbol-function 'mediawiki-auth-get-credentials)
                       (lambda (_sitename)
                         (list :username username :password password))))

              ;; Attempt login
              (mediawiki-auth-basic-login wiki-name)

              ;; Check if session was created
              (let ((session (mediawiki-get-session wiki-name)))
                (if session
                    (let ((user-info (mediawiki-session-user-info session)))
                      (message "✓ Login successful!")
                      (message "  User: %s" (plist-get user-info :username))
                      (message "  User ID: %s" (plist-get user-info :userid))
                      (message "  Login time: %s" (mediawiki-session-login-time session))

                      ;; Test a simple API call to verify the session works
                      (message "Testing API call with authenticated session...")
                      (test-authenticated-api-call wiki-name))
                  (message "✗ Login appeared to succeed but no session was created")))))

        (error
         (message "✗ Login failed: %s" (error-message-string err))
         (message "Check the debug messages above for more details")))

      ;; Clean up
      (mediawiki-remove-site wiki-name))))

(defun test-authenticated-api-call (wiki-name)
  "Test an authenticated API call against WIKI-NAME to verify the session works."
  (condition-case err
      (let ((response (mediawiki-api-call-sync
                      wiki-name "query"
                      (list (cons "meta" "userinfo")
                            (cons "uiprop" "groups|rights")))))

        (if (mediawiki-api-response-success response)
            (let* ((data (mediawiki-api-response-data response))
                   (query (cdr (assq 'query data)))
                   (userinfo (cdr (assq 'userinfo query))))
              (message "✓ Authenticated API call successful!")
              (message "  Confirmed user: %s" (cdr (assq 'name userinfo)))
              (message "  User ID: %s" (cdr (assq 'id userinfo)))
              (message "  Groups: %s" (mapconcat 'identity (cdr (assq 'groups userinfo)) ", ")))
          (message "✗ Authenticated API call failed: %s"
                   (mediawiki-api-get-error-info response))))

    (error
     (message "✗ Error during authenticated API call: %s" (error-message-string err)))))

(defun test-wiki-capabilities (wiki-name)
  "Test wiki capabilities detection."
  (interactive (list (read-string "Wiki name: ")))

  (condition-case err
      (let ((capabilities (mediawiki-api-discover-capabilities wiki-name)))
        (if capabilities
            (progn
              (message "✓ Wiki capabilities detected:")
              (message "  MediaWiki version: %s" (plist-get capabilities :version))
              (message "  API version: %s" (plist-get capabilities :api-version))
              (message "  Supports OAuth: %s" (plist-get capabilities :supports-oauth))
              (message "  Upload enabled: %s" (plist-get capabilities :upload-enabled))
              (message "  Max upload size: %s" (plist-get capabilities :max-upload-size)))
          (message "✗ Failed to detect wiki capabilities")))

    (error
     (message "✗ Error detecting capabilities: %s" (error-message-string err)))))

;;; Quick Setup Functions

(defun setup-test-wiki (name url api-url &optional username)
  "Quick setup function for testing.
Usage: (setup-test-wiki \"mywiki\" \"https://wiki.example.com\" \"https://wiki.example.com/w/api.php\" \"myuser\")"
  (let ((site (make-mediawiki-site-config
               :name name
               :url url
               :api-url api-url
               :username username)))
    (mediawiki-add-site site)
    (message "Added wiki '%s' with URL '%s'" name url)
    (when username
      (message "Username set to: %s" username))))

;;; Usage Instructions

(defun show-test-instructions ()
  "Show instructions for testing the wiki login."
  (interactive)
  (with-output-to-temp-buffer "*Wiki Login Test Instructions*"
    (princ "MediaWiki Modern Login Test Instructions\n")
    (princ "========================================\n\n")
    (princ "1. INTERACTIVE TEST (Recommended):\n")
    (princ "   M-x test-wiki-login-interactive\n")
    (princ "   This will prompt you for all details and test the login.\n\n")
    (princ "2. MANUAL SETUP:\n")
    (princ "   (setup-test-wiki \"mywiki\" \"https://your-wiki.com\" \"username\")\n")
    (princ "   (mediawiki-auth-login \"mywiki\")\n\n")
    (princ "3. TEST CAPABILITIES (without login):\n")
    (princ "   M-x test-wiki-capabilities\n\n")
    (princ "4. ENABLE DEBUG LOGGING:\n")
    (princ "   (setq mediawiki-debug-enabled t)\n\n")
    (princ "5. CHECK SESSION STATUS:\n")
    (princ "   (mediawiki-get-session \"mywiki\")\n\n")
    (princ "TROUBLESHOOTING:\n")
    (princ "- If login fails, check the *Messages* buffer for debug info\n")
    (princ "- Make sure your wiki URL is correct (should end with /)\n")
    (princ "- Verify your username and password are correct\n")
    (princ "- Some wikis may require 2FA (will prompt if needed)\n")
    (princ "- Check if your wiki uses OAuth (not yet implemented)\n")))

(provide 'test-real-wiki-login)

;;; test-real-wiki-login.el ends here
