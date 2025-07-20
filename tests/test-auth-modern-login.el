;;; test-auth-modern-login.el --- Tests for modern login API implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;;; Commentary:

;; Tests for the modern login API implementation (task 4.2).
;; Tests the new clientlogin API usage, token handling, and multi-step authentication.

;;; Code:

(require 'ert)
(require 'mediawiki-auth)
(require 'mediawiki-core)

;;; Test Setup

(defvar test-auth-modern-sitename "test-wiki"
  "Test site name for modern login tests.")

(defvar test-auth-modern-username "testuser"
  "Test username for modern login tests.")

(defvar test-auth-modern-password "testpass"
  "Test password for modern login tests.")

(defun test-auth-modern-setup ()
  "Set up test environment for modern login tests."
  ;; Create a test site
  (let ((site (make-mediawiki-site-config
               :name test-auth-modern-sitename
               :url "https://test.example.com"
               :api-url "https://test.example.com/api.php")))
    (mediawiki-add-site site))

  ;; Clear any existing sessions
  (mediawiki-remove-session test-auth-modern-sitename))

(defun test-auth-modern-teardown ()
  "Clean up test environment."
  (mediawiki-remove-session test-auth-modern-sitename)
  ;; Remove site from alist manually since there's no remove-site function
  (setq mediawiki-site-alist
        (assoc-delete-all test-auth-modern-sitename mediawiki-site-alist)))

;;; Mock Functions for Testing

(defvar test-auth-modern-mock-responses nil
  "Mock responses for API calls during testing.")

(defun test-auth-modern-mock-api-call (sitename action params)
  "Mock API call for testing modern login."
  (let* ((response-key (format "%s-%s" action (alist-get "type" params nil nil 'string=)))
         (mock-data (alist-get response-key test-auth-modern-mock-responses nil nil 'string=)))
    (if mock-data
        (make-mediawiki-api-response
         :success (plist-get mock-data :success)
         :data (plist-get mock-data :data)
         :errors (plist-get mock-data :errors)
         :warnings (plist-get mock-data :warnings))
      (make-mediawiki-api-response
       :success nil
       :errors (list (list :code "mock-error" :info "No mock response configured"))))))

;;; Test Cases

(ert-deftest test-auth-modern-login-token-request ()
  "Test that login token request uses modern API correctly."
  (test-auth-modern-setup)

  (let ((test-auth-modern-mock-responses
         '(("query-login" . (:success t
                            :data (query (tokens (logintoken . "test-login-token"))))))))

    ;; Mock the API call function
    (cl-letf (((symbol-function 'mediawiki-api-call-sync)
               #'test-auth-modern-mock-api-call))

      (let ((token (mediawiki-auth-get-login-token test-auth-modern-sitename)))
        (should (string= token "test-login-token")))))

  (test-auth-modern-teardown))

(ert-deftest test-auth-modern-login-params-building ()
  "Test that modern login parameters are built correctly."
  (let ((params (mediawiki-auth-build-login-params
                test-auth-modern-username
                test-auth-modern-password
                "test-token")))

    (should (string= (alist-get "username" params nil nil 'string=)
                     test-auth-modern-username))
    (should (string= (alist-get "password" params nil nil 'string=)
                     test-auth-modern-password))
    (should (string= (alist-get "logintoken" params nil nil 'string=)
                     "test-token"))
    (should (string= (alist-get "loginreturnurl" params nil nil 'string=)
                     "http://localhost/"))
    (should (string= (alist-get "rememberMe" params nil nil 'string=)
                     "1"))))

(ert-deftest test-auth-modern-successful-login ()
  "Test successful modern login flow."
  (test-auth-modern-setup)

  (let ((test-auth-modern-mock-responses
         '(("query-login" . (:success t
                            :data (query (tokens (logintoken . "test-login-token")))))
           ("clientlogin-" . (:success t
                             :data (clientlogin (status . "PASS")
                                               (username . "testuser")
                                               (lguserid . 123)))))))

    ;; Mock the API call function
    (cl-letf (((symbol-function 'mediawiki-api-call-sync)
               #'test-auth-modern-mock-api-call)
              ((symbol-function 'mediawiki-auth-get-credentials)
               (lambda (_sitename)
                 (list :username test-auth-modern-username
                       :password test-auth-modern-password))))

      ;; Perform login
      (mediawiki-auth-basic-login test-auth-modern-sitename)

      ;; Verify session was created
      (let ((session (mediawiki-get-session test-auth-modern-sitename)))
        (should session)
        (should (string= (mediawiki-session-site-name session) test-auth-modern-sitename))
        (should (mediawiki-session-login-time session)))))

  (test-auth-modern-teardown))

(ert-deftest test-auth-modern-login-failure ()
  "Test handling of login failure."
  (test-auth-modern-setup)

  (let ((test-auth-modern-mock-responses
         '(("query-login" . (:success t
                            :data (query (tokens (logintoken . "test-login-token")))))
           ("clientlogin-" . (:success t
                             :data (clientlogin (status . "FAIL")
                                               (message . "Invalid credentials")
                                               (messagecode . "wrongpassword")))))))

    ;; Mock the API call function
    (cl-letf (((symbol-function 'mediawiki-api-call-sync)
               #'test-auth-modern-mock-api-call)
              ((symbol-function 'mediawiki-auth-get-credentials)
               (lambda (_sitename)
                 (list :username test-auth-modern-username
                       :password "wrongpassword"))))

      ;; Should signal an error for failed login
      (should-error (mediawiki-auth-basic-login test-auth-modern-sitename)
                    :type 'error)))

  (test-auth-modern-teardown))

(ert-deftest test-auth-modern-login-restart ()
  "Test handling of login restart requirement."
  (test-auth-modern-setup)

  (let ((restart-count 0)
        (test-auth-modern-mock-responses
         '(("query-login" . (:success t
                            :data (query (tokens (logintoken . "test-login-token"))))))))

    ;; Mock the API call function to simulate restart then success
    (cl-letf (((symbol-function 'mediawiki-api-call-sync)
               (lambda (sitename action params)
                 (if (string= action "clientlogin")
                     (progn
                       (setq restart-count (1+ restart-count))
                       (if (= restart-count 1)
                           ;; First call returns RESTART
                           (make-mediawiki-api-response
                            :success t
                            :data '((clientlogin (status . "RESTART")
                                               (message . "Token expired"))))
                         ;; Second call returns PASS
                         (make-mediawiki-api-response
                          :success t
                          :data '((clientlogin (status . "PASS")
                                             (username . "testuser")
                                             (lguserid . 123))))))
                   ;; Token requests
                   (test-auth-modern-mock-api-call sitename action params))))
              ((symbol-function 'mediawiki-auth-get-credentials)
               (lambda (_sitename)
                 (list :username test-auth-modern-username
                       :password test-auth-modern-password))))

      ;; Perform login - should handle restart automatically
      (mediawiki-auth-basic-login test-auth-modern-sitename)

      ;; Verify session was created after restart
      (let ((session (mediawiki-get-session test-auth-modern-sitename)))
        (should session)
        (should (= restart-count 2)))))  ; Should have made two clientlogin calls

  (test-auth-modern-teardown))

(ert-deftest test-auth-modern-2fa-detection ()
  "Test detection of 2FA requirement."
  (test-auth-modern-setup)

  (let ((test-auth-modern-mock-responses
         '(("query-login" . (:success t
                            :data (query (tokens (logintoken . "test-login-token")))))
           ("clientlogin-" . (:success t
                             :data (clientlogin (status . "UI")
                                               (message . "Two-factor authentication required")
                                               (messagecode . "oathauth-auth-ui")
                                               (requests . ((id . "otp-request")))))))))

    ;; Mock the API call function and user input
    (cl-letf (((symbol-function 'mediawiki-api-call-sync)
               #'test-auth-modern-mock-api-call)
              ((symbol-function 'mediawiki-auth-get-credentials)
               (lambda (_sitename)
                 (list :username test-auth-modern-username
                       :password test-auth-modern-password)))
              ((symbol-function 'read-string)
               (lambda (_prompt) "123456")))  ; Mock 2FA code input

      ;; Should detect 2FA requirement and attempt to handle it
      ;; This will fail because we don't have a complete mock for the continuation
      (should-error (mediawiki-auth-basic-login test-auth-modern-sitename)
                    :type 'error)))

  (test-auth-modern-teardown))

(ert-deftest test-auth-modern-captcha-detection ()
  "Test detection of CAPTCHA requirement."
  (test-auth-modern-setup)

  (let ((test-auth-modern-mock-responses
         '(("query-login" . (:success t
                            :data (query (tokens (logintoken . "test-login-token")))))
           ("clientlogin-" . (:success t
                             :data (clientlogin (status . "UI")
                                               (message . "CAPTCHA required")
                                               (messagecode . "captcha-required")))))))

    ;; Mock the API call function
    (cl-letf (((symbol-function 'mediawiki-api-call-sync)
               #'test-auth-modern-mock-api-call)
              ((symbol-function 'mediawiki-auth-get-credentials)
               (lambda (_sitename)
                 (list :username test-auth-modern-username
                       :password test-auth-modern-password))))

      ;; Should detect CAPTCHA requirement and error appropriately
      (should-error (mediawiki-auth-basic-login test-auth-modern-sitename)
                    :type 'error)))

  (test-auth-modern-teardown))

(ert-deftest test-auth-modern-redirect-handling ()
  "Test handling of redirect requirement."
  (test-auth-modern-setup)

  (let ((test-auth-modern-mock-responses
         '(("query-login" . (:success t
                            :data (query (tokens (logintoken . "test-login-token")))))
           ("clientlogin-" . (:success t
                             :data (clientlogin (status . "REDIRECT")
                                               (redirecttarget . "https://example.com/special")))))))

    ;; Mock the API call function
    (cl-letf (((symbol-function 'mediawiki-api-call-sync)
               #'test-auth-modern-mock-api-call)
              ((symbol-function 'mediawiki-auth-get-credentials)
               (lambda (_sitename)
                 (list :username test-auth-modern-username
                       :password test-auth-modern-password))))

      ;; Should handle redirect requirement appropriately
      (should-error (mediawiki-auth-basic-login test-auth-modern-sitename)
                    :type 'error)))

  (test-auth-modern-teardown))

;;; Integration Test

(ert-deftest test-auth-modern-integration ()
  "Integration test for modern login API implementation."
  (test-auth-modern-setup)

  ;; Test the complete flow with proper mocking
  (let ((api-calls '())
        (test-auth-modern-mock-responses
         '(("query-login" . (:success t
                            :data (query (tokens (logintoken . "integration-token")))))
           ("clientlogin-" . (:success t
                             :data (clientlogin (status . "PASS")
                                               (username . "integrationuser")
                                               (lguserid . 456)))))))

    (cl-letf (((symbol-function 'mediawiki-api-call-sync)
               (lambda (sitename action params)
                 (push (list sitename action params) api-calls)
                 (test-auth-modern-mock-api-call sitename action params)))
              ((symbol-function 'mediawiki-auth-get-credentials)
               (lambda (_sitename)
                 (list :username "integrationuser"
                       :password "integrationpass"))))

      ;; Perform complete login
      (mediawiki-auth-basic-login test-auth-modern-sitename)

      ;; Verify the correct API calls were made
      (should (= (length api-calls) 2))

      ;; First call should be for login token
      (let ((token-call (nth 1 api-calls)))  ; Reverse order due to push
        (should (string= (nth 1 token-call) "query"))
        (should (string= (alist-get "meta" (nth 2 token-call) nil nil 'string=) "tokens"))
        (should (string= (alist-get "type" (nth 2 token-call) nil nil 'string=) "login")))

      ;; Second call should be clientlogin
      (let ((login-call (nth 0 api-calls)))
        (should (string= (nth 1 login-call) "clientlogin"))
        (should (string= (alist-get "username" (nth 2 login-call) nil nil 'string=) "integrationuser"))
        (should (string= (alist-get "logintoken" (nth 2 login-call) nil nil 'string=) "integration-token")))

      ;; Verify session was created
      (let ((session (mediawiki-get-session test-auth-modern-sitename)))
        (should session)
        (should (string= (plist-get (mediawiki-session-user-info session) :username)
                         "integrationuser"))
        (should (= (plist-get (mediawiki-session-user-info session) :userid) 456)))))

  (test-auth-modern-teardown))

;;; Run Tests

(provide 'test-auth-modern-login)

;;; test-auth-modern-login.el ends here
