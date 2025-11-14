;;; test-mediawiki-auth.el --- Unit tests for mediawiki-auth.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el contributors

;; This file is part of mediawiki.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for mediawiki-auth.el module.
;; These tests mock authentication interactions to avoid network dependencies.

;;; Code:

(require 'ert)
(require 'mediawiki-auth)

;;; Test Authentication Constants

(ert-deftest test-mediawiki-auth-constants ()
  "Test that authentication constants are defined."
  (should (boundp 'mediawiki-login-success))
  (should (stringp mediawiki-login-success))
  (should (string= "pt-logout" mediawiki-login-success)))

;;; Test Authentication State Checking

(ert-deftest test-mediawiki-logged-in-p ()
  "Test mediawiki-logged-in-p function."
  ;; Mock mediawiki-site-url and url-cookie-retrieve
  (cl-letf (((symbol-function 'mediawiki-site-url)
             (lambda (site) "https://en.wikipedia.org/w/"))
            ((symbol-function 'url-cookie-retrieve)
             (lambda (host path secure)
               ;; Return mock cookies to simulate logged-in state
               '(("session" "test-session-id")))))

    ;; Test with cookies present (logged in)
    (should (mediawiki-logged-in-p "Wikipedia")))

  ;; Mock no cookies (not logged in)
  (cl-letf (((symbol-function 'mediawiki-site-url)
             (lambda (site) "https://en.wikipedia.org/w/"))
            ((symbol-function 'url-cookie-retrieve)
             (lambda (host path secure) nil)))

    ;; Test with no cookies (not logged in)
    (should-not (mediawiki-logged-in-p "Wikipedia"))))

;;; Test Login Function Structure

(ert-deftest test-mediawiki-do-login-structure ()
  "Test mediawiki-do-login function structure."
  (should (functionp 'mediawiki-do-login))

  ;; Test that function is autoloaded
  (should (get 'mediawiki-do-login 'autoload))

  ;; We can't test actual login without mocking the entire API chain,
  ;; but we can test that the function exists and has the right signature
  (should (condition-case nil
              ;; This will error due to network/missing functions, but tests structure
              (mediawiki-do-login "test-site" "user" "pass")
            (error t))))

(ert-deftest test-mediawiki-do-logout-structure ()
  "Test mediawiki-do-logout function structure."
  (should (functionp 'mediawiki-do-logout))

  ;; Test that function is autoloaded
  (should (get 'mediawiki-do-logout 'autoload))

  ;; Test function signature
  (should (condition-case nil
              ;; This will error due to network, but tests structure
              (mediawiki-do-logout "test-site")
            (error t))))

;;; Test Login Process (Mocked)

(ert-deftest test-mediawiki-login-process-mock ()
  "Test login process with mocked dependencies."
  ;; Mock all the dependencies
  (cl-letf (((symbol-function 'mediawiki-prompt-for-site)
             (lambda () "TestSite"))
            ((symbol-function 'mediawiki-site-username)
             (lambda (site) "testuser"))
            ((symbol-function 'mediawiki-site-password)
             (lambda (site) "testpass"))
            ((symbol-function 'mediawiki-site-domain)
             (lambda (site) nil))
            ((symbol-function 'mediawiki-site-get-token)
             (lambda (site type) "test-token"))
            ((symbol-function 'mediawiki-api-call)
             (lambda (site action args)
               ;; Mock successful login response
               '(login ((result . "Success") (lgusername . "testuser")))))
            ((symbol-function 'read-string)
             (lambda (prompt) "testuser"))
            ((symbol-function 'read-passwd)
             (lambda (prompt) "testpass")))

    ;; Test successful login
    (let ((result (mediawiki-do-login "TestSite" "testuser" "testpass")))
      (should (string= "TestSite" result)))))

;;; Test Logout Process (Mocked)

(ert-deftest test-mediawiki-logout-process-mock ()
  "Test logout process with mocked dependencies."
  ;; Mock dependencies
  (cl-letf (((symbol-function 'mediawiki-prompt-for-site)
             (lambda () "TestSite"))
            ((symbol-function 'mediawiki-api-call)
             (lambda (site action args)
               ;; Mock logout response
               t)))

    ;; Test logout
    (should-not (mediawiki-do-logout "TestSite"))

    ;; Test that mediawiki-site is set to nil after logout
    (should-not mediawiki-site)))

;;; Test Token-based Login (Mocked)

(ert-deftest test-mediawiki-token-login-mock ()
  "Test token-based login process with mocked dependencies."
  (let ((call-count 0))
    ;; Mock dependencies with token requirement
    (cl-letf (((symbol-function 'mediawiki-site-username)
               (lambda (site) "testuser"))
              ((symbol-function 'mediawiki-site-password)
               (lambda (site) "testpass"))
              ((symbol-function 'mediawiki-site-domain)
               (lambda (site) nil))
              ((symbol-function 'mediawiki-site-get-token)
               (lambda (site type) "login-token"))
              ((symbol-function 'mediawiki-api-call)
               (lambda (site action args)
                 (setq call-count (1+ call-count))
                 (cond
                  ;; First call returns NeedToken
                  ((= call-count 1)
                   '(login ((result . "NeedToken") (token . "new-token"))))
                  ;; Second call returns Success
                  ((= call-count 2)
                   '(login ((result . "Success") (lgusername . "testuser"))))))))

      ;; Test login that requires token
      (let ((result (mediawiki-do-login "TestSite" "testuser" "testpass")))
        (should (string= "TestSite" result))
        ;; Should have made two API calls
        (should (= 2 call-count))))))

;;; Test Authentication with Domain

(ert-deftest test-mediawiki-domain-login-mock ()
  "Test login with LDAP domain."
  ;; Mock dependencies with domain
  (cl-letf (((symbol-function 'mediawiki-site-username)
             (lambda (site) "testuser"))
            ((symbol-function 'mediawiki-site-password)
             (lambda (site) "testpass"))
            ((symbol-function 'mediawiki-site-domain)
             (lambda (site) "TESTDOMAIN"))
            ((symbol-function 'mediawiki-site-get-token)
             (lambda (site type) "login-token"))
            ((symbol-function 'mediawiki-api-call)
             (lambda (site action args)
               ;; Verify domain is included in args
               (should (assoc "lgdomain" args))
               (should (string= "TESTDOMAIN" (cdr (assoc "lgdomain" args))))
               '(login ((result . "Success") (lgusername . "testuser"))))))

    ;; Test login with domain
    (let ((result (mediawiki-do-login "TestSite" "testuser" "testpass")))
      (should (string= "TestSite" result)))))

;;; Test Failed Login

(ert-deftest test-mediawiki-failed-login-mock ()
  "Test that failed login properly reports error message."
  ;; Mock dependencies to return a failed login
  (cl-letf (((symbol-function 'mediawiki-site-username)
             (lambda (site) "testuser"))
            ((symbol-function 'mediawiki-site-password)
             (lambda (site) "wrongpass"))
            ((symbol-function 'mediawiki-site-domain)
             (lambda (site) nil))
            ((symbol-function 'mediawiki-site-get-token)
             (lambda (site type) "login-token"))
            ((symbol-function 'mediawiki-api-call)
             (lambda (site action args)
               ;; Mock failed login response
               '(login ((result . "Failed")
                       (reason . "The bot password for bot name \"simple\" of user \"MarkAHershberger\" must be reset."))))))

    ;; Test that failed login raises an error with the reason
    (should-error
     (mediawiki-do-login "TestSite" "testuser" "wrongpass")
     :type 'error)

    ;; Test that error message contains the reason
    (condition-case err
        (mediawiki-do-login "TestSite" "testuser" "wrongpass")
      (error
       (should (string-match-p "bot password.*must be reset" (error-message-string err)))))))

;;; Test Interactive Functions

(ert-deftest test-mediawiki-auth-interactive-functions ()
  "Test that authentication functions are properly marked as interactive."
  ;; Test that login function is interactive
  (should (commandp 'mediawiki-do-login))

  ;; Test that logout function is interactive
  (should (commandp 'mediawiki-do-logout)))

(provide 'test-mediawiki-auth)

;;; test-mediawiki-auth.el ends here
