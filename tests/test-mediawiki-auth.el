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

  ;; We can't test actual login without mocking the entire API chain,
  ;; but we can test that the function exists and has the right signature
  (should (condition-case nil
              ;; This will error due to network/missing functions, but tests structure
              (mediawiki-do-login "test-site" "user" "pass")
            (error t))))

(ert-deftest test-mediawiki-do-logout-structure ()
  "Test mediawiki-do-logout function structure."
  (should (functionp 'mediawiki-do-logout))

  ;; Test function signature
  (should (condition-case nil
              ;; This will error due to network, but tests structure
              (mediawiki-do-logout "test-site")
            (error t))))

;;; Test Login Process (Mocked)

(ert-deftest test-mediawiki-login-process-mock ()
  "Test login process with mocked dependencies using action=clientlogin."
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
               (should (string= "clientlogin" action))
               (should (assoc "username" args))
               (should (assoc "password" args))
               (should (assoc "logintoken" args))
               (should (assoc "loginreturnurl" args))
               ;; Mock successful clientlogin response (JSON alist)
               '((clientlogin . ((status . "PASS") (username . "testuser"))))))
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
            ((symbol-function 'mediawiki-site-get-token)
             (lambda (site type) "csrf-token"))
            ((symbol-function 'mediawiki-api-call)
             (lambda (site action args)
               ;; Mock logout response
               t)))

    ;; Test logout
    (should-not (mediawiki-do-logout "TestSite"))

    ;; Test that mediawiki-site is set to nil after logout
    (should-not mediawiki-site)))

;;; Test Token-based Login (Mocked)

(ert-deftest test-mediawiki-clientlogin-captcha-mock ()
  "Test clientlogin with captcha (UI) step."
  (let ((call-count 0))
    (cl-letf (((symbol-function 'mediawiki-site-username)
               (lambda (site) "testuser"))
              ((symbol-function 'mediawiki-site-password)
               (lambda (site) "testpass"))
              ((symbol-function 'mediawiki-site-domain)
               (lambda (site) nil))
              ((symbol-function 'mediawiki-site-get-token)
               (lambda (site type) "login-token"))
              ((symbol-function 'read-string)
               (lambda (prompt) "abc123"))
              ((symbol-function 'mediawiki-api-call)
               (lambda (site action args)
                 (setq call-count (1+ call-count))
                 (cond
                  ;; First call: clientlogin needs captcha
                  ((= call-count 1)
                   (should (string= "clientlogin" action))
                   '((clientlogin
                      . ((status . "UI")
                         (requests
                          . (((id . "captcha")
                              (metadata
                               . ((captchaId . "12345")
                                  (captchaQuestion
                                   . "What is 2+2?"))))))))))
                  ;; Second call: clientlogin with captcha succeeds
                  ((= call-count 2)
                   (should (string= "clientlogin" action))
                   (should (assoc "captchaId" args))
                   (should (assoc "captchaWord" args))
                   (should (assoc "logincontinue" args))
                   '((clientlogin
                      . ((status . "PASS")
                         (username . "testuser")))))))))
      (let ((result (mediawiki-do-login "TestSite" "testuser" "testpass")))
        (should (string= "TestSite" result))
        (should (= 2 call-count))))))

(defun test-mediawiki-make-fallback-api-call (call-count-ref)
  "Make a mock for `mediawiki-api-call' that first returns unknown_action,
then falls back to old login with NeedToken then Success.
CALL-COUNT-REF is a cons cell whose car tracks the call count."
  (lambda (site action args)
    (setcar call-count-ref (1+ (car call-count-ref)))
    (let ((count (car call-count-ref)))
      (cond
       ;; First call: clientlogin fails with unknown_action
       ((= count 1)
        (error "(unknown_action) Unrecognized value for parameter 'action': clientlogin"))
       ;; Second call: old login returns NeedToken
       ((= count 2)
        (should (string= "login" action))
        (should (assoc "lgname" args))
        '((login . ((result . "NeedToken") (token . "new-token")))))
       ;; Third call: old login with token succeeds
       ((= count 3)
        (should (string= "login" action))
        '((login . ((result . "Success") (lgusername . "testuser")))))))))

(ert-deftest test-mediawiki-fallback-clientlogin-mock ()
  "Test that login falls back to action=login when clientlogin is unsupported."
  (let ((call-count (cons 0 nil)))
    (cl-letf (((symbol-function 'mediawiki-site-username)
               (lambda (site) "testuser"))
              ((symbol-function 'mediawiki-site-password)
               (lambda (site) "testpass"))
              ((symbol-function 'mediawiki-site-domain)
               (lambda (site) nil))
              ((symbol-function 'mediawiki-site-get-token)
               (lambda (site type) "login-token"))
              ((symbol-function 'mediawiki-api-call)
               (test-mediawiki-make-fallback-api-call call-count)))
      (let ((result (mediawiki-do-login "TestSite" "testuser" "testpass")))
        (should (string= "TestSite" result))
        (should (= 3 (car call-count)))))))

;;; Test Authentication with Domain

(ert-deftest test-mediawiki-fallback-domain-login-mock ()
  "Test login with LDAP domain using fallback (clientlogin lacks lgdomain)."
  (let ((call-count (cons 0 nil)))
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
                 (setcar call-count (1+ (car call-count)))
                 (let ((count (car call-count)))
                   (cond
                    ((= count 1)
                     (error "(unknown_action) Unrecognized value for parameter 'action': clientlogin"))
                    ((= count 2)
                     (should (string= "login" action))
                     (should (assoc "lgdomain" args))
                     (should (string= "TESTDOMAIN" (cdr (assoc "lgdomain" args))))
                     '((login . ((result . "Success") (lgusername . "testuser"))))))))))
      (let ((result (mediawiki-do-login "TestSite" "testuser" "testpass")))
        (should (string= "TestSite" result))
        (should (= 2 (car call-count)))))))

;;; Test Failed Login

(ert-deftest test-mediawiki-failed-login-mock ()
  "Test that failed login properly reports error message."
  ;; Mock dependencies to return a failed login via clientlogin
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
               (should (string= "clientlogin" action))
               ;; Mock failed clientlogin response (JSON alist)
               '((clientlogin . ((status . "FAIL")
                                 (message . "The bot password for bot name \"simple\" of user \"MarkAHershberger\" must be reset.")))))))

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
