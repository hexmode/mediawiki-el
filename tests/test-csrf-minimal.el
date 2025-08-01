;;; test-csrf-minimal.el --- Minimal test for CSRF token functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal ERT tests for CSRF token functionality

;;; Code:

(require 'ert)
(require 'mediawiki-core)

;;; Test Configuration

(defvar test-csrf-minimal-sitename "test-site"
  "Test site name for CSRF minimal tests.")

;;; Test Setup and Teardown

(defun test-csrf-minimal-setup ()
  "Set up test environment for CSRF minimal tests."
  ;; Clean up any existing session
  (mediawiki-remove-session test-csrf-minimal-sitename))

(defun test-csrf-minimal-teardown ()
  "Clean up test environment after CSRF minimal tests."
  (mediawiki-remove-session test-csrf-minimal-sitename))

;;; Test Cases

(ert-deftest test-csrf-token-caching ()
  "Test basic CSRF token caching functionality."
  (test-csrf-minimal-setup)
  (unwind-protect
      (let ((session (make-mediawiki-session
                      :site-name test-csrf-minimal-sitename
                      :tokens (make-hash-table :test 'equal)
                      :user-info '(:username "testuser")
                      :login-time (current-time)
                      :last-activity (current-time))))
        (mediawiki-set-session test-csrf-minimal-sitename session)

        ;; Test token caching
        (puthash "csrf" "test-token-123" (mediawiki-session-tokens session))
        (puthash "csrf-expiry" (time-add (current-time) 3600) (mediawiki-session-tokens session))

        ;; Verify session was created
        (should (mediawiki-get-session test-csrf-minimal-sitename))

        ;; Test token retrieval
        (let ((token (gethash "csrf" (mediawiki-session-tokens session))))
          (should (string= token "test-token-123")))

        ;; Test expiry was set
        (let ((expiry (gethash "csrf-expiry" (mediawiki-session-tokens session))))
          (should expiry)
          (should (time-less-p (current-time) expiry))))

    (test-csrf-minimal-teardown)))

(ert-deftest test-csrf-session-creation ()
  "Test that sessions can be created and retrieved properly."
  (test-csrf-minimal-setup)
  (unwind-protect
      (let ((session (make-mediawiki-session
                      :site-name test-csrf-minimal-sitename
                      :tokens (make-hash-table :test 'equal)
                      :user-info '(:username "testuser")
                      :login-time (current-time)
                      :last-activity (current-time))))
        (mediawiki-set-session test-csrf-minimal-sitename session)

        ;; Verify session properties
        (let ((retrieved-session (mediawiki-get-session test-csrf-minimal-sitename)))
          (should retrieved-session)
          (should (string= (mediawiki-session-site-name retrieved-session) test-csrf-minimal-sitename))
          (should (mediawiki-session-tokens retrieved-session))
          (should (mediawiki-session-user-info retrieved-session))
          (should (mediawiki-session-login-time retrieved-session))))

    (test-csrf-minimal-teardown)))

(provide 'test-csrf-minimal)

;;; test-csrf-minimal.el ends here
