;;; test-csrf-simple.el --- Simple test for CSRF token functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for CSRF token management system functionality

;;; Code:

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-session)

;;; Test Configuration

(defvar test-csrf-simple-sitename "test-site"
  "Test site name for CSRF simple tests.")

;;; Test Setup and Teardown

(defun test-csrf-simple-setup ()
  "Set up test environment for CSRF simple tests."
  (mediawiki-remove-session test-csrf-simple-sitename))

(defun test-csrf-simple-teardown ()
  "Clean up test environment after CSRF simple tests."
  (mediawiki-remove-session test-csrf-simple-sitename))

;;; Test Cases

(ert-deftest test-csrf-token-caching-and-validation ()
  "Test CSRF token caching and validation."
  (test-csrf-simple-setup)
  (unwind-protect
      (let ((session (make-mediawiki-session
                      :site-name test-csrf-simple-sitename
                      :tokens (make-hash-table :test 'equal)
                      :user-info '(:username "testuser")
                      :login-time (current-time)
                      :last-activity (current-time))))
        (mediawiki-set-session test-csrf-simple-sitename session)

        ;; Test token caching
        (mediawiki-session-cache-token session "csrf" "test-csrf-token-123")

        ;; Test token validation
        (should (mediawiki-session-validate-csrf-token test-csrf-simple-sitename "test-csrf-token-123")))
    (test-csrf-simple-teardown)))

(ert-deftest test-csrf-token-info-retrieval ()
  "Test CSRF token info retrieval."
  (test-csrf-simple-setup)
  (unwind-protect
      (let ((session (make-mediawiki-session
                      :site-name test-csrf-simple-sitename
                      :tokens (make-hash-table :test 'equal)
                      :user-info '(:username "testuser")
                      :login-time (current-time)
                      :last-activity (current-time))))
        (mediawiki-set-session test-csrf-simple-sitename session)
        (mediawiki-session-cache-token session "csrf" "test-csrf-token-123")

        ;; Test token info retrieval
        (let ((token-info (mediawiki-session-get-token-info test-csrf-simple-sitename "csrf")))
          (should (plist-get token-info :token-exists))))
    (test-csrf-simple-teardown)))

(ert-deftest test-csrf-token-invalidation ()
  "Test CSRF token invalidation."
  (test-csrf-simple-setup)
  (unwind-protect
      (let ((session (make-mediawiki-session
                      :site-name test-csrf-simple-sitename
                      :tokens (make-hash-table :test 'equal)
                      :user-info '(:username "testuser")
                      :login-time (current-time)
                      :last-activity (current-time))))
        (mediawiki-set-session test-csrf-simple-sitename session)
        (mediawiki-session-cache-token session "csrf" "test-csrf-token-123")

        ;; Test token invalidation
        (mediawiki-session-invalidate-csrf-token test-csrf-simple-sitename)
        (should-not (mediawiki-session-validate-csrf-token test-csrf-simple-sitename "test-csrf-token-123")))
    (test-csrf-simple-teardown)))

(ert-deftest test-csrf-token-error-handling ()
  "Test CSRF token error handling."
  (test-csrf-simple-setup)
  (unwind-protect
      (let ((session (make-mediawiki-session
                      :site-name test-csrf-simple-sitename
                      :tokens (make-hash-table :test 'equal)
                      :user-info '(:username "testuser")
                      :login-time (current-time)
                      :last-activity (current-time))))
        (mediawiki-set-session test-csrf-simple-sitename session)

        ;; Test error handling
        (let ((result (mediawiki-session-handle-token-error test-csrf-simple-sitename "badtoken")))
          (should (eq result 'csrf-token-invalidated))))
    (test-csrf-simple-teardown)))

(ert-deftest test-csrf-operation-token-type-detection ()
  "Test operation token type detection."
  (test-csrf-simple-setup)
  (unwind-protect
      (progn
        ;; Test operation token type detection
        (let ((token-type (mediawiki-session-get-token-type-for-operation 'edit)))
          (should (string= token-type "csrf"))))
    (test-csrf-simple-teardown)))

(ert-deftest test-csrf-token-statistics ()
  "Test token statistics functionality."
  (test-csrf-simple-setup)
  (unwind-protect
      (let ((session (make-mediawiki-session
                      :site-name test-csrf-simple-sitename
                      :tokens (make-hash-table :test 'equal)
                      :user-info '(:username "testuser")
                      :login-time (current-time)
                      :last-activity (current-time))))
        (mediawiki-set-session test-csrf-simple-sitename session)

        ;; Test token statistics
        (let ((stats (mediawiki-session-get-token-statistics test-csrf-simple-sitename)))
          (should (plist-member stats :total-tokens))))
    (test-csrf-simple-teardown)))

(ert-deftest test-csrf-token-cache-health-check ()
  "Test token cache health check."
  (test-csrf-simple-setup)
  (unwind-protect
      (let ((session (make-mediawiki-session
                      :site-name test-csrf-simple-sitename
                      :tokens (make-hash-table :test 'equal)
                      :user-info '(:username "testuser")
                      :login-time (current-time)
                      :last-activity (current-time))))
        (mediawiki-set-session test-csrf-simple-sitename session)

        ;; Test token cache health check
        (let ((health (mediawiki-session-token-cache-health-check test-csrf-simple-sitename)))
          (should (plist-get health :healthy))))
    (test-csrf-simple-teardown)))

(ert-deftest test-csrf-token-cache-cleanup ()
  "Test token cache cleanup."
  (test-csrf-simple-setup)
  (unwind-protect
      (let ((session (make-mediawiki-session
                      :site-name test-csrf-simple-sitename
                      :tokens (make-hash-table :test 'equal)
                      :user-info '(:username "testuser")
                      :login-time (current-time)
                      :last-activity (current-time))))
        (mediawiki-set-session test-csrf-simple-sitename session)

        ;; Test token cache cleanup
        (let ((cleanup-count (mediawiki-session-cleanup-token-cache test-csrf-simple-sitename)))
          (should (numberp cleanup-count))))
    (test-csrf-simple-teardown)))

(ert-deftest test-csrf-token-cache-export ()
  "Test token cache export functionality."
  (test-csrf-simple-setup)
  (unwind-protect
      (let ((session (make-mediawiki-session
                      :site-name test-csrf-simple-sitename
                      :tokens (make-hash-table :test 'equal)
                      :user-info '(:username "testuser")
                      :login-time (current-time)
                      :last-activity (current-time))))
        (mediawiki-set-session test-csrf-simple-sitename session)

        ;; Test token export
        (let ((export (mediawiki-session-export-token-cache test-csrf-simple-sitename)))
          (should (plist-get export :sitename))))
    (test-csrf-simple-teardown)))

(provide 'test-csrf-simple)

;;; test-csrf-simple.el ends here
