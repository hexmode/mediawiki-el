;;; test-session-state-tracking.el --- Test session state tracking implementation -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for session state tracking functionality implemented
;; for task 5.1 of the MediaWiki API modernization spec.

;;; Code:

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-session)

;;; Test Configuration

(defvar test-session-state-sitename "test-wiki"
  "Test site name for session state tracking tests.")

;;; Mock Functions

(defun test-session-state-mock-api-call (sitename action params &optional timeout)
  "Mock API call for testing."
  (make-mediawiki-api-response
   :success t
   :data '((query . ((userinfo . ((name . "testuser") (id . 123))))))
   :warnings nil
   :errors nil))

(defun test-session-state-mock-api-error-info (response)
  "Mock API error info."
  "Mock error")

(defun test-session-state-mock-api-error-code (response)
  "Mock API error code."
  "mock-error")

;;; Test Setup and Teardown

(defun test-session-state-setup ()
  "Set up test environment for session state tracking tests."
  (mediawiki-remove-session test-session-state-sitename)
  (mediawiki-session-clear-all-states t))

(defun test-session-state-teardown ()
  "Clean up test environment after session state tracking tests."
  (mediawiki-remove-session test-session-state-sitename)
  (mediawiki-session-clear-all-states t))

;;; Test Cases

(ert-deftest test-session-state-creation ()
  "Test session state creation."
  (test-session-state-setup)
  (unwind-protect
      (let ((state (mediawiki-session-get-state test-session-state-sitename)))
        (should (mediawiki-session-state-p state))
        (should (eq (mediawiki-session-state-status state) 'disconnected)))
    (test-session-state-teardown)))

(ert-deftest test-session-state-updates ()
  "Test session state updates."
  (test-session-state-setup)
  (unwind-protect
      (progn
        ;; Test active state update
        (mediawiki-session-update-state test-session-state-sitename 'active)
        (let ((state (mediawiki-session-get-state test-session-state-sitename)))
          (should (eq (mediawiki-session-state-status state) 'active))
          (should (> (mediawiki-session-state-validation-count state) 0))))
    (test-session-state-teardown)))

(ert-deftest test-session-state-error-handling ()
  "Test session state error handling."
  (test-session-state-setup)
  (unwind-protect
      (progn
        ;; Test error state handling
        (mediawiki-session-update-state test-session-state-sitename 'error "Test error")
        (let ((state (mediawiki-session-get-state test-session-state-sitename)))
          (should (eq (mediawiki-session-state-status state) 'error))
          (should (> (mediawiki-session-state-error-count state) 0))
          (should (string= (mediawiki-session-state-last-error state) "Test error"))))
    (test-session-state-teardown)))

(ert-deftest test-session-data-structures ()
  "Test session data structures."
  (test-session-state-setup)
  (unwind-protect
      (let ((session (make-mediawiki-session
                      :site-name test-session-state-sitename
                      :tokens (make-hash-table :test 'equal)
                      :login-time (current-time)
                      :last-activity (current-time)
                      :user-info '((name . "testuser") (id . 123)))))
        (mediawiki-set-session test-session-state-sitename session)

        ;; Test session validation
        (should (mediawiki-session-check-activity session))

        ;; Test credential storage tracking
        (mediawiki-session-store-credentials test-session-state-sitename "dummy-credentials")
        (let ((tokens (mediawiki-session-tokens session)))
          (should (gethash "credentials-stored" tokens))))
    (test-session-state-teardown)))

(ert-deftest test-session-health-check ()
  "Test session health check functionality."
  (test-session-state-setup)
  (unwind-protect
      (let ((session (make-mediawiki-session
                      :site-name test-session-state-sitename
                      :tokens (make-hash-table :test 'equal)
                      :login-time (current-time)
                      :last-activity (current-time)
                      :user-info '((name . "testuser") (id . 123)))))
        (mediawiki-set-session test-session-state-sitename session)
        (mediawiki-session-update-state test-session-state-sitename 'active)

        ;; Test health check functionality
        (let ((health (mediawiki-session-health-check test-session-state-sitename)))
          (should (plist-get health :active))
          (should (eq (plist-get health :status) 'active))
          (should (>= (plist-get health :token-count) 0))))
    (test-session-state-teardown)))

(ert-deftest test-session-cleanup ()
  "Test session cleanup."
  (test-session-state-setup)
  (unwind-protect
      (let ((session (make-mediawiki-session
                      :site-name test-session-state-sitename
                      :tokens (make-hash-table :test 'equal)
                      :login-time (current-time)
                      :last-activity (current-time))))
        (mediawiki-set-session test-session-state-sitename session)
        (mediawiki-session-update-state test-session-state-sitename 'active)

        ;; Test session cleanup
        (mediawiki-session-cleanup test-session-state-sitename)
        (should-not (mediawiki-get-session test-session-state-sitename))

        (let ((state (mediawiki-session-get-state test-session-state-sitename)))
          (should (eq (mediawiki-session-state-status state) 'disconnected))))
    (test-session-state-teardown)))

(ert-deftest test-session-state-summary ()
  "Test session state summary."
  (test-session-state-setup)
  (unwind-protect
      (progn
        ;; Create multiple session states
        (mediawiki-session-update-state "wiki1" 'active)
        (mediawiki-session-update-state "wiki2" 'expired)
        (mediawiki-session-update-state "wiki3" 'error "Test error")

        ;; Test session state summary
        (let ((summary (mediawiki-session-get-state-summary)))
          (should (> (plist-get summary :total) 0))))
    (test-session-state-teardown)))

(ert-deftest test-session-logout-procedure ()
  "Test session logout procedure."
  (test-session-state-setup)
  (unwind-protect
      (let ((session (make-mediawiki-session
                      :site-name "logout-test"
                      :tokens (make-hash-table :test 'equal)
                      :login-time (current-time)
                      :last-activity (current-time))))
        (mediawiki-set-session "logout-test" session)
        (mediawiki-session-update-state "logout-test" 'active)

        ;; Mock API functions to avoid network calls
        (cl-letf (((symbol-function 'mediawiki-api-call-sync)
                   #'test-session-state-mock-api-call)
                  ((symbol-function 'mediawiki-api-get-error-info)
                   #'test-session-state-mock-api-error-info))

          ;; Test logout procedure (may fail API call but should clean up)
          (condition-case err
              (mediawiki-session-logout "logout-test")
            (error nil)) ; Ignore API call errors in test

          (should-not (mediawiki-get-session "logout-test"))))
    (test-session-state-teardown)))

(provide 'test-session-state-tracking)

;;; test-session-state-tracking.el ends here
