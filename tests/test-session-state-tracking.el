;;; test-session-state-tracking.el --- Test session state tracking implementation -*- lexical-binding: t; -*-

;; Test for task 5.1: Implement session state tracking

;;; Commentary:

;; This file tests the session state tracking functionality implemented
;; for task 5.1 of the MediaWiki API modernization spec.

;;; Code:

(add-to-list 'load-path ".")
(require 'mediawiki-core)

;; Mock API functions to avoid dependency
(defun mediawiki-api-call-sync (sitename action params &optional timeout)
  "Mock API call for testing."
  (make-mediawiki-api-response
   :success t
   :data '((query . ((userinfo . ((name . "testuser") (id . 123))))))
   :warnings nil
   :errors nil))

(defun mediawiki-api-response-success (response)
  "Mock API response success check."
  (mediawiki-api-response-success response))

(defun mediawiki-api-get-error-info (response)
  "Mock API error info."
  "Mock error")

(defun mediawiki-api-get-error-code (response)
  "Mock API error code."
  "mock-error")

;; Load session module after mocking API functions
(load "mediawiki-session.el")

;; Enable debugging for tests
(setq mediawiki-debug t)

(defun test-session-state-tracking ()
  "Test session state tracking functionality."
  (interactive)
  (message "Testing session state tracking...")
  
  ;; Test 1: Create session state
  (message "Test 1: Creating session state...")
  (let ((state (mediawiki-session-get-state "test-wiki")))
    (unless (mediawiki-session-state-p state)
      (error "Failed to create session state"))
    (unless (eq (mediawiki-session-state-status state) 'disconnected)
      (error "Initial state should be disconnected"))
    (message "✓ Session state created successfully"))
  
  ;; Test 2: Update session state
  (message "Test 2: Updating session state...")
  (mediawiki-session-update-state "test-wiki" 'active)
  (let ((state (mediawiki-session-get-state "test-wiki")))
    (unless (eq (mediawiki-session-state-status state) 'active)
      (error "State should be active after update"))
    (unless (> (mediawiki-session-state-validation-count state) 0)
      (error "Validation count should be incremented"))
    (message "✓ Session state updated successfully"))
  
  ;; Test 3: Error state handling
  (message "Test 3: Testing error state handling...")
  (mediawiki-session-update-state "test-wiki" 'error "Test error")
  (let ((state (mediawiki-session-get-state "test-wiki")))
    (unless (eq (mediawiki-session-state-status state) 'error)
      (error "State should be error"))
    (unless (> (mediawiki-session-state-error-count state) 0)
      (error "Error count should be incremented"))
    (unless (string= (mediawiki-session-state-last-error state) "Test error")
      (error "Last error should be recorded"))
    (message "✓ Error state handling works"))
  
  ;; Test 4: Create mock session for testing
  (message "Test 4: Testing session data structures...")
  (let ((session (make-mediawiki-session
                  :site-name "test-wiki"
                  :tokens (make-hash-table :test 'equal)
                  :login-time (current-time)
                  :last-activity (current-time)
                  :user-info '((name . "testuser") (id . 123)))))
    (mediawiki-set-session "test-wiki" session)
    
    ;; Test session validation
    (unless (mediawiki-session-check-activity session)
      (error "Session should show recent activity"))
    
    ;; Test credential storage tracking
    (mediawiki-session-store-credentials "test-wiki" "dummy-credentials")
    (let ((tokens (mediawiki-session-tokens session)))
      (unless (gethash "credentials-stored" tokens)
        (error "Credentials storage should be tracked"))
      (message "✓ Session data structures work correctly")))
  
  ;; Test 5: Health check functionality
  (message "Test 5: Testing health check functionality...")
  (let ((health (mediawiki-session-health-check "test-wiki")))
    (unless (plist-get health :active)
      (error "Health check should show session as active"))
    (unless (eq (plist-get health :status) 'active)
      (error "Health check should show active status"))
    (unless (> (plist-get health :token-count) 0)
      (error "Health check should show token count"))
    (message "✓ Health check functionality works"))
  
  ;; Test 6: Session cleanup
  (message "Test 6: Testing session cleanup...")
  (mediawiki-session-cleanup "test-wiki")
  (let ((session (mediawiki-get-session "test-wiki")))
    (when session
      (error "Session should be removed after cleanup")))
  (let ((state (mediawiki-session-get-state "test-wiki")))
    (unless (eq (mediawiki-session-state-status state) 'disconnected)
      (error "State should be disconnected after cleanup"))
    (message "✓ Session cleanup works correctly"))
  
  ;; Test 7: Session state summary
  (message "Test 7: Testing session state summary...")
  (mediawiki-session-update-state "wiki1" 'active)
  (mediawiki-session-update-state "wiki2" 'expired)
  (mediawiki-session-update-state "wiki3" 'error "Test error")
  (let ((summary (mediawiki-session-get-state-summary)))
    (unless (> (plist-get summary :total) 0)
      (error "Summary should show total sessions"))
    (message "✓ Session state summary works"))
  
  ;; Test 8: Session logout procedure
  (message "Test 8: Testing logout procedure...")
  (let ((session (make-mediawiki-session
                  :site-name "logout-test"
                  :tokens (make-hash-table :test 'equal)
                  :login-time (current-time)
                  :last-activity (current-time))))
    (mediawiki-set-session "logout-test" session)
    (mediawiki-session-update-state "logout-test" 'active)
    
    ;; Note: This will fail the server notification part, but should still clean up
    (condition-case err
        (mediawiki-session-logout "logout-test")
      (error nil)) ; Ignore API call errors in test
    
    (let ((cleaned-session (mediawiki-get-session "logout-test")))
      (when cleaned-session
        (error "Session should be cleaned up after logout")))
    (message "✓ Logout procedure works"))
  
  ;; Clean up test data
  (mediawiki-session-clear-all-states t)
  
  (message "✅ All session state tracking tests passed!"))

;; Run the test
(test-session-state-tracking)

(provide 'test-session-state-tracking)

;;; test-session-state-tracking.el ends here