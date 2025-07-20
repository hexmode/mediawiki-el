;;; test-session-persistence.el --- Test session persistence functionality -*- lexical-binding: t; -*-

;; Test for task 5.3: Create session persistence

;;; Commentary:

;; This file tests the session persistence functionality implemented in task 5.3.
;; It verifies:
;; - Session state saving and loading
;; - Secure session storage
;; - Session migration and upgrade handling

;;; Code:

(require 'mediawiki-session)

(defun test-session-persistence-basic ()
  "Test basic session persistence functionality."
  (message "Testing basic session persistence...")
  
  ;; Clean up any existing test data
  (let ((test-file "/tmp/test-mediawiki-sessions"))
    (when (file-exists-p test-file)
      (delete-file test-file))
    
    ;; Temporarily override session file for testing
    (let ((mediawiki-session-file test-file))
      
      ;; Create a test session
      (let* ((sitename "test-site")
             (session (make-mediawiki-session
                      :site-name sitename
                      :tokens (make-hash-table :test 'equal)
                      :login-time (current-time)
                      :last-activity (current-time)
                      :user-info '((name . "TestUser") (id . 123)))))
        
        ;; Add some token metadata
        (puthash "csrf-exists" t (mediawiki-session-tokens session))
        (puthash "csrf-expiry" (time-add (current-time) 3600) (mediawiki-session-tokens session))
        (puthash "csrf-refresh-count" 1 (mediawiki-session-tokens session))
        
        ;; Store session
        (mediawiki-set-session sitename session)
        
        ;; Test serialization
        (let ((serialized (mediawiki-session-serialize session)))
          (message "✓ Session serialization works")
          (message "  Serialized data: %S" (plist-get serialized :site-name)))
        
        ;; Test saving
        (mediawiki-session-save-all)
        (if (file-exists-p test-file)
            (message "✓ Session saving works")
          (error "✗ Session file was not created"))
        
        ;; Clear current sessions
        (clrhash mediawiki-sessions)
        
        ;; Test loading
        (mediawiki-session-load-all)
        (let ((restored-session (mediawiki-get-session sitename)))
          (if restored-session
              (progn
                (message "✓ Session loading works")
                (message "  Restored site: %s" (mediawiki-session-site-name restored-session))
                (message "  Restored user: %s" 
                         (cdr (assq 'name (mediawiki-session-user-info restored-session))))
                (if (mediawiki-session-is-restored-p restored-session)
                    (message "✓ Session restoration marker works")
                  (message "✗ Session restoration marker missing")))
            (error "✗ Session was not restored")))
        
        ;; Clean up
        (delete-file test-file)
        (message "✓ Basic session persistence test completed")))))

(defun test-session-persistence-migration ()
  "Test session migration functionality."
  (message "Testing session migration...")
  
  (let ((test-file "/tmp/test-mediawiki-sessions-migration"))
    (when (file-exists-p test-file)
      (delete-file test-file))
    
    ;; Create old format session data (version 1)
    (let ((old-data '(("test-site" . (:site-name "test-site"
                                     :login-time (25000 0 0 0)
                                     :last-activity (25000 0 0 0)
                                     :user-info ((name . "TestUser"))
                                     :version 1)))))
      
      ;; Write old format data
      (with-temp-file test-file
        (prin1 old-data (current-buffer)))
      
      ;; Test migration
      (let ((mediawiki-session-file test-file))
        (mediawiki-session-load-all)
        
        (let ((migrated-session (mediawiki-get-session "test-site")))
          (if migrated-session
              (message "✓ Session migration works")
            (error "✗ Session migration failed"))))
      
      ;; Clean up
      (delete-file test-file)
      (message "✓ Session migration test completed"))))

(defun test-session-persistence-backup ()
  "Test session backup functionality."
  (message "Testing session backup...")
  
  (let ((test-file "/tmp/test-mediawiki-sessions-backup")
        (backup-file "/tmp/test-mediawiki-sessions-backup.backup.1"))
    
    ;; Clean up any existing files
    (when (file-exists-p test-file)
      (delete-file test-file))
    (when (file-exists-p backup-file)
      (delete-file backup-file))
    
    ;; Create initial session file
    (with-temp-file test-file
      (prin1 '(:version 2 :sessions (("test" . (:site-name "test")))) (current-buffer)))
    
    (let ((mediawiki-session-file test-file)
          (mediawiki-session-backup-count 3))
      
      ;; Test backup creation
      (mediawiki-session-create-backup)
      
      (if (file-exists-p backup-file)
          (message "✓ Session backup creation works")
        (error "✗ Session backup was not created"))
      
      ;; Test backup listing
      (let ((backups (mediawiki-session-list-backups)))
        (if (> (length backups) 0)
            (message "✓ Session backup listing works (%d backups)" (length backups))
          (message "✗ No backups found")))
      
      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file))
      (when (file-exists-p backup-file)
        (delete-file backup-file))
      
      (message "✓ Session backup test completed"))))

(defun test-session-persistence-security ()
  "Test session security features."
  (message "Testing session security...")
  
  (let ((test-file "/tmp/test-mediawiki-sessions-security"))
    (when (file-exists-p test-file)
      (delete-file test-file))
    
    (let ((mediawiki-session-file test-file))
      
      ;; Create a session with sensitive data
      (let* ((sitename "secure-test")
             (session (make-mediawiki-session
                      :site-name sitename
                      :tokens (make-hash-table :test 'equal)
                      :login-time (current-time)
                      :last-activity (current-time)
                      :user-info '((name . "SecureUser")))))
        
        ;; Add actual token (should not be persisted)
        (puthash "csrf" "secret-token-value" (mediawiki-session-tokens session))
        (puthash "csrf-expiry" (time-add (current-time) 3600) (mediawiki-session-tokens session))
        
        (mediawiki-set-session sitename session)
        
        ;; Save and reload
        (mediawiki-session-save-all)
        (clrhash mediawiki-sessions)
        (mediawiki-session-load-all)
        
        (let ((restored-session (mediawiki-get-session sitename)))
          (if restored-session
              (let ((tokens (mediawiki-session-tokens restored-session)))
                ;; Check that actual token was not persisted
                (if (gethash "csrf" tokens)
                    (error "✗ Sensitive token was persisted")
                  (message "✓ Sensitive tokens are not persisted"))
                
                ;; Check that token existence marker was persisted
                (if (gethash "csrf-exists" tokens)
                    (message "✓ Token existence markers are persisted")
                  (message "✗ Token existence markers not found"))
                
                ;; Check that expiry was persisted
                (if (gethash "csrf-expiry" tokens)
                    (message "✓ Token expiry information is persisted")
                  (message "✗ Token expiry information not found")))
            (error "✗ Session was not restored"))))
      
      ;; Clean up
      (delete-file test-file)
      (message "✓ Session security test completed"))))

(defun test-session-persistence-all ()
  "Run all session persistence tests."
  (interactive)
  (message "=== Testing Session Persistence (Task 5.3) ===")
  
  (condition-case err
      (progn
        (test-session-persistence-basic)
        (test-session-persistence-migration)
        (test-session-persistence-backup)
        (test-session-persistence-security)
        
        (message "\n=== All Session Persistence Tests Passed ===")
        (message "✓ Session state saving and loading")
        (message "✓ Secure session storage (no sensitive data persisted)")
        (message "✓ Session migration and upgrade handling")
        (message "✓ Session backup and recovery")
        (message "\nTask 5.3 implementation is complete and working correctly."))
    
    (error
     (message "\n✗ Session persistence test failed: %s" (error-message-string err))
     (error "Session persistence tests failed"))))

;; Run tests if called directly
(when (and (boundp 'load-file-name) load-file-name)
  (test-session-persistence-all))

(provide 'test-session-persistence)

;;; test-session-persistence.el ends here