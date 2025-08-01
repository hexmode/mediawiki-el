;;; test-session-persistence.el --- Test session persistence functionality -*- lexical-binding: t; -*-

;; Test for task 5.3: Create session persistence

;;; Commentary:

;; This file tests the session persistence functionality implemented in task 5.3.
;; It verifies:
;; - Session state saving and loading
;; - Secure session storage
;; - Session migration and upgrade handling

;;; Code:

(require 'ert)
(require 'mediawiki-session)

(ert-deftest test-session-persistence-basic ()
  "Test basic session persistence functionality."
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
          (should (plist-get serialized :site-name)))

        ;; Test saving
        (mediawiki-session-save-all)
        (should (file-exists-p test-file))

        ;; Clear current sessions
        (clrhash mediawiki-sessions)

        ;; Test loading
        (mediawiki-session-load-all)
        (let ((restored-session (mediawiki-get-session sitename)))
          (should restored-session)
          (should (string= (mediawiki-session-site-name restored-session) sitename))
          (should (string= (cdr (assq 'name (mediawiki-session-user-info restored-session))) "TestUser"))
          (should (mediawiki-session-is-restored-p restored-session)))

        ;; Clean up
        (delete-file test-file)))))

(ert-deftest test-session-persistence-migration ()
  "Test session migration functionality."
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
          (should migrated-session)))

      ;; Clean up
      (delete-file test-file))))

(ert-deftest test-session-persistence-backup ()
  "Test session backup functionality."
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

      (should (file-exists-p backup-file))

      ;; Test backup listing
      (let ((backups (mediawiki-session-list-backups)))
        (should (> (length backups) 0)))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file))
      (when (file-exists-p backup-file)
        (delete-file backup-file)))))

(ert-deftest test-session-persistence-security ()
  "Test session security features."
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
          (should restored-session)
          (let ((tokens (mediawiki-session-tokens restored-session)))
            ;; Check that actual token was not persisted
            (should-not (gethash "csrf" tokens))

            ;; Check that token existence marker was persisted
            (should (gethash "csrf-exists" tokens))

            ;; Check that expiry was persisted
            (should (gethash "csrf-expiry" tokens))))

        ;; Clean up
        (delete-file test-file)))))

(provide 'test-session-persistence)

;;; test-session-persistence.el ends here
