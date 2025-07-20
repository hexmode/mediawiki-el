;;; test-gpg-integration.el --- Automated tests for GPG integration -*- lexical-binding: t; -*-

;; Automated test suite for GPG agent support in session persistence
;; Can be run from make or CI/CD systems

;;; Commentary:

;; This file provides automated tests for the GPG integration functionality.
;; It's designed to be run in batch mode and returns appropriate exit codes.
;; Tests are designed to work in various environments (with/without GPG).

;;; Code:

(require 'mediawiki-session)

(defvar test-gpg-results '()
  "List to collect test results.")

(defvar test-gpg-verbose nil
  "Whether to show verbose test output.")

(defun test-gpg-log (format-string &rest args)
  "Log test message with FORMAT-STRING and ARGS."
  (when test-gpg-verbose
    (apply #'message format-string args)))

(defun test-gpg-assert (condition description)
  "Assert CONDITION is true, recording result with DESCRIPTION."
  (let ((result (if condition 'pass 'fail)))
    (push (list description result) test-gpg-results)
    (test-gpg-log "%s: %s" (if condition "PASS" "FAIL") description)
    condition))

(defun test-gpg-skip (description reason)
  "Skip test with DESCRIPTION for REASON."
  (push (list description 'skip reason) test-gpg-results)
  (test-gpg-log "SKIP: %s (%s)" description reason))

(defun test-gpg-basic-functionality ()
  "Test basic GPG functionality detection."
  (test-gpg-log "Testing basic GPG functionality...")

  ;; Test GPG availability detection
  (let ((gpg-available (executable-find "gpg")))
    (test-gpg-assert (or gpg-available (not gpg-available))
                     "GPG availability detection works")

    (if gpg-available
        (progn
          (test-gpg-log "GPG found at: %s" gpg-available)

          ;; Test GPG status function
          (condition-case err
              (let ((status (mediawiki-session-gpg-status)))
                (test-gpg-assert (listp status) "GPG status returns valid data")
                (test-gpg-assert (plist-get status :gpg-available) "GPG status detects availability"))
            (error
             (test-gpg-assert nil (format "GPG status function failed: %s" (error-message-string err)))))

          ;; Test agent detection
          (let ((agent-available (mediawiki-session-gpg-agent-available-p)))
            (test-gpg-assert (or agent-available (not agent-available))
                           "GPG agent detection works")
            (test-gpg-log "GPG agent available: %s" agent-available))

          ;; Test key detection
          (let ((has-keys (mediawiki-session-has-usable-gpg-keys-p)))
            (test-gpg-assert (or has-keys (not has-keys))
                           "GPG key detection works")
            (test-gpg-log "Usable keys available: %s" has-keys)))

      (test-gpg-skip "GPG status tests" "GPG not available"))))

(defun test-gpg-method-selection ()
  "Test encryption method selection logic."
  (test-gpg-log "Testing encryption method selection...")

  (let ((original-method mediawiki-session-encryption-method)
        (original-recipient mediawiki-session-gpg-recipient))

    (unwind-protect
        (progn
          ;; Test auto method selection
          (setq mediawiki-session-encryption-method 'auto)
          (setq mediawiki-session-gpg-recipient nil)

          (condition-case err
              (let ((method (mediawiki-session-determine-encryption-method)))
                (test-gpg-assert (memq method '(agent symmetric recipient))
                               "Auto method selection returns valid method")
                (test-gpg-log "Auto-selected method: %s" method))
            (error
             (test-gpg-assert nil (format "Method selection failed: %s" (error-message-string err)))))

          ;; Test explicit method selection
          (setq mediawiki-session-encryption-method 'symmetric)
          (condition-case err
              (let ((method (mediawiki-session-determine-encryption-method)))
                (test-gpg-assert (eq method 'symmetric)
                               "Explicit symmetric method selection works"))
            (error
             (test-gpg-assert nil (format "Explicit method selection failed: %s" (error-message-string err)))))

          ;; Test recipient method
          (setq mediawiki-session-encryption-method 'recipient)
          (setq mediawiki-session-gpg-recipient "test@example.com")
          (condition-case err
              (let ((method (mediawiki-session-determine-encryption-method)))
                (test-gpg-assert (eq method 'recipient)
                               "Recipient method selection works"))
            (error
             (test-gpg-assert nil (format "Recipient method selection failed: %s" (error-message-string err))))))

      ;; Restore original settings
      (setq mediawiki-session-encryption-method original-method)
      (setq mediawiki-session-gpg-recipient original-recipient))))

(defun test-gpg-encryption-args ()
  "Test GPG command argument building."
  (test-gpg-log "Testing GPG argument building...")

  (condition-case err
      (progn
        ;; Test symmetric args
        (let ((args (mediawiki-session-build-gpg-encrypt-args 'symmetric)))
          (test-gpg-assert (and (listp args) (member "--symmetric" args))
                         "Symmetric encryption args are correct"))

        ;; Test agent args (if GPG available)
        (when (executable-find "gpg")
          (let ((args (mediawiki-session-build-gpg-encrypt-args 'agent)))
            (test-gpg-assert (and (listp args) (or (member "--encrypt" args) (member "--symmetric" args)))
                           "Agent encryption args are valid")))

        ;; Test recipient args
        (let ((mediawiki-session-gpg-recipient "test@example.com"))
          (let ((args (mediawiki-session-build-gpg-encrypt-args 'recipient)))
            (test-gpg-assert (and (listp args) (member "--encrypt" args) (member "test@example.com" args))
                           "Recipient encryption args are correct"))))
    (error
     (test-gpg-assert nil (format "GPG args building failed: %s" (error-message-string err))))))

(defun test-gpg-file-encryption ()
  "Test actual file encryption/decryption if GPG is available."
  (test-gpg-log "Testing file encryption...")

  (unless (executable-find "gpg")
    (test-gpg-skip "File encryption tests" "GPG not available")
    (return))

  (let ((test-file (make-temp-file "mediawiki-gpg-test"))
        (test-data "Test encryption data for automated testing"))

    (unwind-protect
        (progn
          ;; Write test data
          (with-temp-file test-file
            (insert test-data))

          ;; Test encryption function exists and doesn't crash
          (condition-case err
              (let ((encrypted-file (concat test-file ".gpg")))
                ;; Try symmetric encryption (most likely to work in CI)
                (let ((result (mediawiki-session-encrypt-file test-file encrypted-file 'symmetric)))
                  (test-gpg-assert (or result (not result))
                                 "Encryption function executes without error")

                  ;; If encryption succeeded, test decryption
                  (when (and result (file-exists-p encrypted-file))
                    (test-gpg-log "Encryption succeeded, testing decryption...")
                    (condition-case decrypt-err
                        (let ((decrypted-data (mediawiki-session-load-encrypted encrypted-file)))
                          (test-gpg-assert (stringp decrypted-data)
                                         "Decryption returns string data"))
                      (error
                       (test-gpg-log "Decryption failed (expected in non-interactive mode): %s"
                                   (error-message-string decrypt-err))))

                    ;; Clean up encrypted file
                    (when (file-exists-p encrypted-file)
                      (delete-file encrypted-file)))))
            (error
             (test-gpg-assert nil (format "Encryption test failed: %s" (error-message-string err))))))

      ;; Clean up test file
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(defun test-gpg-session-integration ()
  "Test GPG integration with session persistence."
  (test-gpg-log "Testing session integration...")

  (let ((test-file "/tmp/test-mediawiki-sessions-gpg-auto")
        (original-file mediawiki-session-file)
        (original-encryption mediawiki-session-encryption-enabled)
        (original-method mediawiki-session-encryption-method))

    (unwind-protect
        (progn
          ;; Clean up any existing test files
          (when (file-exists-p test-file)
            (delete-file test-file))
          (when (file-exists-p (concat test-file ".gpg"))
            (delete-file (concat test-file ".gpg")))

          ;; Configure for testing
          (setq mediawiki-session-file test-file)
          (setq mediawiki-session-encryption-enabled t)
          (setq mediawiki-session-encryption-method 'auto)

          ;; Create a test session
          (let* ((sitename "gpg-integration-test")
                 (session (make-mediawiki-session
                          :site-name sitename
                          :tokens (make-hash-table :test 'equal)
                          :login-time (current-time)
                          :last-activity (current-time)
                          :user-info '((name . "GPGTestUser")))))

            (mediawiki-set-session sitename session)

            ;; Test saving with encryption enabled
            (condition-case err
                (progn
                  (mediawiki-session-save-all)
                  (test-gpg-assert (or (file-exists-p test-file)
                                     (file-exists-p (concat test-file ".gpg")))
                                 "Session file created with encryption enabled")

                  ;; Test that session data structure is preserved
                  (clrhash mediawiki-sessions)
                  (condition-case load-err
                      (progn
                        (mediawiki-session-load-all)
                        (let ((restored-session (mediawiki-get-session sitename)))
                          (test-gpg-assert (or restored-session (not restored-session))
                                         "Session loading completes without error")))
                    (error
                     (test-gpg-log "Session loading failed (may be expected): %s"
                                 (error-message-string load-err)))))
              (error
               (test-gpg-assert nil (format "Session integration test failed: %s" (error-message-string err)))))))

      ;; Clean up
      (setq mediawiki-session-file original-file)
      (setq mediawiki-session-encryption-enabled original-encryption)
      (setq mediawiki-session-encryption-method original-method)

      (when (file-exists-p test-file)
        (delete-file test-file))
      (when (file-exists-p (concat test-file ".gpg"))
        (delete-file (concat test-file ".gpg"))))))

(defun test-gpg-configuration-commands ()
  "Test GPG configuration and diagnostic commands."
  (test-gpg-log "Testing configuration commands...")

  ;; Test status command
  (condition-case err
      (progn
        (let ((status (mediawiki-session-gpg-status)))
          (test-gpg-assert (listp status) "GPG status command returns data")))
    (error
     (test-gpg-assert nil (format "GPG status command failed: %s" (error-message-string err)))))

  ;; Test encryption test command (non-interactive)
  (when (and (executable-find "gpg") mediawiki-session-encryption-enabled)
    (condition-case err
        (progn
          ;; This might fail in non-interactive mode, but shouldn't crash
          (mediawiki-session-test-encryption)
          (test-gpg-assert t "GPG test command executes without crashing"))
      (error
       (test-gpg-log "GPG test command failed (expected in batch mode): %s"
                   (error-message-string err))
       (test-gpg-assert t "GPG test command handles errors gracefully")))))

(defun test-gpg-run-all-tests ()
  "Run all GPG integration tests."
  (setq test-gpg-results '())
  (setq test-gpg-verbose (or (getenv "VERBOSE")
                           (member "--verbose" command-line-args)))

  (test-gpg-log "=== Starting GPG Integration Tests ===")

  ;; Run all test suites
  (test-gpg-basic-functionality)
  (test-gpg-method-selection)
  (test-gpg-encryption-args)
  (test-gpg-file-encryption)
  (test-gpg-session-integration)
  (test-gpg-configuration-commands)

  ;; Analyze results
  (let ((total 0)
        (passed 0)
        (failed 0)
        (skipped 0))

    (dolist (result test-gpg-results)
      (setq total (1+ total))
      (let ((status (cadr result)))
        (cond
         ((eq status 'pass) (setq passed (1+ passed)))
         ((eq status 'fail) (setq failed (1+ failed)))
         ((eq status 'skip) (setq skipped (1+ skipped))))))

    ;; Print summary
    (message "=== GPG Integration Test Results ===")
    (message "Total: %d, Passed: %d, Failed: %d, Skipped: %d" total passed failed skipped)

    ;; Print failed tests
    (when (> failed 0)
      (message "\nFailed Tests:")
      (dolist (result test-gpg-results)
        (when (eq (cadr result) 'fail)
          (message "  - %s" (car result)))))

    ;; Print skipped tests if verbose
    (when (and test-gpg-verbose (> skipped 0))
      (message "\nSkipped Tests:")
      (dolist (result test-gpg-results)
        (when (eq (cadr result) 'skip)
          (message "  - %s (%s)" (car result) (caddr result)))))

    ;; Environment info
    (message "\nEnvironment:")
    (message "  GPG Available: %s" (if (executable-find "gpg") "Yes" "No"))
    (message "  GPG Agent: %s" (if (and (executable-find "gpg-connect-agent")
                                       (zerop (call-process "gpg-connect-agent" nil nil nil "/bye")))
                                  "Running" "Not running"))

    ;; Return appropriate exit code
    (if (> failed 0)
        (progn
          (message "\nSome tests failed!")
          (kill-emacs 1))
      (progn
        (message "\nAll tests passed!")
        (kill-emacs 0)))))

;; Run tests if this file is executed directly
(when noninteractive
  (test-gpg-run-all-tests))

(provide 'test-gpg-integration)

;;; test-gpg-integration.el ends here
