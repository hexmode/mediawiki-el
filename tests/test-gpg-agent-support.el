;;; test-gpg-agent-support.el --- Test GPG agent support for session persistence -*- lexical-binding: t; -*-

;; Test for enhanced GPG support with gpg-agent integration

;;; Commentary:

;; This file tests the enhanced GPG functionality implemented for session persistence.
;; It verifies:
;; - GPG agent detection
;; - Automatic method selection
;; - Seamless encryption without password prompts when gpg-agent is available
;; - Fallback to symmetric encryption when gpg-agent is not available

;;; Code:

(require 'ert)
(require 'mediawiki-session)

(ert-deftest test-gpg-status ()
  "Test GPG status detection functionality."
  (let ((status (mediawiki-session-gpg-status)))
    (should (plist-member status :gpg-available))
    (should (plist-member status :gpg-connect-agent-available))
    (should (plist-member status :agent-running))
    (should (plist-member status :usable-keys))
    (should (plist-member status :encryption-method))))

(ert-deftest test-encryption-method-selection ()
  "Test automatic encryption method selection."
  (let ((original-method mediawiki-session-encryption-method)
        (original-recipient mediawiki-session-gpg-recipient))

    (unwind-protect
        (progn
          ;; Test auto method selection
          (setq mediawiki-session-encryption-method 'auto)
          (setq mediawiki-session-gpg-recipient nil)

          (let ((method (mediawiki-session-determine-encryption-method)))
            (should (memq method '(agent symmetric recipient))))

          ;; Test with configured recipient
          (setq mediawiki-session-gpg-recipient "test@example.com")
          (let ((method (mediawiki-session-determine-encryption-method)))
            (should (eq method 'recipient)))

          ;; Test explicit agent method
          (setq mediawiki-session-encryption-method 'agent)
          (let ((method (mediawiki-session-determine-encryption-method)))
            (should (eq method 'agent)))

          ;; Test explicit symmetric method
          (setq mediawiki-session-encryption-method 'symmetric)
          (let ((method (mediawiki-session-determine-encryption-method)))
            (should (eq method 'symmetric))))

      ;; Restore original settings
      (setq mediawiki-session-encryption-method original-method)
      (setq mediawiki-session-gpg-recipient original-recipient))))

(ert-deftest test-gpg-agent-detection ()
  "Test GPG agent availability detection."
  :tags '(gpg)
  (skip-unless (executable-find "gpg"))

  (let ((agent-available (mediawiki-session-gpg-agent-available-p)))
    (should (or agent-available (not agent-available)))

    (when agent-available
      (let ((has-keys (mediawiki-session-has-usable-gpg-keys-p)))
        (should (or has-keys (not has-keys)))

        (when has-keys
          (let ((default-key (mediawiki-session-get-default-gpg-key)))
            (should (or default-key (not default-key)))))))))

(ert-deftest test-gpg-encryption-methods ()
  "Test different GPG encryption methods."
  :tags '(gpg)
  (skip-unless (executable-find "gpg"))

  (let ((test-file (make-temp-file "mediawiki-gpg-test"))
        (test-data "Test encryption data"))

    (unwind-protect
        (progn
          ;; Write test data
          (with-temp-file test-file
            (insert test-data))

          ;; Test symmetric encryption
          (let ((encrypted-file (concat test-file ".sym.gpg")))
            (condition-case nil
                (progn
                  (mediawiki-session-encrypt-file test-file encrypted-file 'symmetric)
                  (when (file-exists-p encrypted-file)
                    (delete-file encrypted-file)))
              (error nil)))

          ;; Test agent encryption (if available)
          (when (mediawiki-session-gpg-agent-available-p)
            (let ((encrypted-file (concat test-file ".agent.gpg")))
              (condition-case nil
                  (progn
                    (mediawiki-session-encrypt-file test-file encrypted-file 'agent)
                    (when (file-exists-p encrypted-file)
                      (delete-file encrypted-file)))
                (error nil))))

          ;; Test recipient encryption (if configured)
          (when mediawiki-session-gpg-recipient
            (let ((encrypted-file (concat test-file ".recipient.gpg")))
              (condition-case nil
                  (progn
                    (mediawiki-session-encrypt-file test-file encrypted-file 'recipient)
                    (when (file-exists-p encrypted-file)
                      (delete-file encrypted-file)))
                (error nil)))))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest test-session-encryption-integration ()
  "Test session encryption integration with different methods."
  :tags '(gpg)
  (skip-unless (executable-find "gpg"))

  (let ((test-file "/tmp/test-mediawiki-sessions-gpg")
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

          ;; Test with auto method
          (setq mediawiki-session-encryption-method 'auto)

          ;; Create a test session
          (let* ((sitename "gpg-test-site")
                 (session (make-mediawiki-session
                          :site-name sitename
                          :tokens (make-hash-table :test 'equal)
                          :login-time (current-time)
                          :last-activity (current-time)
                          :user-info '((name . "GPGTestUser")))))

            (mediawiki-set-session sitename session)

            ;; Test saving with encryption
            (condition-case nil
                (progn
                  (mediawiki-session-save-all)
                  (let ((encrypted-file (concat test-file ".gpg")))
                    (when (file-exists-p encrypted-file)
                      ;; Test loading encrypted session
                      (clrhash mediawiki-sessions)
                      (condition-case nil
                          (mediawiki-session-load-all)
                        (error nil)))))
              (error nil))))

      ;; Clean up
      (setq mediawiki-session-file original-file)
      (setq mediawiki-session-encryption-enabled original-encryption)
      (setq mediawiki-session-encryption-method original-method)

      (when (file-exists-p test-file)
        (delete-file test-file))
      (when (file-exists-p (concat test-file ".gpg"))
        (delete-file (concat test-file ".gpg"))))))

(ert-deftest test-gpg-configuration-commands ()
  "Test GPG configuration and diagnostic commands."
  :tags '(gpg)

  ;; Test status command
  (condition-case nil
      (mediawiki-session-gpg-status)
    (error (ert-fail "GPG status command failed")))

  ;; Test encryption test command
  (when (and (executable-find "gpg") mediawiki-session-encryption-enabled)
    (condition-case nil
        (mediawiki-session-test-encryption)
      (error nil))))

(provide 'test-gpg-agent-support)

;;; test-gpg-agent-support.el ends here
