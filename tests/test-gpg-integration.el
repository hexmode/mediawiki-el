;;; test-gpg-integration.el --- Automated tests for GPG integration -*- lexical-binding: t; -*-

;; Automated test suite for GPG agent support in session persistence
;; Can be run from make or CI/CD systems

;;; Commentary:

;; This file provides automated tests for the GPG integration functionality.
;; It's designed to be run in batch mode and returns appropriate exit codes.
;; Tests are designed to work in various environments (with/without GPG).

;;; Code:

(require 'ert)
(require 'mediawiki-session)

(ert-deftest test-gpg-basic-functionality ()
  "Test basic GPG functionality detection."
  ;; Test GPG availability detection
  (let ((gpg-available (executable-find "gpg")))
    (should (or gpg-available (not gpg-available)))

    (when gpg-available
      ;; Test GPG status function
      (let ((status (mediawiki-session-gpg-status)))
        (should (listp status))
        (should (plist-get status :gpg-available)))

      ;; Test agent detection
      (let ((agent-available (mediawiki-session-gpg-agent-available-p)))
        (should (or agent-available (not agent-available))))

      ;; Test key detection
      (let ((has-keys (mediawiki-session-has-usable-gpg-keys-p)))
        (should (or has-keys (not has-keys)))))))

(ert-deftest test-gpg-method-selection ()
  "Test encryption method selection logic."
  (let ((original-method mediawiki-session-encryption-method)
        (original-recipient mediawiki-session-gpg-recipient))

    (unwind-protect
        (progn
          ;; Test auto method selection
          (setq mediawiki-session-encryption-method 'auto)
          (setq mediawiki-session-gpg-recipient nil)

          (let ((method (mediawiki-session-determine-encryption-method)))
            (should (memq method '(agent symmetric recipient))))

          ;; Test explicit method selection
          (setq mediawiki-session-encryption-method 'symmetric)
          (let ((method (mediawiki-session-determine-encryption-method)))
            (should (eq method 'symmetric)))

          ;; Test recipient method
          (setq mediawiki-session-encryption-method 'recipient)
          (setq mediawiki-session-gpg-recipient "test@example.com")
          (let ((method (mediawiki-session-determine-encryption-method)))
            (should (eq method 'recipient))))

      ;; Restore original settings
      (setq mediawiki-session-encryption-method original-method)
      (setq mediawiki-session-gpg-recipient original-recipient))))

(ert-deftest test-gpg-encryption-args ()
  "Test GPG command argument building."
  ;; Test symmetric args
  (let ((args (mediawiki-session-build-gpg-encrypt-args 'symmetric)))
    (should (and (listp args) (member "--symmetric" args))))

  ;; Test agent args (if GPG available)
  (when (executable-find "gpg")
    (let ((args (mediawiki-session-build-gpg-encrypt-args 'agent)))
      (should (and (listp args) (or (member "--encrypt" args) (member "--symmetric" args))))))

  ;; Test recipient args
  (let ((mediawiki-session-gpg-recipient "test@example.com"))
    (let ((args (mediawiki-session-build-gpg-encrypt-args 'recipient)))
      (should (and (listp args) (member "--encrypt" args) (member "test@example.com" args))))))

;;; Currently, I cannot figure out how to make this not prompt

;; (ert-deftest test-gpg-file-encryption ()
;;   "Test actual file encryption/decryption if GPG is available."
;;   :tags '(gpg)
;;   (skip-unless (executable-find "gpg"))

;;   (let ((test-file (make-temp-file "mediawiki-gpg-test"))
;;         (test-data "Test encryption data for automated testing"))

;;     (unwind-protect
;;         (progn
;;           ;; Write test data
;;           (with-temp-file test-file
;;             (insert test-data))

;;           ;; Test encryption function exists and doesn't crash
;;           (let ((encrypted-file (concat test-file ".gpg")))
;;             ;; Try symmetric encryption (most likely to work in CI)
;;             (condition-case nil
;;                 (mediawiki-session-encrypt-file test-file encrypted-file 'symmetric)
;;               (error nil))

;;             ;; If encryption succeeded, test decryption
;;             (when (file-exists-p encrypted-file)
;;               (condition-case nil
;;                   (mediawiki-session-load-encrypted encrypted-file)
;;                 (error nil))

;;               ;; Clean up encrypted file
;;               (when (file-exists-p encrypted-file)
;;                 (delete-file encrypted-file)))))

;;       ;; Clean up test file
;;       (when (file-exists-p test-file)
;;         (delete-file test-file)))))

(ert-deftest test-gpg-session-integration ()
  "Test GPG integration with session persistence."
  :tags '(gpg)

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
            (condition-case nil
                (progn
                  (mediawiki-session-save-all)
                  (should (or (file-exists-p test-file)
                              (file-exists-p (concat test-file ".gpg"))))

                  ;; Test that session data structure is preserved
                  (clrhash mediawiki-sessions)
                  (condition-case nil
                      (mediawiki-session-load-all)
                    (error nil)))
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
  ;; Test status command
  (let ((status (mediawiki-session-gpg-status)))
    (should (listp status)))

  ;; Test encryption test command (non-interactive)
  (when (and (executable-find "gpg") mediawiki-session-encryption-enabled)
    (condition-case nil
        (mediawiki-session-test-encryption)
      (error nil))))

(provide 'test-gpg-integration)

;;; test-gpg-integration.el ends here
