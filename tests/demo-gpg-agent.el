;;; demo-gpg-agent.el --- Demonstration of GPG agent support -*- lexical-binding: t; -*-

;; Demonstration of enhanced GPG agent support for session persistence

;;; Commentary:

;; This file demonstrates the enhanced GPG functionality that supports
;; seamless encryption using gpg-agent without password prompts.

;;; Code:

(require 'mediawiki-session)

(defun demo-gpg-agent-support ()
  "Demonstrate GPG agent support for session encryption."
  (interactive)

  (message "=== MediaWiki Session GPG Agent Support Demo ===\n")

  ;; Show current GPG status
  (message "1. Checking GPG configuration...")
  (let ((status (mediawiki-session-gpg-status)))
    (message "   GPG Available: %s"
             (if (plist-get status :gpg-available) "✓ Yes" "✗ No"))
    (message "   GPG Agent Running: %s"
             (if (plist-get status :agent-running) "✓ Yes" "✗ No"))
    (message "   Usable Keys: %s"
             (if (plist-get status :usable-keys) "✓ Yes" "✗ No"))
    (message "   Default Key: %s"
             (or (plist-get status :default-key) "None"))

    (when (and (plist-get status :gpg-available)
               (plist-get status :agent-running)
               (plist-get status :usable-keys))

      (message "\n2. Testing automatic method selection...")

      ;; Test different configurations
      (let ((original-method mediawiki-session-encryption-method)
            (original-recipient mediawiki-session-gpg-recipient))

        (unwind-protect
            (progn
              ;; Test auto method with gpg-agent available
              (setq mediawiki-session-encryption-method 'auto)
              (setq mediawiki-session-gpg-recipient nil)

              (let ((method (mediawiki-session-determine-encryption-method)))
                (message "   Auto-detected method: %s" method)
                (if (eq method 'agent)
                    (message "   ✓ Correctly chose GPG agent (no password prompt)")
                  (message "   ⚠ Did not choose agent method")))

              ;; Test with configured recipient
              (setq mediawiki-session-gpg-recipient "user@example.com")
              (let ((method (mediawiki-session-determine-encryption-method)))
                (message "   With recipient configured: %s" method)
                (if (eq method 'recipient)
                    (message "   ✓ Correctly chose recipient method")
                  (message "   ⚠ Did not choose recipient method")))

              ;; Test explicit agent method
              (setq mediawiki-session-encryption-method 'agent)
              (setq mediawiki-session-gpg-recipient nil)
              (let ((method (mediawiki-session-determine-encryption-method)))
                (message "   Explicit agent method: %s" method)
                (if (eq method 'agent)
                    (message "   ✓ Correctly uses explicit agent method")
                  (message "   ⚠ Did not use agent method"))))

          ;; Restore original settings
          (setq mediawiki-session-encryption-method original-method)
          (setq mediawiki-session-gpg-recipient original-recipient)))

      (message "\n3. Testing seamless encryption (no password prompt)...")

      ;; Test actual encryption with agent
      (let ((test-file (make-temp-file "mediawiki-gpg-demo"))
            (test-data "Demo session data for GPG agent"))

        (unwind-protect
            (progn
              ;; Write test data
              (with-temp-file test-file
                (insert test-data))

              ;; Test agent encryption (should not prompt for password)
              (let ((encrypted-file (concat test-file ".gpg")))
                (message "   Encrypting with GPG agent...")
                (if (mediawiki-session-encrypt-file test-file encrypted-file 'agent)
                    (progn
                      (message "   ✓ Encryption successful (no password prompt)")

                      ;; Test decryption
                      (message "   Decrypting...")
                      (condition-case err
                          (let ((decrypted-data (mediawiki-session-load-encrypted encrypted-file)))
                            (if (string= test-data decrypted-data)
                                (message "   ✓ Decryption successful")
                              (message "   ✗ Decryption data mismatch")))
                        (error
                         (message "   ⚠ Decryption failed: %s" (error-message-string err))))

                      ;; Clean up encrypted file
                      (when (file-exists-p encrypted-file)
                        (delete-file encrypted-file)))
                  (message "   ✗ Encryption failed")))

          ;; Clean up test file
          (when (file-exists-p test-file)
            (delete-file test-file))))

      (message "\n4. Session persistence with GPG agent...")

      ;; Demonstrate session persistence with encryption
      (let ((test-session-file "/tmp/demo-mediawiki-sessions")
            (original-file mediawiki-session-file)
            (original-encryption mediawiki-session-encryption-enabled)
            (original-method mediawiki-session-encryption-method))

        (unwind-protect
            (progn
              ;; Configure for demo
              (setq mediawiki-session-file test-session-file)
              (setq mediawiki-session-encryption-enabled t)
              (setq mediawiki-session-encryption-method 'agent)

              ;; Clean up any existing files
              (when (file-exists-p test-session-file)
                (delete-file test-session-file))
              (when (file-exists-p (concat test-session-file ".gpg"))
                (delete-file (concat test-session-file ".gpg")))

              ;; Create a demo session
              (let* ((sitename "demo-site")
                     (session (make-mediawiki-session
                              :site-name sitename
                              :tokens (make-hash-table :test 'equal)
                              :login-time (current-time)
                              :last-activity (current-time)
                              :user-info '((name . "DemoUser")))))

                (mediawiki-set-session sitename session)

                ;; Save with encryption
                (message "   Saving session with GPG agent encryption...")
                (mediawiki-session-save-all)

                (let ((encrypted-file (concat test-session-file ".gpg")))
                  (if (file-exists-p encrypted-file)
                      (progn
                        (message "   ✓ Session encrypted and saved")

                        ;; Clear sessions and reload
                        (clrhash mediawiki-sessions)
                        (message "   Loading encrypted session...")
                        (mediawiki-session-load-all)

                        (let ((restored-session (mediawiki-get-session sitename)))
                          (if restored-session
                              (message "   ✓ Session restored from encrypted storage")
                            (message "   ✗ Failed to restore session"))))
                    (message "   ⚠ Encrypted file not created")))))

          ;; Restore original settings and clean up
          (setq mediawiki-session-file original-file)
          (setq mediawiki-session-encryption-enabled original-encryption)
          (setq mediawiki-session-encryption-method original-method)

          (when (file-exists-p test-session-file)
            (delete-file test-session-file))
          (when (file-exists-p (concat test-session-file ".gpg"))
            (delete-file (concat test-session-file ".gpg")))))

      (message "\n=== Demo Complete ===")
      (message "\nKey Benefits of GPG Agent Support:")
      (message "• No password prompts when gpg-agent is running")
      (message "• Automatic method selection based on availability")
      (message "• Secure encryption using your existing GPG keys")
      (message "• Seamless integration with existing workflows")
      (message "• Fallback to symmetric encryption if agent unavailable")

      (message "\nTo enable GPG agent encryption:")
      (message "1. Ensure gpg-agent is running")
      (message "2. Set: (setq mediawiki-session-encryption-enabled t)")
      (message "3. Set: (setq mediawiki-session-encryption-method 'auto)")
      (message "4. Or run: M-x mediawiki-session-configure-encryption"))

    (unless (plist-get status :gpg-available)
      (message "\n⚠ GPG is not available. Please install GnuPG to use encryption."))

    (unless (plist-get status :agent-running)
      (message "\n⚠ GPG agent is not running. Start it with: gpg-agent --daemon"))

    (unless (plist-get status :usable-keys)
      (message "\n⚠ No usable GPG keys found. Generate keys with: gpg --gen-key")))))

;; Run demo if called directly
(when (and (boundp 'load-file-name) load-file-name)
  (demo-gpg-agent-support))

(provide 'demo-gpg-agent)

;;; demo-gpg-agent.el ends here
