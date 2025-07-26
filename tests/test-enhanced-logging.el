;;; test-enhanced-logging.el --- Test enhanced logging functionality -*- lexical-binding: t; -*-

;; Test implementation of task 7.4: Create enhanced debugging and logging
;; Tests log filtering and export capabilities

(require 'ert)
(require 'mediawiki-core)

;;; Test Setup

(defvar test-logging-debug-buffer "*Test MediaWiki Debug*"
  "Test debug buffer name.")

(defun test-logging-setup ()
  "Set up test environment."
  (let ((old-buffer-name mediawiki-debug-buffer))
    (setq mediawiki-debug-buffer test-logging-debug-buffer)
    (setq mediawiki-debug t)
    ;; Clean up any existing test buffer
    (when (get-buffer test-logging-debug-buffer)
      (kill-buffer test-logging-debug-buffer))
    old-buffer-name))

(defun test-logging-teardown (old-buffer-name)
  "Clean up test environment."
  (when (get-buffer test-logging-debug-buffer)
    (kill-buffer test-logging-debug-buffer))
  (setq mediawiki-debug-buffer old-buffer-name))

(defun test-logging-populate-buffer ()
  "Populate test debug buffer with sample log entries."
  (mediawiki-debug-log "Starting auth flow for test-site")
  (mediawiki-debug-log "API call to test-site: action=query")
  (mediawiki-debug-log "HTTP request failed with status 500")
  (mediawiki-debug-log "Session token expired for test-site")
  (mediawiki-debug-log "Page save conflict detected")
  (mediawiki-debug-log "Error during authentication: bad credentials"))

;;; Test Log Filtering

(ert-deftest test-debug-filter-by-pattern ()
  "Test filtering debug log by pattern."
  (let ((old-buffer-name (test-logging-setup)))
    (unwind-protect
        (progn
          (test-logging-populate-buffer)

          ;; Test filtering by pattern
          (let ((filtered (mediawiki-debug-filter-by-pattern "auth")))
            (should (string-match-p "auth flow" filtered))
            (should (string-match-p "authentication" filtered))
            (should-not (string-match-p "HTTP request" filtered))))

      (test-logging-teardown old-buffer-name))))

(ert-deftest test-debug-filter-by-module ()
  "Test filtering debug log by module."
  (let ((old-buffer-name (test-logging-setup)))
    (unwind-protect
        (progn
          (test-logging-populate-buffer)

          ;; Test auth module filtering
          (let ((auth-filtered (mediawiki-debug-filter-by-module "auth")))
            (should (string-match-p "auth flow" auth-filtered))
            (should (string-match-p "authentication" auth-filtered))
            (should (string-match-p "token expired" auth-filtered)))

          ;; Test API module filtering
          (let ((api-filtered (mediawiki-debug-filter-by-module "api")))
            (should (string-match-p "API call" api-filtered)))

          ;; Test HTTP module filtering
          (let ((http-filtered (mediawiki-debug-filter-by-module "http")))
            (should (string-match-p "HTTP request" http-filtered))))

      (test-logging-teardown old-buffer-name))))

(ert-deftest test-debug-filter-by-time ()
  "Test filtering debug log by time range."
  (let ((old-buffer-name (test-logging-setup)))
    (unwind-protect
        (progn
          ;; Create entries with specific times
          (let ((current-time (format-time-string "%H:%M:%S")))
            (mediawiki-debug-log "First entry")
            (sit-for 0.1) ; Small delay to ensure different timestamps
            (mediawiki-debug-log "Second entry")

            ;; Test time filtering (this is basic since we can't easily control exact timestamps)
            (let ((filtered (mediawiki-debug-filter-by-time "00:00:00" "23:59:59")))
              (should (string-match-p "First entry" filtered))
              (should (string-match-p "Second entry" filtered)))))

      (test-logging-teardown old-buffer-name))))

;;; Test Log Management

(ert-deftest test-debug-clear ()
  "Test clearing debug buffer."
  (let ((old-buffer-name (test-logging-setup)))
    (unwind-protect
        (progn
          (test-logging-populate-buffer)

          ;; Verify buffer has content
          (with-current-buffer (get-buffer test-logging-debug-buffer)
            (should (> (buffer-size) 0)))

          ;; Clear buffer
          (mediawiki-debug-clear)

          ;; Verify buffer is empty
          (with-current-buffer (get-buffer test-logging-debug-buffer)
            (should (= (buffer-size) 0))))

      (test-logging-teardown old-buffer-name))))

(ert-deftest test-debug-buffer-size-management ()
  "Test automatic buffer size management."
  (let ((old-buffer-name (test-logging-setup))
        (old-max-size mediawiki-debug-max-buffer-size))
    (unwind-protect
        (progn
          ;; Set small buffer size for testing
          (setq mediawiki-debug-max-buffer-size 500)

          ;; Fill buffer beyond limit - generate enough content to exceed the limit
          (dotimes (i 30)
            (mediawiki-debug-log "This is a very long test message that should help fill up the buffer quickly to test the size management functionality. Message number %d with extra padding." i))

          ;; Manually trigger buffer size management to ensure it happens
          (with-current-buffer (get-buffer test-logging-debug-buffer)
            (mediawiki-debug-manage-buffer-size))

          ;; Check that buffer was trimmed (allow some tolerance)
          (with-current-buffer (get-buffer test-logging-debug-buffer)
            (should (<= (buffer-size) (* 1.1 mediawiki-debug-max-buffer-size))) ; Allow 10% tolerance
            (should (string-match-p "trimmed" (buffer-string)))))

      (setq mediawiki-debug-max-buffer-size old-max-size)
      (test-logging-teardown old-buffer-name))))

;;; Test Log Export

(ert-deftest test-debug-export-to-file ()
  "Test exporting debug log to file."
  (let ((old-buffer-name (test-logging-setup))
        (test-file (make-temp-file "mediawiki-debug-test-")))
    (unwind-protect
        (progn
          (test-logging-populate-buffer)

          ;; Export to file
          (mediawiki-debug-export-to-file test-file)

          ;; Verify file was created and has content
          (should (file-exists-p test-file))
          (with-temp-buffer
            (insert-file-contents test-file)
            (should (string-match-p "MediaWiki Debug Log Export" (buffer-string)))
            (should (string-match-p "auth flow" (buffer-string)))
            (should (string-match-p "API call" (buffer-string)))))

      (when (file-exists-p test-file)
        (delete-file test-file))
      (test-logging-teardown old-buffer-name))))

(ert-deftest test-debug-export-filtered ()
  "Test exporting filtered debug log."
  (let ((old-buffer-name (test-logging-setup))
        (test-file (make-temp-file "mediawiki-debug-filtered-test-")))
    (unwind-protect
        (progn
          (test-logging-populate-buffer)

          ;; Export filtered by module
          (mediawiki-debug-export-to-file test-file 'mediawiki-debug-filter-by-module "auth")

          ;; Verify file contains only auth-related entries
          (should (file-exists-p test-file))
          (with-temp-buffer
            (insert-file-contents test-file)
            (should (string-match-p "auth flow" (buffer-string)))
            (should (string-match-p "authentication" (buffer-string)))
            (should-not (string-match-p "HTTP request" (buffer-string)))))

      (when (file-exists-p test-file)
        (delete-file test-file))
      (test-logging-teardown old-buffer-name))))

;;; Test Enhanced Logging Functions

(ert-deftest test-debug-log-with-module ()
  "Test logging with module prefix."
  (let ((old-buffer-name (test-logging-setup)))
    (unwind-protect
        (progn
          (mediawiki-debug-log-with-module "auth" "Test authentication message")
          (mediawiki-debug-log-with-module "api" "Test API message")

          ;; Verify module prefixes are present
          (with-current-buffer (get-buffer test-logging-debug-buffer)
            (let ((content (buffer-string)))
              (should (string-match-p "\\[AUTH\\]" content))
              (should (string-match-p "\\[API\\]" content))
              (should (string-match-p "Test authentication message" content))
              (should (string-match-p "Test API message" content)))))

      (test-logging-teardown old-buffer-name))))

(ert-deftest test-debug-view ()
  "Test viewing debug buffer."
  (let ((old-buffer-name (test-logging-setup)))
    (unwind-protect
        (progn
          (test-logging-populate-buffer)

          ;; Test that buffer can be viewed
          ;; Note: We can't easily test the interactive window switching,
          ;; but we can verify the buffer exists and has content
          (let ((buffer (get-buffer test-logging-debug-buffer)))
            (should buffer)
            (with-current-buffer buffer
              (should (> (buffer-size) 0)))))

      (test-logging-teardown old-buffer-name))))

(provide 'test-enhanced-logging)

;;; test-enhanced-logging.el ends here
