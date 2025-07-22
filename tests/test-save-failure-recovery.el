;;; test-save-failure-recovery.el --- Test save failure recovery functionality -*- lexical-binding: t; -*-

;; Test implementation of task 6.4: Implement save failure recovery
;; Tests automatic retry logic, draft saving, and recovery options

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-api)
(require 'mediawiki-session)
(require 'mediawiki-page)

;;; Test Configuration

(defvar test-recovery-sitename "test-recovery-wiki"
  "Test site name for save recovery tests.")

(defvar test-recovery-title "Test Recovery Page"
  "Test page title for save recovery tests.")

(defvar test-recovery-content "This is test content for save recovery tests."
  "Content for save recovery tests.")

(defvar test-recovery-mock-token "test-csrf-token-12345"
  "Mock CSRF token for testing.")

(defvar test-recovery-temp-dir nil
  "Temporary directory for draft files during tests.")

;;; Mock Functions for Testing

(defun test-recovery-setup-mock-session ()
  "Set up a mock session for testing save recovery."
  (let ((session (make-mediawiki-session
                  :site-name test-recovery-sitename
                  :tokens (make-hash-table :test 'equal)
                  :user-info '(:username "testuser" :userid 123)
                  :login-time (current-time)
                  :last-activity (current-time))))
    (mediawiki-set-session test-recovery-sitename session)
    ;; Add mock token
    (puthash "csrf" test-recovery-mock-token (mediawiki-session-tokens session))
    (puthash "csrf-expiry" (time-add (current-time) 3600) (mediawiki-session-tokens session))
    session))

(defvar test-recovery-retry-count 0
  "Counter for retry attempts in tests.")

(defun test-recovery-mock-api-call-with-token (sitename action params token-type)
  "Mock API call with token for testing save recovery."
  (cond
   ;; Rate limited response for retry testing
   ((and (string= action "edit")
         (string= sitename "retry-test-wiki")
         (< test-recovery-retry-count 2))
    (setq test-recovery-retry-count (1+ test-recovery-retry-count))
    (make-mediawiki-api-response
     :success nil
     :errors (list (list :code "ratelimited" :info "Rate limited, please wait"))))

   ;; Success after retries
   ((and (string= action "edit")
         (string= sitename "retry-test-wiki")
         (>= test-recovery-retry-count 2))
    (make-mediawiki-api-response
     :success t
     :data '((edit . ((result . "Success")
                      (pageid . 12345)
                      (title . "Test Recovery Page")
                      (contentmodel . "wikitext")
                      (oldrevid . 100)
                      (newrevid . 101)
                      (newtimestamp . "2025-07-20T12:34:56Z"))))))

   ;; Permanent failure for draft saving test
   ((and (string= action "edit")
         (string= sitename "draft-test-wiki"))
    (make-mediawiki-api-response
     :success nil
     :errors (list (list :code "permissiondenied" :info "Permission denied"))))

   ;; Page deleted error
   ((and (string= action "edit")
         (string= sitename "page-deleted-wiki"))
    (make-mediawiki-api-response
     :success nil
     :errors (list (list :code "pagedeleted" :info "Page was deleted since you started editing"))))

   ;; Default success response
   (t
    (make-mediawiki-api-response
     :success t
     :data '((edit . ((result . "Success")
                      (pageid . 12345)
                      (title . "Test Recovery Page")
                      (contentmodel . "wikitext")
                      (oldrevid . 100)
                      (newrevid . 101)
                      (newtimestamp . "2025-07-20T12:34:56Z")))))))

(defun test-recovery-mock-y-or-n-p (prompt)
  "Mock y-or-n-p to simulate user confirmation during tests."
  t)  ; Always confirm

(defun test-recovery-mock-sit-for (seconds)
  "Mock sit-for to avoid delays during tests."
  t)  ; Return immediately

(defun test-recovery-mock-run-with-timer (secs repeat function &rest args)
  "Mock run-with-timer to execute immediately during tests."
  (apply function args)
  nil)  ; Return nil as timer

;;; Test Functions

(ert-deftest test-save-retry-logic ()
  "Test automatic retry logic for failed saves."
  ;; Set up test environment
  (test-recovery-setup-mock-session)
  (setq test-recovery-retry-count 0)

  ;; Mock the necessary functions
  (advice-add 'mediawiki-api-call-with-token :override #'test-recovery-mock-api-call-with-token)
  (advice-add 'sit-for :override #'test-recovery-mock-sit-for)

  (unwind-protect
      (progn
        ;; Test: Retry logic with eventual success
        (let ((mediawiki-page-save-retry-count 3)
              (mediawiki-page-save-retry-delay 0.1))
          (let ((result (mediawiki-page-save "retry-test-wiki"
                                            test-recovery-title
                                            test-recovery-content
                                            (list :summary "Test edit"))))
            ;; Should succeed after retries
            (should (plist-get result :success))
            ;; Should have retried twice
            (should (= test-recovery-retry-count 2)))))

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-with-token #'test-recovery-mock-api-call-with-token)
    (advice-remove 'sit-for #'test-recovery-mock-sit-for)
    (mediawiki-remove-session test-recovery-sitename)))

(ert-deftest test-save-retry-async ()
  "Test automatic retry logic for asynchronous failed saves."
  ;; Set up test environment
  (test-recovery-setup-mock-session)
  (setq test-recovery-retry-count 0)

  ;; Mock the necessary functions
  (advice-add 'mediawiki-api-call-with-token :override #'test-recovery-mock-api-call-with-token)
  (advice-add 'run-with-timer :override #'test-recovery-mock-run-with-timer)

  (unwind-protect
      (progn
        ;; Test: Async retry logic with eventual success
        (let ((mediawiki-page-save-retry-count 3)
              (mediawiki-page-save-retry-delay 0.1)
              (success-called nil)
              (error-called nil))
          
          (mediawiki-page-save "retry-test-wiki"
                              test-recovery-title
                              test-recovery-content
                              (list :summary "Test edit"
                                    :callback (lambda (_) (setq success-called t))
                                    :error-callback (lambda (_) (setq error-called t))))
          
          ;; Should have succeeded after retries
          (should success-called)
          (should-not error-called)
          ;; Should have retried twice
          (should (= test-recovery-retry-count 2))))

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-with-token #'test-recovery-mock-api-call-with-token)
    (advice-remove 'run-with-timer #'test-recovery-mock-run-with-timer)
    (mediawiki-remove-session test-recovery-sitename)))

(ert-deftest test-draft-saving ()
  "Test draft saving functionality for failed edits."
  ;; Set up test environment
  (test-recovery-setup-mock-session)
  
  ;; Create temporary directory for drafts
  (setq test-recovery-temp-dir (make-temp-file "mediawiki-test-drafts-" t))
  
  ;; Mock the necessary functions
  (advice-add 'mediawiki-api-call-with-token :override #'test-recovery-mock-api-call-with-token)
  (advice-add 'y-or-n-p :override #'test-recovery-mock-y-or-n-p)

  (unwind-protect
      (progn
        ;; Test: Draft saving on permanent failure
        (let ((mediawiki-page-save-retry-count 1)
              (mediawiki-page-save-retry-delay 0.1)
              (mediawiki-page-save-draft-on-failure t)
              (mediawiki-page-draft-directory test-recovery-temp-dir))
          
          ;; Should fail but save draft
          (condition-case err
              (mediawiki-page-save "draft-test-wiki"
                                  test-recovery-title
                                  test-recovery-content
                                  (list :summary "Test edit"))
            (error
             ;; Should have error message about permission denied
             (should (string-match-p "Permission denied" (error-message-string err)))))
          
          ;; Check if draft was saved
          (let* ((site-dir (expand-file-name "draft-test-wiki/" test-recovery-temp-dir))
                 (draft-files (directory-files site-dir t "\\.wiki$")))
            (should (= (length draft-files) 1))
            
            ;; Check content of draft file
            (with-temp-buffer
              (insert-file-contents (car draft-files))
              (should (string= (buffer-string) test-recovery-content)))
            
            ;; Check metadata file
            (let ((meta-file (concat (file-name-sans-extension (car draft-files)) ".meta")))
              (should (file-exists-p meta-file))
              (with-temp-buffer
                (insert-file-contents meta-file)
                (should (string-match-p (regexp-quote test-recovery-title) (buffer-string)))
                (should (string-match-p "draft-test-wiki" (buffer-string))))))))

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-with-token #'test-recovery-mock-api-call-with-token)
    (advice-remove 'y-or-n-p #'test-recovery-mock-y-or-n-p)
    (mediawiki-remove-session test-recovery-sitename)
    
    ;; Delete temporary directory
    (when (and test-recovery-temp-dir (file-directory-p test-recovery-temp-dir))
      (delete-directory test-recovery-temp-dir t))))

(ert-deftest test-page-deleted-handling ()
  "Test handling of page deleted errors."
  ;; Set up test environment
  (test-recovery-setup-mock-session)
  
  ;; Create temporary directory for drafts
  (setq test-recovery-temp-dir (make-temp-file "mediawiki-test-drafts-" t))
  
  ;; Mock the necessary functions
  (advice-add 'mediawiki-api-call-with-token :override #'test-recovery-mock-api-call-with-token)
  (advice-add 'y-or-n-p :override #'test-recovery-mock-y-or-n-p)

  (unwind-protect
      (progn
        ;; Test: Page deleted handling
        (let ((mediawiki-page-draft-directory test-recovery-temp-dir))
          
          ;; Should handle page deleted error
          (mediawiki-page-save "page-deleted-wiki"
                              test-recovery-title
                              test-recovery-content
                              (list :summary "Test edit"))
          
          ;; Check if draft was saved
          (let* ((site-dir (expand-file-name "page-deleted-wiki/" test-recovery-temp-dir))
                 (draft-files (directory-files site-dir t "\\.wiki$")))
            (should (= (length draft-files) 1)))))

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-with-token #'test-recovery-mock-api-call-with-token)
    (advice-remove 'y-or-n-p #'test-recovery-mock-y-or-n-p)
    (mediawiki-remove-session test-recovery-sitename)
    
    ;; Delete temporary directory
    (when (and test-recovery-temp-dir (file-directory-p test-recovery-temp-dir))
      (delete-directory test-recovery-temp-dir t))))

(ert-deftest test-draft-metadata-handling ()
  "Test loading and handling of draft metadata."
  ;; Create temporary directory for drafts
  (setq test-recovery-temp-dir (make-temp-file "mediawiki-test-drafts-" t))
  
  (unwind-protect
      (progn
        ;; Create test metadata file
        (let* ((meta-file (expand-file-name "test-draft.meta" test-recovery-temp-dir))
               (expected-title "Test Title")
               (expected-site "test-site")
               (expected-summary "Test summary"))
          
          ;; Write test metadata
          (with-temp-file meta-file
            (insert (format "Title: %s\n" expected-title))
            (insert (format "Site: %s\n" expected-site))
            (insert (format "Summary: %s\n" expected-summary))
            (insert "Param-minor: 1\n")
            (insert "Param-section: 2\n"))
          
          ;; Test loading metadata
          (let ((metadata (mediawiki-page-load-draft-metadata meta-file)))
            (should (string= (plist-get metadata :title) expected-title))
            (should (string= (plist-get metadata :sitename) expected-site))
            
            ;; Check params
            (let ((params (plist-get metadata :params)))
              (should (string= (cdr (assoc "summary" params)) expected-summary))
              (should (string= (cdr (assoc "minor" params)) "1"))
              (should (string= (cdr (assoc "section" params)) "2"))))))
    
    ;; Delete temporary directory
    (when (and test-recovery-temp-dir (file-directory-p test-recovery-temp-dir))
      (delete-directory test-recovery-temp-dir t))))

(provide 'test-save-failure-recovery)

;;; test-save-failure-recovery.el ends here