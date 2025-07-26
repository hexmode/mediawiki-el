;;; test-page-saving.el --- Test modern page saving functionality -*- lexical-binding: t; -*-

;; Test implementation of task 6.2: Implement modern page saving
;; Tests page saving with proper edit tokens, conflict detection, and edit summary support

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-api)
(require 'mediawiki-session)
(require 'mediawiki-page)

;;; Test Configuration

(defvar test-page-save-sitename "test-wiki"
  "Test site name for page saving tests.")

(defvar test-page-save-title "Test Page"
  "Test page title for page saving tests.")

(defvar test-page-save-content "This is test content for page saving."
  "Test content for page saving tests.")

(defvar test-page-save-summary "Test edit summary"
  "Test edit summary for page saving tests.")

(defvar test-page-save-mock-token "test-csrf-token-12345"
  "Mock CSRF token for testing.")

;;; Mock Functions for Testing

(defun test-page-save-setup-mock-session ()
  "Set up a mock session for testing page saving functionality."
  (let ((session (make-mediawiki-session
                  :site-name test-page-save-sitename
                  :tokens (make-hash-table :test 'equal)
                  :user-info '(:username "testuser" :userid 123)
                  :login-time (current-time)
                  :last-activity (current-time))))
    (mediawiki-set-session test-page-save-sitename session)
    ;; Add mock token
    (puthash "csrf" test-page-save-mock-token (mediawiki-session-tokens session))
    (puthash "csrf-expiry" (time-add (current-time) 3600) (mediawiki-session-tokens session))
    session))

(defun test-page-save-mock-api-call-with-token (sitename action params token-type)
  "Mock API call with token for testing page saving."
  (cond
   ;; Successful edit
   ((and (string= action "edit")
         (string= sitename test-page-save-sitename)
         (string= (cdr (assoc "title" params)) test-page-save-title))
    (make-mediawiki-api-response
     :success t
     :data `((edit . ((result . "Success")
                      (pageid . 12345)
                      (title . ,test-page-save-title)
                      (contentmodel . "wikitext")
                      (oldrevid . 100)
                      (newrevid . 101)
                      (newtimestamp . "2025-07-20T12:34:56Z"))))))

   ;; Edit conflict
   ((and (string= action "edit")
         (string= (cdr (assoc "summary" params)) test-page-save-summary)
         (string= sitename "conflict-wiki"))
    (make-mediawiki-api-response
     :success nil
     :errors (list (list :code "editconflict" :info "Edit conflict detected"))))

   ;; Permission error
   ((and (string= action "edit")
         (string= sitename "permission-error-wiki"))
    (make-mediawiki-api-response
     :success nil
     :errors (list (list :code "permissiondenied" :info "Permission denied"))))

   ;; Rate limit error
   ((and (string= action "edit")
         (string= sitename "rate-limited-wiki"))
    (make-mediawiki-api-response
     :success nil
     :errors (list (list :code "ratelimited" :info "Rate limit exceeded"))))

   ;; Default successful response
   (t
    (make-mediawiki-api-response
     :success t
     :data '((result . "Success"))))))

(defun test-page-save-mock-page-get (sitename title &optional options)
  "Mock page retrieval for testing edit conflicts."
  (when (string= sitename "conflict-wiki")
    (make-mediawiki-page-data
     :title title
     :content "This is the server version of the content."
     :metadata '((pageid . 12345) (title . "Test Page"))
     :revisions (list (list :revid 200 :user "otheruser" :timestamp "2025-07-20T12:00:00Z"))
     :timestamp (current-time))))

;;; Test Functions

(ert-deftest test-page-save-basic-functionality ()
  "Test basic page saving functionality."
  ;; Set up test environment
  (test-page-save-setup-mock-session)

  ;; Mock the API call function
  (advice-add 'mediawiki-api-call-with-token :override #'test-page-save-mock-api-call-with-token)

  (unwind-protect
      (progn
        ;; Test 1: Basic page save
        (let ((result (mediawiki-page-save test-page-save-sitename
                                          test-page-save-title
                                          test-page-save-content
                                          (list :summary test-page-save-summary))))
          (should (plist-get result :success))
          (should (string= (plist-get result :result) "Success"))
          (should (= (plist-get result :pageid) 12345))
          (should (= (plist-get result :newrevid) 101)))

        ;; Test 2: Page save with minor edit flag
        (let ((result (mediawiki-page-save test-page-save-sitename
                                          test-page-save-title
                                          test-page-save-content
                                          (list :summary test-page-save-summary
                                                :minor t))))
          (should (plist-get result :success)))

        ;; Test 3: Page save with section
        (let ((result (mediawiki-page-save test-page-save-sitename
                                          test-page-save-title
                                          test-page-save-content
                                          (list :summary test-page-save-summary
                                                :section 1))))
          (should (plist-get result :success)))

        ;; Test 4: Page save with base revision
        (let ((result (mediawiki-page-save test-page-save-sitename
                                          test-page-save-title
                                          test-page-save-content
                                          (list :summary test-page-save-summary
                                                :base-revision 100))))
          (should (plist-get result :success))))

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-with-token #'test-page-save-mock-api-call-with-token)
    (mediawiki-remove-session test-page-save-sitename)))

(ert-deftest test-page-save-edit-conflict-handling ()
  "Test edit conflict detection and handling."
  ;; Set up test environment
  (test-page-save-setup-mock-session)

  ;; Mock the API call and page get functions
  (advice-add 'mediawiki-api-call-with-token :override #'test-page-save-mock-api-call-with-token)
  (advice-add 'mediawiki-page-get :override #'test-page-save-mock-page-get)

  ;; Mock the conflict resolution prompt to always return 'mine'
  (advice-add 'mediawiki-page-conflict-resolution-prompt :override
              (lambda (_title) 'mine))

  (unwind-protect
      (progn
        ;; Test: Edit conflict handling with 'mine' resolution
        ;; This should trigger the conflict handling code path
        (let ((mediawiki-page-edit-conflict-resolution 'mine))
          (condition-case err
              (mediawiki-page-save "conflict-wiki"
                                  test-page-save-title
                                  test-page-save-content
                                  (list :summary test-page-save-summary))
            (error
             (should (string-match-p "Edit conflict" (error-message-string err)))))))

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-with-token #'test-page-save-mock-api-call-with-token)
    (advice-remove 'mediawiki-page-get #'test-page-save-mock-page-get)
    (advice-remove 'mediawiki-page-conflict-resolution-prompt #'mediawiki-page-conflict-resolution-prompt)))

(ert-deftest test-page-save-error-handling ()
  "Test error handling during page saving."
  ;; Set up test environment
  (test-page-save-setup-mock-session)

  ;; Mock the API call function
  (advice-add 'mediawiki-api-call-with-token :override #'test-page-save-mock-api-call-with-token)

  ;; Mock the draft saving function to avoid file system operations
  (advice-add 'mediawiki-page-save-draft :override
              (lambda (_sitename _params) "/tmp/mock-draft-file.wiki"))

  ;; Mock the draft removal function to avoid file system operations
  (advice-add 'mediawiki-page-remove-draft :override
              (lambda (_sitename _title) t))

  (unwind-protect
      (progn
        ;; Test 1: Permission error
        (let ((mediawiki-page-save-draft-on-failure t))
          (condition-case err
              (mediawiki-page-save "permission-error-wiki"
                                  test-page-save-title
                                  test-page-save-content
                                  (list :summary test-page-save-summary))
            (error
             (should (string-match-p "Permission denied: Permission denied" (error-message-string err))))))

        ;; Test 2: Rate limit error with retry disabled
        (let ((mediawiki-page-save-retry-count 0))
          (condition-case err
              (mediawiki-page-save "rate-limited-wiki"
                                  test-page-save-title
                                  test-page-save-content
                                  (list :summary test-page-save-summary))
            (error
             (should (string-match-p "Rate limited: Rate limit exceeded" (error-message-string err)))))))

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-with-token #'test-page-save-mock-api-call-with-token)
    (advice-remove 'mediawiki-page-save-draft #'mediawiki-page-save-draft)))

(ert-deftest test-page-save-convenience-functions ()
  "Test convenience functions for page saving."
  ;; Set up test environment
  (test-page-save-setup-mock-session)

  ;; Mock the API call function
  (advice-add 'mediawiki-api-call-with-token :override #'test-page-save-mock-api-call-with-token)

  ;; Mock the page get function
  (advice-add 'mediawiki-page-get-content :override
              (lambda (_sitename _title &optional _options) "Existing content."))

  ;; Mock the draft saving function to avoid file system operations
  (advice-add 'mediawiki-page-save-draft :override
              (lambda (_sitename _params) "/tmp/mock-draft-file.wiki"))

  ;; Mock the draft removal function to avoid file system operations
  (advice-add 'mediawiki-page-remove-draft :override
              (lambda (_sitename _title) t))

  (unwind-protect
      (progn
        ;; Test 1: Save section
        (let ((result (mediawiki-page-save-section test-page-save-sitename
                                                  test-page-save-title
                                                  1
                                                  test-page-save-content
                                                  (list :summary test-page-save-summary))))
          (should (plist-get result :success)))

        ;; Test 2: Append content
        (let ((result (mediawiki-page-append test-page-save-sitename
                                            test-page-save-title
                                            test-page-save-content
                                            (list :summary test-page-save-summary))))
          (should (plist-get result :success)))

        ;; Test 3: Prepend content
        (let ((result (mediawiki-page-prepend test-page-save-sitename
                                             test-page-save-title
                                             test-page-save-content
                                             (list :summary test-page-save-summary))))
          (should (plist-get result :success)))

        ;; Test 4: Replace content
        (let ((result (mediawiki-page-replace test-page-save-sitename
                                             test-page-save-title
                                             "Existing content."
                                             test-page-save-content
                                             (list :summary test-page-save-summary))))
          (should (plist-get result :success))))

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-with-token #'test-page-save-mock-api-call-with-token)
    (advice-remove 'mediawiki-page-get-content #'mediawiki-page-get-content)
    (mediawiki-remove-session test-page-save-sitename)))

(provide 'test-page-saving)

;;; test-page-saving.el ends here
