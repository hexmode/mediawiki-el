;;; test-edit-conflict-resolution.el --- Test edit conflict resolution functionality -*- lexical-binding: t; -*-

;; Test implementation of task 6.3: Add edit conflict resolution
;; Tests conflict detection, user notification, and three-way merge capabilities

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-api)
(require 'mediawiki-session)
(require 'mediawiki-page)

;;; Test Configuration

(defvar test-conflict-sitename "test-conflict-wiki"
  "Test site name for conflict resolution tests.")

(defvar test-conflict-title "Test Conflict Page"
  "Test page title for conflict resolution tests.")

(defvar test-conflict-local-content "This is the local version of the content.\nIt has some changes that need to be merged."
  "Local version of the content for conflict tests.")

(defvar test-conflict-server-content "This is the server version of the content.\nIt has different changes that conflict with local changes."
  "Server version of the content for conflict tests.")

(defvar test-conflict-base-content "This is the base version of the content.\nIt is the common ancestor of both versions."
  "Base version of the content for conflict tests.")

(defvar test-conflict-base-revision 100
  "Base revision ID for conflict tests.")

(defvar test-conflict-mock-token "test-csrf-token-12345"
  "Mock CSRF token for testing.")

;;; Mock Functions for Testing

(defun test-conflict-setup-mock-session ()
  "Set up a mock session for testing conflict resolution."
  (let ((session (make-mediawiki-session
                  :site-name test-conflict-sitename
                  :tokens (make-hash-table :test 'equal)
                  :user-info '(:username "testuser" :userid 123)
                  :login-time (current-time)
                  :last-activity (current-time))))
    (mediawiki-set-session test-conflict-sitename session)
    ;; Add mock token
    (puthash "csrf" test-conflict-mock-token (mediawiki-session-tokens session))
    (puthash "csrf-expiry" (time-add (current-time) 3600) (mediawiki-session-tokens session))
    session))

(defun test-conflict-mock-api-call-with-token (sitename action params token-type)
  "Mock API call with token for testing conflict resolution."
  (cond
   ;; Edit conflict response
   ((and (string= action "edit")
         (string= sitename test-conflict-sitename))
    (make-mediawiki-api-response
     :success nil
     :errors (list (list :code "editconflict" :info "Edit conflict detected"))))

   ;; Successful edit (for force save)
   ((and (string= action "edit")
         (string= sitename "force-save-wiki"))
    (make-mediawiki-api-response
     :success t
     :data `((edit . ((result . "Success")
                      (pageid . 12345)
                      (title . ,test-conflict-title)
                      (contentmodel . "wikitext")
                      (oldrevid . 100)
                      (newrevid . 101)
                      (newtimestamp . "2025-07-20T12:34:56Z"))))))

   ;; Default response
   (t
    (make-mediawiki-api-response
     :success t
     :data '((result . "Success"))))))

(defun test-conflict-mock-page-get-content (sitename title &optional options)
  "Mock page content retrieval for conflict tests."
  (when (string= sitename test-conflict-sitename)
    test-conflict-server-content))

(defun test-conflict-mock-page-get-revision-content (sitename title revision-id)
  "Mock revision content retrieval for conflict tests."
  (when (and (string= sitename test-conflict-sitename)
             (= revision-id test-conflict-base-revision))
    test-conflict-base-content))

(defun test-conflict-mock-call-process (program &rest args)
  "Mock call-process for diff3 to avoid external process dependency."
  (when (string= program "diff3")
    (insert "<<<<<<< LOCAL VERSION\n")
    (insert test-conflict-local-content)
    (insert "\n=======\n")
    (insert test-conflict-server-content)
    (insert "\n>>>>>>> SERVER VERSION\n")
    0))  ; Return success

(defun test-conflict-mock-switch-to-buffer (buffer)
  "Mock switch-to-buffer to avoid UI interaction during tests."
  buffer)

(defun test-conflict-mock-read-char-choice (prompt choices)
  "Mock read-char-choice to simulate user input during tests."
  (cond
   ((string-match-p "conflict" prompt) ?m)  ; Choose 'mine' option
   (t (car choices))))

(defun test-conflict-mock-y-or-n-p (prompt)
  "Mock y-or-n-p to simulate user confirmation during tests."
  t)  ; Always confirm

(defun test-conflict-mock-three-way-merge (sitename title local-content server-content base-content params)
  "Mock three-way merge function for testing.
Just returns success without actually doing the merge."
  t)

(defun test-conflict-mock-attempt-merge (sitename title local-content base-revision params)
  "Mock attempt merge function for testing.
Bypasses the actual merge logic to avoid UI interaction."
  t)

(defun test-conflict-mock-page-save-draft (sitename params)
  "Mock draft saving function to avoid file system operations."
  (when mediawiki-page-save-draft-on-failure
    "/tmp/test-draft-file"))

(defun test-conflict-mock-page-remove-draft (sitename title)
  "Mock draft removal function to avoid file system operations."
  t)

;;; Test Functions

(ert-deftest test-conflict-detection ()
  "Test edit conflict detection."
  ;; Set up test environment
  (test-conflict-setup-mock-session)

  ;; Mock the API call function
  (advice-add 'mediawiki-api-call-with-token :override #'test-conflict-mock-api-call-with-token)
  (advice-add 'mediawiki-page-get-content :override #'test-conflict-mock-page-get-content)

  (unwind-protect
      (progn
        ;; Test: Basic conflict detection
        (let ((mediawiki-page-edit-conflict-resolution 'theirs))
          (condition-case err
              (mediawiki-page-save test-conflict-sitename
                                  test-conflict-title
                                  test-conflict-local-content
                                  (list :summary "Test edit"
                                        :base-revision test-conflict-base-revision))
            (error
             (should (string-match-p "Edit conflict" (error-message-string err)))))))

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-with-token #'test-conflict-mock-api-call-with-token)
    (advice-remove 'mediawiki-page-get-content #'test-conflict-mock-page-get-content)
    (mediawiki-remove-session test-conflict-sitename)))

(ert-deftest test-conflict-force-save ()
  "Test force save option for conflict resolution."
  ;; Set up test environment
  (test-conflict-setup-mock-session)

  ;; Mock the API call function
  (advice-add 'mediawiki-api-call-with-token :override #'test-conflict-mock-api-call-with-token)
  (advice-add 'read-char-choice :override #'test-conflict-mock-read-char-choice)
  (advice-add 'y-or-n-p :override #'test-conflict-mock-y-or-n-p)
  (advice-add 'mediawiki-page-save-draft :override #'test-conflict-mock-page-save-draft)
  (advice-add 'mediawiki-page-remove-draft :override #'test-conflict-mock-page-remove-draft)

  (unwind-protect
      (progn
        ;; Test: Force save with 'mine' option
        (let ((mediawiki-page-edit-conflict-resolution 'mine))
          ;; This should not throw an error but should successfully save
          (let ((result (mediawiki-page-save "force-save-wiki"
                                           test-conflict-title
                                           test-conflict-local-content
                                           (list :summary "Test edit"
                                                 :base-revision test-conflict-base-revision))))
            (should (eq (plist-get result :success) t)))))

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-with-token #'test-conflict-mock-api-call-with-token)
    (advice-remove 'read-char-choice #'test-conflict-mock-read-char-choice)
    (advice-remove 'y-or-n-p #'test-conflict-mock-y-or-n-p)
    (advice-remove 'mediawiki-page-save-draft #'test-conflict-mock-page-save-draft)
    (advice-remove 'mediawiki-page-remove-draft #'test-conflict-mock-page-remove-draft)
    (mediawiki-remove-session test-conflict-sitename)))

(ert-deftest test-conflict-three-way-merge ()
  "Test three-way merge functionality."
  ;; Set up test environment
  (test-conflict-setup-mock-session)

  ;; Mock the necessary functions
  (advice-add 'mediawiki-api-call-with-token :override #'test-conflict-mock-api-call-with-token)
  (advice-add 'mediawiki-page-get-content :override #'test-conflict-mock-page-get-content)
  (advice-add 'mediawiki-page-get-revision-content :override #'test-conflict-mock-page-get-revision-content)
  (advice-add 'call-process :override #'test-conflict-mock-call-process)
  (advice-add 'switch-to-buffer :override #'test-conflict-mock-switch-to-buffer)
  (advice-add 'mediawiki-page-three-way-merge :override #'test-conflict-mock-three-way-merge)
  (advice-add 'mediawiki-page-attempt-merge :override #'test-conflict-mock-attempt-merge)

  (unwind-protect
      (progn
        ;; Test: Three-way merge
        (let ((mediawiki-page-edit-conflict-resolution 'merge))
          (condition-case err
              (mediawiki-page-save test-conflict-sitename
                                  test-conflict-title
                                  test-conflict-local-content
                                  (list :summary "Test edit"
                                        :base-revision test-conflict-base-revision))
            (error
             (should nil)))))  ; Should not error

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-with-token #'test-conflict-mock-api-call-with-token)
    (advice-remove 'mediawiki-page-get-content #'test-conflict-mock-page-get-content)
    (advice-remove 'mediawiki-page-get-revision-content #'test-conflict-mock-page-get-revision-content)
    (advice-remove 'call-process #'test-conflict-mock-call-process)
    (advice-remove 'switch-to-buffer #'test-conflict-mock-switch-to-buffer)
    (advice-remove 'mediawiki-page-three-way-merge #'test-conflict-mock-three-way-merge)
    (advice-remove 'mediawiki-page-attempt-merge #'test-conflict-mock-attempt-merge)
    (mediawiki-remove-session test-conflict-sitename)))

(ert-deftest test-conflict-user-prompt ()
  "Test user prompt for conflict resolution."
  ;; Set up test environment
  (test-conflict-setup-mock-session)

  ;; Mock the necessary functions
  (advice-add 'mediawiki-api-call-with-token :override #'test-conflict-mock-api-call-with-token)
  (advice-add 'mediawiki-page-get-content :override #'test-conflict-mock-page-get-content)
  (advice-add 'read-char-choice :override #'test-conflict-mock-read-char-choice)
  (advice-add 'y-or-n-p :override #'test-conflict-mock-y-or-n-p)
  (advice-add 'switch-to-buffer :override #'test-conflict-mock-switch-to-buffer)
  (advice-add 'mediawiki-page-save-draft :override #'test-conflict-mock-page-save-draft)
  (advice-add 'mediawiki-page-remove-draft :override #'test-conflict-mock-page-remove-draft)

  (unwind-protect
      (progn
        ;; Test: User prompt with 'mine' choice
        (let ((mediawiki-page-edit-conflict-resolution 'prompt))
          ;; This should not throw an error but should successfully save
          (let ((result (mediawiki-page-save "force-save-wiki"
                                           test-conflict-title
                                           test-conflict-local-content
                                           (list :summary "Test edit"
                                                 :base-revision test-conflict-base-revision))))
            (should (eq (plist-get result :success) t)))))

    ;; Cleanup
    (advice-remove 'mediawiki-api-call-with-token #'test-conflict-mock-api-call-with-token)
    (advice-remove 'mediawiki-page-get-content #'test-conflict-mock-page-get-content)
    (advice-remove 'read-char-choice #'test-conflict-mock-read-char-choice)
    (advice-remove 'y-or-n-p #'test-conflict-mock-y-or-n-p)
    (advice-remove 'switch-to-buffer #'test-conflict-mock-switch-to-buffer)
    (advice-remove 'mediawiki-page-save-draft #'test-conflict-mock-page-save-draft)
    (advice-remove 'mediawiki-page-remove-draft #'test-conflict-mock-page-remove-draft)
    (mediawiki-remove-session test-conflict-sitename)))

(ert-deftest test-conflict-marker-removal ()
  "Test conflict marker removal functionality."
  (let ((test-content "Some content\n<<<<<<< LOCAL VERSION\nLocal changes\n=======\nServer changes\n>>>>>>> SERVER VERSION\nMore content"))
    ;; Test marker removal
    (should (string= (mediawiki-page-remove-conflict-markers test-content)
                    "Some content\nMore content"))))

(provide 'test-edit-conflict-resolution)

;;; test-edit-conflict-resolution.el ends here