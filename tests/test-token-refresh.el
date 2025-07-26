;;; test-token-refresh.el --- Test automatic token refresh functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;;; Commentary:

;; Tests for automatic token refresh functionality (task 4.4).
;; Tests token expiration detection, automatic refresh, and session persistence.

;;; Code:

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-session)
(require 'mediawiki-api)

;;; Test Configuration

(defvar test-token-refresh-sitename "TestWiki"
  "Test site name for token refresh tests.")

(defvar test-token-refresh-mock-responses nil
  "Mock responses for API calls during testing.")

;;; Test Setup and Teardown

(defun test-token-refresh-setup ()
  "Set up test environment for token refresh tests."
  ;; Create test site configuration
  (let ((site (make-mediawiki-site-config
               :name test-token-refresh-sitename
               :url "https://test.example.com"
               :api-url "https://test.example.com/api.php")))
    (mediawiki-add-site site))

  ;; Create test session
  (let ((session (make-mediawiki-session
                  :site-name test-token-refresh-sitename
                  :tokens (make-hash-table :test 'equal)
                  :login-time (current-time)
                  :last-activity (current-time))))
    (mediawiki-set-session test-token-refresh-sitename session))

  ;; Reset mock responses
  (setq test-token-refresh-mock-responses nil))

(defun test-token-refresh-teardown ()
  "Clean up test environment."
  (mediawiki-remove-site test-token-refresh-sitename)
  (mediawiki-remove-session test-token-refresh-sitename)
  (setq test-token-refresh-mock-responses nil))

(defun test-token-refresh-mock-api-call (sitename action params)
  "Mock API call function for testing."
  (let* ((key (format "%s-%s" action (or (cdr (assoc "type" params)) "")))
         (response-data (cdr (assoc key test-token-refresh-mock-responses))))

    (if response-data
        (make-mediawiki-api-response
         :success (plist-get response-data :success)
         :data (plist-get response-data :data)
         :errors (plist-get response-data :errors)
         :warnings (plist-get response-data :warnings))
      ;; Default error response
      (make-mediawiki-api-response
       :success nil
       :errors (list (list :code "mock-error" :info "No mock response configured"))))))

;;; Test Cases

(ert-deftest test-token-refresh-expiration-detection ()
  "Test that token expiration is correctly detected."
  (test-token-refresh-setup)

  (let ((session (mediawiki-get-session test-token-refresh-sitename))
        (past-time (time-subtract (current-time) 7200)) ; 2 hours ago
        (future-time (time-add (current-time) 7200)))   ; 2 hours from now

    ;; Test expired token
    (should (not (mediawiki-session-token-valid-p past-time)))
    (should (mediawiki-session-token-needs-refresh-p past-time))

    ;; Test valid token
    (should (mediawiki-session-token-valid-p future-time))
    (should (not (mediawiki-session-token-needs-refresh-p future-time)))

    ;; Test token near expiry (within threshold)
    (let ((near-expiry (time-add (current-time) 200))) ; 200 seconds from now
      (should (not (mediawiki-session-token-valid-p near-expiry)))
      (should (mediawiki-session-token-needs-refresh-p near-expiry))))

  (test-token-refresh-teardown))

(ert-deftest test-token-refresh-cache-management ()
  "Test token cache management functions."
  (test-token-refresh-setup)

  (let ((session (mediawiki-get-session test-token-refresh-sitename)))
    ;; Cache a test token
    (mediawiki-session-cache-token session "csrf" "test-csrf-token")

    ;; Verify token is cached
    (should (string= (gethash "csrf" (mediawiki-session-tokens session))
                     "test-csrf-token"))
    (should (gethash "csrf-expiry" (mediawiki-session-tokens session)))
    (should (= (gethash "csrf-refresh-count" (mediawiki-session-tokens session)) 1))

    ;; Test token info
    (let ((info (mediawiki-session-get-token-info test-token-refresh-sitename "csrf")))
      (should (plist-get info :token-exists))
      (should (= (plist-get info :refresh-count) 1)))

    ;; Clear token
    (mediawiki-session-clear-token test-token-refresh-sitename "csrf")
    (should (not (gethash "csrf" (mediawiki-session-tokens session))))
    (should (not (gethash "csrf-expiry" (mediawiki-session-tokens session)))))

  (test-token-refresh-teardown))

(ert-deftest test-token-refresh-automatic-refresh ()
  "Test automatic token refresh functionality."
  (test-token-refresh-setup)

  ;; Set up mock response for token refresh
  (setq test-token-refresh-mock-responses
        '(("query-csrf" . (:success t
                          :data ((query . ((tokens . ((csrftoken . "new-csrf-token"))))))))))

  ;; Mock the API call function
  (cl-letf (((symbol-function 'mediawiki-api-call-sync)
             #'test-token-refresh-mock-api-call))

    ;; Test getting token when none exists
    (let ((token (mediawiki-session-get-token test-token-refresh-sitename "csrf")))
      (should (string= token "new-csrf-token")))

    ;; Test getting cached token
    (let ((token (mediawiki-session-get-token test-token-refresh-sitename "csrf")))
      (should (string= token "new-csrf-token")))

    ;; Test refresh when token is expired
    (let ((session (mediawiki-get-session test-token-refresh-sitename)))
      ;; Set token to expired
      (puthash "csrf-expiry" (time-subtract (current-time) 3600)
               (mediawiki-session-tokens session))

      ;; Update mock response for refresh
      (setq test-token-refresh-mock-responses
            '(("query-csrf" . (:success t
                              :data ((query . ((tokens . ((csrftoken . "refreshed-csrf-token"))))))))))

      ;; Get token should trigger refresh
      (let ((token (mediawiki-session-get-token test-token-refresh-sitename "csrf")))
        (should (string= token "refreshed-csrf-token")))))

  (test-token-refresh-teardown))

(ert-deftest test-token-refresh-error-handling ()
  "Test error handling during token refresh."
  (test-token-refresh-setup)

  ;; Set up mock response for failed token refresh
  (setq test-token-refresh-mock-responses
        '(("query-csrf" . (:success nil
                          :errors ((:code "badtoken" :info "Invalid token"))))))

  ;; Mock the API call function and sleep-for to avoid delays
  (cl-letf (((symbol-function 'mediawiki-api-call-sync) #'test-token-refresh-mock-api-call)
            ((symbol-function 'sleep-for) (lambda (_seconds) nil))
            ((symbol-function 'auth-source-netrc-parse) (lambda (&key file max host user port require
                                                                      allow-null &allow-other-keys)
                                                          '()))
            ((symbol-function 'auth-source-netrc-saver) (lambda (_file _add) t))
            ((symbol-function 'read-string) (lambda (_prompt) "test-user"))
            ((symbol-function 'read-passwd) (lambda (_prompt) "test-pass")))

    ;; Test that error is properly handled
    (should-error (mediawiki-session-get-token test-token-refresh-sitename "csrf")))

  (test-token-refresh-teardown))

(ert-deftest test-session-persistence ()
  "Test session persistence across restarts."
  (test-token-refresh-setup)

  ;; Create a session with some data
  (let ((session (mediawiki-get-session test-token-refresh-sitename)))
    (setf (mediawiki-session-user-info session)
          (list :username "testuser" :userid 123))
    (mediawiki-session-cache-token session "csrf" "test-token"))

  ;; Test serialization
  (let* ((session (mediawiki-get-session test-token-refresh-sitename))
         (serialized (mediawiki-session-serialize session)))

    (should (plist-get serialized :site-name))
    (should (plist-get serialized :login-time))
    (should (plist-get serialized :user-info))
    (should (= (plist-get serialized :version) 1)))

  ;; Test deserialization
  (let* ((session (mediawiki-get-session test-token-refresh-sitename))
         (serialized (mediawiki-session-serialize session))
         (deserialized (mediawiki-session-deserialize serialized)))

    (should (string= (mediawiki-session-site-name deserialized) test-token-refresh-sitename))
    (should (mediawiki-session-login-time deserialized))
    (should (mediawiki-session-user-info deserialized))
    (should (mediawiki-session-is-restored-p deserialized)))

  ;; Test validation
  (let* ((session (mediawiki-get-session test-token-refresh-sitename))
         (serialized (mediawiki-session-serialize session))
         (deserialized (mediawiki-session-deserialize serialized)))

    (should (mediawiki-session-validate-loaded deserialized)))

  (test-token-refresh-teardown))

(ert-deftest test-session-health-monitoring ()
  "Test session health monitoring functionality."
  (test-token-refresh-setup)

  ;; Test health check for active session
  (let ((health (mediawiki-session-health-check test-token-refresh-sitename)))
    (should (plist-get health :active))
    (should (plist-get health :login-time))
    (should (plist-get health :last-activity))
    (should (>= (plist-get health :token-count) 0)))

  ;; Test health check for non-existent session
  (mediawiki-remove-session test-token-refresh-sitename)
  (let ((health (mediawiki-session-health-check test-token-refresh-sitename)))
    (should (not (plist-get health :active))))

  (test-token-refresh-teardown))

;;; Integration Tests

(ert-deftest test-api-call-with-token-integration ()
  "Test API call with automatic token handling."
  (test-token-refresh-setup)

  ;; Set up mock responses
  (setq test-token-refresh-mock-responses
        '(("query-csrf" . (:success t
                          :data ((query . ((tokens . ((csrftoken . "auto-token"))))))))
          ("edit-" . (:success t
                     :data ((edit . ((result . "Success"))))))))

  ;; Mock the API call function
  (cl-letf (((symbol-function 'mediawiki-api-call-sync)
             #'test-token-refresh-mock-api-call))

    ;; Test API call with automatic token injection
    (let ((response (mediawiki-api-call-with-token
                    test-token-refresh-sitename "edit"
                    '(("title" . "Test Page") ("text" . "Test content"))
                    "csrf")))

      (should (mediawiki-api-response-success response))))

  (test-token-refresh-teardown))

;; Add tags to tests
(put 'test-token-refresh-expiration-detection 'ert-tags '(:token-refresh))
(put 'test-token-refresh-cache-management 'ert-tags '(:token-refresh))
(put 'test-token-refresh-automatic-refresh 'ert-tags '(:token-refresh))
(put 'test-token-refresh-error-handling 'ert-tags '(:token-refresh))
(put 'test-session-persistence 'ert-tags '(:token-refresh))
(put 'test-session-health-monitoring 'ert-tags '(:token-refresh))
(put 'test-api-call-with-token-integration 'ert-tags '(:token-refresh))

(provide 'test-token-refresh)

;;; test-token-refresh.el ends here
