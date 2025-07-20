;;; run-tests.el --- Test runner for MediaWiki.el tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple test runner that loads all test files and runs them

;;; Code:

;; Load core modules first
(load-file "mediawiki-core.el")

;; Mock API functions for tests that need them
(defun mediawiki-api-call-sync (sitename action params)
  "Mock API call function for testing."
  (make-mediawiki-api-response
   :success t
   :data '((query . ((tokens . ((logintoken . "mock-token"))))))
   :warnings nil
   :errors nil))

(defun mediawiki-api-get-error-info (response)
  "Mock error info function."
  "Mock error")

;; Load auth module
(load-file "mediawiki-auth.el")

;; Find and load all test files
(let ((test-files (directory-files "." nil "^test-.*\\.el$")))
  (dolist (test-file test-files)
    (unless (string= test-file "run-tests.el")
      (message "Loading test file: %s" test-file)
      (condition-case err
          (load-file test-file)
        (error
         (message "Error loading %s: %s" test-file (error-message-string err)))))))

;; Run all tests
(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here