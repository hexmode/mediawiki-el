;;; test-auth-source-mock.el --- Test auth-source integration with mocks -*- lexical-binding: t; -*-

;;; Commentary:
;; Test auth-source integration with mocked auth-source functions

;;; Code:

(require 'ert)

;; Load core module
(load-file "mediawiki-core.el")

;; Mock API functions
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

;;; Test Setup

(defvar test-auth-source-data
  '((:host "test.example.com" :user "testuser" :secret "testpass")
    (:host "wiki.example.org" :user "wikiuser" :secret "wikipass"))
  "Mock auth-source data for testing.")

(defun mock-auth-source-search (&rest spec)
  "Mock auth-source-search function."
  (let ((host (plist-get spec :host))
        (user (plist-get spec :user))
        (max (or (plist-get spec :max) 1)))

    (let ((matches '()))
      (dolist (entry test-auth-source-data)
        (when (and (or (not host) (string= (plist-get entry :host) host))
                   (or (not user) (string= (plist-get entry :user) user)))
          (push (list :host (plist-get entry :host)
                     :user (plist-get entry :user)
                     :secret (lambda () (plist-get entry :secret)))
                matches)))

      (if (> max 1)
          matches
        (when matches (list (car matches)))))))

;;; Tests

(ert-deftest test-auth-source-integration-basic ()
  "Test basic auth-source integration."
  ;; Set up test site
  (let ((test-site (make-mediawiki-site-config
                    :name "test-wiki"
                    :url "https://test.example.com/wiki/"
                    :api-url "https://test.example.com/wiki/api.php"
                    :username "testuser"
                    :auth-method 'basic)))
    (mediawiki-add-site test-site))

  (unwind-protect
      (progn
        ;; Clear cache
        (mediawiki-auth-clear-all-cached-credentials)

        ;; Mock auth-source-search
        (cl-letf (((symbol-function 'auth-source-search) #'mock-auth-source-search))

          ;; Test getting credentials
          (let ((credentials (mediawiki-auth-get-from-auth-source "test-wiki")))
            (should credentials)
            (should (string= (plist-get credentials :username) "testuser"))
            (should (string= (plist-get credentials :password) "testpass")))))

    ;; Cleanup
    (mediawiki-remove-site "test-wiki")
    (mediawiki-auth-clear-all-cached-credentials)))

(ert-deftest test-auth-source-host-extraction ()
  "Test host extraction for different URL formats."
  (should (string= (mediawiki-auth-extract-host "https://en.wikipedia.org/w/api.php")
                   "en.wikipedia.org"))
  (should (string= (mediawiki-auth-extract-host "http://localhost:8080/mediawiki/")
                   "localhost:8080"))
  (should (string= (mediawiki-auth-extract-host "https://wiki.company.com/")
                   "wiki.company.com")))

;;; test-auth-source-mock.el ends here
