;;; test-auth-minimal.el --- Minimal tests for auth-source integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal tests for auth-source integration that don't require full API

;;; Code:

(require 'ert)

;; Load only the core module first
(load-file "mediawiki-core.el")

;; Mock the API functions that auth module needs
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

;; Now load the auth module
(load-file "mediawiki-auth.el")

;;; Test the core auth-source integration functions

(ert-deftest test-auth-cache-basic ()
  "Test basic credential caching functionality."
  (let ((cache-key "test-site")
        (credentials '(:username "testuser" :password "testpass")))

    ;; Clear any existing cache
    (mediawiki-auth-clear-all-cached-credentials)

    ;; Test caching
    (mediawiki-auth-cache-credentials cache-key credentials)

    ;; Test retrieval
    (let ((cached (mediawiki-auth-get-cached-credentials cache-key)))
      (should cached)
      (should (string= (plist-get cached :username) "testuser"))
      (should (string= (plist-get cached :password) "testpass")))

    ;; Clean up
    (mediawiki-auth-clear-all-cached-credentials)))

(ert-deftest test-auth-url-parsing ()
  "Test URL parsing for auth-source."
  (should (string= (mediawiki-auth-extract-host "https://example.com/wiki/")
                   "example.com"))
  (should (string= (mediawiki-auth-extract-port "https://example.com/") "https"))
  (should (string= (mediawiki-auth-extract-port "http://example.com/") "http")))

(ert-deftest test-auth-cache-expiration ()
  "Test credential cache expiration."
  (let ((cache-key "expire-test"))

    ;; Clear cache
    (mediawiki-auth-clear-all-cached-credentials)

    ;; Add expired entry manually
    (puthash cache-key
             (list :username "user"
                   :password "pass"
                   :expiry (time-subtract (current-time) 10))
             mediawiki-auth-credential-cache)

    ;; Should return nil for expired entry
    (should-not (mediawiki-auth-get-cached-credentials cache-key))

    ;; Should be removed from cache
    (should-not (gethash cache-key mediawiki-auth-credential-cache))))

(ert-deftest test-auth-cache-status ()
  "Test cache status reporting."
  (mediawiki-auth-clear-all-cached-credentials)

  ;; Add test entries
  (puthash "active"
           (list :username "user1" :password "pass1"
                 :expiry (time-add (current-time) 3600))
           mediawiki-auth-credential-cache)

  (puthash "expired"
           (list :username "user2" :password "pass2"
                 :expiry (time-subtract (current-time) 10))
           mediawiki-auth-credential-cache)

  (let ((status (mediawiki-auth-get-cache-status)))
    (should (= (plist-get status :total-entries) 2))
    (should (= (plist-get status :expired-entries) 1))
    (should (= (plist-get status :active-entries) 1)))

  (mediawiki-auth-clear-all-cached-credentials))

;;; test-auth-minimal.el ends here
