;;; test-direct-login.el --- Direct login test with known credentials -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script to directly test login with bot credentials using ERT framework

;;; Code:

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-http)
(require 'mediawiki-api)
(require 'mediawiki-auth)

(defvar test-direct-login-site-name "test-wiki"
  "Test site name for direct login tests.")

(defvar test-direct-login-credentials nil
  "Test credentials for direct login tests.")

(ert-deftest test-direct-login-site-setup ()
  "Test setting up a site for direct login testing."
  (let ((site (make-mediawiki-site-config
               :name test-direct-login-site-name
               :url "https://wiki.nichework.com/"
               :api-url "https://wiki.nichework.com/w/api.php"
               :username "MarkAHershberger@emacs")))

    (mediawiki-add-site site)

    (unwind-protect
        (let ((retrieved-site (mediawiki-get-site test-direct-login-site-name)))
          (should retrieved-site)
          (should (string= (mediawiki-site-config-url retrieved-site) "https://wiki.nichework.com/"))
          (should (string= (mediawiki-site-config-api-url retrieved-site) "https://wiki.nichework.com/w/api.php"))
          (should (string= (mediawiki-site-config-username retrieved-site) "MarkAHershberger@emacs")))

      ;; Cleanup
      (mediawiki-remove-site test-direct-login-site-name))))

(ert-deftest test-credential-override-mechanism ()
  "Test the credential override mechanism for testing."
  (let ((test-username "test-user")
        (test-password "test-password"))

    ;; Override the credential function
    (cl-letf (((symbol-function 'mediawiki-auth-get-credentials)
               (lambda (_sitename)
                 (list :username test-username
                       :password test-password))))

      (let ((credentials (mediawiki-auth-get-credentials "any-site")))
        (should (equal (plist-get credentials :username) test-username))
        (should (equal (plist-get credentials :password) test-password))))))

(ert-deftest test-login-token-extraction ()
  "Test login token extraction from API response."
  (let ((mock-response (make-mediawiki-api-response
                        :success t
                        :data '((query . ((tokens . ((logintoken . "test-login-token-123")))))))))

    (let ((token (mediawiki-auth-extract-token mock-response "login")))
      (should (string= token "test-login-token-123")))))

(ert-deftest test-login-api-parameters ()
  "Test construction of login API parameters."
  (let ((username "test-user")
        (password "test-pass")
        (token "test-token"))

    (let ((params (list (cons "lgname" username)
                       (cons "lgpassword" password)
                       (cons "lgtoken" token))))

      (should (string= (cdr (assoc "lgname" params)) username))
      (should (string= (cdr (assoc "lgpassword" params)) password))
      (should (string= (cdr (assoc "lgtoken" params)) token)))))

(ert-deftest test-session-validation ()
  "Test session validation after login."
  (let ((mock-session (make-mediawiki-session
                       :site-name test-direct-login-site-name
                       :user-info '(:username "test-user" :userid 123)
                       :login-time (current-time))))

    (should (mediawiki-session-p mock-session))
    (should (string= (mediawiki-session-site-name mock-session) test-direct-login-site-name))
    (should (equal (plist-get (mediawiki-session-user-info mock-session) :username) "test-user"))
    (should (equal (plist-get (mediawiki-session-user-info mock-session) :userid) 123))))

(ert-deftest test-login-response-parsing ()
  "Test parsing of login API response."
  (let ((success-response (make-mediawiki-api-response
                          :success t
                          :data '((login . ((result . "Success")
                                           (lguserid . 123)
                                           (lgusername . "test-user"))))))
        (failure-response (make-mediawiki-api-response
                          :success nil
                          :errors '((:code "WrongPass" :info "Incorrect password")))))

    ;; Test successful response
    (should (mediawiki-api-response-success success-response))
    (let* ((data (mediawiki-api-response-data success-response))
           (login-data (cdr (assq 'login data)))
           (result (cdr (assq 'result login-data))))
      (should (string= result "Success")))

    ;; Test failure response
    (should (not (mediawiki-api-response-success failure-response)))
    (should (mediawiki-api-response-errors failure-response))))

;; Interactive test functions for manual testing
(defun test-direct-bot-login-interactive ()
  "Interactive test for direct bot login (for manual testing)."
  (interactive)
  (let ((old-debug mediawiki-debug))
    (setq mediawiki-debug t)

    (unwind-protect
        (let ((site (make-mediawiki-site-config
                     :name test-direct-login-site-name
                     :url "https://wiki.nichework.com/"
                     :api-url "https://wiki.nichework.com/w/api.php"
                     :username "MarkAHershberger@emacs")))

          (mediawiki-add-site site)

          (let ((bot-password (read-passwd "Enter bot password for MarkAHershberger@emacs: ")))

            (cl-letf (((symbol-function 'mediawiki-auth-get-credentials)
                       (lambda (_sitename)
                         (list :username "MarkAHershberger@emacs"
                               :password bot-password))))

              (message "Attempting login with bot credentials...")

              (condition-case err
                  (progn
                    (mediawiki-auth-basic-login test-direct-login-site-name)

                    (let ((session (mediawiki-get-session test-direct-login-site-name)))
                      (if session
                          (let ((user-info (mediawiki-session-user-info session)))
                            (message "✓ Login successful!")
                            (message "  User: %s" (plist-get user-info :username))
                            (message "  User ID: %s" (plist-get user-info :userid))
                            (message "  Login time: %s" (mediawiki-session-login-time session)))
                        (message "✗ Login appeared to succeed but no session was created"))))

                (error
                 (message "✗ Login failed: %s" (error-message-string err))
                 (message "Check the *MediaWiki Debug* buffer for detailed logs"))))))

      ;; Cleanup
      (mediawiki-remove-session test-direct-login-site-name)
      (mediawiki-remove-site test-direct-login-site-name)
      (setq mediawiki-debug old-debug))))

(defun test-manual-api-login-interactive ()
  "Interactive test for manual API login steps (for manual testing)."
  (interactive)
  (let ((old-debug mediawiki-debug))
    (setq mediawiki-debug t)

    (unwind-protect
        (let ((site (make-mediawiki-site-config
                     :name test-direct-login-site-name
                     :url "https://wiki.nichework.com/"
                     :api-url "https://wiki.nichework.com/w/api.php")))

          (mediawiki-add-site site)

          (let ((username "MarkAHershberger@emacs")
                (password (read-passwd "Enter bot password: ")))

            (message "Step 1: Getting login token...")

            (let ((token-response (mediawiki-api-call-sync
                                  test-direct-login-site-name "query"
                                  (list (cons "meta" "tokens")
                                        (cons "type" "login")))))

              (if (mediawiki-api-response-success token-response)
                  (let ((login-token (mediawiki-auth-extract-token token-response "login")))
                    (message "✓ Got login token: %s" login-token)

                    (message "Step 2: Performing login...")

                    (let ((login-params (list (cons "lgname" username)
                                             (cons "lgpassword" password)
                                             (cons "lgtoken" login-token))))

                      (message "Login parameters: %S" login-params)

                      (let ((login-response (mediawiki-api-call-sync
                                            test-direct-login-site-name "login" login-params)))

                        (message "Login response success: %s"
                                 (mediawiki-api-response-success login-response))
                        (message "Login response data: %S"
                                 (mediawiki-api-response-data login-response))

                        (if (mediawiki-api-response-success login-response)
                            (let* ((data (mediawiki-api-response-data login-response))
                                   (login-data (cdr (assq 'login data)))
                                   (result (cdr (assq 'result login-data))))
                              (message "Login result: %s" result)
                              (message "Full login data: %S" login-data))
                          (message "✗ API call failed: %s"
                                   (mediawiki-api-get-error-info login-response))))))

                (message "✗ Failed to get login token: %s"
                         (mediawiki-api-get-error-info token-response))))))

      ;; Cleanup
      (mediawiki-remove-session test-direct-login-site-name)
      (mediawiki-remove-site test-direct-login-site-name)
      (setq mediawiki-debug old-debug))))

(provide 'test-direct-login)

;;; test-direct-login.el ends here
