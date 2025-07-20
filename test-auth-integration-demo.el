;;; test-auth-integration-demo.el --- Demo of complete auth-source integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Demonstration of complete auth-source integration workflow

;;; Code:

(require 'ert)

;; Load modules
(load-file "mediawiki-core.el")

;; Mock API functions for complete flow
(defvar mock-login-call-count 0)

(defun mediawiki-api-call-sync (sitename action params)
  "Mock API call function that simulates login flow."
  (cond
   ;; Token request
   ((string= action "query")
    (make-mediawiki-api-response
     :success t
     :data '((query . ((tokens . ((logintoken . "mock-login-token"))))))
     :warnings nil
     :errors nil))
   
   ;; Login request
   ((string= action "login")
    (setq mock-login-call-count (1+ mock-login-call-count))
    (let ((username (cdr (assoc "lgname" params)))
          (password (cdr (assoc "lgpassword" params)))
          (token (cdr (assoc "lgtoken" params))))
      
      (if (and (string= username "demo-user")
               (string= password "demo-pass")
               (string= token "mock-login-token"))
          (make-mediawiki-api-response
           :success t
           :data '((login . ((result . "Success")
                           (lguserid . 123)
                           (lgusername . "demo-user"))))
           :warnings nil
           :errors nil)
        (make-mediawiki-api-response
         :success t
         :data '((login . ((result . "Failed"))))
         :warnings nil
         :errors nil))))
   
   ;; Default
   (t (make-mediawiki-api-response :success t :data nil))))

(defun mediawiki-api-get-error-info (response)
  "Mock error info function."
  "Mock error")

;; Load auth module
(load-file "mediawiki-auth.el")

;;; Demo Test

(ert-deftest test-complete-auth-source-integration ()
  "Test complete auth-source integration workflow."
  ;; Set up test site
  (let ((test-site (make-mediawiki-site
                    :name "demo-wiki"
                    :url "https://demo.example.com/wiki/"
                    :api-url "https://demo.example.com/wiki/api.php"
                    :username "demo-user"
                    :auth-method 'basic)))
    (mediawiki-add-site test-site))
  
  (unwind-protect
      (progn
        ;; Clear cache and reset counters
        (mediawiki-auth-clear-all-cached-credentials)
        (setq mock-login-call-count 0)
        
        ;; Mock auth-source to return demo credentials
        (cl-letf (((symbol-function 'auth-source-search)
                   (lambda (&rest spec)
                     (list (list :host "demo.example.com"
                               :user "demo-user"
                               :secret (lambda () "demo-pass"))))))
          
          ;; Test first login - should hit auth-source and cache credentials
          (let ((result1 (mediawiki-auth-basic-login "demo-wiki")))
            (should (= mock-login-call-count 1))
            
            ;; Verify session was created
            (let ((session (mediawiki-get-session "demo-wiki")))
              (should session)
              (should (mediawiki-session-login-time session)))
            
            ;; Test credential caching - get credentials again without login
            (let ((cached-creds (mediawiki-auth-get-credentials "demo-wiki")))
              (should cached-creds)
              (should (string= (plist-get cached-creds :username) "demo-user"))
              (should (string= (plist-get cached-creds :password) "demo-pass")))
            
            ;; Test logout clears cache
            (mediawiki-auth-logout "demo-wiki")
            (should-not (mediawiki-get-session "demo-wiki"))
            
            ;; Verify cache status
            (let ((status (mediawiki-auth-get-cache-status)))
              (should (>= (plist-get status :total-entries) 0))))))
    
    ;; Cleanup
    (setq mediawiki-site-alist
          (assoc-delete-all "demo-wiki" mediawiki-site-alist))
    (mediawiki-auth-clear-all-cached-credentials)))

(ert-deftest test-auth-source-security-features ()
  "Test security features of auth-source integration."
  ;; Set up test site
  (let ((test-site (make-mediawiki-site
                    :name "security-wiki"
                    :url "https://security.example.com/wiki/"
                    :api-url "https://security.example.com/wiki/api.php"
                    :username "security-user"
                    :auth-method 'basic)))
    (mediawiki-add-site test-site))
  
  (unwind-protect
      (progn
        ;; Clear cache
        (mediawiki-auth-clear-all-cached-credentials)
        
        ;; Test credential invalidation
        (let ((cache-key (mediawiki-auth-make-cache-key "security-wiki"))
              (test-creds '(:username "security-user" :password "secret-pass")))
          
          ;; Cache credentials
          (mediawiki-auth-cache-credentials cache-key test-creds)
          (should (mediawiki-auth-get-cached-credentials cache-key))
          
          ;; Test invalidation
          (mediawiki-auth-invalidate-credentials "security-wiki")
          (should-not (mediawiki-auth-get-cached-credentials cache-key))
          
          ;; Test cache cleanup
          (mediawiki-auth-force-cache-cleanup)
          (let ((status (mediawiki-auth-get-cache-status)))
            (should (numberp (plist-get status :total-entries))))))
    
    ;; Cleanup
    (setq mediawiki-site-alist
          (assoc-delete-all "security-wiki" mediawiki-site-alist))
    (mediawiki-auth-clear-all-cached-credentials)))

;; Run tests
(ert-run-tests-batch-and-exit)

;;; test-auth-integration-demo.el ends here