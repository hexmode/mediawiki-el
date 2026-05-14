;;; test-mediawiki-api-xml.el --- JSON format tests for mediawiki API -*- lexical-binding: t; -*-

;; Copyright (C) 2026 MediaWiki.el contributors

;;; Commentary:
;; These tests verify the JSON-format MediaWiki API response parsing
;; introduced in mediawiki-el 3.0.0 (issue #39).
;;
;; Tests mock HTTP at the url-http-post level and verify the full
;; parsing chain from raw JSON string to page data extraction.

;;; Code:
(require 'ert)
(require 'mediawiki-api)
(require 'mediawiki-auth)

;;; 1. Full HTTP mock: token response

(ert-deftest test-mw-api-call-token-response ()
  "Mock url-http-post returning token JSON, verify mediawiki-api-call structure.
JSON: {\"curtimestamp\":\"2024-01-01\",\"query\":{\"tokens\":{\"csrftoken\":\"def456+\\\\\",\"logintoken\":\"abc123+\\\\\"}}}
Parsed: ((curtimestamp . \"2024-01-01\") (query . ((tokens . ((csrftoken . \"def456+\\\\\") (logintoken . \"abc123+\\\\\"))))))"
  (cl-letf (((symbol-function 'url-http-post)
             (lambda (url args)
               "{\"curtimestamp\":\"2024-01-01\",\"query\":{\"tokens\":{\"csrftoken\":\"def456+\\\\\",\"logintoken\":\"abc123+\\\\\"}}}"))
            ((symbol-function 'mediawiki-make-api-url)
             (lambda (&optional site) "https://example.com/w/api.php"))
            ((symbol-function 'mediawiki-debug-line) #'ignore))
    (let ((result (mediawiki-api-call "TestSite" "query"
                    (list (cons "meta" "tokens") (cons "type" "csrf")))))
      (should (listp result))
      ;; curtimestamp is at top level in JSON
      (should (assq 'curtimestamp result))
      ;; query contains tokens
      (should (assq 'query result))
      (should (alist-get 'tokens (alist-get 'query result))))))

;;; 2. Token extraction via mediawiki-site-get-token

(ert-deftest test-mw-site-get-token ()
  "Mock mediawiki-api-call returning JSON token result, verify token string extracted.
mediawiki-site-get-token uses alist-get on query.tokens to extract token by type."
  (cl-letf (((symbol-function 'mediawiki-api-call)
             (lambda (site action args)
               ;; Return full JSON alist as api-call now returns
               '((curtimestamp . "2024-01-01")
                 (query . ((tokens . ((csrftoken . "def456+\\")
                                     (logintoken . "abc123+\\")))))))))
    (let ((token (mediawiki-site-get-token "TestSite" "csrf")))
      (should (stringp token))
      (should (string= "def456+\\" token)))))

;;; 3. Full HTTP mock: error response

(ert-deftest test-mw-api-call-error-response ()
  "Mock url-http-post returning error JSON, verify mediawiki-api-call signals error.
JSON: {\"error\":{\"code\":\"readapidenied\",\"info\":\"You need read permission.\"}}
mediawiki-api-call checks for 'error key and calls (error ...) with code/info."
  (cl-letf (((symbol-function 'url-http-post)
             (lambda (url args)
               "{\"error\":{\"code\":\"readapidenied\",\"info\":\"You need read permission.\"}}"))
            ((symbol-function 'mediawiki-make-api-url)
             (lambda (&optional site) "https://example.com/w/api.php"))
            ((symbol-function 'mediawiki-debug-line) #'ignore))
    (should-error (mediawiki-api-call "TestSite" "query" nil)
                  :type 'error)
    (condition-case err
        (mediawiki-api-call "TestSite" "query" nil)
      (error
       (should (string-match-p "readapidenied" (error-message-string err)))))))

;;; 4. Full HTTP mock: warning response (no error, just message)

(ert-deftest test-mw-api-call-warning-response ()
  "Mock url-http-post returning warning JSON, verify no error raised and query returned.
JSON: {\"warnings\":{\"revisions\":{\"warnings\":\"rvslots not specified\"}},
       \"query\":{\"pages\":{\"1\":{\"pageid\":1,\"ns\":0,\"title\":\"Test Page\"}}}}
mediawiki-api-call calls (message ...) for warnings, not (error ...)."
  (let (messages-logged)
    (cl-letf (((symbol-function 'url-http-post)
               (lambda (url args)
                 "{\"warnings\":{\"revisions\":{\"warnings\":\"rvslots not specified\"}},\"query\":{\"pages\":{\"1\":{\"pageid\":1,\"ns\":0,\"title\":\"Test Page\"}}}}"))
              ((symbol-function 'mediawiki-make-api-url)
               (lambda (&optional site) "https://example.com/w/api.php"))
              ((symbol-function 'mediawiki-debug-line) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages-logged))))
      ;; Should NOT error, just warn
      (let ((result (mediawiki-api-call "TestSite" "query" nil)))
        (should result)
        (should (assq 'query result))))))

;;; 5. Content extraction from JSON slots page structure

(ert-deftest test-mw-page-get-revision-content-slots ()
  "Test content, timestamp, and user extraction from the JSON slots structure."
  (let ((page '((pageid . 1) (ns . 0) (title . "Test Page")
                (revisions . (((revid . 100) (user . "Admin")
                                (timestamp . "2024-01-01T00:00:00Z")
                                (slots . ((main . ((content . "Hello World")))))))))))
    (should (string= "Hello World" (mediawiki-page-get-revision page 0 'content)))
    (should (string= "2024-01-01T00:00:00Z" (mediawiki-page-get-revision page 0 'timestamp)))
    (should (string= "Admin" (mediawiki-page-get-revision page 0 'user)))))

;;; 6. Title extraction from parsed page element

(ert-deftest test-mw-page-get-title-from-parsed ()
  "Test title extraction from a JSON page alist.
mediawiki-page-get-title uses (alist-get 'title page)."
  (let ((page '((pageid . 1) (ns . 0) (title . "Test Page"))))
    (should (string= "Test Page" (mediawiki-page-get-title page)))))

;;; 7. Page lookup in realistic JSON pagelist

(ert-deftest test-mw-pagelist-find-page ()
  "Test page lookup in a realistic JSON pagelist structure.
pagelist = ((curtimestamp . \"...\") (pages . ((\\1 . page) (\\2 . page) ...)))
pages is an alist of (id-symbol . page-alist) pairs."
  (let ((pagelist '((curtimestamp . "2024-01-01")
                    (pages . ((\1 . ((pageid . 1) (ns . 0) (title . "Main Page")))
                              (\2 . ((pageid . 2) (ns . 0) (title . "Test Page"))))))))
    ;; Find by exact title
    (let ((found (mediawiki-pagelist-find-page pagelist "Test Page")))
      (should found)
      (should (string= "Test Page" (mediawiki-page-get-title found))))
    ;; Find with underscore translation
    (let ((found (mediawiki-pagelist-find-page pagelist "Main_Page")))
      (should found)
      (should (string= "Main Page" (mediawiki-page-get-title found))))
    ;; Not found returns nil
    (should-not (mediawiki-pagelist-find-page pagelist "Nonexistent"))))

;;; 8. mediawiki-api-query-revisions returns correct pagelist structure

(ert-deftest test-mw-api-query-revisions ()
  "Mock mediawiki-api-call returning JSON query result, verify pagelist returned.
mediawiki-api-query-revisions returns ((curtimestamp . \"...\") (pages . (...)))."
  (cl-letf (((symbol-function 'mediawiki-api-call)
             (lambda (site action args)
               ;; Simulate full JSON result as returned by api-call
               '((curtimestamp . "2024-01-01")
                 (query . ((pages . ((\1 . ((pageid . 1) (ns . 0) (title . "Test Page")
                                            (revisions . (((revid . 100) (user . "Admin")
                                                            (timestamp . "2024-01-01T00:00:00Z")
                                                            (slots . ((main . ((content . "Hello World"))))))))))))))))))
    (let ((result (mediawiki-api-query-revisions "TestSite" "Test Page" '("content"))))
      (should (listp result))
      ;; curtimestamp at top level
      (should (assq 'curtimestamp result))
      ;; pages present
      (should (assq 'pages result)))))

;;; 9. Full chain: HTTP mock → query-revisions → pagelist-find-page → content

(ert-deftest test-mw-full-chain-get-page ()
  "End-to-end test: HTTP mock → mediawiki-api-query-revisions → content extraction.
JSON: {\"curtimestamp\":\"2024-01-01T00:00:01Z\",\"query\":{\"pages\":{\"1\":{
  \"pageid\":1,\"ns\":0,\"title\":\"Test Page\",\"revisions\":[{
    \"revid\":100,\"user\":\"Admin\",\"timestamp\":\"2024-01-01T00:00:00Z\",
    \"slots\":{\"main\":{\"contentmodel\":\"wikitext\",\"content\":\"Wiki page content here\"}}}]}}}}"
  (cl-letf (((symbol-function 'url-http-post)
             (lambda (url args)
               "{\"curtimestamp\":\"2024-01-01T00:00:01Z\",\"query\":{\"pages\":{\"1\":{\"pageid\":1,\"ns\":0,\"title\":\"Test Page\",\"revisions\":[{\"revid\":100,\"user\":\"Admin\",\"timestamp\":\"2024-01-01T00:00:00Z\",\"slots\":{\"main\":{\"contentmodel\":\"wikitext\",\"content\":\"Wiki page content here\"}}}]}}}}"))
            ((symbol-function 'mediawiki-make-api-url)
             (lambda (&optional site) "https://example.com/w/api.php"))
            ((symbol-function 'mediawiki-debug-line) #'ignore))
    (let* ((pagelist (mediawiki-api-query-revisions "TestSite" "Test Page" '("content")))
           (page (mediawiki-pagelist-find-page pagelist "Test Page")))
      (should page)
      (should (string= "Test Page" (mediawiki-page-get-title page)))
      (should (string= "Wiki page content here"
                       (mediawiki-page-get-revision page 0 'content))))))

;;; 10. Login success via mocked api-call

(ert-deftest test-mw-login-success ()
  "Mock mediawiki-api-call returning JSON login success, verify mediawiki-do-login returns sitename.
mediawiki-do-login uses (alist-get 'login (api-call ...)) as result alist."
  (cl-letf (((symbol-function 'mediawiki-site-username) (lambda (site) "testuser"))
            ((symbol-function 'mediawiki-site-password) (lambda (site) "testpass"))
            ((symbol-function 'mediawiki-site-domain)   (lambda (site) nil))
            ((symbol-function 'mediawiki-site-get-token) (lambda (site type) "logintoken+\\"))
            ((symbol-function 'mediawiki-api-call)
             (lambda (site action args)
               ;; Return full JSON alist with login key
               '((login . ((result . "Success") (lguserid . 1) (lgusername . "testuser")))))))
    (let ((result (mediawiki-do-login "TestSite" "testuser" "testpass")))
      (should (string= "TestSite" result)))))

;;; 11. Login failure via mocked api-call

(ert-deftest test-mw-login-failed ()
  "Mock mediawiki-api-call returning JSON login failure, verify mediawiki-do-login signals error.
Error message must contain the 'reason' field from the login response."
  (cl-letf (((symbol-function 'mediawiki-site-username) (lambda (site) "testuser"))
            ((symbol-function 'mediawiki-site-password) (lambda (site) "wrongpass"))
            ((symbol-function 'mediawiki-site-domain)   (lambda (site) nil))
            ((symbol-function 'mediawiki-site-get-token) (lambda (site type) "logintoken+\\"))
            ((symbol-function 'mediawiki-api-call)
             (lambda (site action args)
               '((login . ((result . "Failed") (reason . "Wrong password")))))))
    (should-error (mediawiki-do-login "TestSite" "testuser" "wrongpass"))
    (condition-case err
        (mediawiki-do-login "TestSite" "testuser" "wrongpass")
      (error (should (string-match-p "Wrong password" (error-message-string err)))))))

(provide 'test-mediawiki-api-xml)

;;; test-mediawiki-api-xml.el ends here
