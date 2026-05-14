;;; test-mediawiki-api-xml.el --- XML format tests for mediawiki API -*- lexical-binding: t; -*-

;; Copyright (C) 2026 MediaWiki.el contributors

;;; Commentary:
;; Tests for the XML-format MediaWiki API response parsing.
;; These tests mock HTTP at the url-http-post level and verify the full
;; parsing chain from raw XML string to page data extraction.
;;
;; NOTE: These tests will need to be updated when the XML→JSON migration
;; (issue #39) is performed. They serve as the regression harness for
;; that migration.

;;; Code:
(require 'ert)
(require 'mediawiki-api)
(require 'mediawiki-auth)

;;; 1. Full HTTP mock: token response

(ert-deftest test-mw-api-call-token-response ()
  "Mock url-http-post returning token XML, verify mediawiki-api-call structure.
XML: <api curtimestamp='2024-01-01'><query><tokens csrftoken='def456+\\' logintoken='abc123+\\'/></query></api>
Parsed: (api ((curtimestamp . \"2024-01-01\"))
           (query nil (tokens ((csrftoken . \"def456+\\\\\") (logintoken . \"abc123+\\\\\")))))
After api-call processing curtimestamp is pushed into query's attrs."
  (cl-letf (((symbol-function 'url-http-post)
             (lambda (url args)
               "<api curtimestamp='2024-01-01'><query><tokens csrftoken='def456+\\' logintoken='abc123+\\'/></query></api>"))
            ((symbol-function 'mediawiki-make-api-url)
             (lambda (&optional site) "https://example.com/w/api.php"))
            ((symbol-function 'mediawiki-debug-line) #'ignore))
    (let ((result (mediawiki-api-call "TestSite" "query"
                    (list (cons "meta" "tokens") (cons "type" "csrf")))))
      (should (listp result))
      (should (eq 'query (car result)))
      ;; curtimestamp was pushed into query's attrs
      (should (assq 'curtimestamp (cadr result)))
      ;; tokens element is accessible in cddr
      (should (assq 'tokens (cddr result))))))

;;; 2. Token extraction via mediawiki-site-get-token

(ert-deftest test-mw-site-get-token ()
  "Mock mediawiki-api-call returning token result, verify token string extracted.
mediawiki-site-get-token uses (cdr (caadar (cddr result))) to extract the
first attribute value of the tokens element."
  (cl-letf (((symbol-function 'mediawiki-api-call)
             (lambda (site action args)
               ;; Return what api-call returns after processing token XML
               '(query ((curtimestamp . "2024-01-01"))
                       (tokens ((csrftoken . "def456+\\") (logintoken . "abc123+\\")))))))
    (let ((token (mediawiki-site-get-token "TestSite" "csrf")))
      (should (stringp token))
      (should (string= "def456+\\" token)))))

;;; 3. Full HTTP mock: error response

(ert-deftest test-mw-api-call-error-response ()
  "Mock url-http-post returning error XML, verify mediawiki-api-call signals error.
XML: <api><error code='readapidenied' info='You need read permission.'/>
Parsed: (api nil (error ((code . \"readapidenied\") (info . \"You need read permission.\"))))
mediawiki-raise walks the error attrs alist and calls (error ...) with label/info."
  (cl-letf (((symbol-function 'url-http-post)
             (lambda (url args)
               "<api><error code='readapidenied' info='You need read permission.'/></api>"))
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
  "Mock url-http-post returning warning XML, verify no error is raised and query returned.
XML: <api><warnings><revisions xml:space='preserve'>rvslots not specified</revisions></warnings>
       <query><pages><page pageid='1' ns='0' title='Test Page'/></pages></query></api>
Parsed: (api nil
           (warnings nil (revisions ((xml:space . \"preserve\")) \"rvslots not specified\"))
           (query nil (pages nil (page ((pageid . \"1\") ...))))))
mediawiki-raise calls (message ...) for warnings, not (error ...)."
  (let (messages-logged)
    (cl-letf (((symbol-function 'url-http-post)
               (lambda (url args)
                 "<api><warnings><revisions xml:space='preserve'>rvslots not specified</revisions></warnings><query><pages><page pageid='1' ns='0' title='Test Page'/></pages></query></api>"))
              ((symbol-function 'mediawiki-make-api-url)
               (lambda (&optional site) "https://example.com/w/api.php"))
              ((symbol-function 'mediawiki-debug-line) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages-logged))))
      ;; Should NOT error, just warn
      (let ((result (mediawiki-api-call "TestSite" "query" nil)))
        (should result)
        (should (eq 'query (car result)))))))

;;; 5. Content extraction from rvslots-style page structure

(ert-deftest test-mw-page-get-revision-content-slots ()
  "Test content, timestamp, and user extraction from the rvslots XML structure.
Verified parse structure for:
  <page pageid='1' ns='0' title='Test Page'>
    <revisions><rev revid='100' user='Admin' timestamp='2024-01-01T00:00:00Z'>
      <slots><slot role='main'>Hello World</slot></slots></rev></revisions></page>"
  (let ((page '(page ((pageid . "1") (ns . "0") (title . "Test Page"))
                 (revisions nil
                   (rev ((revid . "100") (user . "Admin") (timestamp . "2024-01-01T00:00:00Z"))
                     (slots nil
                       (slot ((role . "main"))
                         "Hello World")))))))
    (should (string= "Hello World" (mediawiki-page-get-revision page 0 'content)))
    (should (string= "2024-01-01T00:00:00Z" (mediawiki-page-get-revision page 0 'timestamp)))
    (should (string= "Admin" (mediawiki-page-get-revision page 0 'user)))))

;;; 6. Title extraction from parsed page element

(ert-deftest test-mw-page-get-title-from-parsed ()
  "Test title extraction from a parsed page element.
mediawiki-page-get-title uses (cdr (assq 'title (cadr page)))."
  (let ((page '(page ((pageid . "1") (ns . "0") (title . "Test Page"))
                 (revisions nil))))
    (should (string= "Test Page" (mediawiki-page-get-title page)))))

;;; 7. Page lookup in realistic pagelist

(ert-deftest test-mw-pagelist-find-page ()
  "Test page lookup in a realistic pagelist structure.
pagelist = ((curtimestamp . \"...\") (pages nil (page ...) (page ...)))
pages must have nil as attrs slot so cddr gives all children."
  (let ((pagelist '((curtimestamp . "2024-01-01")
                    (pages nil
                      (page ((pageid . "1") (ns . "0") (title . "Main Page"))
                        (revisions nil))
                      (page ((pageid . "2") (ns . "0") (title . "Test Page"))
                        (revisions nil))))))
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
  "Mock mediawiki-api-call returning query result, verify pagelist returned.
mediawiki-api-query-revisions pushes curtimestamp into (cddr qresult) then
returns (cddr qresult), giving ((curtimestamp . \"...\") (pages nil ...))."
  (cl-letf (((symbol-function 'mediawiki-api-call)
             (lambda (site action args)
               ;; Simulate what api-call returns after processing revision XML
               '(query ((curtimestamp . "2024-01-01"))
                       (pages nil
                         (page ((pageid . "1") (ns . "0") (title . "Test Page"))
                           (revisions nil
                             (rev ((revid . "100") (user . "Admin")
                                   (timestamp . "2024-01-01T00:00:00Z"))
                               (slots nil
                                 (slot ((role . "main"))
                                   "Hello World"))))))))))
    (let ((result (mediawiki-api-query-revisions "TestSite" "Test Page" '("content"))))
      (should (listp result))
      ;; curtimestamp pushed into the top-level alist
      (should (assq 'curtimestamp result))
      ;; pages present
      (should (assq 'pages result)))))

;;; 9. Full chain: HTTP mock → query-revisions → pagelist-find-page → content

(ert-deftest test-mw-full-chain-get-page ()
  "End-to-end test: HTTP mock → mediawiki-api-query-revisions → content extraction.
XML: <api curtimestamp='2024-01-01T00:00:01Z'><query><pages>
       <page pageid='1' ns='0' title='Test Page'>
         <revisions><rev revid='100' user='Admin' timestamp='2024-01-01T00:00:00Z'>
           <slots><slot role='main'>Wiki page content here</slot></slots>
         </rev></revisions></page></pages></query></api>"
  (cl-letf (((symbol-function 'url-http-post)
             (lambda (url args)
               "<api curtimestamp='2024-01-01T00:00:01Z'><query><pages><page pageid='1' ns='0' title='Test Page'><revisions><rev revid='100' user='Admin' timestamp='2024-01-01T00:00:00Z'><slots><slot role='main'>Wiki page content here</slot></slots></rev></revisions></page></pages></query></api>"))
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
  "Mock mediawiki-api-call returning login success, verify mediawiki-do-login returns sitename.
mediawiki-do-login uses (cadr (api-call ...)) as result alist, checks (result . \"Success\")."
  (cl-letf (((symbol-function 'mediawiki-site-username) (lambda (site) "testuser"))
            ((symbol-function 'mediawiki-site-password) (lambda (site) "testpass"))
            ((symbol-function 'mediawiki-site-domain)   (lambda (site) nil))
            ((symbol-function 'mediawiki-site-get-token) (lambda (site type) "logintoken+\\"))
            ((symbol-function 'mediawiki-api-call)
             (lambda (site action args)
               ;; Return what api-call returns for login success
               '(login ((result . "Success") (lguserid . "1") (lgusername . "testuser"))))))
    (let ((result (mediawiki-do-login "TestSite" "testuser" "testpass")))
      (should (string= "TestSite" result)))))

;;; 11. Login failure via mocked api-call

(ert-deftest test-mw-login-failed ()
  "Mock mediawiki-api-call returning login failure, verify mediawiki-do-login signals error.
Error message must contain the 'reason' field from the login response."
  (cl-letf (((symbol-function 'mediawiki-site-username) (lambda (site) "testuser"))
            ((symbol-function 'mediawiki-site-password) (lambda (site) "wrongpass"))
            ((symbol-function 'mediawiki-site-domain)   (lambda (site) nil))
            ((symbol-function 'mediawiki-site-get-token) (lambda (site type) "logintoken+\\"))
            ((symbol-function 'mediawiki-api-call)
             (lambda (site action args)
               '(login ((result . "Failed") (reason . "Wrong password"))))))
    (should-error (mediawiki-do-login "TestSite" "testuser" "wrongpass"))
    (condition-case err
        (mediawiki-do-login "TestSite" "testuser" "wrongpass")
      (error (should (string-match-p "Wrong password" (error-message-string err)))))))

(provide 'test-mediawiki-api-xml)

;;; test-mediawiki-api-xml.el ends here
