;;; test-mediawiki-oauth.el --- Unit tests for mediawiki-oauth.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el contributors

;; This file is part of mediawiki.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for mediawiki-oauth.el module.
;; These tests mock network interactions to avoid external dependencies.

;;; Code:

(require 'ert)
(require 'mediawiki-oauth)

;;; Test Fixtures

(defmacro test-mediawiki-oauth-with-site (site-name &rest body)
  "Bind a test site configuration and evaluate BODY."
  `(let ((mediawiki-site-alist
          (list (append (list ,site-name
                              "https://test.example.org/w/"
                              "testuser"
                              "testpass"
                              "")
                        (list :description "Test Site")))))
     ,@body))

;;; Test OAuth Configuration Detection

(ert-deftest test-mediawiki-oauth-configured-p-no-oauth ()
  "Test that OAuth is not configured when no OAuth properties are set."
  (test-mediawiki-oauth-with-site "TestSite"
    (should-not (mediawiki-oauth-configured-p "TestSite"))))

(ert-deftest test-mediawiki-oauth-configured-p-with-access-token ()
  "Test that OAuth is configured when an access token is present."
  (test-mediawiki-oauth-with-site "TestSite"
    (let ((site (assoc "TestSite" mediawiki-site-alist)))
      (plist-put (nthcdr 5 site) :oauth-access-token "my-token"))
    (should (mediawiki-oauth-configured-p "TestSite"))))

(ert-deftest test-mediawiki-oauth-configured-p-with-client-credentials ()
  "Test that OAuth is configured when client ID and secret are present."
  (test-mediawiki-oauth-with-site "TestSite"
    (let ((site (assoc "TestSite" mediawiki-site-alist)))
      (plist-put (nthcdr 5 site) :oauth-client-id "my-client-id")
      (plist-put (nthcdr 5 site) :oauth-client-secret "my-secret"))
    (should (mediawiki-oauth-configured-p "TestSite"))))

(ert-deftest test-mediawiki-oauth-configured-p-client-id-only ()
  "Test that OAuth is NOT configured with only client ID."
  (test-mediawiki-oauth-with-site "TestSite"
    (let ((site (assoc "TestSite" mediawiki-site-alist)))
      (plist-put (nthcdr 5 site) :oauth-client-id "my-client-id"))
    (should-not (mediawiki-oauth-configured-p "TestSite"))))

;;; Test Token Endpoint Derivation

(ert-deftest test-mediawiki-oauth-token-endpoint-w-path ()
  "Test token endpoint derivation for /w/ path."
  (test-mediawiki-oauth-with-site "TestSite"
    (should (string= "https://test.example.org/w/rest.php/oauth2/access_token"
                     (mediawiki-oauth-token-endpoint "TestSite")))))

(ert-deftest test-mediawiki-oauth-token-endpoint-wiki-path ()
  "Test token endpoint derivation for /wiki/ path."
  (let ((mediawiki-site-alist
         (list (append (list "WikiSite"
                             "https://wiki.example.org/wiki/"
                             "user" "pass" "")
                       '(:description "Wiki Site")))))
    (should (string= "https://wiki.example.org/wiki/rest.php/oauth2/access_token"
                     (mediawiki-oauth-token-endpoint "WikiSite")))))

(ert-deftest test-mediawiki-oauth-token-endpoint-custom ()
  "Test custom token endpoint override."
  (test-mediawiki-oauth-with-site "TestSite"
    (let ((site (assoc "TestSite" mediawiki-site-alist)))
      (plist-put (nthcdr 5 site) :oauth-token-url "https://custom.example/token"))
    (should (string= "https://custom.example/token"
                     (mediawiki-oauth-token-endpoint "TestSite")))))

;;; Test Auth Header Creation

(ert-deftest test-mediawiki-oauth-make-auth-header ()
  "Test Authorization header creation."
  (let ((header (mediawiki-oauth-make-auth-header "test-token-123")))
    (should (consp header))
    (should (string= "Authorization" (car header)))
    (should (string= "Bearer test-token-123" (cdr header)))))

;;; Test Token Storage and Retrieval

(ert-deftest test-mediawiki-oauth-access-token-storage ()
  "Test storing and retrieving an access token."
  (test-mediawiki-oauth-with-site "TestSite"
    (mediawiki-oauth-set-tokens "TestSite" "access123" "refresh456" 3600)
    (should (string= "access123" (mediawiki-oauth-access-token "TestSite")))
    (should (string= "refresh456" (mediawiki-oauth-refresh-token "TestSite")))))

(ert-deftest test-mediawiki-oauth-access-token-expired ()
  "Test that expired access tokens return nil."
  (test-mediawiki-oauth-with-site "TestSite"
    ;; Token expired 1 second ago
    (mediawiki-oauth-set-tokens "TestSite" "expired-token" nil -1)
    (should-not (mediawiki-oauth-access-token "TestSite"))))

(ert-deftest test-mediawiki-oauth-access-token-valid ()
  "Test that valid access tokens are returned."
  (test-mediawiki-oauth-with-site "TestSite"
    ;; Token valid for 1 hour
    (mediawiki-oauth-set-tokens "TestSite" "valid-token" nil 3600)
    (should (string= "valid-token" (mediawiki-oauth-access-token "TestSite")))))

(ert-deftest test-mediawiki-oauth-access-token-no-expiry ()
  "Test access token with no expiry is always valid."
  (test-mediawiki-oauth-with-site "TestSite"
    (mediawiki-oauth-set-tokens "TestSite" "no-expiry-token")
    (should (string= "no-expiry-token" (mediawiki-oauth-access-token "TestSite")))))

;;; Test Token Retrieval (Client Credentials Mock)

(ert-deftest test-mediawiki-oauth-request-access-token-mock ()
  "Test client credentials token request with mocked HTTP."
  (test-mediawiki-oauth-with-site "TestSite"
    (let ((site (assoc "TestSite" mediawiki-site-alist)))
      (plist-put (nthcdr 5 site) :oauth-client-id "client-id")
      (plist-put (nthcdr 5 site) :oauth-client-secret "client-secret"))

    ;; Mock url-retrieve-synchronously to return a mock buffer
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url)
                 (let ((buf (generate-new-buffer "*mock-oauth*")))
                   (with-current-buffer buf
                     (setq url-http-end-of-headers (point-min))
                     (insert "\n")
                     (insert "{\"access_token\": \"mock-token-123\", \"expires_in\": 7200}"))
                   buf)))
              ((symbol-function 'mediawiki-oauth-retrieve-body)
               (lambda (buf)
                 (with-current-buffer buf
                   (buffer-substring-no-properties (point-min) (point-max))))))

      (let ((token (mediawiki-oauth-request-access-token "TestSite")))
        (should (string= "mock-token-123" token))
        ;; Verify token was stored
        (should (string= "mock-token-123" (mediawiki-oauth-access-token "TestSite")))))))

(ert-deftest test-mediawiki-oauth-request-access-token-error-mock ()
  "Test that token request errors are properly reported."
  (test-mediawiki-oauth-with-site "TestSite"
    (let ((site (assoc "TestSite" mediawiki-site-alist)))
      (plist-put (nthcdr 5 site) :oauth-client-id "client-id")
      (plist-put (nthcdr 5 site) :oauth-client-secret "client-secret"))

    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url)
                 (let ((buf (generate-new-buffer "*mock-oauth-error*")))
                   (with-current-buffer buf
                     (setq url-http-end-of-headers (point-min))
                     (insert "\n")
                     (insert "{\"error\": \"invalid_client\", \"error_description\": \"Bad credentials\"}"))
                   buf)))
              ((symbol-function 'mediawiki-oauth-retrieve-body)
               (lambda (buf)
                 (with-current-buffer buf
                   (buffer-substring-no-properties (point-min) (point-max))))))

      (should-error (mediawiki-oauth-request-access-token "TestSite")
                    :type 'error))))

;;; Test Token Refresh (Mock)

(ert-deftest test-mediawiki-oauth-refresh-access-token-mock ()
  "Test token refresh with mocked HTTP."
  (test-mediawiki-oauth-with-site "TestSite"
    (let ((site (assoc "TestSite" mediawiki-site-alist)))
      (plist-put (nthcdr 5 site) :oauth-client-id "client-id")
      (plist-put (nthcdr 5 site) :oauth-client-secret "client-secret")
      (plist-put (nthcdr 5 site) :oauth-refresh-token "old-refresh-token"))

    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url)
                 (let ((buf (generate-new-buffer "*mock-oauth-refresh*")))
                   (with-current-buffer buf
                     (setq url-http-end-of-headers (point-min))
                     (insert "\n")
                     (insert "{\"access_token\": \"new-access-token\", \"refresh_token\": \"new-refresh-token\", \"expires_in\": 3600}"))
                   buf)))
              ((symbol-function 'mediawiki-oauth-retrieve-body)
               (lambda (buf)
                 (with-current-buffer buf
                   (buffer-substring-no-properties (point-min) (point-max))))))

      (let ((token (mediawiki-oauth-refresh-access-token "TestSite")))
        (should (string= "new-access-token" token))
        ;; Verify both tokens were updated
        (should (string= "new-access-token" (mediawiki-oauth-access-token "TestSite")))
        (should (string= "new-refresh-token" (mediawiki-oauth-refresh-token "TestSite")))))))

;;; Test Get Access Token (Integration of storage + request)

(ert-deftest test-mediawiki-oauth-get-access-token-existing ()
  "Test that get-access-token returns existing valid token."
  (test-mediawiki-oauth-with-site "TestSite"
    (mediawiki-oauth-set-tokens "TestSite" "existing-token" nil 3600)
    ;; Should return existing token without making network request
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url) (error "Should not be called"))))
      (should (string= "existing-token" (mediawiki-oauth-get-access-token "TestSite"))))))

(ert-deftest test-mediawiki-oauth-get-access-token-expired-with-refresh ()
  "Test that get-access-token refreshes expired token."
  (test-mediawiki-oauth-with-site "TestSite"
    (let ((site (assoc "TestSite" mediawiki-site-alist)))
      (plist-put (nthcdr 5 site) :oauth-client-id "client-id")
      (plist-put (nthcdr 5 site) :oauth-client-secret "client-secret"))
    ;; Set expired token with refresh token
    (mediawiki-oauth-set-tokens "TestSite" "expired-token" "refresh-123" -1)

    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url)
                 (let ((buf (generate-new-buffer "*mock-refresh*")))
                   (with-current-buffer buf
                     (setq url-http-end-of-headers (point-min))
                     (insert "\n")
                     (insert "{\"access_token\": \"refreshed-token\", \"expires_in\": 3600}"))
                   buf)))
              ((symbol-function 'mediawiki-oauth-retrieve-body)
               (lambda (buf)
                 (with-current-buffer buf
                   (buffer-substring-no-properties (point-min) (point-max))))))

      (should (string= "refreshed-token" (mediawiki-oauth-get-access-token "TestSite"))))))

(ert-deftest test-mediawiki-oauth-get-access-token-request-new ()
  "Test that get-access-token requests new token when none exists."
  (test-mediawiki-oauth-with-site "TestSite"
    (let ((site (assoc "TestSite" mediawiki-site-alist)))
      (plist-put (nthcdr 5 site) :oauth-client-id "client-id")
      (plist-put (nthcdr 5 site) :oauth-client-secret "client-secret"))

    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url)
                 (let ((buf (generate-new-buffer "*mock-new*")))
                   (with-current-buffer buf
                     (setq url-http-end-of-headers (point-min))
                     (insert "\n")
                     (insert "{\"access_token\": \"brand-new-token\", \"expires_in\": 7200}"))
                   buf)))
              ((symbol-function 'mediawiki-oauth-retrieve-body)
               (lambda (buf)
                 (with-current-buffer buf
                   (buffer-substring-no-properties (point-min) (point-max))))))

      (should (string= "brand-new-token" (mediawiki-oauth-get-access-token "TestSite"))))))

;;; Test OAuth Setup Interactive Function

(ert-deftest test-mediawiki-oauth-setup-site ()
  "Test configuring OAuth for a site."
  (test-mediawiki-oauth-with-site "TestSite"
    (mediawiki-oauth-setup-site "TestSite" "my-id" "my-secret" "my-token")
    (should (string= "my-id" (mediawiki-oauth-client-id "TestSite")))
    (should (string= "my-secret" (mediawiki-oauth-client-secret "TestSite")))
    (should (string= "my-token" (mediawiki-oauth-access-token "TestSite")))))

(ert-deftest test-mediawiki-oauth-setup-site-no-token ()
  "Test configuring OAuth without pre-obtained token."
  (test-mediawiki-oauth-with-site "TestSite"
    (mediawiki-oauth-setup-site "TestSite" "my-id" "my-secret")
    (should (string= "my-id" (mediawiki-oauth-client-id "TestSite")))
    (should (string= "my-secret" (mediawiki-oauth-client-secret "TestSite")))
    (should-not (mediawiki-oauth-access-token "TestSite"))))

(ert-deftest test-mediawiki-oauth-clear-tokens ()
  "Test clearing OAuth tokens."
  (test-mediawiki-oauth-with-site "TestSite"
    (mediawiki-oauth-setup-site "TestSite" "my-id" "my-secret" "my-token")
    (mediawiki-oauth-clear-tokens "TestSite")
    (should-not (mediawiki-oauth-access-token "TestSite"))
    (should-not (mediawiki-oauth-refresh-token "TestSite"))
    ;; Client ID and secret should remain
    (should (string= "my-id" (mediawiki-oauth-client-id "TestSite")))))

;;; Test Client ID / Secret Accessors

(ert-deftest test-mediawiki-oauth-client-id ()
  "Test client ID accessor."
  (test-mediawiki-oauth-with-site "TestSite"
    (let ((site (assoc "TestSite" mediawiki-site-alist)))
      (plist-put (nthcdr 5 site) :oauth-client-id "test-client-id"))
    (should (string= "test-client-id" (mediawiki-oauth-client-id "TestSite")))))

(ert-deftest test-mediawiki-oauth-client-secret ()
  "Test client secret accessor."
  (test-mediawiki-oauth-with-site "TestSite"
    (let ((site (assoc "TestSite" mediawiki-site-alist)))
      (plist-put (nthcdr 5 site) :oauth-client-secret "test-secret"))
    (should (string= "test-secret" (mediawiki-oauth-client-secret "TestSite")))))

;;; Test Retrieve Body Helper

(ert-deftest test-mediawiki-oauth-retrieve-body ()
  "Test response body extraction from buffer."
  (let ((buf (generate-new-buffer "*test-body*")))
    (with-current-buffer buf
      (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n")
      (setq url-http-end-of-headers (point))
      (insert "\r\n{\"access_token\": \"abc123\"}"))
    (let ((body (mediawiki-oauth-retrieve-body buf)))
      (should (string= "{\"access_token\": \"abc123\"}" body)))
    (kill-buffer buf)))

(ert-deftest test-mediawiki-oauth-retrieve-body-no-headers ()
  "Test error when no headers found in buffer."
  (let ((buf (generate-new-buffer "*test-no-headers*")))
    (with-current-buffer buf
      (setq url-http-end-of-headers nil)
      (insert "{\"error\": \"test\"}"))
    (should-error (mediawiki-oauth-retrieve-body buf) :type 'error)
    (kill-buffer buf)))

;;; Test Non-existent Site

(ert-deftest test-mediawiki-oauth-set-tokens-missing-site ()
  "Test error when setting tokens for non-existent site."
  (let ((mediawiki-site-alist nil))
    (should-error (mediawiki-oauth-set-tokens "MissingSite" "token")
                  :type 'error)))

(ert-deftest test-mediawiki-oauth-setup-site-missing-site ()
  "Test error when configuring OAuth for non-existent site."
  (let ((mediawiki-site-alist nil))
    (should-error (mediawiki-oauth-setup-site "MissingSite" "id" "secret")
                  :type 'error)))

;;; Test that setup-site mutates mediawiki-site-alist in place

(ert-deftest test-mediawiki-oauth-setup-site-mutates-alist ()
  "Test that mediawiki-oauth-setup-site actually mutates mediawiki-site-alist.
This guards against a bug where plist-put on (nthcdr 5 site) returns
a new list that is discarded, leaving the site entry unchanged."
  (let ((mediawiki-site-alist
         (list (list "TestSite"
                     "https://test.example.org/w/"
                     "" "" ""
                     :description "Test Site"))))
    ;; Sanity: no OAuth properties before
    (should-not (mediawiki-oauth-configured-p "TestSite"))
    (mediawiki-oauth-setup-site "TestSite" "my-id" "my-secret" "my-token")
    ;; Verify properties are now in the actual alist
    (should (mediawiki-oauth-configured-p "TestSite"))
    (let ((site (assoc "TestSite" mediawiki-site-alist)))
      (should (equal
               (plist-get (nthcdr 5 site) :oauth-client-id)
               "my-id"))
      (should (equal
               (plist-get (nthcdr 5 site) :oauth-client-secret)
               "my-secret"))
      (should (equal
               (plist-get (nthcdr 5 site) :oauth-access-token)
               "my-token")))))

(ert-deftest test-mediawiki-oauth-setup-site-mutates-legacy-entry ()
  "Test setup-site works on legacy site entries without an existing plist."
  (let ((mediawiki-site-alist
         (list (list "TestSite"
                     "https://test.example.org/w/"
                     "testuser" "testpass" ""
                     "Main Page"))))
    ;; Sanity: no plist, just a positional first-page
    (mediawiki-oauth-setup-site "TestSite" "my-id" "my-secret" "my-token")
    ;; Verify the plist was created and first-page preserved
    (let ((site (assoc "TestSite" mediawiki-site-alist)))
      (should (keywordp (nth 5 site)))
      (should (equal (plist-get (nthcdr 5 site) :first-page) "Main Page"))
      (should (equal (plist-get (nthcdr 5 site) :oauth-client-id) "my-id"))
      (should (equal (plist-get (nthcdr 5 site) :oauth-access-token) "my-token")))))

;;; User name discovery tests

(ert-deftest test-mediawiki-oauth-userinfo-name ()
  "The wiki's userinfo names the OAuth user, and the answer is cached."
  (test-mediawiki-oauth-with-site "TestSite"
    (mediawiki-oauth-forget-userinfo)
    (let ((calls 0))
      (cl-letf (((symbol-function 'mediawiki-api-call)
                 (lambda (&rest _)
                   (setq calls (1+ calls))
                   '((query . ((userinfo . ((name . "Alice")))))))))
        (should (equal (mediawiki-oauth-userinfo "TestSite") "Alice"))
        (should (equal (mediawiki-oauth-userinfo "TestSite") "Alice"))
        (should (= 1 calls))))))

(ert-deftest test-mediawiki-oauth-userinfo-anonymous ()
  "An anonymous token yields nil, not a name."
  (test-mediawiki-oauth-with-site "TestSite"
    (mediawiki-oauth-forget-userinfo)
    (cl-letf (((symbol-function 'mediawiki-api-call)
               (lambda (&rest _)
                 '((query . ((userinfo . ((anon . t)))))))))
      (should-not (mediawiki-oauth-userinfo "TestSite")))))

(ert-deftest test-mediawiki-oauth-userinfo-error ()
  "A failed request yields nil instead of signalling."
  (test-mediawiki-oauth-with-site "TestSite"
    (mediawiki-oauth-forget-userinfo)
    (cl-letf (((symbol-function 'mediawiki-api-call)
               (lambda (&rest _) (error "network down"))))
      (should-not (mediawiki-oauth-userinfo "TestSite")))))

(ert-deftest test-mediawiki-oauth-userinfo-failure-is-not-cached ()
  "A failed lookup does not poison the cache once things are fixed."
  (test-mediawiki-oauth-with-site "TestSite"
    (mediawiki-oauth-forget-userinfo)
    (cl-letf (((symbol-function 'mediawiki-api-call)
               (lambda (&rest _) (error "network down"))))
      (should-not (mediawiki-oauth-userinfo "TestSite")))
    (cl-letf (((symbol-function 'mediawiki-api-call)
               (lambda (&rest _)
                 '((query . ((userinfo . ((name . "Alice")))))))))
      (should (equal (mediawiki-oauth-userinfo "TestSite") "Alice")))))

(ert-deftest test-mediawiki-oauth-setup-forgets-userinfo ()
  "Configuring OAuth clears any earlier answer for the site."
  (test-mediawiki-oauth-with-site "TestSite"
    (mediawiki-oauth-forget-userinfo)
    (puthash "TestSite" 'anonymous mediawiki-oauth--username-cache)
    (cl-letf (((symbol-function 'mediawiki-api-call)
               (lambda (&rest _)
                 '((query . ((userinfo . ((name . "Alice")))))))))
      (mediawiki-oauth-setup-site "TestSite" "cid" "secret")
      (should (equal (mediawiki-oauth-userinfo "TestSite") "Alice")))))

(ert-deftest test-mediawiki-oauth-site-username-uses-userinfo ()
  "An OAuth site with no configured user asks the wiki, not auth-source."
  (let ((mediawiki-site-alist
         (list (list "TestSite" "https://test.example.org/w/" "" "" ""
                     :description "Test Site"))))
    (mediawiki-oauth-forget-userinfo)
    (mediawiki-oauth-setup-site "TestSite" "cid" "secret")
    (cl-letf (((symbol-function 'mediawiki-api-call)
               (lambda (&rest _) '((query . ((userinfo . ((name . "Alice"))))))))
              ((symbol-function 'url-user-for-url)
               (lambda (&rest _) "cid")))
      (should (equal (mediawiki-site-username "TestSite") "Alice")))))

(ert-deftest test-mediawiki-oauth-site-username-without-oauth ()
  "Without OAuth the old auth-source lookup still applies."
  (let ((mediawiki-site-alist
         (list (list "TestSite" "https://test.example.org/w/" "" "" ""
                     :description "Test Site"))))
    (cl-letf (((symbol-function 'url-user-for-url)
               (lambda (&rest _) "authuser")))
      (should (equal (mediawiki-site-username "TestSite") "authuser")))))

(ert-deftest test-mediawiki-oauth-new-token-forgets-userinfo ()
  "Storing a new token drops the cached user name."
  (test-mediawiki-oauth-with-site "TestSite"
    (mediawiki-oauth-forget-userinfo)
    (let ((name "Alice"))
      (cl-letf (((symbol-function 'mediawiki-api-call)
                 (lambda (&rest _)
                   `((query . ((userinfo . ((name . ,name)))))))))
        (should (equal (mediawiki-oauth-userinfo "TestSite") "Alice"))
        (setq name "Bob")
        (mediawiki-oauth-set-tokens "TestSite" "new-token" nil 3600)
        (should (equal (mediawiki-oauth-userinfo "TestSite") "Bob"))))))

;;; Secret storage tests

(ert-deftest test-mediawiki-oauth-authinfo-entry ()
  "An authinfo line has the fields auth-source expects."
  (should (equal (mediawiki-oauth--authinfo-entry
                  "example.org" "cid" "mediawiki-client-secret" "s3cret")
                 (concat "machine example.org login cid password s3cret"
                         " port mediawiki-client-secret"))))

(ert-deftest test-mediawiki-oauth-store-authinfo-appends ()
  "Storing two secrets adds two lines and keeps the first."
  (let ((file (make-temp-file "mw-authinfo-"))
        ;; `--store-authinfo' registers the file with auth-source; keep
        ;; that out of the rest of the suite.
        (auth-sources auth-sources))
    (unwind-protect
        (progn
          (mediawiki-oauth--store-authinfo file "example.org" "cid"
                                           "mediawiki-client-secret" "one")
          (mediawiki-oauth--store-authinfo file "example.org" "cid"
                                           "mediawiki-access-token" "two")
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-match-p "port mediawiki-client-secret"
                                    (buffer-string)))
            (should (string-match-p "port mediawiki-access-token"
                                    (buffer-string)))
            (should (= 2 (how-many "^machine " (point-min) (point-max))))))
      (delete-file file))))

(ert-deftest test-mediawiki-oauth-choose-storage-when-configured ()
  "A remembered choice is used without asking."
  (let ((mediawiki-oauth-secret-storage 'authinfo))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _) (ert-fail "should not ask"))))
      (should (eq (mediawiki-oauth--choose-secret-storage t) 'authinfo)))))

(ert-deftest test-mediawiki-oauth-choose-storage-asks-and-remembers ()
  "Authinfo plus encryption is saved for next time."
  (let ((mediawiki-oauth-secret-storage 'ask)
        (answers '(t t))
        (saved nil))
    (cl-letf (((symbol-function 'mediawiki-oauth--authinfo-files)
               (lambda () '("~/.authinfo.gpg")))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) (pop answers)))
              ((symbol-function 'customize-save-variable)
               (lambda (sym val) (setq saved (cons sym val)))))
      (should (eq (mediawiki-oauth--choose-secret-storage t) 'authinfo))
      (should (equal saved '(mediawiki-oauth-secret-storage . authinfo))))))

(ert-deftest test-mediawiki-oauth-choose-storage-plain ()
  "Declining encryption keeps the entry in a plaintext file."
  (let ((mediawiki-oauth-secret-storage 'ask)
        (answers '(t nil))
        (saved nil))
    (cl-letf (((symbol-function 'mediawiki-oauth--authinfo-files)
               (lambda () '("~/.authinfo")))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) (pop answers)))
              ((symbol-function 'customize-save-variable)
               (lambda (sym val) (setq saved (cons sym val)))))
      (should (eq (mediawiki-oauth--choose-secret-storage t) 'authinfo-plain))
      (should (equal saved
                     '(mediawiki-oauth-secret-storage . authinfo-plain))))))

(ert-deftest test-mediawiki-oauth-choose-storage-declines ()
  "Declining authinfo keeps the secret in the site configuration."
  (let ((mediawiki-oauth-secret-storage 'ask)
        (saved nil))
    (cl-letf (((symbol-function 'mediawiki-oauth--authinfo-files)
               (lambda () '("~/.authinfo.gpg")))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
              ((symbol-function 'customize-save-variable)
               (lambda (sym val) (setq saved (cons sym val)))))
      (should (eq (mediawiki-oauth--choose-secret-storage t) 'site-alist))
      (should (equal saved '(mediawiki-oauth-secret-storage . site-alist))))))

(ert-deftest test-mediawiki-oauth-choose-storage-no-authinfo ()
  "Without an authinfo file there is nothing to ask about."
  (let ((mediawiki-oauth-secret-storage 'ask))
    (cl-letf (((symbol-function 'mediawiki-oauth--authinfo-files)
               (lambda () nil))
              ((symbol-function 'y-or-n-p)
               (lambda (&rest _) (ert-fail "should not ask")))
              ((symbol-function 'customize-save-variable)
               (lambda (&rest _) (ert-fail "should not save"))))
      (should (eq (mediawiki-oauth--choose-secret-storage t) 'site-alist)))))

(ert-deftest test-mediawiki-oauth-choose-storage-non-interactive ()
  "Non-interactive callers never prompt."
  (let ((mediawiki-oauth-secret-storage 'ask))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _) (ert-fail "should not ask"))))
      (should (eq (mediawiki-oauth--choose-secret-storage) 'site-alist)))))

(ert-deftest test-mediawiki-oauth-setup-site-authinfo-storage ()
  "The secret and token go to authinfo, not the site entry."
  (test-mediawiki-oauth-with-site "TestSite"
    (let ((mediawiki-oauth-secret-storage 'authinfo)
          (written nil))
      (cl-letf (((symbol-function 'mediawiki-oauth--store-authinfo)
                 (lambda (file host id port secret)
                   (push (list file host id port secret) written))))
        (mediawiki-oauth-setup-site "TestSite" "cid" "csecret" "atoken")
        (should (equal (mediawiki-oauth-client-id "TestSite") "cid"))
        (should-not (mediawiki-site-property "TestSite" :oauth-client-secret))
        (should-not (mediawiki-site-property "TestSite" :oauth-access-token))
        (should (member (list (mediawiki-oauth--authinfo-file t)
                              "test.example.org" "cid"
                              "mediawiki-client-secret" "csecret")
                        written))
        (should (member (list (mediawiki-oauth--authinfo-file t)
                              "test.example.org" "cid"
                              "mediawiki-access-token" "atoken")
                        written))))))

(provide 'test-mediawiki-oauth)

;;; test-mediawiki-oauth.el ends here
