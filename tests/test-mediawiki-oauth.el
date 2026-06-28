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

(provide 'test-mediawiki-oauth)

;;; test-mediawiki-oauth.el ends here
