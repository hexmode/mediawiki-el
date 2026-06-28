;;; test-mediawiki-site.el --- Unit tests for mediawiki-site.el -*- lexical-binding: t; -*-

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

;; Unit tests for mediawiki-site.el module.

;;; Code:

(require 'ert)
(require 'mediawiki-site)

;;; Test Site Configuration

(ert-deftest test-mediawiki-site-alist ()
  "Test that mediawiki-site-alist is properly defined."
  (should (boundp 'mediawiki-site-alist))
  (should (listp mediawiki-site-alist))
  (should (> (length mediawiki-site-alist) 0))

  ;; Test that default sites are present
  (should (assoc "Wikipedia" mediawiki-site-alist))
  (should (assoc "Wiktionary" mediawiki-site-alist))
  (should (assoc "Wikimedia Commons" mediawiki-site-alist))

  ;; Test structure of first site entry
  (let ((wikipedia-entry (assoc "Wikipedia" mediawiki-site-alist)))
    (should (>= (length wikipedia-entry) 6)) ; name + 5 basic fields
    (should (stringp (nth 1 wikipedia-entry))) ; URL
    (should (stringp (nth 2 wikipedia-entry))) ; username
    (should (stringp (nth 3 wikipedia-entry))) ; password
    (should (stringp (nth 4 wikipedia-entry))))) ; domain

;;; Test Site Extraction Functions

(ert-deftest test-mediawiki-site-extract ()
  "Test mediawiki-site-extract function."
  ;; Test extracting URL (index 1)
  (should (string-match-p "wikipedia\\.org"
                          (mediawiki-site-extract "Wikipedia" 1)))

  ;; Test extracting username (index 2)
  (should (stringp (mediawiki-site-extract "Wikipedia" 2)))

  ;; Test extracting from non-existent site
  (should-not (mediawiki-site-extract "NonExistentSite" 1)))

(ert-deftest test-mediawiki-site-url ()
  "Test mediawiki-site-url function."
  (let ((url (mediawiki-site-url "Wikipedia")))
    (should (stringp url))
    (should (string-match-p "^https?://" url))
    (should (string-match-p "wikipedia\\.org" url))))

(ert-deftest test-mediawiki-site-username ()
  "Test mediawiki-site-username function."
  ;; Mock url-user-for-url to avoid auth dependencies
  (cl-letf (((symbol-function 'url-user-for-url)
             (lambda (url) "auth-user")))

    ;; Test with configured username
    (let ((username (mediawiki-site-username "Wikipedia")))
      (should (stringp username)))))

(ert-deftest test-mediawiki-site-password ()
  "Test mediawiki-site-password function."
  ;; Mock url-password-for-url to avoid auth dependencies
  (cl-letf (((symbol-function 'url-password-for-url)
             (lambda (url) "auth-pass")))

    ;; Test with configured password
    (let ((password (mediawiki-site-password "Wikipedia")))
      (should (stringp password)))))

(ert-deftest test-mediawiki-site-domain ()
  "Test mediawiki-site-domain function."
  ;; Test with Wikipedia (should have empty domain)
  (let ((domain (mediawiki-site-domain "Wikipedia")))
    (should (or (null domain) (string= "" domain))))

  ;; Test with mock site that has domain
  (let ((mediawiki-site-alist '(("TestSite" "http://test.com" "user" "pass" "TESTDOMAIN"))))
    (should (string= "TESTDOMAIN" (mediawiki-site-domain "TestSite")))))

(ert-deftest test-mediawiki-site-first-page ()
  "Test mediawiki-site-first-page function."
  ;; Test with Wikipedia (should have Main Page)
  (should (string= "Main Page" (mediawiki-site-first-page "Wikipedia")))

  ;; Test with mock site that has custom first page
  (let ((mediawiki-site-alist '(("TestSite" "http://test.com" "user" "pass" ""
                                  :first-page "Custom Page"))))
    (should (string= "Custom Page" (mediawiki-site-first-page "TestSite")))))

;;; Test Site Selection Functions

(ert-deftest test-mediawiki-prompt-for-site ()
  "Test mediawiki-prompt-for-site function."
  ;; Mock completing-read
  (cl-letf (((symbol-function 'completing-read)
             (lambda (prompt collection &rest args) "Wikipedia")))

    ;; Test site selection
    (should (string= "Wikipedia" (mediawiki-prompt-for-site))))

  ;; Test with current site default
  (let ((mediawiki-site "TestSite"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &rest args) "")))

      ;; Should return current site when empty string entered
      (should (string= "TestSite" (mediawiki-prompt-for-site))))))

;;; Test Site Management Functions

(ert-deftest test-mediawiki-site-list ()
  "Test mediawiki-site-list function."
  (let ((sites (mediawiki-site-list)))
    (should (listp sites))
    (should (> (length sites) 0))
    (should (member "Wikipedia" sites))
    (should (member "Wiktionary" sites))

    ;; All elements should be strings
    (dolist (site sites)
      (should (stringp site)))))

(ert-deftest test-mediawiki-site-description ()
  "Test mediawiki-site-description function."
  ;; Test with Wikipedia
  (let ((desc (mediawiki-site-description "Wikipedia")))
    (should (stringp desc))
    (should (string-match-p "English Wikipedia" desc)))

  ;; Test with non-existent site
  (should-not (mediawiki-site-description "NonExistentSite")))

(ert-deftest test-mediawiki-site-property ()
  "Test mediawiki-site-property function."
  ;; Test getting description property
  (should (stringp (mediawiki-site-property "Wikipedia" :description)))

  ;; Test getting first-page property
  (should (stringp (mediawiki-site-property "Wikipedia" :first-page)))

  ;; Test getting non-existent property
  (should-not (mediawiki-site-property "Wikipedia" :nonexistent))

  ;; Test with non-existent site
  (should-not (mediawiki-site-property "NonExistentSite" :description)))

(ert-deftest test-mediawiki-site-valid-p ()
  "Test mediawiki-site-valid-p function."
  ;; Test with valid sites
  (should (mediawiki-site-valid-p "Wikipedia"))
  (should (mediawiki-site-valid-p "Wiktionary"))

  ;; Test with invalid site
  (should-not (mediawiki-site-valid-p "NonExistentSite"))
  (should-not (mediawiki-site-valid-p nil)))

;;; Test Interactive Functions

(ert-deftest test-mediawiki-browse-structure ()
  "Test mediawiki-browse function structure."
  (should (functionp 'mediawiki-browse))
  (should (commandp 'mediawiki-browse)))

(ert-deftest test-mediawiki-site-function-structure ()
  "Test mediawiki-site function structure."
  (should (functionp 'mediawiki-site))
  (should (commandp 'mediawiki-site)))

;;; Test Customization

(ert-deftest test-mediawiki-site-customization ()
  "Test site-related customization variables."
  (should (boundp 'mediawiki-pop-buffer-hook))
  (should (listp mediawiki-pop-buffer-hook))

  ;; Test that site-alist has proper custom type
  (should (get 'mediawiki-site-alist 'custom-type)))

;;; Test Site Configuration Structure

(ert-deftest test-mediawiki-site-alist-structure ()
  "Test the structure of mediawiki-site-alist entries."
  (dolist (site-entry mediawiki-site-alist)
    ;; Each entry should be a list
    (should (listp site-entry))

    ;; Should have at least 6 elements (name + 5 basic fields)
    (should (>= (length site-entry) 6))

    ;; First element should be site name (string)
    (should (stringp (nth 0 site-entry)))

    ;; Second element should be URL (string)
    (should (stringp (nth 1 site-entry)))
    (should (string-match-p "^https?://" (nth 1 site-entry)))

    ;; Third through fifth elements should be strings
    (should (stringp (nth 2 site-entry))) ; username
    (should (stringp (nth 3 site-entry))) ; password
    (should (stringp (nth 4 site-entry))) ; domain

    ;; Remaining elements should form a valid plist if present
    (when (> (length site-entry) 5)
      (let ((plist (nthcdr 5 site-entry)))
        ;; Should have even number of elements (key-value pairs)
        (should (= 0 (mod (length plist) 2)))))))

;;; Tests for Issue #46 — Legacy positional first-page config

(ert-deftest test-mediawiki-site-first-page-positional ()
  "Legacy positional config: 6th element (index 5) used as first-page (issue #46)."
  (let ((mediawiki-site-alist
         '(("TestWiki" "http://test.example.com/w/" "user" "pass" "" "CustomPage"))))
    (should (string= "CustomPage" (mediawiki-site-first-page "TestWiki")))))

(ert-deftest test-mediawiki-site-first-page-keyword ()
  "Keyword :first-page config should still work after fix (issue #46)."
  (let ((mediawiki-site-alist
         '(("TestWiki" "http://test.example.com/w/" "user" "pass" ""
            :first-page "KeywordPage"))))
    (should (string= "KeywordPage" (mediawiki-site-first-page "TestWiki")))))

(ert-deftest test-mediawiki-site-first-page-default ()
  "Empty config falls back to Main Page (issue #46)."
  (let ((mediawiki-site-alist
         '(("TestWiki" "http://test.example.com/w/" "user" "pass" ""))))
    (should (string= "Main Page" (mediawiki-site-first-page "TestWiki")))))

;;; Tests for optional positional fields (username/password/domain)

(ert-deftest test-mediawiki-site-no-positional-fields ()
  "Site entry with no username/password/domain — only URL + plist."
  (let ((mediawiki-site-alist
         '(("NoUP" "https://test.example.org/w/"
            :description "No positional fields"
            :first-page "Custom Page"
            :oauth-client-id "abc123"))))
    (should (string= "https://test.example.org/w/"
                     (mediawiki-site-url "NoUP")))
    (should (string= "Custom Page"
                     (mediawiki-site-first-page "NoUP")))
    (should (string= "No positional fields"
                     (mediawiki-site-property "NoUP" :description)))
    (should (string= "abc123"
                     (mediawiki-site-property "NoUP" :oauth-client-id)))
    (should-not (mediawiki-site-property "NoUP" :nonexistent))))

(ert-deftest test-mediawiki-site-no-positional-username-fallback ()
  "Username extracts to nil when absent; caller uses auth-source."
  (let ((mediawiki-site-alist
         '(("NoUP" "https://test.example.org/w/"
            :description "No username"))))
    (should-not (mediawiki-site-extract "NoUP" 2))))

(ert-deftest test-mediawiki-site-no-positional-password-fallback ()
  "Password extracts to nil when absent; caller uses auth-source."
  (let ((mediawiki-site-alist
         '(("NoUP" "https://test.example.org/w/"
            :description "No password"))))
    (should-not (mediawiki-site-extract "NoUP" 3))))

(ert-deftest test-mediawiki-site-no-positional-domain-fallback ()
  "Domain extracts to nil when absent."
  (let ((mediawiki-site-alist
         '(("NoUP" "https://test.example.org/w/"
            :description "No domain"))))
    (should-not (mediawiki-site-domain "NoUP"))))

(ert-deftest test-mediawiki-site-partial-positional-username-only ()
  "Site entry with username only — plist starts after it."
  (let ((mediawiki-site-alist
         '(("Partial" "https://test.example.org/w/" "myuser"
            :first-page "Partial Page"
            :description "Username only"))))
    (should (string= "myuser" (mediawiki-site-extract "Partial" 2)))
    (should-not (mediawiki-site-extract "Partial" 3))
    (should-not (mediawiki-site-extract "Partial" 4))
    (should (string= "Partial Page"
                     (mediawiki-site-first-page "Partial")))
    (should (string= "Username only"
                     (mediawiki-site-property "Partial" :description)))))

(ert-deftest test-mediawiki-site-partial-positional-user-pass ()
  "Site entry with username and password only — plist starts after password."
  (let ((mediawiki-site-alist
         '(("Partial2" "https://test.example.org/w/" "myuser" "mypass"
            :first-page "Partial2 Page"))))
    (should (string= "myuser" (mediawiki-site-extract "Partial2" 2)))
    (should (string= "mypass" (mediawiki-site-extract "Partial2" 3)))
    (should-not (mediawiki-site-extract "Partial2" 4))
    (should (string= "Partial2 Page"
                     (mediawiki-site-first-page "Partial2")))))

(ert-deftest test-mediawiki-site-full-legacy-format ()
  "Full legacy format — all 5 positional fields + optional first-page string."
  (let ((mediawiki-site-alist
         '(("Legacy" "https://test.example.org/w/" "leguser" "legpass"
            "LEGDOMAIN" "LegacyPage"))))
    (should (string= "leguser" (mediawiki-site-extract "Legacy" 2)))
    (should (string= "legpass" (mediawiki-site-extract "Legacy" 3)))
    (should (string= "LEGDOMAIN" (mediawiki-site-domain "Legacy")))
    (should (string= "LegacyPage" (mediawiki-site-first-page "Legacy")))))

(ert-deftest test-mediawiki-site-full-legacy-with-plist ()
  "Full legacy 5 fields + keyword plist (mixed old and new style)."
  (let ((mediawiki-site-alist
         '(("Mixed" "https://test.example.org/w/" "muser" "mpass" ""
            :first-page "Mixed Page"
            :description "Full legacy with plist"))))
    (should (string= "muser" (mediawiki-site-extract "Mixed" 2)))
    (should (string= "mpass" (mediawiki-site-extract "Mixed" 3)))
    (should-not (mediawiki-site-domain "Mixed"))
    (should (string= "Mixed Page" (mediawiki-site-first-page "Mixed")))
    (should (string= "Full legacy with plist"
                     (mediawiki-site-property "Mixed" :description)))))

(ert-deftest test-mediawiki-site-mixed-forms-in-alist ()
  "All forms can coexist in the same mediawiki-site-alist."
  (let ((mediawiki-site-alist
         '(("NoUP"    "https://test.example.org/w/"
            :description "No user-pass")
           ("Partial" "https://test.example.org/w/" "partialuser"
            :description "Just username")
           ("Legacy"  "https://test.example.org/w/" "leguser" "legpass"
            "LEG" "OldPage")
           ("Full"    "https://test.example.org/w/" "fulluser" "fullpass"
            ""
            :first-page "Full Page"
            :description "Full with plist"))))
    ;; NoUP: no positional
    (should-not (mediawiki-site-extract "NoUP" 2))
    (should (string= "No user-pass"
                     (mediawiki-site-property "NoUP" :description)))
    ;; Partial: username only
    (should (string= "partialuser" (mediawiki-site-extract "Partial" 2)))
    (should-not (mediawiki-site-extract "Partial" 3))
    (should (string= "Just username"
                     (mediawiki-site-property "Partial" :description)))
    ;; Legacy: all positional
    (should (string= "leguser" (mediawiki-site-extract "Legacy" 2)))
    (should (string= "legpass" (mediawiki-site-extract "Legacy" 3)))
    (should (string= "LEG" (mediawiki-site-domain "Legacy")))
    (should (string= "OldPage" (mediawiki-site-first-page "Legacy")))
    ;; Full: all positional + plist
    (should (string= "fulluser" (mediawiki-site-extract "Full" 2)))
    (should (string= "Full Page" (mediawiki-site-first-page "Full")))
    (should (string= "Full with plist"
                     (mediawiki-site-property "Full" :description)))))

(ert-deftest test-mediawiki-site-only-url-and-keyword ()
  "Minimal form: just sitename + URL + keyword plist (just 2 positional)."
  (let ((mediawiki-site-alist
         '(("Min" "https://test.example.org/w/"
            :first-page "Minimal"))))
    (should (string= "https://test.example.org/w/" (mediawiki-site-url "Min")))
    (should-not (mediawiki-site-extract "Min" 2))
    (should-not (mediawiki-site-extract "Min" 3))
    (should-not (mediawiki-site-extract "Min" 4))
    (should (string= "Minimal" (mediawiki-site-first-page "Min")))))

(provide 'test-mediawiki-site)

;;; test-mediawiki-site.el ends here
