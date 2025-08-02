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
  (should (get 'mediawiki-browse 'autoload))
  (should (commandp 'mediawiki-browse)))

(ert-deftest test-mediawiki-site-function-structure ()
  "Test mediawiki-site function structure."
  (should (functionp 'mediawiki-site))
  (should (get 'mediawiki-site 'autoload))
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

(provide 'test-mediawiki-site)

;;; test-mediawiki-site.el ends here
