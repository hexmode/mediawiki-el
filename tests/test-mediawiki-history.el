;;; test-mediawiki-history.el --- Unit tests for mediawiki-history.el -*- lexical-binding: t; -*-

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

;; Unit tests for mediawiki-history.el module.
;; These tests verify entry formatting and mode setup without
;; making network calls.

;;; Code:

(require 'ert)
(require 'mediawiki-history)
(require 'mediawiki-utils)

;;; Test mediawiki-history--make-entry

(ert-deftest test-mediawiki-history--make-entry ()
  "Test mediawiki-history--make-entry function."
  (let* ((mock-rev '((revid . 101)
                     (parentid . 100)
                     (timestamp . "2026-01-01T00:00:00Z")
                     (user . "TestUser")
                     (comment . "test comment")
                     (size . 200)
                     (sizediff . 50)))
         (mediawiki-history--page-title "Test Page")
         (result (mediawiki-history--make-entry mock-rev)))

    ;; Verify result is a vector of length 5
    (should (vectorp result))
    (should (= (length result) 5))

    ;; Verify first element (rev-str) is propertized with rev-id, parent-id, mw-title
    (should (= (get-text-property 0 'rev-id (aref result 0)) 101))
    (should (= (get-text-property 0 'parent-id (aref result 0)) 100))
    (should (string= (get-text-property 0 'mw-title (aref result 0)) "Test Page"))

    ;; Verify rev-str text itself is the revision number
    (should (string= (aref result 0) "101"))

    ;; Verify vector contains the time string, username, size change string, and comment
    (should (stringp (aref result 1)))    ; time string (relative, so check type only)
    (should (string= (aref result 2) "TestUser"))
    (should (string-equal (aref result 3) "+50"))  ; sizediff=50 -> formatted as "+50"
    (should (string= (aref result 4) "test comment"))))

;;; Test mediawiki-history-mode

(ert-deftest test-mediawiki-history-mode ()
  "Test mediawiki-history-mode major mode setup."
  (with-temp-buffer
    (mediawiki-history-mode)

    ;; Verify major mode is mediawiki-history-mode
    (should (eq major-mode 'mediawiki-history-mode))

    ;; Verify tabulated-list-format is set (a vector)
    (should (vectorp tabulated-list-format))
    (should (>= (length tabulated-list-format) 4))

    ;; Verify tabulated-list-sort-key is set
    (should (consp tabulated-list-sort-key))))

;;; Test mediawiki-history--refresh-table

(ert-deftest test-mediawiki-history--refresh-table ()
  "Test mediawiki-history--refresh-table function."
  (with-temp-buffer
    (mediawiki-history-mode)

    ;; Set up a mock revision list
    (let* ((mock-rev '((revid . 101)
                       (parentid . 100)
                       (timestamp . "2026-01-01T00:00:00Z")
                       (user . "TestUser")
                       (comment . "test comment")
                       (size . 200)
                       (sizediff . 50)))
           (mediawiki-history--page-title "Test Page")
           (mediawiki-history--revisions (list mock-rev)))

      ;; Call the refresh function
      (mediawiki-history--refresh-table)

      ;; Verify tabulated-list-entries is a list of 2-element proper lists
      (should (consp tabulated-list-entries))
      (should (= (length tabulated-list-entries) 1))

      ;; Verify first entry's car is the revid
      (let ((first-entry (car tabulated-list-entries)))
        (should (consp first-entry))
        (should (= (car first-entry) 101))

        ;; Verify first entry's cadr is a vector
        (should (vectorp (cadr first-entry)))
        (should (= (length (cadr first-entry)) 5))))))

(provide 'test-mediawiki-history)

;;; test-mediawiki-history.el ends here
