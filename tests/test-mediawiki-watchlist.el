;;; test-mediawiki-watchlist.el --- Unit tests for mediawiki-watchlist.el  -*- lexical-binding: t; -*-

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

;; Unit tests for mediawiki-watchlist.el module.

;;; Code:

(require 'ert)
(require 'mediawiki-watchlist)
(require 'mediawiki-utils)

;;; Test Grouping

(ert-deftest test-watchlist-group-entries-different-pages ()
  "Test `mediawiki-watchlist--group-entries' with different pages.
Groups should be sorted by most recent timestamp, newest first."
  (let* ((entry-a '((title . "Page A") (revid . 101) (timestamp . "2026-06-01")
                    (user . "Alice") (comment . "Edit A") (oldlen . 100) (newlen . 200)))
         (entry-b '((title . "Page B") (revid . 201) (timestamp . "2026-06-02")
                    (user . "Bob") (comment . "Edit B") (oldlen . 200) (newlen . 300)))
         (entries (list entry-a entry-b))
         (result (mediawiki-watchlist--group-entries entries)))
    ;; Should have 2 groups
    (should (= (length result) 2))
    ;; Groups should be sorted by most recent timestamp (Page B has later timestamp)
    (should (string= (caar result) "Page B"))
    (should (string= (caadr result) "Page A"))))

(ert-deftest test-watchlist-group-entries-same-page ()
  "Test `mediawiki-watchlist--group-entries' with two entries for same page.
They should be grouped together under one title."
  (let* ((entry1 '((title . "Page A") (revid . 101) (timestamp . "2026-06-01T12:00:00Z")
                   (user . "Alice") (comment . "First edit") (oldlen . 100) (newlen . 200)))
         (entry2 '((title . "Page A") (revid . 102) (timestamp . "2026-06-01T14:00:00Z")
                   (user . "Alice") (comment . "Second edit") (oldlen . 200) (newlen . 300)))
         (entries (list entry1 entry2))
         (result (mediawiki-watchlist--group-entries entries)))
    ;; Should have 1 group
    (should (= (length result) 1))
    ;; Grouped under "Page A"
    (should (string= (caar result) "Page A"))
    ;; Both entries in the group
    (should (= (length (cdar result)) 2))))

;;; Test Group Entry Creation

(ert-deftest test-watchlist-make-group-entry ()
  "Test `mediawiki-watchlist--make-group-entry' creates proper tabulated-list entry."
  (let* ((entry1 '((title . "Page A") (revid . 101) (old_revid . 100)
                   (timestamp . "2026-06-01T12:00:00Z") (user . "Alice")
                   (comment . "First edit") (oldlen . 100) (newlen . 200)))
         (entry2 '((title . "Page A") (revid . 102) (old_revid . 101)
                   (timestamp . "2026-06-02T12:00:00Z") (user . "Bob")
                   (comment . "Second edit") (oldlen . 200) (newlen . 300)))
         (entries (list entry1 entry2)))
    (with-temp-buffer
      (mediawiki-watchlist-mode)
      (let ((result (mediawiki-watchlist--make-group-entry "Page A" entries)))
        ;; Result should be a proper list (ID VECTOR)
        (should (consp result))
        (should (= (length result) 2))
        ;; ID should be (group . "Page A")
        (let ((id (car result)))
          (should (eq (car id) 'group))
          (should (string= (cdr id) "Page A")))
        ;; Vector should have 7 elements
        (let ((vector (cadr result)))
          (should (vectorp vector))
          (should (= (length vector) 7))
          ;; "C" column (index 1) should show count > 1 (propertized)
          (should (string-match-p "2" (substring-no-properties (aref vector 1))))
          ;; "User" column (index 4) should show users joined with comma
          (let ((user-col (aref vector 4)))
            (should (stringp user-col))
            (should (string-match-p "Alice" user-col))
            (should (string-match-p "Bob" user-col))))))))

;;; Test Child Entry Creation

(ert-deftest test-watchlist-make-child-entry ()
  "Test `mediawiki-watchlist--make-child-entry' creates proper tabulated-list entry."
  (let ((entry '((title . "Page A") (revid . 101) (old_revid . 100)
                 (timestamp . "2026-06-01T12:00:00Z") (user . "Alice")
                 (comment . "Test edit") (oldlen . 100) (newlen . 200))))
    (with-temp-buffer
      (mediawiki-watchlist-mode)
      (let ((result (mediawiki-watchlist--make-child-entry entry)))
        ;; Result should be a proper list (ID VECTOR)
        (should (consp result))
        (should (= (length result) 2))
        ;; ID should be (child . ("Page A" . 101))
        (let ((id (car result)))
          (should (eq (car id) 'child))
          (should (string= (car (cdr id)) "Page A"))
          (should (= (cdr (cdr id)) 101)))
        ;; Vector should have 7 elements
        (let ((vector (cadr result)))
          (should (vectorp vector))
          (should (= (length vector) 7))
          ;; Page column (index 2) should have the title
          (should (string-match-p "Page A"
                                  (substring-no-properties (aref vector 2)))))))))

;;; Test Mode

(ert-deftest test-watchlist-mode ()
  "Test `mediawiki-watchlist-mode' setup."
  (with-temp-buffer
    (mediawiki-watchlist-mode)
    ;; Verify major mode
    (should (eq major-mode 'mediawiki-watchlist-mode))
    ;; Verify tabulated-list-format has 7 columns
    (should (vectorp tabulated-list-format))
    (should (= (length tabulated-list-format) 7))
    ;; Verify tabulated-list-entries is a function
    (should (functionp tabulated-list-entries))))

(provide 'test-mediawiki-watchlist)

;;; test-mediawiki-watchlist.el ends here
