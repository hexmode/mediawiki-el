;;; test-mediawiki-user.el --- Unit tests for mediawiki-user.el -*- lexical-binding: t; -*-

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

;; Unit tests for mediawiki-user.el module.

;;; Code:

(require 'ert)
(require 'mediawiki-user)

;;; Function Existence

(ert-deftest test-mediawiki-user-contributions-entry-point ()
  "Test that the main entry point function exists."
  (should (functionp 'mediawiki-user-contributions)))

(ert-deftest test-mediawiki-user-mode ()
  "Test mediawiki-user-contributions-mode setup."
  (with-temp-buffer
    (mediawiki-user-contributions-mode)
    (should (eq major-mode 'mediawiki-user-contributions-mode))))

;;; Variable Existence

(ert-deftest test-mediawiki-user-variables ()
  "Test that internal buffer-local variables are defined."
  (should (boundp 'mediawiki-user--username))
  (should (boundp 'mediawiki-user--contribs))
  (should (boundp 'mediawiki-user--sitename)))

;;; Internal Functions

(ert-deftest test-mediawiki-user--make-entry ()
  "Test mediawiki-user--make-entry creates a valid tabulated-list entry."
  (let* ((mock-contrib '((revid . 101)
                         (title . "Test Page")
                         (timestamp . "2026-01-01T00:00:00Z")
                         (user . "TestUser")
                         (comment . "test contribution")
                         (sizediff . 50)))
         (result (mediawiki-user--make-entry mock-contrib)))
    (should (listp result))
    (should (= (length result) 2))
    (should (vectorp (cadr result)))
    ;; Vector should have 5 elements: Rev, Title, Time, User, Comment
    (should (= (length (cadr result)) 5))))

;;; Refresh and Commands

(ert-deftest test-mediawiki-user-commands ()
  "Test that user contribution commands exist."
  (should (functionp 'mediawiki-user-contributions-refresh))
  (should (functionp 'mediawiki-user-open-page))
  (should (functionp 'mediawiki-user-view-revision))
  (should (functionp 'mediawiki-user-diff-previous))
  (should (functionp 'mediawiki-user-browse-revision)))

(provide 'test-mediawiki-user)

;;; test-mediawiki-user.el ends here
