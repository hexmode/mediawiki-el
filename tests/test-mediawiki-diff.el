;;; test-mediawiki-diff.el --- Unit tests for mediawiki-diff.el -*- lexical-binding: t; -*-

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

;; Unit tests for mediawiki-diff.el module.
;; These tests verify function existence and structural properties
;; without making network calls.

;;; Code:

(require 'ert)
(require 'mediawiki-diff)
(require 'mediawiki-utils)

;;; Test Function Existence

(ert-deftest test-mediawiki-diff-last-edit ()
  "Test that mediawiki-diff-last-edit function exists."
  (should (functionp 'mediawiki-diff-last-edit))
  (should (commandp 'mediawiki-diff-last-edit)))

(ert-deftest test-mediawiki-diff-show-diff ()
  "Test that mediawiki-diff-show-diff function exists."
  (should (functionp 'mediawiki-diff-show-diff))
  (should (commandp 'mediawiki-diff-show-diff)))

(ert-deftest test-mediawiki-diff-show-revision ()
  "Test that mediawiki-diff-show-revision function exists."
  (should (functionp 'mediawiki-diff-show-revision))
  (should (commandp 'mediawiki-diff-show-revision)))

(ert-deftest test-mediawiki-diff-ediff ()
  "Test that mediawiki-diff-ediff function exists."
  (should (functionp 'mediawiki-diff-ediff))
  (should (not (commandp 'mediawiki-diff-ediff))))

;;; Test Variables

(ert-deftest test-mediawiki-diff-function-variable ()
  "Test that mediawiki-diff-function variable exists and defaults to `unified'."
  (should (boundp 'mediawiki-diff-function))
  (should (eq mediawiki-diff-function 'unified)))

;;; Test Minor Mode

(ert-deftest test-mediawiki-diff-follow-mode ()
  "Test that mediawiki-diff-follow-mode minor mode is defined."
  (should (boundp 'mediawiki-diff-follow-mode))
  (should (fboundp 'mediawiki-diff-follow-mode)))

(provide 'test-mediawiki-diff)

;;; test-mediawiki-diff.el ends here
