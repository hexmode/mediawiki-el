;;; test-mediawiki-autoloads.el --- Tests for MediaWiki autoload functionality  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mark A. Hershberger

;; Author: Mark A. Hershberger <mah@everybody.org>
;; Keywords: mediawiki wikipedia network wiki

;; This file is NOT (yet) part of GNU Emacs.

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

;; This file contains tests specifically for checking that MediaWiki functions
;; are properly autoloaded. These tests must be run before the actual functions
;; are loaded to verify the autoload mechanism works correctly.

;;; Code:

(require 'ert)

;;; Test MediaWiki Mode Autoloads

(ert-deftest test-mediawiki-mode-autoload ()
  "Test that mediawiki-mode is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-mode))))

(ert-deftest test-mediawiki-next-header-autoload ()
  "Test that mediawiki-next-header is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-next-header))))

(ert-deftest test-mediawiki-prev-header-autoload ()
  "Test that mediawiki-prev-header is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-prev-header))))

(ert-deftest test-mediawiki-terminate-paragraph-autoload ()
  "Test that mediawiki-terminate-paragraph is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-terminate-paragraph))))

(ert-deftest test-mediawiki-fill-article-autoload ()
  "Test that mediawiki-fill-article is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-fill-article))))

(ert-deftest test-mediawiki-unfill-article-autoload ()
  "Test that mediawiki-unfill-article is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-unfill-article))))

(ert-deftest test-mediawiki-reply-at-point-simple-autoload ()
  "Test that mediawiki-reply-at-point-simple is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-reply-at-point-simple))))

(ert-deftest test-mediawiki-goto-next-link-autoload ()
  "Test that mediawiki-goto-next-link is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-goto-next-link))))

(ert-deftest test-mediawiki-goto-prev-link-autoload ()
  "Test that mediawiki-goto-prev-link is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-goto-prev-link))))

(ert-deftest test-mediawiki-goto-previous-page-autoload ()
  "Test that mediawiki-goto-previous-page is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-goto-previous-page))))

(ert-deftest test-mediawiki-insert-bold-autoload ()
  "Test that mediawiki-insert-bold is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-insert-bold))))

(ert-deftest test-mediawiki-insert-italics-autoload ()
  "Test that mediawiki-insert-italics is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-insert-italics))))

(ert-deftest test-mediawiki-insert-strong-emphasis-autoload ()
  "Test that mediawiki-insert-strong-emphasis is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-insert-strong-emphasis))))

(ert-deftest test-mediawiki-insert-header-autoload ()
  "Test that mediawiki-insert-header is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-insert-header))))

(ert-deftest test-mediawiki-insert-link-autoload ()
  "Test that mediawiki-insert-link is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-insert-link))))

(ert-deftest test-mediawiki-insert-signature-autoload ()
  "Test that mediawiki-insert-signature is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-insert-signature))))

(ert-deftest test-mediawiki-insert-hline-autoload ()
  "Test that mediawiki-insert-hline is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-insert-hline))))

(ert-deftest test-mediawiki-insert-enumerate-autoload ()
  "Test that mediawiki-insert-enumerate is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-insert-enumerate))))

(ert-deftest test-mediawiki-insert-itemize-autoload ()
  "Test that mediawiki-insert-itemize is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-insert-itemize))))

(ert-deftest test-mediawiki-simple-outline-promote-autoload ()
  "Test that mediawiki-simple-outline-promote is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-simple-outline-promote))))

;;; Test MediaWiki Page Autoloads

(ert-deftest test-mediawiki-open-autoload ()
  "Test that mediawiki-open is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-open))))

(ert-deftest test-mediawiki-save-autoload ()
  "Test that mediawiki-save is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-save))))

(ert-deftest test-mediawiki-save-as-autoload ()
  "Test that mediawiki-save-as is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-save-as))))

(ert-deftest test-mediawiki-save-and-bury-autoload ()
  "Test that mediawiki-save-and-bury is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-save-and-bury))))

(ert-deftest test-mediawiki-open-page-at-point-autoload ()
  "Test that mediawiki-open-page-at-point is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-open-page-at-point))))

;;; Test MediaWiki Site Autoloads

(ert-deftest test-mediawiki-browse-autoload ()
  "Test that mediawiki-browse is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-browse))))

(ert-deftest test-mediawiki-site-autoload ()
  "Test that mediawiki-site is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-site))))

;;; Test MediaWiki Auth Autoloads

(ert-deftest test-mediawiki-do-login-autoload ()
  "Test that mediawiki-do-login is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-do-login))))

(ert-deftest test-mediawiki-do-logout-autoload ()
  "Test that mediawiki-do-logout is properly autoloaded."
  (should (autoloadp (symbol-function 'mediawiki-do-logout))))

(provide 'test-mediawiki-autoloads)

;;; test-mediawiki-autoloads.el ends here
