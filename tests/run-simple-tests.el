;;; run-simple-tests.el --- Simple test runner for mediawiki.el -*- lexical-binding: t; -*-

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

;; Simple test runner for mediawiki.el that focuses on core functionality.
;; Run with: emacs -batch -l test/run-simple-tests.el

;;; Code:

;; Add the project root to load path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;; Add test directory to load path
(add-to-list 'load-path (file-name-directory load-file-name))

;; Load ERT
(require 'ert)

;; Load simple test file
(require 'test-mediawiki-simple)

;; Function to run simple tests
(defun mediawiki-run-simple-tests ()
  "Run simple mediawiki.el tests."
  (interactive)
  (ert-run-tests-batch-and-exit "^test-mediawiki-simple-"))

;; When run in batch mode, execute simple tests
(when noninteractive
  (mediawiki-run-simple-tests))

(provide 'run-simple-tests)

;;; run-simple-tests.el ends here
