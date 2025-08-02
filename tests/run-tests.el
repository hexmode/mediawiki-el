;;; run-tests.el --- Test runner for mediawiki.el unit tests -*- lexical-binding: t; -*-

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

;; Test runner for all mediawiki.el unit tests.
;; Run with: emacs -batch -l test/run-tests.el

;;; Code:

;; Add the project root to load path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;; Add test directory to load path
(add-to-list 'load-path (file-name-directory load-file-name))

;; Load ERT
(require 'ert)

;; Load all test files
(require 'test-mediawiki-simple)
(require 'test-mediawiki-core)
(require 'test-mediawiki-utils)
(require 'test-mediawiki-faces)
(require 'test-mediawiki-font-lock)
(require 'test-mediawiki-http)
(require 'test-mediawiki-api)
(require 'test-mediawiki-auth)
(require 'test-mediawiki-site)
(require 'test-mediawiki-page)
(require 'test-mediawiki-draft)
(require 'test-mediawiki-mode)
(require 'test-mediawiki-integration)

;; Function to run all tests
(defun mediawiki-run-all-tests ()
  "Run all mediawiki.el unit tests."
  (interactive)
  (let ((test-results (ert-run-tests-batch-and-exit "^test-mediawiki-")))
    test-results))

;; Function to run tests for a specific module
(defun mediawiki-run-module-tests (module)
  "Run tests for a specific MODULE."
  (interactive "sModule name: ")
  (ert-run-tests-batch-and-exit (format "^test-mediawiki-%s-" module)))

;; Function to run tests interactively
(defun mediawiki-run-tests-interactive ()
  "Run tests interactively with ERT."
  (interactive)
  (ert "^test-mediawiki-"))

;; Print test summary
(defun mediawiki-test-summary ()
  "Print a summary of available tests."
  (interactive)
  (let ((test-count 0)
        (modules '()))
    (mapatoms
     (lambda (symbol)
       (when (and (fboundp symbol)
                  (string-match "^test-mediawiki-\\([^-]+\\)-" (symbol-name symbol)))
         (setq test-count (1+ test-count))
         (let ((module (match-string 1 (symbol-name symbol))))
           (unless (member module modules)
             (push module modules))))))

    (message "MediaWiki.el Test Suite")
    (message "=====================")
    (message "Total tests: %d" test-count)
    (message "Modules tested: %s" (mapconcat 'identity (sort modules 'string<) ", "))
    (message "")
    (message "Run all tests: M-x mediawiki-run-all-tests")
    (message "Run interactively: M-x mediawiki-run-tests-interactive")
    (message "Run specific module: M-x mediawiki-run-module-tests")))

;; When run in batch mode, execute all tests
(when noninteractive
  (mediawiki-run-all-tests))

(provide 'run-tests)

;;; run-tests.el ends here
