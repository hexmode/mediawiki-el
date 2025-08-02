;;; test-mediawiki-integration.el --- Integration tests for mediawiki.el -*- lexical-binding: t; -*-

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

;; Integration tests for mediawiki.el that test module interactions
;; and complete workflows. These tests mock network operations to
;; avoid external dependencies.

;;; Code:

(require 'ert)
(require 'mediawiki)

;;; Test Module Loading and Dependencies

(ert-deftest test-mediawiki-module-loading ()
  "Test that all modules load correctly and dependencies are satisfied."
  ;; Test that main mediawiki.el loads all required modules
  (should (featurep 'mediawiki-core))
  (should (featurep 'mediawiki-utils))
  (should (featurep 'mediawiki-http))
  (should (featurep 'mediawiki-faces))
  (should (featurep 'mediawiki-font-lock))
  (should (featurep 'mediawiki-api))
  (should (featurep 'mediawiki-auth))
  (should (featurep 'mediawiki-site))
  (should (featurep 'mediawiki-page))
  (should (featurep 'mediawiki-draft))
  (should (featurep 'mediawiki-mode))
  (should (featurep 'mediawiki)))

(ert-deftest test-mediawiki-backward-compatibility ()
  "Test that all public functions from the original monolithic file are available."
  ;; Test core functions
  (should (functionp 'mediawiki-translate-pagename))
  (should (functionp 'mediawiki-make-url))

  ;; Test site functions
  (should (functionp 'mediawiki-site))
  (should (functionp 'mediawiki-site-url))
  (should (functionp 'mediawiki-prompt-for-site))

  ;; Test authentication functions
  (should (functionp 'mediawiki-do-login))
  (should (functionp 'mediawiki-do-logout))
  (should (functionp 'mediawiki-logged-in-p))

  ;; Test page functions
  (should (functionp 'mediawiki-edit))
  (should (functionp 'mediawiki-save))
  (should (functionp 'mediawiki-save-as))

  ;; Test mode functions
  (should (functionp 'mediawiki-mode))
  (should (functionp 'mediawiki-insert-bold))
  (should (functionp 'mediawiki-insert-italics))

  ;; Test draft functions
  (should (functionp 'mediawiki-draft))
  (should (functionp 'mediawiki-draft-buffer)))

;;; Test Module Interactions

(ert-deftest test-mediawiki-core-utils-integration ()
  "Test integration between core and utils modules."
  ;; Test that utils can access core variables
  (should (boundp 'mediawiki-debug))
  (should (boundp 'mediawiki-debug-buffer))

  ;; Test debug functionality with core functions
  (let ((mediawiki-debug t))
    (with-temp-buffer
      (insert "test content")
      ;; This should not error and should use core variables
      (should-not (condition-case nil
                      (mediawiki-debug (current-buffer) "test-function")
                    (error t))))))

(ert-deftest test-mediawiki-faces-font-lock-integration ()
  "Test integration between faces and font-lock modules."
  ;; Test that font-lock can access face definitions
  (should (facep 'font-mediawiki-bold-face))
  (should (facep 'font-mediawiki-italic-face))

  ;; Test that font-lock keywords reference valid faces
  (dolist (keyword mediawiki-font-lock-keywords)
    (let ((face (if (listp keyword)
                    (if (listp (cadr keyword))
                        (cadr (cadr keyword))
                      (cadr keyword))
                  (cdr keyword))))
      (when (symbolp face)
        (should (or (facep face) (boundp face)))))))

(ert-deftest test-mediawiki-api-http-integration ()
  "Test integration between API and HTTP modules."
  ;; Mock HTTP functions to test API integration
  (cl-letf (((symbol-function 'url-http-post)
             (lambda (url params &rest args)
               "<?xml version=\"1.0\"?><api><query><pages><page title=\"Test\"/></pages></query></api>"))
            ((symbol-function 'mediawiki-site-url)
             (lambda (site) "https://test.example.com/w/")))

    ;; Test that API functions can use HTTP functions
    (should (functionp 'mediawiki-make-api-url))
    (let ((api-url (mediawiki-make-api-url "TestSite")))
      (should (stringp api-url))
      (should (string-match-p "api\\.php$" api-url)))))

(ert-deftest test-mediawiki-site-auth-integration ()
  "Test integration between site and auth modules."
  ;; Mock dependencies
  (cl-letf (((symbol-function 'mediawiki-api-call)
             (lambda (site action args)
               '(login ((result . "Success")))))
            ((symbol-function 'mediawiki-site-get-token)
             (lambda (site type) "test-token"))
            ((symbol-function 'url-cookie-retrieve)
             (lambda (host path secure) '(("session" "test")))))

    ;; Test that auth functions can access site configuration
    (should (mediawiki-logged-in-p "Wikipedia"))

    ;; Test that site functions work with auth
    (should (stringp (mediawiki-site-url "Wikipedia")))))

;;; Test Complete Workflows (Mocked)

(ert-deftest test-mediawiki-edit-workflow-mock ()
  "Test complete edit workflow with mocked dependencies."
  ;; Mock all network and UI dependencies
  (cl-letf (((symbol-function 'mediawiki-api-query-title)
             (lambda (site title)
               '(page ((title . "Test Page") (edittoken . "token"))
                      (revisions (rev () "Test content")))))
            ((symbol-function 'mediawiki-do-login)
             (lambda (site) site))
            ((symbol-function 'mediawiki-logged-in-p)
             (lambda (site) t))
            ((symbol-function 'get-buffer-create)
             (lambda (name) (generate-new-buffer name)))
            ((symbol-function 'mediawiki-pop-to-buffer)
             (lambda (buffer) buffer))
            ((symbol-function 'ring-insert)
             (lambda (ring item) nil))
            ((symbol-function 'make-ring)
             (lambda (size) (make-vector size nil))))

    ;; Test that edit workflow integrates multiple modules
    (let ((buffer (mediawiki-edit "TestSite" "Test Page")))
      (should (bufferp buffer))
      (with-current-buffer buffer
        (should (eq major-mode 'mediawiki-mode))
        (should (string= mediawiki-site "TestSite"))
        (should (string= mediawiki-page-title "Test Page")))
      (kill-buffer buffer))))

(ert-deftest test-mediawiki-mode-integration ()
  "Test mediawiki-mode integration with other modules."
  (with-temp-buffer
    (mediawiki-mode)

    ;; Test that mode integrates font-lock
    (should font-lock-defaults)
    (should (eq (car font-lock-defaults) 'mediawiki-font-lock-keywords))

    ;; Test that mode sets up faces correctly
    (should (facep 'font-mediawiki-bold-face))

    ;; Test that mode integrates with core variables
    (should (local-variable-p 'mediawiki-site))
    (should (local-variable-p 'mediawiki-page-title))

    ;; Test that mode keymap includes commands from multiple modules
    (should (lookup-key mediawiki-mode-map "\C-c\C-f\C-b")) ; from mode
    (should (lookup-key mediawiki-mode-map "\C-x\C-s"))     ; save from page
    ))

;;; Test Error Handling Across Modules

(ert-deftest test-mediawiki-error-handling-integration ()
  "Test error handling across module boundaries."
  ;; Test that errors in one module don't break others
  (with-temp-buffer
    (mediawiki-mode)

    ;; Test that mode still works even if some functions fail
    (cl-letf (((symbol-function 'mediawiki-api-call)
               (lambda (&rest args) (error "Network error"))))

      ;; Mode should still be functional
      (should (eq major-mode 'mediawiki-mode))

      ;; Text formatting should still work
      (mediawiki-insert-bold)
      (should (string-match-p "'''" (buffer-string))))))

;;; Test Customization Integration

(ert-deftest test-mediawiki-customization-integration ()
  "Test that customization works across modules."
  ;; Test that customization groups are properly organized
  (should (get 'mediawiki 'group-documentation))
  (should (get 'font-mediawiki-highlighting-faces 'group-documentation))
  (should (get 'mediawiki-draft 'group-documentation))

  ;; Test that variables from different modules can be customized
  (should (get 'mediawiki-site-default 'custom-type))
  (should (get 'mediawiki-debug 'custom-type))
  (should (get 'mediawiki-site-alist 'custom-type)))

;;; Test Memory and Resource Management

(ert-deftest test-mediawiki-resource-management ()
  "Test that modules properly manage resources."
  ;; Test that buffers are properly managed
  (let ((initial-buffers (length (buffer-list))))

    ;; Create and destroy some mediawiki buffers
    (dotimes (i 3)
      (let ((buf (get-buffer-create (format "*test-mediawiki-%d*" i))))
        (with-current-buffer buf
          (mediawiki-mode))
        (kill-buffer buf)))

    ;; Should not leak buffers
    (should (<= (length (buffer-list)) (+ initial-buffers 1)))))

;;; Test Autoload Integration

(ert-deftest test-mediawiki-autoload-integration ()
  "Test that autoloads work correctly across modules."
  ;; Test that interactive functions are properly autoloaded
  (should (get 'mediawiki-site 'autoload))
  (should (get 'mediawiki-do-login 'autoload))
  (should (get 'mediawiki-draft 'autoload))
  (should (get 'mediawiki-mode 'autoload))

  ;; Test that autoloaded functions are interactive
  (should (commandp 'mediawiki-site))
  (should (commandp 'mediawiki-do-login))
  (should (commandp 'mediawiki-draft))
  (should (commandp 'mediawiki-mode)))

;;; Test Package Structure

(ert-deftest test-mediawiki-package-structure ()
  "Test that package structure is correct."
  ;; Test that all modules provide their features
  (should (featurep 'mediawiki-core))
  (should (featurep 'mediawiki-utils))
  (should (featurep 'mediawiki-http))
  (should (featurep 'mediawiki-faces))
  (should (featurep 'mediawiki-font-lock))
  (should (featurep 'mediawiki-api))
  (should (featurep 'mediawiki-auth))
  (should (featurep 'mediawiki-site))
  (should (featurep 'mediawiki-page))
  (should (featurep 'mediawiki-draft))
  (should (featurep 'mediawiki-mode))
  (should (featurep 'mediawiki))

  ;; Test that main package loads all dependencies
  (should (featurep 'mediawiki)))

(provide 'test-mediawiki-integration)

;;; test-mediawiki-integration.el ends here
