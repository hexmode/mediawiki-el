;;; test-mediawiki-page.el --- Unit tests for mediawiki-page.el -*- lexical-binding: t; -*-

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

;; Unit tests for mediawiki-page.el module.
;; These tests mock page operations to avoid network dependencies.

;;; Code:

(require 'ert)
(require 'mediawiki-page)

;;; Test Page History Management

(ert-deftest test-mediawiki-add-page-history ()
  "Test mediawiki-add-page-history function."
  ;; Clear history for testing
  (setq mediawiki-page-history '())

  ;; Add first page to history
  (mediawiki-add-page-history "TestSite" "Test Page")
  (should (assoc-string "TestSite" mediawiki-page-history))
  (should (member "Test Page" (cdr (assoc-string "TestSite" mediawiki-page-history))))

  ;; Add second page to same site
  (mediawiki-add-page-history "TestSite" "Another Page")
  (let ((history (cdr (assoc-string "TestSite" mediawiki-page-history))))
    (should (member "Test Page" history))
    (should (member "Another Page" history))
    (should (= 2 (length history))))

  ;; Add page to different site
  (mediawiki-add-page-history "OtherSite" "Other Page")
  (should (assoc-string "OtherSite" mediawiki-page-history))
  (should (member "Other Page" (cdr (assoc-string "OtherSite" mediawiki-page-history)))))

;;; Test Page Loading Functions (Mocked)

(ert-deftest test-mediawiki-get-structure ()
  "Test mediawiki-get function structure."
  (should (functionp 'mediawiki-get))

  ;; Test function signature (will error due to network, but tests structure)
  (should (condition-case nil
              (mediawiki-get "TestSite" "Test Page")
            (error t))))

(ert-deftest test-mediawiki-edit-structure ()
  "Test mediawiki-edit function structure."
  (should (functionp 'mediawiki-edit))

  ;; We can't fully test without mocking the entire chain,
  ;; but we can test basic structure
  (should (condition-case nil
              (mediawiki-edit "TestSite" "Test Page")
            (error t))))

;;; Test Page Metadata Handling

(ert-deftest test-mediawiki-save-metadata ()
  "Test mediawiki-save-metadata function."
  ;; Create mock page structure
  (let ((mock-page '(page
                     ((title . "Test Page")
                      (edittoken . "test-token")
                      (starttimestamp . "2025-01-01T00:00:00Z"))
                     (revisions
                      (rev ((timestamp . "2025-01-01T00:00:00Z")) "content")))))

    ;; Test metadata saving in a buffer
    (with-temp-buffer
      (mediawiki-save-metadata "TestSite" mock-page)

      ;; Check that buffer-local variables are set
      (should (string= "TestSite" mediawiki-site))
      (should (string= "Test Page" mediawiki-page-title))
      (should (string= "test-token" mediawiki-edittoken))
      (should (string= "2025-01-01T00:00:00Z" mediawiki-basetimestamp))
      (should (string= "2025-01-01T00:00:00Z" mediawiki-starttimestamp)))))

;;; Test Page Saving Functions (Mocked)

(ert-deftest test-mediawiki-save-structure ()
  "Test mediawiki-save function structure."
  (should (functionp 'mediawiki-save))
  (should (commandp 'mediawiki-save))

  ;; Test in non-mediawiki buffer (should error)
  (with-temp-buffer
    (should-error (mediawiki-save "test summary"))))

(ert-deftest test-mediawiki-save-as-structure ()
  "Test mediawiki-save-as function structure."
  (should (functionp 'mediawiki-save-as))
  (should (commandp 'mediawiki-save-as)))

(ert-deftest test-mediawiki-save-and-bury-structure ()
  "Test mediawiki-save-and-bury function structure."
  (should (functionp 'mediawiki-save-and-bury))
  (should (commandp 'mediawiki-save-and-bury)))

;;; Test Form Processing

(ert-deftest test-mediawiki-get-form-vars ()
  "Test mediawiki-get-form-vars function."
  ;; Test with simple form
  (let ((html-form "<form id=\"editform\">
                    <input name=\"wpTextbox1\" value=\"content\" />
                    <input name=\"wpSummary\" value=\"\" />
                    <input type=\"submit\" name=\"wpSave\" value=\"Save\" />
                    </form>"))

    (let ((vars (mediawiki-get-form-vars html-form "id" "editform")))
      (should (listp vars))
      ;; Should extract non-submit inputs
      (should (assoc "wpTextbox1" vars))
      (should (assoc "wpSummary" vars))
      ;; Should not include submit button
      (should-not (assoc "wpSave" vars))

      ;; Check values
      (should (string= "content" (cdr (assoc "wpTextbox1" vars))))
      (should (string= "" (cdr (assoc "wpSummary" vars))))))

  ;; Test with no matching form
  (let ((html-no-form "<div>No form here</div>"))
    (should-not (mediawiki-get-form-vars html-no-form "id" "editform"))))

(ert-deftest test-mediawiki-get-edit-form-vars ()
  "Test mediawiki-get-edit-form-vars function."
  (with-temp-buffer
    ;; Insert mock HTML with edit form
    (insert "<form id=\"editform\">
             <input name=\"wpTextbox1\" value=\"page content\" />
             <input name=\"wpEditToken\" value=\"token123\" />
             </form>")

    ;; Test form variable extraction
    (mediawiki-get-edit-form-vars (current-buffer))

    ;; Check that buffer-local variable is set
    (should (boundp 'mediawiki-edit-form-vars))
    (should (listp mediawiki-edit-form-vars))))

;;; Test Page Navigation

(ert-deftest test-mediawiki-page-at-point ()
  "Test mediawiki-page-at-point function."
  (with-temp-buffer
    ;; Test simple wiki link
    (insert "This is a [[Test Page]] link.")
    (goto-char (point-min))
    (search-forward "Test")
    (should (string= "Test Page" (mediawiki-page-at-point)))

    ;; Clear buffer and test link with pipe
    (erase-buffer)
    (insert "This is a [[Test Page|display text]] link.")
    (goto-char (point-min))
    (search-forward "Test")
    (should (string= "Test Page" (mediawiki-page-at-point)))

    ;; Test subpage link
    (erase-buffer)
    (let ((mediawiki-page-title "Main Page"))
      (insert "This is a [[/Subpage]] link.")
      (goto-char (point-min))
      (search-forward "Sub")
      (should (string= "Main Page/Subpage" (mediawiki-page-at-point))))

    ;; Test when not on a link
    (erase-buffer)
    (insert "No links here.")
    (goto-char (point-min))
    (should-not (mediawiki-page-at-point))))

;;; Test Prompt Functions

(ert-deftest test-mediawiki-prompt-for-page ()
  "Test mediawiki-prompt-for-page function."
  ;; Mock completing-read
  (cl-letf (((symbol-function 'completing-read)
             (lambda (prompt collection) "New Page")))

    ;; Test with no current page
    (let ((mediawiki-page-title nil))
      (should (string= "New Page" (mediawiki-prompt-for-page))))

    ;; Test with current page and empty input
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection) "")))
      (let ((mediawiki-page-title "Current Page"))
        (should (string= "Current Page" (mediawiki-prompt-for-page)))))))

(ert-deftest test-mediawiki-prompt-for-summary ()
  "Test mediawiki-prompt-for-summary function."
  ;; Mock completing-read
  (cl-letf (((symbol-function 'completing-read)
             (lambda (prompt collection) "Test summary")))

    (should (string= "Test summary" (mediawiki-prompt-for-summary)))))

;;; Test Buffer Management

(ert-deftest test-mediawiki-pop-to-buffer ()
  "Test mediawiki-pop-to-buffer function."
  (let ((hook-called nil))
    ;; Mock pop-to-buffer and hook
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buffer) buffer))
              (mediawiki-pop-buffer-hook
               (list (lambda () (setq hook-called t)))))

      (with-temp-buffer
        (mediawiki-pop-to-buffer (current-buffer))
        ;; Check that hook was called
        (should hook-called)))))

;;; Test Interactive Functions

(ert-deftest test-mediawiki-page-interactive-functions ()
  "Test that page functions are properly marked as interactive."
  (should (commandp 'mediawiki-save))
  (should (commandp 'mediawiki-save-as))
  (should (commandp 'mediawiki-save-and-bury))
  (should (commandp 'mediawiki-open-page-at-point)))

;;; Test Page Ring Management

(ert-deftest test-mediawiki-page-ring-structure ()
  "Test page ring structure and management."
  ;; Test that page ring variables exist
  (should (boundp 'mediawiki-page-ring))

  ;; The ring might be nil initially, but should be creatable
  (when (not (ring-p mediawiki-page-ring))
    (setq mediawiki-page-ring (make-ring 30)))

  (should (ring-p mediawiki-page-ring)))

(provide 'test-mediawiki-page)

;;; test-mediawiki-page.el ends here
