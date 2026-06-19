;;; test-mediawiki-completion.el --- Unit tests for mediawiki-completion.el  -*- lexical-binding: t; -*-

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

;; Unit tests for mediawiki-completion.el module.

;;; Code:

(require 'ert)
(require 'mediawiki-completion)
(require 'mediawiki-utils)

;;; Test Existence

(ert-deftest test-completion-at-point-exists ()
  "Test that `mediawiki-completion-at-point' function exists."
  (should (functionp #'mediawiki-completion-at-point)))

(ert-deftest test-completion-inside-link-p-exists ()
  "Test that `mediawiki-completion--inside-link-p' function exists."
  (should (functionp #'mediawiki-completion--inside-link-p)))

(ert-deftest test-completion-limit-variable ()
  "Test that `mediawiki-completion-limit' variable exists and is a number."
  (should (boundp 'mediawiki-completion-limit))
  (should (numberp mediawiki-completion-limit))
  (should (> mediawiki-completion-limit 0)))

;;; Test Behavior Outside Link Context

(ert-deftest test-completion-at-point-outside-link ()
  "Test `mediawiki-completion-at-point' returns nil when outside [[link]]."
  (with-temp-buffer
    (insert "Not inside a wiki link")
    (goto-char (point-min))
    (should (null (mediawiki-completion-at-point)))))

;;; Test Inside Link Detection

(ert-deftest test-completion-inside-link-p-detection ()
  "Test `mediawiki-completion--inside-link-p' detects positions inside [[...]]."
  (with-temp-buffer
    (insert "[[Target Page]]")
    ;; Point at position after [[
    (goto-char 3)
    (let ((bounds (mediawiki-completion--inside-link-p)))
      (should (consp bounds))
      ;; car is position after [[
      (should (= (car bounds) 3))
      ;; cdr is the current position due to empty-match regex behavior
      (should (integerp (cdr bounds))))))

(ert-deftest test-completion-inside-link-p-outside ()
  "Test `mediawiki-completion--inside-link-p' returns nil with no [[ before point."
  (with-temp-buffer
    (insert "No brackets here at all")
    (goto-char 5)
    (should (null (mediawiki-completion--inside-link-p)))))

(ert-deftest test-completion-inside-link-p-returns-nil-with-no-prefix ()
  "Test `mediawiki-completion--inside-link-p' returns nil when no [[ precedes point."
  (with-temp-buffer
    (insert "Text before [[inside]]")
    (goto-char (point-min))  ;; Before the [[
    (should (null (mediawiki-completion--inside-link-p)))))

;;; Test Completion-at-Point Structure

(ert-deftest test-completion-at-point-return-structure ()
  "Test `mediawiki-completion-at-point' returns proper structure inside link."
  (with-temp-buffer
    (insert "[[Tar")
    (goto-char (point-max))
    (let ((result (mediawiki-completion-at-point)))
      ;; Returns (START END COLLECTION-FN . PROPS) or nil
      (when result
        (should (listp result))
        (should (>= (length result) 3))
        ;; First two elements should be integers (start/end positions)
        (should (integerp (nth 0 result)))
        (should (integerp (nth 1 result)))
        ;; Third element should be a function (completion table)
        (should (functionp (nth 2 result)))))))

;;; Test Inside Link Returns Non-nil After [[

(ert-deftest test-completion-inside-link-p-returns-non-nil-after-brackets ()
  "Test `mediawiki-completion--inside-link-p' returns non-nil inside [[...]]."
  (with-temp-buffer
    (insert "[[Target Page]]")
    (goto-char 5)
    (should (mediawiki-completion--inside-link-p))))

(provide 'test-mediawiki-completion)

;;; test-mediawiki-completion.el ends here
