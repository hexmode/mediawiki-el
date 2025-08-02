;;; test-mediawiki-faces.el --- Unit tests for mediawiki-faces.el -*- lexical-binding: t; -*-

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

;; Unit tests for mediawiki-faces.el module.

;;; Code:

(require 'ert)
(require 'mediawiki-faces)

;;; Test Face Variables

(ert-deftest test-mediawiki-face-variables ()
  "Test that face variables are properly defined."
  (should (boundp 'font-mediawiki-sedate-face))
  (should (boundp 'font-mediawiki-italic-face))
  (should (boundp 'font-mediawiki-bold-face))
  (should (boundp 'font-mediawiki-math-face))
  (should (boundp 'font-mediawiki-string-face))
  (should (boundp 'font-mediawiki-verbatim-face))

  ;; Test that variables point to the correct face symbols
  (should (eq 'font-mediawiki-sedate-face font-mediawiki-sedate-face))
  (should (eq 'font-mediawiki-italic-face font-mediawiki-italic-face))
  (should (eq 'font-mediawiki-bold-face font-mediawiki-bold-face))
  (should (eq 'font-mediawiki-math-face font-mediawiki-math-face))
  (should (eq 'font-mediawiki-string-face font-mediawiki-string-face))
  (should (eq 'font-mediawiki-verbatim-face font-mediawiki-verbatim-face)))

;;; Test Face Definitions

(ert-deftest test-mediawiki-face-definitions ()
  "Test that faces are properly defined."
  (should (facep 'font-mediawiki-bold-face))
  (should (facep 'font-mediawiki-italic-face))
  (should (facep 'font-mediawiki-math-face))
  (should (facep 'font-mediawiki-sedate-face))
  (should (facep 'font-mediawiki-string-face))
  (should (facep 'font-mediawiki-warning-face))
  (should (facep 'font-mediawiki-verbatim-face)))

;;; Test Face Properties

(ert-deftest test-mediawiki-bold-face-properties ()
  "Test properties of font-mediawiki-bold-face."
  (let ((face-spec (face-all-attributes 'font-mediawiki-bold-face)))
    ;; The face should have weight property set to bold
    (should (or (eq (plist-get face-spec :weight) 'bold)
                ;; Face might inherit bold from display properties
                (face-bold-p 'font-mediawiki-bold-face)))))

(ert-deftest test-mediawiki-italic-face-properties ()
  "Test properties of font-mediawiki-italic-face."
  (let ((face-spec (face-all-attributes 'font-mediawiki-italic-face)))
    ;; The face should have slant property set to italic
    (should (or (eq (plist-get face-spec :slant) 'italic)
                ;; Face might inherit italic from display properties
                (face-italic-p 'font-mediawiki-italic-face)))))

(ert-deftest test-mediawiki-math-face-properties ()
  "Test properties of font-mediawiki-math-face."
  (let ((face-spec (face-all-attributes 'font-mediawiki-math-face)))
    ;; The face should have underline property
    (should (or (plist-get face-spec :underline)
                ;; Check if face has underline through face-attribute
                (face-attribute 'font-mediawiki-math-face :underline)))))

(ert-deftest test-mediawiki-verbatim-face-properties ()
  "Test properties of font-mediawiki-verbatim-face."
  ;; The verbatim face should inherit from fixed-pitch
  (let ((inherit (face-attribute 'font-mediawiki-verbatim-face :inherit)))
    (should (or (eq inherit 'fixed-pitch)
                (and (listp inherit) (memq 'fixed-pitch inherit))))))

;;; Test Customization Groups

(ert-deftest test-mediawiki-faces-customization-group ()
  "Test that font-mediawiki-highlighting-faces group is defined."
  (should (get 'font-mediawiki-highlighting-faces 'group-documentation)))

;;; Test Face Inheritance and Display

(ert-deftest test-mediawiki-faces-display ()
  "Test that faces can be displayed without errors."
  ;; This test ensures faces don't cause display errors
  (dolist (face '(font-mediawiki-bold-face
                  font-mediawiki-italic-face
                  font-mediawiki-math-face
                  font-mediawiki-sedate-face
                  font-mediawiki-string-face
                  font-mediawiki-warning-face
                  font-mediawiki-verbatim-face))
    (should (facep face))
    ;; Test that face-attribute doesn't error
    (should (face-attribute face :foreground nil t))))

;;; Test Face Documentation

(ert-deftest test-mediawiki-faces-documentation ()
  "Test that faces have proper documentation."
  (should (documentation-property 'font-mediawiki-bold-face 'face-documentation))
  (should (documentation-property 'font-mediawiki-italic-face 'face-documentation))
  (should (documentation-property 'font-mediawiki-math-face 'face-documentation))
  (should (documentation-property 'font-mediawiki-sedate-face 'face-documentation))
  (should (documentation-property 'font-mediawiki-string-face 'face-documentation))
  (should (documentation-property 'font-mediawiki-warning-face 'face-documentation))
  (should (documentation-property 'font-mediawiki-verbatim-face 'face-documentation)))

(provide 'test-mediawiki-faces)

;;; test-mediawiki-faces.el ends here
