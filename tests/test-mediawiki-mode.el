;;; test-mediawiki-mode.el --- Unit tests for mediawiki-mode.el -*- lexical-binding: t; -*-

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

;; Unit tests for mediawiki-mode.el module.

;;; Code:

(require 'ert)
(require 'mediawiki-mode)

;;; Test Mode Variables

(ert-deftest test-mediawiki-mode-variables ()
  "Test that mode variables are properly defined."
  (should (boundp 'mediawiki-enumerate-with-terminate-paragraph))
  (should (boundp 'mediawiki-english-or-german))
  (should (boundp 'mediawiki-user-simplify-signature))
  (should (boundp 'mediawiki-reply-with-hline))
  (should (boundp 'mediawiki-reply-with-quote))
  (should (boundp 'mediawiki-imenu-generic-expression))

  ;; Test types
  (should (listp mediawiki-imenu-generic-expression)))

;;; Test Keymap

(ert-deftest test-mediawiki-mode-keymap ()
  "Test that mediawiki-mode keymap is properly defined."
  (should (boundp 'mediawiki-mode-map))
  (should (keymapp mediawiki-mode-map))

  ;; Test some key bindings exist
  (should (lookup-key mediawiki-mode-map "\M-n"))
  (should (lookup-key mediawiki-mode-map "\M-p"))
  (should (lookup-key mediawiki-mode-map "\C-c\C-f\C-b"))
  (should (lookup-key mediawiki-mode-map "\C-c\C-f\C-i")))

;;; Test Interactive Commands

(ert-deftest test-mediawiki-next-header ()
  "Test mediawiki-next-header function."
  (should (functionp 'mediawiki-next-header))
  (should (get 'mediawiki-next-header 'autoload))
  (should (commandp 'mediawiki-next-header))

  (with-temp-buffer
    (insert "Some text\n== Header 1 ==\nContent\n== Header 2 ==\nMore content")
    (goto-char (point-min))
    (mediawiki-next-header)
    (should (looking-at "== Header 1 =="))))

(ert-deftest test-mediawiki-prev-header ()
  "Test mediawiki-prev-header function."
  (should (functionp 'mediawiki-prev-header))
  (should (get 'mediawiki-prev-header 'autoload))
  (should (commandp 'mediawiki-prev-header))

  (with-temp-buffer
    (insert "== Header 1 ==\nContent\n== Header 2 ==\nMore content")
    (goto-char (point-max))
    (mediawiki-prev-header)
    (should (looking-at "== Header 2 =="))))

(ert-deftest test-mediawiki-terminate-paragraph ()
  "Test mediawiki-terminate-paragraph function."
  (should (functionp 'mediawiki-terminate-paragraph))
  (should (get 'mediawiki-terminate-paragraph 'autoload))
  (should (commandp 'mediawiki-terminate-paragraph))

  (with-temp-buffer
    (insert "Some text")
    (mediawiki-terminate-paragraph)
    (should (string-match-p "\n\n$" (buffer-string)))))

;;; Test Text Formatting Functions

(ert-deftest test-mediawiki-insert-bold ()
  "Test mediawiki-insert-bold function."
  (should (functionp 'mediawiki-insert-bold))
  (should (get 'mediawiki-insert-bold 'autoload))
  (should (commandp 'mediawiki-insert-bold))

  (with-temp-buffer
    (mediawiki-insert-bold)
    (should (string= "''' '''" (buffer-string)))
    (should (= 4 (point)))))

(ert-deftest test-mediawiki-insert-italics ()
  "Test mediawiki-insert-italics function."
  (should (functionp 'mediawiki-insert-italics))
  (should (get 'mediawiki-insert-italics 'autoload))
  (should (commandp 'mediawiki-insert-italics))

  (with-temp-buffer
    (mediawiki-insert-italics)
    (should (string= "'' ''" (buffer-string)))
    (should (= 3 (point)))))

(ert-deftest test-mediawiki-insert-strong-emphasis ()
  "Test mediawiki-insert-strong-emphasis function."
  (should (functionp 'mediawiki-insert-strong-emphasis))
  (should (get 'mediawiki-insert-strong-emphasis 'autoload))
  (should (commandp 'mediawiki-insert-strong-emphasis))

  (with-temp-buffer
    (mediawiki-insert-strong-emphasis)
    (should (string= "'''' ''''" (buffer-string)))
    (should (= 5 (point)))))

(ert-deftest test-mediawiki-insert-header ()
  "Test mediawiki-insert-header function."
  (should (functionp 'mediawiki-insert-header))
  (should (get 'mediawiki-insert-header 'autoload))
  (should (commandp 'mediawiki-insert-header))

  (with-temp-buffer
    (mediawiki-insert-header)
    (should (string= "== ==" (buffer-string)))
    (should (= 3 (point)))))

(ert-deftest test-mediawiki-insert-link ()
  "Test mediawiki-insert-link function."
  (should (functionp 'mediawiki-insert-link))
  (should (get 'mediawiki-insert-link 'autoload))
  (should (commandp 'mediawiki-insert-link))

  (with-temp-buffer
    (mediawiki-insert-link)
    (should (string= "[[ ]]" (buffer-string)))
    (should (= 3 (point)))))

;;; Test Text Formatting with Regions

(ert-deftest test-mediawiki-insert-bold-with-region ()
  "Test mediawiki-insert-bold with active region."
  (with-temp-buffer
    (insert "selected text")
    (mark-whole-buffer)
    (mediawiki-insert-bold)
    (should (string= "'''selected text'''" (buffer-string)))))

(ert-deftest test-mediawiki-insert-italics-with-region ()
  "Test mediawiki-insert-italics with active region."
  (with-temp-buffer
    (insert "selected text")
    (mark-whole-buffer)
    (mediawiki-insert-italics)
    (should (string= "''selected text''" (buffer-string)))))

;;; Test List Functions

(ert-deftest test-mediawiki-insert-enumerate ()
  "Test mediawiki-insert-enumerate function."
  (should (functionp 'mediawiki-insert-enumerate))
  (should (get 'mediawiki-insert-enumerate 'autoload))
  (should (commandp 'mediawiki-insert-enumerate))

  (with-temp-buffer
    (let ((mediawiki-enumerate-with-terminate-paragraph nil))
      (mediawiki-insert-enumerate)
      (should (string-match-p ":#$" (buffer-string))))))

(ert-deftest test-mediawiki-insert-itemize ()
  "Test mediawiki-insert-itemize function."
  (should (functionp 'mediawiki-insert-itemize))
  (should (get 'mediawiki-insert-itemize 'autoload))
  (should (commandp 'mediawiki-insert-itemize))

  (with-temp-buffer
    (let ((mediawiki-enumerate-with-terminate-paragraph nil))
      (mediawiki-insert-itemize)
      (should (string-match-p ":\\*$" (buffer-string))))))

;;; Test Other Insert Functions

(ert-deftest test-mediawiki-insert-signature ()
  "Test mediawiki-insert-signature function."
  (should (functionp 'mediawiki-insert-signature))
  (should (get 'mediawiki-insert-signature 'autoload))
  (should (commandp 'mediawiki-insert-signature))

  (with-temp-buffer
    (mediawiki-insert-signature)
    (should (string= "~~~~: " (buffer-string)))))

(ert-deftest test-mediawiki-insert-hline ()
  "Test mediawiki-insert-hline function."
  (should (functionp 'mediawiki-insert-hline))
  (should (get 'mediawiki-insert-hline 'autoload))
  (should (commandp 'mediawiki-insert-hline))

  (with-temp-buffer
    (mediawiki-insert-hline)
    (should (string= "\n----\n" (buffer-string)))))

;;; Test Fill Functions

(ert-deftest test-mediawiki-fill-article ()
  "Test mediawiki-fill-article function."
  (should (functionp 'mediawiki-fill-article))
  (should (get 'mediawiki-fill-article 'autoload))
  (should (commandp 'mediawiki-fill-article)))

(ert-deftest test-mediawiki-unfill-article ()
  "Test mediawiki-unfill-article function."
  (should (functionp 'mediawiki-unfill-article))
  (should (get 'mediawiki-unfill-article 'autoload))
  (should (commandp 'mediawiki-unfill-article))

  (with-temp-buffer
    (insert "Line one\nLine two\n\nParagraph break")
    (mediawiki-unfill-article)
    ;; Should join lines that aren't paragraph breaks
    (should (string-match-p "Line one Line two\n\nParagraph break" (buffer-string)))))

;;; Test Link Navigation

(ert-deftest test-mediawiki-goto-next-link ()
  "Test mediawiki-goto-next-link function."
  (should (functionp 'mediawiki-goto-next-link))
  (should (get 'mediawiki-goto-next-link 'autoload))
  (should (commandp 'mediawiki-goto-next-link))

  (with-temp-buffer
    (insert "Text before [[First Link]] and [[Second Link]] after")
    (goto-char (point-min))
    (mediawiki-goto-next-link)
    (should (= (point) 14)))) ; Should be at "First Link"

(ert-deftest test-mediawiki-goto-prev-link ()
  "Test mediawiki-goto-prev-link function."
  (should (functionp 'mediawiki-goto-prev-link))
  (should (get 'mediawiki-goto-prev-link 'autoload))
  (should (commandp 'mediawiki-goto-prev-link))

  (with-temp-buffer
    (insert "Text before [[First Link]] and [[Second Link]] after")
    (goto-char (point-max))
    (mediawiki-goto-prev-link)
    (should (= (point) 33)))) ; Should be at "Second Link"

;;; Test Major Mode

(ert-deftest test-mediawiki-mode ()
  "Test mediawiki-mode major mode."
  (should (functionp 'mediawiki-mode))
  (should (get 'mediawiki-mode 'autoload))

  (with-temp-buffer
    (mediawiki-mode)

    ;; Test that mode is set correctly
    (should (eq major-mode 'mediawiki-mode))
    (should (string= mode-name "MW"))

    ;; Test that it's derived from text-mode
    (should (eq (get 'mediawiki-mode 'derived-mode-parent) 'text-mode))

    ;; Test that local variables are set
    (should (local-variable-p 'mediawiki-site))
    (should (local-variable-p 'mediawiki-page-title))
    (should (local-variable-p 'mediawiki-edittoken))

    ;; Test font-lock setup
    (should (local-variable-p 'font-lock-defaults))
    (should font-lock-defaults)

    ;; Test outline support
    (should (local-variable-p 'outline-regexp))
    (should (string= outline-regexp "==+"))))

;;; Test Mode Hooks

(ert-deftest test-mediawiki-mode-hooks ()
  "Test that mode hooks are properly configured."
  ;; Test that outline-minor-mode hook is added
  (should (member 'mediawiki-outline-magic-keys outline-minor-mode-hook)))

;;; Test Utility Functions

(ert-deftest test-mediawiki-link-fill-nobreak-p ()
  "Test mediawiki-link-fill-nobreak-p function."
  (should (functionp 'mediawiki-link-fill-nobreak-p))

  (with-temp-buffer
    (mediawiki-mode)

    ;; Test with preformatted text (starts with space)
    (insert " preformatted text")
    (goto-char (point-min))
    (forward-char)
    (should (mediawiki-link-fill-nobreak-p))

    ;; Test with regular text
    (erase-buffer)
    (insert "regular text")
    (goto-char (point-min))
    (should-not (mediawiki-link-fill-nobreak-p))))

;;; Test Page Ring Functions

(ert-deftest test-mediawiki-page-ring-functions ()
  "Test page ring navigation functions."
  (should (functionp 'mediawiki-goto-previous-page))
  (should (get 'mediawiki-goto-previous-page 'autoload))
  (should (commandp 'mediawiki-goto-previous-page))

  (should (functionp 'mediawiki-goto-next-page))
  (should (get 'mediawiki-goto-next-page 'autoload))
  (should (commandp 'mediawiki-goto-next-page)))

;;; Test Outline Functions

(ert-deftest test-mediawiki-outline-functions ()
  "Test outline-related functions."
  (should (functionp 'mediawiki-simple-outline-promote))
  (should (get 'mediawiki-simple-outline-promote 'autoload))
  (should (commandp 'mediawiki-simple-outline-promote))

  (should (functionp 'mediawiki-simple-outline-demote))
  (should (get 'mediawiki-simple-outline-demote 'autoload))
  (should (commandp 'mediawiki-simple-outline-demote))

  (with-temp-buffer
    (insert "== Header ==")
    (goto-char (point-min))
    (mediawiki-simple-outline-demote)
    (should (string= "=== Header ===" (buffer-string)))))

;;; Test Reply Function

(ert-deftest test-mediawiki-reply-at-point-simple ()
  "Test mediawiki-reply-at-point-simple function."
  (should (functionp 'mediawiki-reply-at-point-simple))
  (should (get 'mediawiki-reply-at-point-simple 'autoload))
  (should (commandp 'mediawiki-reply-at-point-simple)))

(provide 'test-mediawiki-mode)

;;; test-mediawiki-mode.el ends here
