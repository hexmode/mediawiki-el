;;; mediawiki-font-lock.el --- Font-lock keywords and syntax highlighting for MediaWiki mode -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2025 Mark A. Hershberger

;; Author: Mark A. Hershberger <mah@everybody.org>
;; URL: https://github.com/hexmode/mediawiki-el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains font-lock keywords and syntax highlighting rules for MediaWiki markup.
;; It defines patterns for highlighting various MediaWiki elements like headers, links,
;; templates, tags, and other markup constructs.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-faces)

;;; Tag Definitions

(defvar mediawiki-simple-tags
  '("b" "big" "blockquote" "br" "caption" "code" "center" "cite" "del"
     "dfn" "dl" "em" "i" "ins" "kbd" "math" "nowiki" "ol" "pre" "samp"
     "small" "strike" "strong" "sub" "sup" "tt" "u" "ul" "var")
  "Tags that do not accept arguments.")

(defvar mediawiki-complex-tags
  '("a" "div" "font" "table" "td" "th" "tr")
  "Tags that accept arguments.")

;;; URL Protocol Definitions

(defvar mediawiki-url-protocols
  '("ftp" "gopher" "http" "https" "mailto" "news")
  "Valid protocols for URLs in Wikipedia articles.")

;;; Font-Lock Keywords

(defvar mediawiki-font-lock-keywords
  (list

    ;; Apostrophe-style text markup
    (cons "''''\\([^']\\|[^']'\\)*?\\(''''\\|\n\n\\)"
      'font-lock-builtin-face)
    (cons "'''\\([^']\\|[^']'\\)*?\\('''\\|\n\n\\)"
                                        ;'font-lock-builtin-face)
      'font-mediawiki-bold-face)
    (cons "''\\([^']\\|[^']'\\)*?\\(''\\|\n\n\\)"
      'font-mediawiki-italic-face)

    ;; Headers and dividers
    (list "^\\(==+\\)\\(.*\\)\\(\\1\\)"
      '(1 font-lock-builtin-face)
                                        ;'(2 mediawiki-header-face)
      '(2 font-mediawiki-sedate-face)
      '(3 font-lock-builtin-face))
    (cons "^-----*" 'font-lock-builtin-face)

    ;; Bare URLs and ISBNs
    (cons (concat "\\(^\\| \\)" (regexp-opt mediawiki-url-protocols t)
            "://[-A-Za-z0-9._\/~%+&#?!=()@]+")
      'font-lock-variable-name-face)
    (cons "\\(^\\| \\)ISBN [-0-9A-Z]+" 'font-lock-variable-name-face)

    ;; Colon indentation, lists, definitions, and tables
    (cons "^\\(:+\\|[*#]+\\||[}-]?\\|{|\\)" 'font-lock-builtin-face)
    (list "^\\(;\\)\\([^:\n]*\\)\\(:?\\)"
      '(1 font-lock-builtin-face)
      '(2 font-lock-keyword-face)
      '(3 font-lock-builtin-face))

    ;; Tags and comments
    (list (concat "\\(</?\\)"
            (regexp-opt mediawiki-simple-tags t) "\\(>\\)")
      '(1 font-lock-builtin-face t t)
      '(2 font-lock-function-name-face t t)
      '(3 font-lock-builtin-face t t))
    (list (concat "\\(</?\\)"
            (regexp-opt mediawiki-complex-tags t)
            "\\(\\(?: \\(?:[^\"'/><]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?\\)\\(>\\)")
      '(1 font-lock-builtin-face t t)
      '(2 font-lock-function-name-face t t)
      '(3 font-lock-keyword-face t t)
      '(4 font-lock-builtin-face t t))
    (cons (concat "<!-- \\([^->]\\|>\\|-\\([^-]\\|-[^>]\\)\\)*-->")
      '(0 font-lock-comment-face t t))

    ;; External Links
    (list (concat "\\(\\[\\)\\(\\(?:"
            (regexp-opt mediawiki-url-protocols)
            "\\)://[-A-Za-z0-9._\/~%-+&#?!=()@]+\\)\\(\\(?: [^]\n]*\\)?\\)\\(\\]\\)")
      '(1 font-lock-builtin-face t t)
      '(2 font-lock-variable-name-face t t)
      '(3 font-lock-keyword-face t t)
      '(4 font-lock-builtin-face t t))

    ;; Wiki links
    '("\\(\\[\\[\\)\\([^]\n|]*\\)\\(|?\\)\\([^]\n]*\\)\\(\\]\\]\\)"
       (1 font-lock-builtin-face t t)
       (2 font-lock-variable-name-face t t)
       (3 font-lock-builtin-face t t)
       (4 font-lock-keyword-face t t)
       (5 font-lock-builtin-face t t))

    ;; Semantic relations
    '("\\(\\[\\[\\)\\([^]\n|]*\\)\\(::\\)\\([^]\n|]*\\)\\(|?\\)\\([^]\n]*\\)\\(\\]\\]\\)"
       (1 font-lock-builtin-face t t)
       (2 font-lock-variable-name-face t t)
       (3 font-lock-builtin-face t t)
       (4 font-lock-constant-face t t)
       (5 font-lock-builtin-face t t)
       (6 font-lock-keyword-face t t)
       (7 font-lock-builtin-face t t))

    ;; Wiki variables
    '("\\({{\\)\\(.+?\\)\\(}}\\)"
       (1 font-lock-builtin-face t t)
       (2 font-lock-variable-name-face t t)
       (3 font-lock-builtin-face t t))

    ;; Semantic variables
    '("\\({{{\\)\\(.+?\\)\\(}}}\\)"
       (1 font-lock-builtin-face t t)
       (2 font-lock-variable-name-face t t)
       (3 font-lock-builtin-face t t))

    ;; Character entity references
    (cons "&#?[a-zA-Z0-9]+;" '(0 font-lock-type-face t t))

    ;; Preformatted text
    (cons "^ .*$" '(0 font-lock-constant-face t t))

    ;; Math environment (uniform highlight only, no TeX markup)
    (list "<math>\\(\\(\n?.\\)*?\\)</math>"
      '(1 font-lock-keyword-face t t)))
  "Font-lock keywords for MediaWiki mode.")

(provide 'mediawiki-font-lock)

;;; mediawiki-font-lock.el ends here
