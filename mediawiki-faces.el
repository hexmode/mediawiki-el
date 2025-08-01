;;; mediawiki-faces.el --- Font-lock face definitions for MediaWiki mode -*- lexical-binding: t; -*-

;; Copyright (C) 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017 Mark A. Hershberger

;; Author: Mark A. Hershberger <mah@everybody.org>
;; Version: 2.2.9
;; Created: Sep 30 2008
;; Keywords: mediawiki wikipedia network wiki
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

;; This file contains font-lock face definitions for MediaWiki syntax highlighting.
;; It provides faces for bold, italic, math, verbatim, and other MediaWiki markup elements.

;;; Code:

(require 'mediawiki-core)

;;; Customization Groups

(defgroup font-mediawiki-highlighting-faces nil
  "Faces for highlighting MediaWiki markup."
  :group 'mediawiki
  :prefix "font-mediawiki-")

;;; Face Variables

(defvar font-mediawiki-sedate-face 'font-mediawiki-sedate-face
  "Face to use for mediawiki minor keywords.")

(defvar font-mediawiki-italic-face 'font-mediawiki-italic-face
  "Face to use for mediawiki italics.")

(defvar font-mediawiki-bold-face 'font-mediawiki-bold-face
  "Face to use for mediawiki bolds.")

(defvar font-mediawiki-math-face 'font-mediawiki-math-face
  "Face to use for mediawiki math environments.")

(defvar font-mediawiki-string-face 'font-mediawiki-string-face
  "Face to use for strings.")

(defvar font-mediawiki-verbatim-face 'font-mediawiki-verbatim-face
  "Face to use for text in verbatim macros or environments.")

;;; Face Definitions

(defface font-mediawiki-bold-face
    `((((class grayscale) (background light))
       :foreground "DimGray"
       :weight bold)
      (((class grayscale) (background dark))
       :foreground "LightGray"
       :weight bold)
      (((class color) (background light))
       :foreground "DarkOliveGreen"
       :weight bold)
      (((class color) (background dark))
       :foreground "OliveDrab"
       :weight bold)
      (t
       :weight bold))
  "Face used to highlight text to be typeset in bold."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-italic-face
  `((((class grayscale) (background light))
     :foreground "DimGray"
     :slant italic)
    (((class grayscale) (background dark))
     :foreground "LightGray"
     :slant italic)
    (((class color) (background light))
     :foreground "DarkOliveGreen"
     :slant italic)
    (((class color) (background dark))
     :foreground "OliveDrab"
     :slant italic)
    (t
     :slant italic))
  "Face used to highlight text to be typeset in italic."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-math-face
  `((((class grayscale) (background light))
     :foreground "DimGray"
     :underline t)
    (((class grayscale) (background dark))
     :foreground "LightGray"
     :underline t)
    (((class color) (background light))
     :foreground "SaddleBrown"
     :underline t)
    (((class color) (background dark))
     :foreground "burlywood"
     :underline t)
    (t
     :underline t))
  "Face used to highlight math."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-sedate-face
  '((((class grayscale) (background light))
     :foreground "DimGray")
    (((class grayscale) (background dark))
     :foreground "LightGray")
    (((class color) (background light))
     :foreground "DimGray")
    (((class color) (background dark))
     :foreground "LightGray"))
  "Face used to highlight sedate stuff."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-string-face
  `((((type tty) (class color))
     :foreground "green"
     :slant italic)
    (((class grayscale) (background light))
     :foreground "DimGray"
     :slant italic)
    (((class grayscale) (background dark))
     :foreground "LightGray"
     :slant italic)
    (((class color) (background light))
     :foreground "RosyBrown"
     :slant italic)
    (((class color) (background dark))
     :foreground "LightSalmon"
     :slant italic)
    (t
     :slant italic))
  "Face used to highlight strings."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-warning-face
  `((((class grayscale)(background light))
     :foreground "DimGray"
     :weight bold)
    (((class grayscale)(background dark))
     :foreground "LightGray"
     :weight bold)
    (((class color)(background light))
     :foreground "red"
     :weight bold)
    (((class color)(background dark))
     :foreground "red"
     :weight bold)
    (t
     :weight bold))
  "Face for important keywords."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-verbatim-face
  `((((class grayscale) (background light))
     :foreground "DimGray"
     :inherit fixed-pitch)
    (((class grayscale) (background dark))
     :foreground "LightGray"
     :inherit fixed-pitch)
    (((class color) (background light))
     :foreground "SaddleBrown"
     :inherit fixed-pitch)
    (((class color) (background dark))
     :foreground "burlywood"
     :inherit fixed-pitch)
    (t
     :inherit fixed-pitch))
  "Face used to highlight TeX verbatim environments."
  :group 'font-mediawiki-highlighting-faces)

(provide 'mediawiki-faces)

;;; mediawiki-faces.el ends here
