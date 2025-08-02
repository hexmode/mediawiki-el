;;; mediawiki-mode.el --- Major mode definition for MediaWiki editing  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2025 Mark A. Hershberger

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

;; This file contains the major mode definition for MediaWiki editing,
;; including the mode definition, keymap, menu definitions, and interactive
;; commands for user interface operations.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-faces)
(require 'mediawiki-font-lock)
(require 'mediawiki-site)
(require 'ring)

;;; Variables

(defvar mediawiki-enumerate-with-terminate-paragraph nil
  "*Before insert enumerate/itemize do \\[mediawiki-terminate-paragraph].")

(defvar mediawiki-english-or-german t
  "*Variable in order to set the english (t) or german (nil) environment.")

(defcustom mediawiki-user-simplify-signature t
  "Simplify other user's signatures."
  :type 'boolean
  :group 'mediawiki)

(defvar mediawiki-reply-with-hline nil
  "*Whether to use a hline as a header seperator in the reply.")

(defvar mediawiki-reply-with-quote nil
  "*Whether to use a quotation tempalate or not.")

(defvar mediawiki-imenu-generic-expression
  (list '(nil "^==+ *\\(.*[^\n=]\\)==+" 1))
  "Imenu expression for `mediawiki-mode'.  See `imenu-generic-expression'.")

;;; Keymap and Menu

(defvar mediawiki-mode-map nil "Keymap for `mediawiki-mode'.")
(progn
  (setq mediawiki-mode-map (make-sparse-keymap "mediawiki"))
  (define-key mediawiki-mode-map [menu-bar mediawiki]
    (cons "MediaWiki" mediawiki-mode-map))
  (define-key mediawiki-mode-map [unfill-article]
    '("Unfill article" . mediawiki-unfill-article))
  (define-key mediawiki-mode-map [fill-article]
    '("Fill article" . mediawiki-fill-article))
  (define-key mediawiki-mode-map [separator-fill] '("--"))
  (define-key mediawiki-mode-map [next-header]
    '("Next header" . mediawiki-next-header))
  (define-key mediawiki-mode-map [prev-header]
    '("Previous header" . mediawiki-prev-header))
  (define-key mediawiki-mode-map [separator-header] '("--"))
  (define-key mediawiki-mode-map [outline]
    '("Toggle Outline Mode..." . outline-minor-mode))

  (define-key mediawiki-mode-map "\M-n" 'mediawiki-next-header)
  (define-key mediawiki-mode-map "\C-c\C-n" 'mediawiki-next-long-line)
  (define-key mediawiki-mode-map "\M-p" 'mediawiki-prev-header)
  (define-key mediawiki-mode-map [(meta down)] 'mediawiki-next-header)
  (define-key mediawiki-mode-map [(meta up)]   'mediawiki-prev-header)
  (define-key mediawiki-mode-map "\C-j" 'mediawiki-terminate-paragraph)

  (define-key mediawiki-mode-map "\C-c\C-q" 'mediawiki-unfill-article)
  (define-key mediawiki-mode-map "\C-c\M-q" 'mediawiki-fill-article)
  (define-key mediawiki-mode-map "\C-c\M-u" 'mediawiki-unfill-paragraph-or-region)
  (define-key mediawiki-mode-map "\C-c\C-u" 'mediawiki-unfill-paragraph-simple)
  (define-key mediawiki-mode-map "\C-c\C-f\C-s" 'mediawiki-insert-strong-emphasis)
  (define-key mediawiki-mode-map "\C-c\C-f\C-b" 'mediawiki-insert-bold)
  (define-key mediawiki-mode-map "\C-c\C-f\C-i" 'mediawiki-insert-italics)
  (define-key mediawiki-mode-map "\C-c\C-f\C-e" 'mediawiki-insert-header)
  (define-key mediawiki-mode-map "\C-c\C-f\C-l" 'mediawiki-insert-link)
  (define-key mediawiki-mode-map "\C-c\C-f\C-u" 'mediawiki-insert-user)
  (define-key mediawiki-mode-map "\C-c\C-f\C-q" 'mediawiki-insert-quotation)
  (define-key mediawiki-mode-map "\C-c\C-f\C-v" 'mediawiki-insert-bible-verse-template)
  (define-key mediawiki-mode-map "\C-c\C-w" 'mediawiki-insert-signature)
  (define-key mediawiki-mode-map "\C-c\C-l" 'mediawiki-insert-hline)
  (define-key mediawiki-mode-map [(meta f8)] 'mediawiki-reply-at-point-simple)
  (define-key mediawiki-mode-map "\C-c\C-r" 'mediawiki-reply-at-point-simple)

  (define-key mediawiki-mode-map [(control meta prior)] 'mediawiki-enhance-indent)
  (define-key mediawiki-mode-map [(control meta next)] 'mediawiki-yank-prefix)
  (define-key mediawiki-mode-map [(meta return)] 'mediawiki-insert-enumerate)
  ;; Note: mediawiki-insert-enumerate-nonewline doesn't exist, removing this binding
  ;; (define-key mediawiki-mode-map [(meta control return)] 'mediawiki-insert-enumerate-nonewline)
  ;; private setting
  (define-key mediawiki-mode-map [(shift return)] 'newline-and-indent)
  (define-key mediawiki-mode-map "\C-\\" 'mediawiki-insert-itemize)
  (define-key mediawiki-mode-map [(control return)] 'mediawiki-insert-itemize)
  (define-key mediawiki-mode-map "\C-ca" 'auto-capitalize-mode)
  ;; (define-key mediawiki-mode-map "\C-ci" 'set-input-method)
  ;; (define-key mediawiki-mode-map "\C-ct" 'toggle-input-method)

  (define-key mediawiki-mode-map [(backtab)] 'mediawiki-goto-prev-link)
  (define-key mediawiki-mode-map [(tab)]     'mediawiki-goto-next-link)
  (define-key mediawiki-mode-map "\M-g"      'mediawiki-reload)
  (define-key mediawiki-mode-map "\C-x\C-s"  'mediawiki-save)
  (define-key mediawiki-mode-map "\C-c\C-c"  'mediawiki-save-and-bury)
  (define-key mediawiki-mode-map "\C-x\C-w"  'mediawiki-save-as)
  (define-key mediawiki-mode-map "\C-c\C-o"  'mediawiki-open)
  (define-key mediawiki-mode-map "\M-p"
    'mediawiki-goto-previous-page)
  (define-key mediawiki-mode-map "\M-n"      'mediawiki-goto-next-page)
  (define-key mediawiki-mode-map [(control return)]
    'mediawiki-open-page-at-point))

;;; Interactive Commands

;;;###autoload
(defun mediawiki-next-header ()
  "Move point to the end of the next section header."
  (interactive)
  (let ((oldpoint (point)))
    (end-of-line)
    (if (re-search-forward "\\(^==+\\).*\\1" (point-max) t)
      (beginning-of-line)
      (goto-char oldpoint)
      (message "No section headers after point."))))

;;;###autoload
(defun mediawiki-prev-header ()
  "Move point to the start of the previous section header."
  (interactive)
  (unless (re-search-backward "\\(^==+\\).*\\1" (point-min) t)
    (message "No section headers before point.")))

;;;###autoload
(defun mediawiki-terminate-paragraph ()	;Version:1.58
  "End a paragraph by any means necessary.
In a list, start a new list item; in a paragraph, start a new paragraph;
if the current paragraph is colon indented, the new
paragraph will be indented in the same way."
  (interactive)
  (let (indent-chars)
    (save-excursion
      (beginning-of-line)
      (while (cond ((looking-at "^$") nil)
               ((looking-at "^\\(\\(?: \\|:+\\|[#*]+\\) *\\)")
                 (setq indent-chars (match-string 1)) nil)
               ((eq (point) (point-min)) nil)
               ((progn (forward-line -1) t)))
        t))
    (newline) (if (not indent-chars) (newline)
		(insert indent-chars))))

;;;###autoload
(defun mediawiki-terminate-paragraph-and-indent ()
  "In a list, start a new list item.
In a paragraph, start a new paragraph but *,# will be ignored; if
the current paragraph is colon indented, the new paragraph will
be indented in the same way."
  (interactive)
  (let (indent-chars)
    (save-excursion
      (beginning-of-line)
      (while (cond ((looking-at "^$") nil)
               ((looking-at "^\\(\\(?: \\|:+\\) *\\)")
                 (setq indent-chars (match-string 1)) nil)
               ((eq (point) (point-min)) nil)
               ((progn (forward-line -1) t)))
        t))
    (newline)
    (if (not indent-chars) (newline)
      (insert indent-chars))))

(defun mediawiki-link-fill-nobreak-p ()
  "When filling, don't break the line for preformatted (fixed-width)
text or inside a Wiki link.  See `fill-nobreak-predicate'."
  (save-excursion
    (let ((pos (point)))
      (or (eq (char-after (line-beginning-position)) ? )
        (if (re-search-backward "\\[\\[" (line-beginning-position) t)
          ;; Break if the link is really really long.
          ;; You often get this with captioned images.
          (null (or (> (- pos (point)) fill-column)
                  (re-search-forward "\\]\\]" pos t))))))))

;;;###autoload
(defun mediawiki-fill-article ()
  "Fill the entire article."
  (interactive)
  (save-excursion
    (fill-region (point-min) (point-max))))

;;;###autoload
(defun mediawiki-unfill-article ()
  "Undo filling, deleting stand-alone newlines.
Stand-alone newlines are those that do not end paragraphs, list
entries, etc."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ".\\(\n\\)\\([^# *;:|!\n]\\|----\\)" nil t)
      (replace-match " " nil nil nil 1)))
  (message "Stand-alone newlines deleted"))

;;;###autoload
(defun mediawiki-reply-at-point-simple ()
  "Very simple function to reply to posts in the discussion forum.
You have to set the point around the signature, then the
functions inserts the following
:'''Re: [[User:foo]]'''."
  (interactive)
  (beginning-of-line 1)
  (if mediawiki-english-or-german
    (progn
      (search-forward "(UTC)")
      (search-backward "[[User:"))
    (search-forward "(CET)")
    (search-backward "[[Benutzer:"))
  (if mediawiki-user-simplify-signature
    (mark-word 2)
    (mark-word 3))
  (copy-to-register ?M (region-beginning) (region-end) nil)
  (end-of-line 1)
  (mediawiki-terminate-paragraph-and-indent)
  (insert ":'''Re: ")
  (insert-register ?M 1)
  (if mediawiki-user-simplify-signature
    (insert "|]]''' ")
    (insert "]]''' ")))

(defvar mediawiki-page-ring nil
  "Ring that holds names of buffers we navigate through.")

(defvar mediawiki-page-ring-index 0)

(defvar mediawiki-page-ring nil
  "Ring that holds names of buffers we navigate through.")

(defvar mediawiki-page-ring-index 0)
(declare-function mediawiki-pop-to-buffer "mediawiki-page")
(defmacro mediawiki-goto-relative-page (direction)
  "Go to the next page in DIRECTION."
  `(let ((buff (ring-ref mediawiki-page-ring
                 (setq mediawiki-page-ring-index
                   (,direction mediawiki-page-ring-index 1)))))
     (while (not (buffer-live-p buff))
       (setq buff
         (ring-ref mediawiki-page-ring
           (setq mediawiki-page-ring-index
             (,direction mediawiki-page-ring-index 1)))))
     (mediawiki-pop-to-buffer buff)))

;;;###autoload
(defun mediawiki-goto-previous-page ()
  "Pop up the previous page being editted."
  (interactive)
  (mediawiki-goto-relative-page -))

;;;###autoload
(defun mediawiki-goto-next-page ()
  "Pop up the previous page being editted."
  (interactive)
  (mediawiki-goto-relative-page +))

(defun mediawiki-goto-relative-link (&optional backward)
  "Move point to a link.
If BACKWARD is t, will search backwards."
  (let* ((search (if backward 're-search-backward
                   're-search-forward))
          (limitfunc (if backward 'point-min
                       'point-max))
          (point (funcall search "\\[\\[.+\\]\\]" (funcall limitfunc) t)))
    (when point
      (let ((point (match-beginning 0)))
        (goto-char (+ point 2))))))

;;;###autoload
(defun mediawiki-goto-next-link ()
  "Go to the next link in the page."
  (interactive)
  (mediawiki-goto-relative-link))

;;;###autoload
(defun mediawiki-goto-prev-link ()
  "Go to the previous link in the page."
  (interactive)
  (mediawiki-goto-relative-link t))

;;;###autoload
(defun mediawiki-insert-enumerate ()
  "Primitive function for inserting enumerated items.
Check the variable mediawiki-enumerate-with-terminate-paragraph.
Note however that the function \\[mediawiki-terminate-paragraph]
does not work very well will longlines-mode."
  (interactive)
  (if mediawiki-enumerate-with-terminate-paragraph
    (progn
      (mediawiki-terminate-paragraph)
      (insert "#"))
    (newline nil)
    (insert ":#")))

;;;###autoload
(defun mediawiki-insert-itemize ()
  "Primitive function for inserting no enumerated items.
Check the variable mediawiki-enumerate-with-terminate-paragraph.
Note however that the function \\[mediawiki-terminate-paragraph]
does not work very well will longlines-mode."
  (interactive)
  (if mediawiki-enumerate-with-terminate-paragraph
    (progn
      (mediawiki-terminate-paragraph)
      (insert "*"))
    (newline nil)
    (insert ":*")))

(defun mediawiki-insert (pre post)
  "Wrap the current region with PRE and POST."
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
        (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
    (let ((beg (region-beginning))
           (end (region-end)))
      (save-excursion
        (goto-char beg)
        (insert pre)
        (goto-char (+ end (string-width pre)))
        (insert post)))
    (insert (concat pre " " post))
    (backward-char (+ 1 (string-width post)))))

;;;###autoload
(defun mediawiki-insert-strong-emphasis ()
  "Mark with strong emphasis italics.
Uses four apostrophes (e.g. ''''FOO'''').  When mark is active,
surrounds region."
  (interactive)
  (mediawiki-insert "''''" "''''"))

;;;###autoload
(defun mediawiki-insert-bold ()
  "Mark bold.
Uses three apostrophes (e.g. '''FOO''').  When mark is active,
surrounds region."
  (interactive)
  (mediawiki-insert "'''" "'''"))

;;;###autoload
(defun mediawiki-insert-italics ()
  "Mark italics.
Uses TWO apostrophes (e.g. ''FOO'').  When mark is active,
surrounds region."
  (interactive)
  (mediawiki-insert "''" "''"))

;;;###autoload
(defun mediawiki-insert-quotation-with-signature ()
  "Mark the current region as a quotation with your signature."
  (interactive)
  (mediawiki-insert "{{Quotation|}}" "{{~~~~}}"))

;;;###autoload
(defun mediawiki-insert-quotation ()
  "Mark the current selection as a quote.
Use the form {{Quotation}}{{}}.  When mark is active,
surrounds region."
  (interactive)
  (mediawiki-insert "{{Quotation|}}{{" "}}"))

;;;###autoload
(defun mediawiki-insert-bible-verse-template ()
  "Insert a template for the quotation of Bible verses."
  (interactive)
  (insert "({{niv|")
  (let ((name    (read-string "Name: ")))
    (insert (concat name "|"))
    (let ((verse (read-string "Verse: ")))
      (insert (concat verse "|" name " " verse "}})")))))

;;;###autoload
(defun mediawiki-insert-user ()
  "Interactively insert a user name."
  (interactive)
  (if mediawiki-english-or-german
    (let ((user (read-string "Name of user: " )))
      (insert (concat "[[User:" user "|" user "]]")))
    (let ((user (read-string "Name des Benutzers: " )))
      (insert (concat "[[Benutzer:" user "|" user "]]")))))

;;;###autoload
(defun mediawiki-insert-reply-prefix ()
  "Quotation box of the form {{Quotation}}{{}}."
  (interactive)
  (beginning-of-line 1)
  (search-forward "[[")
  (backward-char 2)
  (mark-sexp 1)
  (copy-to-register ?M (region-beginning) (region-end) nil)
  (end-of-line 1)
  (mediawiki-terminate-paragraph)
  (beginning-of-line 1)
  (kill-line nil)
  (insert "----")
  (newline 1)
  (yank)
  (insert ":'''Re: ")
  (insert-register ?M 1)
  (insert "''' ")
  (end-of-line 1))

;;;###autoload
(defun mediawiki-insert-header ()
  "Insert subheader via == (e.g. == FOO ==)."
  (interactive)
  (mediawiki-insert "==" "=="))

;;;###autoload
(defun mediawiki-insert-link ()
  "Insert link (e.g. [[FOO]])."
  (interactive)
  (mediawiki-insert "[[" "]]"))

;;;###autoload
(defun mediawiki-insert-link-www ()
  "Insert link (e.g. [://FOO])."
  (interactive)
  (mediawiki-insert "[://" "]"))

;;;###autoload
(defun mediawiki-insert-image ()
  "Insert image link (e.g. [[Image:FOO]]).
Checks the variable mediawiki-english-or-german."
  (interactive)
  (mediawiki-insert (if mediawiki-english-or-german
                      "[[Image:"
                      "[[Bild:") "]]"))

;;;###autoload
(defun mediawiki-insert-audio ()
  "Insert audio link (e.g. [[Media:FOO]])
Checks The variable mediawiki-english-or-german."
  (interactive)
  (mediawiki-insert (if mediawiki-english-or-german
                      "[[Media:"
                      "[[Bild:") "]]"))

;;;###autoload
(defun mediawiki-insert-signature ()
  "Insert signature (e.g. \"~~~~:\")."
  (interactive)
  (insert "~~~~: "))

;;;###autoload
(defun mediawiki-insert-hline ()
  "Insert hline (e.g. \"----\")."
  (interactive)
  (insert "\n----\n"))

;;;###autoload
(defun mediawiki-unfill-paragraph-or-region ()
  "Unfill region.
This function does NOT explicitly search for \"soft newlines\"
as does mediawiki-unfill-region."
  (interactive)
  (set (make-local-variable 'paragraph-start) "[ \t\n\f]")
  (set (make-local-variable 'paragraph-start)
    "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$")
  (set-fill-prefix)
  (beginning-of-line 1)

  (if use-hard-newlines
    (progn
      (set (make-local-variable 'use-hard-newlines) nil)
      (set (make-local-variable 'sentence-end-double-space) t))
    (set (make-local-variable 'sentence-end-double-space) nil)
    (set (make-local-variable 'use-hard-newlines) t))
  (let ((fill-column (point-max)))
    (if (fboundp 'fill-paragraph-or-region)
      (fill-paragraph-or-region nil)
      (fill-paragraph nil))))

;;;###autoload
(defun mediawiki-start-paragraph ()
  "Start a Paragraph."
  (interactive)
  (set (make-local-variable 'paragraph-start)
    "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$"))

;;;###autoload
(defun mediawiki-hardlines ()
  "Set `use-hard-newlines' to NIL."
  (interactive)
  (setq use-hard-newlines nil))

;;;###autoload
(defun mediawiki-next-long-line ()
  "Move forward to the next long line.
Lines are considered long if their length is greater than `fill-column'.

TODO: When function reaches end of buffer, `save-excursion' to starting
point.  Generalise to make `previous-long-line'."
  (interactive)
  ;; global-variable: fill-column
  (if (= (forward-line) 0)
    (let ((line-length
	    (save-excursion
	      (end-of-line)
	      (current-column))))
      (if (<= line-length fill-column)
	(mediawiki-next-long-line)
	(message "Long line found")))
    ;; Stop, end of buffer reached.
    (error "Long line not found")))

;;;###autoload
(defun mediawiki-unfill-paragraph-simple ()
  "A very simple function for unfilling a paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;;###autoload
(defun mediawiki-outline-magic-keys ()
  "Set up outline magic keys.
See https://www.emacswiki.org/emacs/OutlineMagic"
  (interactive)
  (unless  (featurep 'xemacs)
    (local-set-key [(shift iso-lefttab)] 'outline-cycle)
    (local-set-key [iso-left-tab] 'outline-cycle))
  (local-set-key [(meta left)]  'outline-promote)
  (local-set-key [(meta right)] 'outline-demote)
  (local-set-key [(shift return)] 'newline-and-indent)
  (local-set-key [(control left)]  'mediawiki-simple-outline-promote)
  (local-set-key [(control right)] 'mediawiki-simple-outline-demote)
  (local-set-key [(control up)] 'outline-move-subtree-up)
  (local-set-key [(control down)] 'outline-move-subtree-down))

;;;###autoload
(defun mediawiki-enhance-indent ()
  "Indent a region using MediaWiki markup (e.g \":\")."
  (interactive)
  (string-rectangle (region-beginning) (region-end) ":"))

;;;###autoload
(defun mediawiki-yank-prefix ()
  "Remove indent markup from region.
FIXME!!!"
  (interactive)
  (string-rectangle (region-beginning) (region-end) " "))

;;;###autoload
(defun mediawiki-simple-outline-promote ()
  "Simple-minded promotion of current line.
This function simply deletes one \"=\" from the beginning and end
of the line.  It does not promote the whole tree!"
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (search-forward "=")
    (delete-char 1 nil)
    (end-of-line 1)
    (search-backward "=")
    (delete-char 1 nil)))

;;;###autoload
(defun mediawiki-simple-outline-demote ()
  "Simple-minded demotion of the current line.
This function simple adds \"=\" to the beginning and end of the
line.  It does not promote the whole tree!"
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (search-forward "=")
    (insert "=")
    (end-of-line 1)
    (search-backward "=")
    (insert "=")))

;;;###autoload
(defun mediawiki-rename-buffer ()
  "Make sure that the option UNIQUE is used."
  (interactive)
  (rename-buffer (read-string "Name of new buffer (unique): " ) 1))

;;;###autoload
(defun mediawiki-mark-section ()
  "Set mark at end of current logical section, and point at top."
  (interactive)
  (re-search-forward (concat  "== "  "[a-z,A-z \t]*" " =="))
  (re-search-backward "^")
  (set-mark (point))
  (re-search-backward (concat "== "  "[a-z,A-z \t]*" " "))
  (when (fboundp 'zmacs-activate-region)
    (zmacs-activate-region)))

;;;###autoload
(defun mediawiki-mark-signature ()
  "Set mark at end of current logical section, and point at top."
  (interactive)
  (re-search-forward "]]") ;;[[ ]]
  (re-search-backward "^")
  (set-mark (point))
  (re-search-backward "[[")
  (when (fboundp 'zmacs-activate-region)
    (zmacs-activate-region)))

(declare-function mediawiki-open "mediawiki")
(defvar mediawiki-page-title nil
  "The title of the page corresponding to the current buffer.")

;;;###autoload
(defun mediawiki-reload ()
  "Reload the page from the server."
  (interactive)
  (when (not mediawiki-site)
    (setq mediawiki-site (mediawiki-prompt-for-site)))
  (if mediawiki-page-title
    (mediawiki-open mediawiki-page-title)
    (error "Error: %s is not a mediawiki document" (buffer-name))))

;;; Major Mode Definition

(defvar mediawiki-edittoken nil
  "The edit token for this page.")
(defvar mediawiki-starttimestamp nil
  "The starttimestamp for this page.")
(defvar mediawiki-basetimestamp nil
  "The base timestamp for this page.")

;;;###autoload
(define-derived-mode mediawiki-mode text-mode "MW"
  "Major mode for editing articles written in MediaWiki's wikitext.

Wikipedia articles are usually unfilled: newline characters are not used
for breaking paragraphs into lines.  In case something goes wrong, the
following commands may come in handy:

\\[mediawiki-fill-article] fills the buffer.
\\[mediawiki-unfill-article] unfills the buffer.

Be warned that function can be dead slow, better use
`mediawiki-unfill-paragraph-or-region'.
\\[mediawiki-unfill-paragraph-or-region] unfills the paragraph
\\[mediawiki-unfill-paragraph-simple] does the same but simpler.

The following commands put in markup structures.
\\[mediawiki-insert-strong-emphasis] inserts italics
\\[mediawiki-insert-bold] inserts bold text
\\[mediawiki-insert-italics] italics
\\[mediawiki-insert-header] header
\\[mediawiki-insert-link] inserts a link

The following commands are also defined:
\\[mediawiki-insert-user] inserts user name
\\[mediawiki-insert-signature] inserts ~~~~
\\[mediawiki-insert-enumerate] inserts enumerate type structures
\\[mediawiki-insert-itemize] inserts itemize type structures
\\[mediawiki-insert-hline] inserts a hline

Replying functionality
\\[mediawiki-reply-at-point-simple]

The register functionality
\\[mediawiki-copy-page-to-register]
\\[defun mediawiki-insert-page-to-register]

Some simple editing commands.
\\[mediawiki-enhance-indent]
\\[mediawiki-yank-prefix]
\\[mediawiki-unfill-paragraph-or-region]

\\[mediawiki-terminate-paragraph] starts a new list item or paragraph in
a context-aware manner.
\\[mediawiki-next-header]     moves to the next (sub)section header.
\\[mediawiki-prev-header]     moves to the previous (sub)section header."

  (make-local-variable 'change-major-mode-hook)
  (make-local-variable 'mediawiki-edittoken)
  (make-local-variable 'mediawiki-starttimestamp)
  (make-local-variable 'mediawiki-basetimestamp)
  (make-local-variable 'mediawiki-site)
  (make-local-variable 'mediawiki-edit-form-vars)
  (make-local-variable 'mediawiki-page-title)
  (set (make-local-variable 'adaptive-fill-regexp) "[ ]*")
  (set (make-local-variable 'comment-start-skip) "\\(?:<!\\)?-- *")
  (set (make-local-variable 'comment-end-skip) " *--\\([ \n]*>\\)?")
  (set (make-local-variable 'comment-start) "<!-- ")
  (set (make-local-variable 'comment-end) " -->")
  (set (make-local-variable 'paragraph-start)
    "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$")
  (set (make-local-variable 'sentence-end-double-space) nil)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'font-lock-defaults)
    '(mediawiki-font-lock-keywords t nil nil nil))
  (set (make-local-variable 'fill-nobreak-predicate)
    'mediawiki-link-fill-nobreak-p)
  (set (make-local-variable 'auto-fill-inhibit-regexp) "^[ *#:|;]")

  ;; Support for outline-minor-mode. No key conflicts, so we'll use
  ;; the normal outline-mode prefix.
  (set (make-local-variable 'outline-regexp) "==+")
  (when (boundp 'outline-minor-mode-prefix)
    (set (make-local-variable 'outline-minor-mode-prefix) "\C-c\C-o"))
                                        ; (set (make-local-variable 'outline-regexp) "=+")
                                        ; (set (make-local-variable 'outline-regexp) ":")

  ;; Turn on the Imenu automatically.
  (when menu-bar-mode
    (set (make-local-variable 'imenu-generic-expression)
      mediawiki-imenu-generic-expression)
    (imenu-add-to-menubar "Contents"))

  (modify-syntax-entry ?< "(>" mediawiki-mode-syntax-table)
  (modify-syntax-entry ?> ")<" mediawiki-mode-syntax-table))

;;; Mode Hooks

(add-hook 'mediawiki-mode-hook (lambda () (outline-minor-mode nil)))
(add-hook 'outline-minor-mode-hook 'mediawiki-outline-magic-keys)

(provide 'mediawiki-mode)

;;; mediawiki-mode.el ends here
