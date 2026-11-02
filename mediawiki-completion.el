;;; mediawiki-completion.el --- Wiki link completion for mediawiki.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el contributors

;; Author: MediaWiki.el contributors
;; URL: https://github.com/hexmode/mediawiki-el

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

;; Provides `completion-at-point' for [[wiki links]] in mediawiki-mode,
;; backed by the MediaWiki prefixsearch API.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-api)

;; User option
(defcustom mediawiki-completion-limit 20
  "Maximum number of completion candidates to fetch."
  :type 'integer
  :group 'mediawiki)

;;; Internal Functions

(defun mediawiki-completion--inside-link-p ()
  "Return non-nil if point is inside [[...]] brackets.
Returns (START . END) where START is after [[ and END is before
]] or before | (whichever comes first), or nil if not inside a link."
  (save-excursion
    (let* ((pos (point))
           (bol (line-beginning-position))
           (eol (line-end-position))
           (start (when (search-backward "[[" bol t)
                    (+ (point) 2)))
           (end (when (and start
                           (goto-char pos)
                           (re-search-forward "\\(?:\\]\\]\\)\\|\\|" eol t))
                  (match-beginning 0))))
      (when (and start end (<= pos end) (>= pos start))
        (cons start end)))))

;;; Page Search

(defun mediawiki-completion-search (prefix &optional sitename)
  "Return page titles on SITENAME starting with PREFIX.
SITENAME defaults to `mediawiki-site'.  Returns nil when there is no
page to search from or the query fails."
  (let ((sitename (or sitename mediawiki-site)))
    (when (and sitename (not (string-empty-p (string-trim prefix))))
      (ignore-errors
        (let* ((result (mediawiki-api-call
                        sitename "query"
                        (list (cons "list" "prefixsearch")
                              (cons "pssearch" prefix)
                              (cons "pslimit"
                                    (int-to-string mediawiki-completion-limit)))))
               (candidates (alist-get 'prefixsearch (alist-get 'query result))))
          (mapcar (lambda (candidate) (alist-get 'title candidate))
                  candidates))))))

;;; Main Completion Function

;;;###autoload
(defun mediawiki-completion-at-point ()
  "Complete the wiki link at point using the prefixsearch API.
Intended for use as a `completion-at-point' function.
Returns (START END COLLECTION-FN . PROPS) when inside [[...]],
or nil otherwise."
      (when-let* ((bounds (mediawiki-completion--inside-link-p)))
    (let ((start (car bounds))
          (end (cdr bounds))
          (sitename mediawiki-site))
      (list start end
            (lambda (str pred action)
              (if (eq action 'metadata)
                  '(metadata (category . mediawiki-page))
                (complete-with-action
                 action (mediawiki-completion-search str sitename) str pred)))
            :exclusive 'no))))

;;; Company Integration

(defvar company-backends)
(defvar company-mode)
(defvar mediawiki-mode-map)

(defun mediawiki-completion-use-company ()
  "Put `company-capf' first in this buffer's `company-backends'.
That makes `company-complete' offer the wiki link completion without
changing the user's global backend list, and does nothing when company
is not loaded.  Existing entries are kept and never duplicated."
  (when (boundp 'company-backends)
    (setq-local company-backends
                (cons 'company-capf
                      (delq 'company-capf (copy-sequence company-backends))))))

(defun mediawiki-completion--company-loaded ()
  "Set the company backend in mediawiki buffers once company is loaded."
  (add-hook 'mediawiki-mode-hook #'mediawiki-completion-use-company)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'mediawiki-mode)
        (mediawiki-completion-use-company)))))

(with-eval-after-load 'company
  (mediawiki-completion--company-loaded))

;;; Completion Command

(defvar company-mode)

;;;###autoload
(defun mediawiki-complete ()
  "Complete at point, preferring company when it is active.
Company's `company-complete' errors when company-mode is off, which is
what a buffer-wide M-TAB binding would otherwise hit; this falls back to
`completion-at-point' so the key works either way."
  (interactive)
  (if (and (fboundp 'company-complete)
           (bound-and-true-p company-mode))
      (company-complete)
    (completion-at-point)))

;;; Completion Registration

;;;###autoload
(with-eval-after-load 'mediawiki-mode
  (define-key mediawiki-mode-map (kbd "M-TAB") #'mediawiki-complete)
  ;; GUI frames deliver M-TAB as the `M-tab' function key.
  (define-key mediawiki-mode-map (kbd "<M-tab>") #'mediawiki-complete)
  (add-hook 'mediawiki-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        #'mediawiki-completion-at-point nil 'local)
              ;; This form also lands in mediawiki-autoloads.el, which can run
              ;; before this file is loaded (a buffer that only needed
              ;; mediawiki-mode); the CAPF above is autoloaded, this is not.
              (when (fboundp 'mediawiki-completion-use-company)
                (mediawiki-completion-use-company)))))

(provide 'mediawiki-completion)

;;; mediawiki-completion.el ends here
