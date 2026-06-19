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
                (let* ((result (mediawiki-api-call
                                sitename "query"
                                (list (cons "list" "prefixsearch")
                                      (cons "pssearch" str)
                                      (cons "pslimit"
                                            (int-to-string
                                             mediawiki-completion-limit)))))
                       (candidates (alist-get 'prefixsearch
                                              (alist-get 'query result)))
                       (titles (mapcar (lambda (c) (alist-get 'title c))
                                       candidates)))
                  (complete-with-action action titles str pred))))
            :exclusive 'no))))

;;; Completion Registration

;;;###autoload
(with-eval-after-load 'mediawiki-mode
  (add-hook 'mediawiki-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        #'mediawiki-completion-at-point nil 'local))))

(provide 'mediawiki-completion)

;;; mediawiki-completion.el ends here
