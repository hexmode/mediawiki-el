;;; mediawiki-user.el --- User contributions browser for mediawiki.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 MediaWiki.el contributors

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

;; Provides a user contributions browser for MediaWiki, displaying a user's
;; edits in a sortable tabulated-list table with diff and navigation.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-api)
(require 'mediawiki-utils)
(require 'mediawiki-diff)
(require 'tabulated-list)
(require 'cl-lib)

(defvar mediawiki-site)
(defvar mediawiki-page-title)
(defvar mediawiki-user--username)
(defvar mediawiki-user--contribs)
(defvar mediawiki-user--sitename)

(declare-function mediawiki-edit "mediawiki-page")
(declare-function mediawiki-diff-show-revision "mediawiki-diff")
(declare-function mediawiki-diff-show-diff "mediawiki-diff")

;;;###autoload
(defun mediawiki-user-contributions (username &optional sitename limit)
  "Display contributions for USERNAME on SITENAME.
LIMIT is max contributions (default 50).
Interactively, prompt for USERNAME and use the current site."
  (interactive
   (list (read-string "Username: ")
         (or mediawiki-site (mediawiki-prompt-for-site))
         nil))
  (setq sitename (or sitename mediawiki-site
                     (error "No site configured")))
  (let* ((limit (or limit 50))
         (bufname (format "*MW Contributions: %s*" username))
         (contribs (mediawiki-api-get-user-contributions
                    sitename username limit))
         (buffer (get-buffer-create bufname)))
    (with-current-buffer buffer
      (mediawiki-user-contributions-mode)
      (setq mediawiki-user--username username)
      (setq mediawiki-user--sitename sitename)
      (setq mediawiki-user--contribs contribs)
      (mediawiki-user-contributions--refresh)
      (tabulated-list-print))
    (pop-to-buffer buffer)))

;;; Mode Definition

(defvar mediawiki-user-contributions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'mediawiki-user-open-page)
    (define-key map (kbd "o")   'mediawiki-user-open-page)
    (define-key map (kbd "v")   'mediawiki-user-view-revision)
    (define-key map (kbd "d")   'mediawiki-user-diff-previous)
    (define-key map (kbd "b")   'mediawiki-user-browse-revision)
    (define-key map (kbd "g")   'mediawiki-user-contributions-refresh)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap for `mediawiki-user-contributions-mode'.")

(define-derived-mode mediawiki-user-contributions-mode tabulated-list-mode
  "MW-Contribs"
  "Major mode for browsing user contributions.
\\{mediawiki-user-contributions-mode-map}"
  (setq tabulated-list-format
        (vector (list "Rev"   10 t :right-align t)
                (list "Page"  40 t)
                (list "Date"  20 t)
                (list "Change" 8 t :right-align t)
                (list "Summary" 0 nil)))
  (setq tabulated-list-sort-key (cons "Date" t))
  (add-hook 'tabulated-list-revert-hook
            #'mediawiki-user-contributions--refresh nil t))

;;; Refresh

(defun mediawiki-user-contributions--refresh ()
  "Re-fetch contributions and rebuild the tabulated entries."
  (setq mediawiki-user--contribs
        (mediawiki-api-get-user-contributions
         mediawiki-user--sitename
         mediawiki-user--username
         (length mediawiki-user--contribs)))
  (setq tabulated-list-entries
        (mapcar #'mediawiki-user--make-entry
                mediawiki-user--contribs)))

(defun mediawiki-user-contributions-refresh (&optional arg)
  "Refresh the contributions list.
With prefix argument ARG, prompt for a new limit."
  (interactive "P")
  (when arg
    (let ((new-limit (read-number "Max contributions: "
                                  (length mediawiki-user--contribs))))
      (setq mediawiki-user--contribs
            (mediawiki-api-get-user-contributions
             mediawiki-user--sitename
             mediawiki-user--username
             new-limit))))
  (mediawiki-user-contributions--refresh)
  (tabulated-list-print))

;;; Entry Construction

(defun mediawiki-user--make-entry (contrib)
  "Return a tabulated-list entry vector for CONTRIB alist.
The id is the revid for the entry."
  (let* ((revid   (alist-get 'revid contrib))
         (title   (alist-get 'title contrib))
         (ts      (mediawiki-format-timestamp
                   (alist-get 'timestamp contrib)))
         (comment (or (alist-get 'comment contrib) ""))
         (sizediff (alist-get 'sizediff contrib))
         (minor   (alist-get 'minor contrib))
         (size-str (mediawiki-format-size-change sizediff))
         (display-title (if minor
                            (propertize title 'face 'italic)
                          title)))
    (list revid
          (vector (number-to-string revid)
                  display-title
                  ts
                  size-str
                  comment))))

;;; Interactive Commands

(defun mediawiki-user--contrib-at-point ()
  "Return the contribution alist at point."
  (let ((id (tabulated-list-get-id)))
    (when id
      (cl-find id mediawiki-user--contribs
               :key (lambda (c) (alist-get 'revid c))
               :test 'eql))))

(defun mediawiki-user--username ()
  "Return the username for the current contributions buffer."
  (or mediawiki-user--username
      (error "No username set in this buffer")))

(defun mediawiki-user-open-page ()
  "Open the page at point for editing."
  (interactive)
  (let* ((contrib (mediawiki-user--contrib-at-point))
         (title (alist-get 'title contrib)))
    (unless title
      (user-error "No page at point"))
    (mediawiki-edit mediawiki-user--sitename title)))

(defun mediawiki-user-view-revision ()
  "View the revision at point."
  (interactive)
  (let* ((contrib (mediawiki-user--contrib-at-point))
         (title   (alist-get 'title contrib))
         (revid   (alist-get 'revid contrib)))
    (unless (and title revid)
      (user-error "No revision at point"))
    (mediawiki-diff-show-revision revid title)))

(defun mediawiki-user-diff-previous ()
  "Diff the revision at point against its parent."
  (interactive)
  (let* ((contrib (mediawiki-user--contrib-at-point))
         (title   (alist-get 'title contrib))
         (revid   (alist-get 'revid contrib))
         (parent  (alist-get 'parentid contrib)))
    (unless (and title revid)
      (user-error "No revision at point"))
    (if (and parent (> parent 0))
        (mediawiki-diff-show-diff parent revid title)
      (message "This is the first revision of this page (no parent)"))))

(defun mediawiki-user-browse-revision ()
  "Open the revision at point in a web browser."
  (interactive)
  (let* ((contrib (mediawiki-user--contrib-at-point))
         (title   (alist-get 'title contrib))
         (revid   (alist-get 'revid contrib)))
    (unless (and title revid)
      (user-error "No revision at point"))
    (browse-url (mediawiki-make-revision-url
                 mediawiki-user--sitename title revid))))

;;; Buffer-local Variables

(defvar-local mediawiki-user--username nil
  "Username for the current contributions buffer.")

(defvar-local mediawiki-user--contribs nil
  "List of contribution alists for the current buffer.")

(defvar-local mediawiki-user--sitename nil
  "Sitename for the current contributions buffer.")

(provide 'mediawiki-user)

;;; mediawiki-user.el ends here
