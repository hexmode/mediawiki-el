;;; mediawiki-recent.el --- Remember edited pages and discussion desks  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mark A. Hershberger

;; Author: Mark A. Hershberger <mah@everybody.org>
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

;; Keeps a small, user-editable file of the pages you have saved and the
;; discussion desks you have opened, each with the site it belongs to, so
;; `mediawiki-open' and `mediawiki-discussion-tools' can offer them from any
;; buffer and reuse that site's authentication.
;;
;; The file holds one Lisp datum and is read with `read', never loaded, so it
;; cannot execute code.  Site names and page titles only: no credentials.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-site)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pp)

;;; Customization

(defcustom mediawiki-recent-file
  (expand-file-name "mw-hist.el" user-emacs-directory)
  "File where mediawiki.el keeps the pages you have used.
It holds site names and page titles only; no credentials are stored."
  :type 'file
  :group 'mediawiki)

(defcustom mediawiki-recent-max-entries 100
  "How many entries to keep in each of the recent page lists."
  :type 'integer
  :group 'mediawiki)

(defcustom mediawiki-recent-record-pages t
  "Whether to remember the pages you save."
  :type 'boolean
  :group 'mediawiki)

(defcustom mediawiki-recent-record-discussions t
  "Whether to remember the desks opened with the discussion browser."
  :type 'boolean
  :group 'mediawiki)

;;; State

(defconst mediawiki-recent--version 1
  "Format version written to `mediawiki-recent-file'.")

(defvar mediawiki-recent--data nil
  "The whole recent-pages plist, as read from the file.")

(defvar mediawiki-recent--loaded nil
  "Non-nil once the file has been read or found missing.")

(defvar mediawiki-recent--dirty nil
  "Non-nil when there are changes that have not reached the file.")

;;; Loading and saving

(defun mediawiki-recent--empty-data ()
  "Return an empty recent-pages plist."
  (list :version mediawiki-recent--version :pages nil :discussions nil))

(defun mediawiki-recent--valid-entry-p (entry)
  "Return non-nil when ENTRY looks like a stored page."
  (and (listp entry)
       (stringp (plist-get entry :site))
       (stringp (plist-get entry :title))
       (numberp (or (plist-get entry :used) 0))
       (numberp (or (plist-get entry :count) 0))))

(defun mediawiki-recent--valid-p (data)
  "Return non-nil when DATA is a recent-pages plist we can use."
  (and (listp data)
       (equal (plist-get data :version) mediawiki-recent--version)
       (listp (plist-get data :pages))
       (listp (plist-get data :discussions))
       (cl-every #'mediawiki-recent--valid-entry-p
                 (append (plist-get data :pages)
                         (plist-get data :discussions)))))

(defun mediawiki-recent--backup-name (file)
  "Return a free backup name based on FILE, numbering past existing ones."
  (let ((base (concat file ".bak"))
        (n 0)
        candidate)
    (setq candidate base)
    (while (file-exists-p candidate)
      (setq n (1+ n)
            candidate (format "%s.%d" base n)))
    candidate))

(defun mediawiki-recent--quarantine (file reason)
  "Move the unusable FILE aside and return empty data.
REASON says why it was unusable.  An existing .bak is never overwritten."
  (let ((target (mediawiki-recent--backup-name file)))
    (condition-case err
        (rename-file file target)
      (error
       (warn "mediawiki-recent: could not move %s aside: %s"
             file (error-message-string err))
       (setq target nil)))
    (if target
        (warn "mediawiki-recent: %s (%s); moved it to %s and started a fresh list"
              file reason target)
      (warn "mediawiki-recent: %s (%s); starting a fresh list" file reason)))
  (mediawiki-recent--empty-data))

(defun mediawiki-recent--read-file (file)
  "Read FILE and return its data, or empty data when it is unusable."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((data (read (current-buffer))))
          (if (mediawiki-recent--valid-p data)
              data
            (mediawiki-recent--quarantine
             file "not a valid recent pages file"))))
    (error (mediawiki-recent--quarantine file (error-message-string err)))))

(defun mediawiki-recent--load ()
  "Read the recent-pages file if it has not been read yet."
  (unless mediawiki-recent--loaded
    (setq mediawiki-recent--data
          (if (file-exists-p mediawiki-recent-file)
              (mediawiki-recent--read-file mediawiki-recent-file)
            (mediawiki-recent--empty-data))
          mediawiki-recent--loaded t
          mediawiki-recent--dirty nil)))

(defun mediawiki-recent-save ()
  "Write the recent-pages file atomically, with mode 0600."
  (interactive)
  (mediawiki-recent--load)
  (let* ((file (expand-file-name mediawiki-recent-file))
         (dir (file-name-directory file))
         (tmp nil))
    (condition-case err
        (progn
          (make-directory dir t)
          (setq tmp (make-temp-file (expand-file-name "mw-hist-" dir) nil ".el"))
          (with-temp-file tmp
            (insert ";;; " (file-name-nondirectory file)
                    " --- pages used by mediawiki.el  -*- mode: emacs-lisp; -*-\n"
                    ";; Written by mediawiki-recent.el; safe to edit by hand.\n\n")
            (let ((print-length nil)
                  (print-level nil))
              (pp mediawiki-recent--data (current-buffer)))
            (insert "\n"))
          (set-file-modes tmp #o600)
          (rename-file tmp file t)
          (setq tmp nil)
          (setq mediawiki-recent--dirty nil))
      (error
       (warn "mediawiki-recent: could not save %s: %s"
             file (error-message-string err))))
    (when (and tmp (file-exists-p tmp))
      (ignore-errors (delete-file tmp)))))

(defun mediawiki-recent--save-if-dirty ()
  "Save the recent-pages file at exit when something changed."
  (when (and mediawiki-recent--loaded mediawiki-recent--dirty)
    (mediawiki-recent-save)))

(add-hook 'kill-emacs-hook #'mediawiki-recent--save-if-dirty)

(defun mediawiki-recent-reload ()
  "Re-read the recent-pages file, discarding in-memory changes."
  (interactive)
  (setq mediawiki-recent--loaded nil)
  (mediawiki-recent--load)
  (message "Re-read %s" mediawiki-recent-file))

;;; Entries

(defun mediawiki-recent--key (kind)
  "Return the plist key holding the list for KIND."
  (pcase kind
    ('pages :pages)
    ('discussions :discussions)
    (_ (error "Unknown recent page kind: %S" kind))))

(defun mediawiki-recent-entries (kind)
  "Return the stored entries for KIND, a symbol: `pages' or `discussions'."
  (mediawiki-recent--load)
  (copy-sequence (plist-get mediawiki-recent--data
                            (mediawiki-recent--key kind))))

(defun mediawiki-recent--same-title-p (a b)
  "Return non-nil when A and B name the same page.
MediaWiki ignores the case of the first character only."
  (and (stringp a) (stringp b)
       (> (length a) 0) (> (length b) 0)
       (eq (downcase (aref a 0)) (downcase (aref b 0)))
       (equal (substring a 1) (substring b 1))))

(defun mediawiki-recent--find (entries site title)
  "Return the entry in ENTRIES for TITLE on SITE, or nil."
  (seq-find (lambda (entry)
              (and (equal (plist-get entry :site) site)
                   (mediawiki-recent--same-title-p
                    (plist-get entry :title) title)))
            entries))

(defun mediawiki-recent--recording-p (kind)
  "Return non-nil when KIND should be recorded."
  (pcase kind
    ('pages mediawiki-recent-record-pages)
    ('discussions mediawiki-recent-record-discussions)))

(defun mediawiki-recent-record (kind sitename title)
  "Record that TITLE on SITENAME was used with KIND, then save.
Does nothing when KIND is disabled or the arguments are empty."
  (when (and (mediawiki-recent--recording-p kind)
             (stringp sitename) (not (string-empty-p sitename))
             (stringp title) (not (string-empty-p title)))
    (mediawiki-recent--load)
    (let* ((key (mediawiki-recent--key kind))
           (entries (plist-get mediawiki-recent--data key))
           (existing (mediawiki-recent--find entries sitename title))
           (entry (list :site sitename
                        :title title
                        :used (float-time)
                        :count (1+ (or (plist-get existing :count) 0)))))
      (setq entries (cons entry (remove existing entries)))
      (when (> (length entries) mediawiki-recent-max-entries)
        (setq entries (seq-take entries mediawiki-recent-max-entries)))
      (setq mediawiki-recent--data
            (plist-put mediawiki-recent--data key entries)
            mediawiki-recent--dirty t)
      (mediawiki-recent-save))))

(defun mediawiki-recent-forget (kind sitename title)
  "Remove TITLE on SITENAME from the KIND list."
  (interactive
   (let* ((kind (intern (completing-read
                         "List: " '("pages" "discussions") nil t)))
          (site (read-string "Site: "))
          (title (read-string "Page: ")))
     (list kind site title)))
  (mediawiki-recent--load)
  (let* ((key (mediawiki-recent--key kind))
         (entries (plist-get mediawiki-recent--data key))
         (entry (mediawiki-recent--find entries sitename title)))
    (when entry
      (setq mediawiki-recent--data
            (plist-put mediawiki-recent--data key (remove entry entries))
            mediawiki-recent--dirty t)
      (mediawiki-recent-save)
      (message "Forgot %s on %s" title sitename))))

(defun mediawiki-recent-clear (&optional kind)
  "Remove every recent entry, or only KIND's."
  (interactive)
  (mediawiki-recent--load)
  (setq mediawiki-recent--data
        (if kind
            (plist-put mediawiki-recent--data (mediawiki-recent--key kind) nil)
          (mediawiki-recent--empty-data))
        mediawiki-recent--dirty t)
  (mediawiki-recent-save)
  (message "Cleared recent %s" (or kind "entries")))

;;; Prompting

(defun mediawiki-recent--age (entry)
  "Return a short string saying when ENTRY was last used."
  (let ((used (plist-get entry :used)))
    (if (not (numberp used))
        ""
      (let ((secs (- (float-time) used)))
        (cond ((< secs 3600) "just now")
              ((< secs 86400) (format "%dh ago" (floor secs 3600)))
              ((< secs 604800) (format "%dd ago" (floor secs 86400)))
              (t (format "%dw ago" (floor secs 604800))))))))

(defun mediawiki-recent--label (entry prefer-site)
  "Return the completion label for ENTRY, hiding its site when it is PREFER-SITE."
  (let ((site (plist-get entry :site))
        (title (plist-get entry :title)))
    (if (and prefer-site (equal site prefer-site))
        title
      (format "%s: %s" site title))))

(defun mediawiki-recent--ordered (kind prefer-site)
  "Return KIND's entries, newest first, with PREFER-SITE's entries first."
  (let ((sorted (sort (copy-sequence (mediawiki-recent-entries kind))
                      (lambda (a b)
                        (> (or (plist-get a :used) 0)
                           (or (plist-get b :used) 0))))))
    (if prefer-site
        (append (seq-filter (lambda (entry)
                              (equal (plist-get entry :site) prefer-site))
                            sorted)
                (seq-remove (lambda (entry)
                              (equal (plist-get entry :site) prefer-site))
                            sorted))
      sorted)))

(defun mediawiki-recent--prompt (kind)
  "Return the prompt string for KIND."
  (if (eq kind 'pages) "Wiki Page: " "Discussion page: "))

(defun mediawiki-recent--default-label (choices default)
  "Return the label in CHOICES matching DEFAULT, or DEFAULT, or the first label."
  (or (when (and (stringp default) (not (string-empty-p default)))
        (or (car (seq-find
                  (lambda (choice)
                    (mediawiki-recent--same-title-p (cddr choice) default))
                  choices))
            default))
      (car (car choices))))

(defun mediawiki-recent--completion-table (choices)
  "Return a completion table over CHOICES that shows ages as annotations."
  (let* ((strings (mapcar #'car choices))
         (annotate (lambda (choice)
                     (when-let* ((entry (cdr (assoc choice choices))))
                       (format "  (%s)" (mediawiki-recent--age entry))))))
    (if (fboundp 'completion-table-with-metadata)
        (completion-table-with-metadata
         strings (list :annotation-function annotate))
      strings)))

(defun mediawiki-recent-read (kind &optional sitename default extra)
  "Prompt for a page of KIND, offering SITENAME's entries first.
KIND is `pages' or `discussions'.  DEFAULT is preselected when it names
one of the entries.  EXTRA is a list of titles to offer for SITENAME
after its stored ones, such as the site-local `mediawiki-page-history',
without adding them to the file.  Returns (SITE . TITLE); SITE is nil
when TITLE was typed rather than chosen, so the caller can ask for a
site."
  (let* ((entries (mediawiki-recent--ordered kind sitename))
         (local (when sitename
                  (seq-filter (lambda (entry)
                                (equal (plist-get entry :site) sitename))
                              entries)))
         (others (if sitename
                     (seq-remove (lambda (entry)
                                   (equal (plist-get entry :site) sitename))
                                 entries)
                   entries))
         (local-choices
          (mapcar (lambda (entry)
                    (let ((title (plist-get entry :title)))
                      (cons title (cons sitename title))))
                  local))
         (extra-choices
          (seq-remove
           (lambda (choice) (assoc (car choice) local-choices))
           (mapcar (lambda (title) (cons title (cons sitename title)))
                   (or extra nil))))
         (other-choices
          (mapcar (lambda (entry)
                    (cons (mediawiki-recent--label entry nil)
                          (cons (plist-get entry :site)
                                (plist-get entry :title))))
                  others))
         (choices (append local-choices extra-choices other-choices))
         (answer (completing-read
                  (mediawiki-recent--prompt kind)
                  (mediawiki-recent--completion-table choices)
                  nil nil nil nil
                  (mediawiki-recent--default-label choices default))))
    (or (cdr (assoc answer choices))
        (cons nil answer))))

(provide 'mediawiki-recent)

;;; mediawiki-recent.el ends here
