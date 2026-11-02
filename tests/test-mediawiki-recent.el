;;; test-mediawiki-recent.el --- Unit tests for mediawiki-recent.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 MediaWiki.el contributors

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

;; Unit tests for mediawiki-recent.el.  Every test uses a temporary
;; recent-pages file and never touches the network.

;;; Code:

(require 'ert)
(require 'mediawiki-recent)
(require 'mediawiki-page)
(require 'mediawiki-discussion-tools)

;;; Helpers

(defmacro test-mediawiki-recent-with-file (&rest body)
  "Run BODY with an unused recent-pages file and clean state."
  (declare (indent 0))
  `(let* ((file (make-temp-name
                 (expand-file-name "mw-hist-test-" temporary-file-directory)))
          (mediawiki-recent-file file)
          (mediawiki-recent--loaded nil)
          (mediawiki-recent--data nil)
          (mediawiki-recent--dirty nil)
          (mediawiki-recent-record-pages t)
          (mediawiki-recent-record-discussions t))
     (unwind-protect
         (progn ,@body)
       (dolist (candidate (cons file (file-expand-wildcards
                                      (concat file ".bak*"))))
         (when (file-exists-p candidate)
           (delete-file candidate))))))

(defun test-mediawiki-recent--titles (kind)
  "Return the titles stored for KIND."
  (mapcar (lambda (entry) (plist-get entry :title))
          (mediawiki-recent-entries kind)))

;;; Storage tests

(ert-deftest test-mediawiki-recent-round-trip ()
  "Entries survive a write and a fresh read."
  (test-mediawiki-recent-with-file
    (mediawiki-recent-record 'pages "Wikipedia" "Emacs")
    (mediawiki-recent-record 'pages "Wikipedia" "GNU Emacs")
    (mediawiki-recent-record 'discussions "mediawiki.org" "Project:Support desk")
    (should (file-exists-p mediawiki-recent-file))
    ;; Forget everything in memory, then read it back.
    (setq mediawiki-recent--loaded nil
          mediawiki-recent--data nil)
    (should (equal (test-mediawiki-recent--titles 'pages)
                   '("GNU Emacs" "Emacs")))
    (should (equal (test-mediawiki-recent--titles 'discussions)
                   '("Project:Support desk")))))

(ert-deftest test-mediawiki-recent-dedupe-and-count ()
  "Re-recording a page keeps one entry and bumps its count."
  (test-mediawiki-recent-with-file
    (mediawiki-recent-record 'pages "Wikipedia" "Emacs")
    (mediawiki-recent-record 'pages "Wikipedia" "emacs")
    (let ((entries (mediawiki-recent-entries 'pages)))
      (should (= 1 (length entries)))
      (should (= 2 (plist-get (car entries) :count)))
      (should (equal (plist-get (car entries) :title) "emacs")))))

(ert-deftest test-mediawiki-recent-dedupe-first-letter-only ()
  "Only the first letter is case-insensitive."
  (test-mediawiki-recent-with-file
    (mediawiki-recent-record 'pages "Wikipedia" "Emacs")
    (mediawiki-recent-record 'pages "Wikipedia" "EMacs")
    (should (= 2 (length (mediawiki-recent-entries 'pages))))))

(ert-deftest test-mediawiki-recent-cap ()
  "The list keeps only the newest entries."
  (test-mediawiki-recent-with-file
    (let ((mediawiki-recent-max-entries 3))
      (dotimes (i 5)
        (mediawiki-recent-record 'pages "Wikipedia" (format "Page %d" i)))
      (should (equal (test-mediawiki-recent--titles 'pages)
                     '("Page 4" "Page 3" "Page 2"))))))

(ert-deftest test-mediawiki-recent-no-file-until-recorded ()
  "Reading an empty history does not create the file."
  (test-mediawiki-recent-with-file
    (should-not (mediawiki-recent-entries 'pages))
    (should-not (file-exists-p mediawiki-recent-file))))

(ert-deftest test-mediawiki-recent-file-mode ()
  "The file is written with mode 0600 and holds readable Lisp."
  (test-mediawiki-recent-with-file
    (mediawiki-recent-record 'pages "Wikipedia" "Emacs")
    (should (= #o600 (logand (file-modes mediawiki-recent-file) #o777)))
    (with-temp-buffer
      (insert-file-contents mediawiki-recent-file)
      (goto-char (point-min))
      (should (mediawiki-recent--valid-p (read (current-buffer)))))))

(ert-deftest test-mediawiki-recent-corrupt-file-is-quarantined ()
  "A broken file is renamed to .bak and an empty list is used."
  (test-mediawiki-recent-with-file
    (with-temp-file mediawiki-recent-file
      (insert "(this is not a recent pages file"))
    (let ((warnings 0))
      (cl-letf (((symbol-function 'warn)
                 (lambda (&rest _) (setq warnings (1+ warnings)))))
        (should-not (mediawiki-recent-entries 'pages)))
      (should (= 1 warnings)))
    (should (file-exists-p (concat mediawiki-recent-file ".bak")))
    (should-not (file-exists-p mediawiki-recent-file))))

(ert-deftest test-mediawiki-recent-second-corruption-keeps-first-backup ()
  "A second quarantine numbers the backup instead of overwriting it."
  (test-mediawiki-recent-with-file
    (with-temp-file mediawiki-recent-file (insert "junk one"))
    (cl-letf (((symbol-function 'warn) #'ignore))
      (mediawiki-recent-entries 'pages))
    ;; A new session reads the file again.
    (setq mediawiki-recent--loaded nil)
    (with-temp-file mediawiki-recent-file (insert "junk two"))
    (cl-letf (((symbol-function 'warn) #'ignore))
      (mediawiki-recent-entries 'pages))
    (should (file-exists-p (concat mediawiki-recent-file ".bak")))
    (should (file-exists-p (concat mediawiki-recent-file ".bak.1")))))

(ert-deftest test-mediawiki-recent-file-is-read-not-loaded ()
  "A file that looks like code is never evaluated."
  (test-mediawiki-recent-with-file
    (with-temp-file mediawiki-recent-file
      (insert "(setq mediawiki-recent--was-evaluated t)"))
    (cl-letf (((symbol-function 'warn) #'ignore))
      (mediawiki-recent-entries 'pages))
    (should-not (boundp 'mediawiki-recent--was-evaluated))))

(ert-deftest test-mediawiki-recent-unknown-version-is-quarantined ()
  "A newer format is set aside rather than guessed at."
  (test-mediawiki-recent-with-file
    (with-temp-file mediawiki-recent-file
      (insert "(:version 99 :pages nil :discussions nil)"))
    (cl-letf (((symbol-function 'warn) #'ignore))
      (mediawiki-recent-entries 'pages))
    (should (file-exists-p (concat mediawiki-recent-file ".bak")))
    (should-not (file-exists-p mediawiki-recent-file))))

(ert-deftest test-mediawiki-recent-recording-can-be-disabled ()
  "A disabled kind records nothing."
  (test-mediawiki-recent-with-file
    (let ((mediawiki-recent-record-pages nil))
      (mediawiki-recent-record 'pages "Wikipedia" "Emacs"))
    (should-not (file-exists-p mediawiki-recent-file))))

(ert-deftest test-mediawiki-recent-forget-and-clear ()
  "Entries can be removed one at a time or wholesale."
  (test-mediawiki-recent-with-file
    (mediawiki-recent-record 'pages "Wikipedia" "Emacs")
    (mediawiki-recent-record 'pages "Wikipedia" "GNU Emacs")
    (mediawiki-recent-forget 'pages "Wikipedia" "Emacs")
    (should (equal (test-mediawiki-recent--titles 'pages) '("GNU Emacs")))
    (mediawiki-recent-clear 'pages)
    (should-not (mediawiki-recent-entries 'pages))))

;;; Prompting tests

(ert-deftest test-mediawiki-recent-read-choice ()
  "Choosing an entry returns its site and title."
  (test-mediawiki-recent-with-file
    (mediawiki-recent-record 'pages "Wikipedia" "Emacs")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Wikipedia: Emacs")))
      (should (equal (mediawiki-recent-read 'pages)
                     '("Wikipedia" . "Emacs"))))))

(ert-deftest test-mediawiki-recent-read-new-page ()
  "Typing a new page returns it with no site."
  (test-mediawiki-recent-with-file
    (mediawiki-recent-record 'pages "Wikipedia" "Emacs")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Some New Page")))
      (should (equal (mediawiki-recent-read 'pages)
                     '(nil . "Some New Page"))))))

(ert-deftest test-mediawiki-recent-read-empty ()
  "An empty answer returns an empty title."
  (test-mediawiki-recent-with-file
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
      (should (equal (mediawiki-recent-read 'pages) '(nil . ""))))))

(ert-deftest test-mediawiki-recent-local-first ()
  "The preferred site's entries sort before other sites'."
  (test-mediawiki-recent-with-file
    (mediawiki-recent-record 'pages "Wikipedia" "Emacs")
    (mediawiki-recent-record 'pages "Wiktionary" "lexeme")
    (let ((ordered (mediawiki-recent--ordered 'pages "Wiktionary")))
      (should (equal (mapcar (lambda (entry) (plist-get entry :site)) ordered)
                     '("Wiktionary" "Wikipedia"))))))

(ert-deftest test-mediawiki-recent-labels ()
  "Local entries hide their site; other sites show it."
  (should (equal (mediawiki-recent--label '(:site "Wikipedia" :title "Emacs")
                                          "Wikipedia")
                 "Emacs"))
  (should (equal (mediawiki-recent--label '(:site "Wikipedia" :title "Emacs")
                                          "Wiktionary")
                 "Wikipedia: Emacs")))

;;; Integration tests

(ert-deftest test-mediawiki-recent-save-records-page ()
  "A successful save adds the page to the list."
  (test-mediawiki-recent-with-file
    (cl-letf (((symbol-function 'mediawiki-site-get-token)
               (lambda (&rest _) "token"))
              ((symbol-function 'mediawiki-api-call)
               (lambda (&rest _) '((edit (("result" . "Success")))))))
      (with-temp-buffer
        (mediawiki-save-page "Wikipedia" "Emacs" "summary" "new text")))
    (let ((entries (mediawiki-recent-entries 'pages)))
      (should (= 1 (length entries)))
      (should (equal (plist-get (car entries) :site) "Wikipedia"))
      (should (equal (plist-get (car entries) :title) "Emacs")))))

(ert-deftest test-mediawiki-recent-save-failure-records-nothing ()
  "A save that turns into an error adds nothing."
  (test-mediawiki-recent-with-file
    (cl-letf (((symbol-function 'mediawiki-site-get-token)
               (lambda (&rest _) "token"))
              ((symbol-function 'mediawiki-api-call)
               (lambda (&rest _) (error "nope")))
              ((symbol-function 'mediawiki-retry-save-page)
               (lambda (&rest _) (error "nope"))))
      (with-temp-buffer
        (should-error (mediawiki-save-page "Wikipedia" "Emacs" "s" "t"))))
    (should-not (mediawiki-recent-entries 'pages))))

(ert-deftest test-mediawiki-recent-open-uses-entry-site ()
  "Interactive `mediawiki-open' opens a remembered page on its site."
  (test-mediawiki-recent-with-file
    (let ((mediawiki-site nil)
          (opened nil))
      (cl-letf (((symbol-function 'mediawiki-recent-read)
                 (lambda (&rest _) '("Wikipedia" . "Emacs")))
                ((symbol-function 'mediawiki-edit)
                 (lambda (site name) (setq opened (list site name)))))
        (with-temp-buffer
          (call-interactively #'mediawiki-open)))
      (should (equal opened '("Wikipedia" "Emacs"))))))

(ert-deftest test-mediawiki-recent-open-new-page-asks-site ()
  "A new page opens on the prompted site."
  (test-mediawiki-recent-with-file
    (let ((mediawiki-site nil)
          (opened nil))
      (cl-letf (((symbol-function 'mediawiki-recent-read)
                 (lambda (&rest _) '(nil . "Brand New")))
                ((symbol-function 'mediawiki-prompt-for-site)
                 (lambda () "Wiktionary"))
                ((symbol-function 'mediawiki-edit)
                 (lambda (site name) (setq opened (list site name)))))
        (with-temp-buffer
          (call-interactively #'mediawiki-open)))
      (should (equal opened '("Wiktionary" "Brand New"))))))

(ert-deftest test-mediawiki-recent-discussion-tools-records-desk ()
  "Opening a desk records it once the thread list is built."
  (test-mediawiki-recent-with-file
    (unwind-protect
        (cl-letf (((symbol-function 'mediawiki-discussion-tools--fetch-threads)
                   (lambda (&rest _)
                     (list '((id . "h-Topic-20260616005000")
                             (title . "Topic")
                             (status . active)))))
                  ((symbol-function 'mediawiki-discussion-tools--refresh-table)
                   #'ignore)
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buf &rest _) (set-buffer buf))))
          (with-temp-buffer
            (mediawiki-discussion-tools "Talk:Main Page" "Wikipedia")))
      (when (get-buffer "*MW Discussion: Talk:Main Page*")
        (kill-buffer "*MW Discussion: Talk:Main Page*")))
    (let ((entries (mediawiki-recent-entries 'discussions)))
      (should (= 1 (length entries)))
      (should (equal (plist-get (car entries) :site) "Wikipedia"))
      (should (equal (plist-get (car entries) :title) "Talk:Main Page")))))

(provide 'test-mediawiki-recent)

;;; test-mediawiki-recent.el ends here
