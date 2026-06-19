;;; mediawiki-cache.el --- Revision cache for mediawiki.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el contributors

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

;; This module provides an in-memory revision cache for mediawiki.el.
;; When the diff engine or history viewer requests revision content, the
;; cache stores it so subsequent access is instant.  It also supports
;; async prefetching for watchlist diffs.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-api)
(require 'cl-lib)

;;; Configuration

(defcustom mediawiki-cache-max-size 200
  "Maximum number of revision entries in the cache."
  :type 'integer
  :group 'mediawiki)

;;; Internal State

(defvar mediawiki-cache--table (make-hash-table :test 'eql)
  "Hash table mapping revision IDs to content strings.")

(defvar mediawiki-cache--order nil
  "List of revision IDs in insertion order (LRU at end).")

;;; Cache Access Functions

(defun mediawiki-cache-get (revid)
  "Return the cached content for revision REVID, or nil if not cached."
  (gethash revid mediawiki-cache--table))

(defun mediawiki-cache-put (revid content)
  "Store CONTENT for revision REVID in the cache.
If the cache exceeds `mediawiki-cache-max-size', evict the oldest entry."
  (puthash revid content mediawiki-cache--table)
  (setq mediawiki-cache--order (cons revid (delq revid mediawiki-cache--order)))
  (when (> (hash-table-count mediawiki-cache--table) mediawiki-cache-max-size)
    (let ((oldest (car (last mediawiki-cache--order))))
      (remhash oldest mediawiki-cache--table)
      (setq mediawiki-cache--order (nbutlast mediawiki-cache--order)))))

(defun mediawiki-cache-has-p (revid)
  "Return non-nil if revision REVID is in the cache."
  (gethash revid mediawiki-cache--table))

(defun mediawiki-cache-clear ()
  "Remove all entries from the cache."
  (clrhash mediawiki-cache--table)
  (setq mediawiki-cache--order nil))

(defun mediawiki-cache-size ()
  "Return the number of entries in the cache."
  (hash-table-count mediawiki-cache--table))

;;; Async Prefetching

(defun mediawiki-cache-prefetch (sitename title revids &optional callback)
  "Prefetch revision content for multiple REVIDS on SITENAME for TITLE.
For each revid not already cached, fetch asynchronously and store.
CALLBACK is called when all fetches complete (with nil args)."
  (let* ((needed (cl-remove-if (lambda (r) (gethash r mediawiki-cache--table)) revids))
         (pending 0))
    (dolist (revid needed)
      (cl-incf pending)
      (mediawiki-api-call-async
       sitename "query"
       (list (cons "prop" "revisions")
             (cons "titles" title)
             (cons "revids" (number-to-string revid))
             (cons "rvprop" "content")
             (cons "rvslots" "main"))
       (lambda (result)
         (let* ((pages (alist-get 'pages (alist-get 'query result)))
                (page (cdar pages))
                (revisions (alist-get 'revisions page))
                (rev (car revisions))
                (content (alist-get '*
                                   (alist-get 'slots rev))))
           (when content
             (mediawiki-cache-put revid content)))
         (cl-decf pending)
         (when (and (zerop pending) callback)
           (funcall callback)))))))

(defun mediawiki-cache-prefetch-watchlist-diffs (sitename entries)
  "Prefetch both sides of diffs for watchlist ENTRIES on SITENAME.
ENTRIES is a list of entry alists with `revid' and `old_revid' keys.
Fetches both old and new revision content for each entry."
  (let ((revids (cl-loop for entry in entries
                         for rev = (alist-get 'revid entry)
                         for old = (alist-get 'old_revid entry)
                         when (and rev (not (zerop rev))) collect rev
                         when (and old (not (zerop old))) collect old)))
    (when revids
      (mediawiki-cache-prefetch sitename "" (delete-dups revids) nil))))

(provide 'mediawiki-cache)

;;; mediawiki-cache.el ends here
