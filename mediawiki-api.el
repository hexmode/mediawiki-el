;;; mediawiki-api.el --- MediaWiki API interaction functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2025 Mark A. Hershberger

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

;; This module provides MediaWiki API interaction functions including
;; API call functions, query functions, parameter formatting functions,
;; and page data extraction functions.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-http)
(require 'mediawiki-utils)
(require 'url)
(require 'json)
(require 'mm-url)
(require 'mediawiki-site)

;;; API URL Construction

(defun mediawiki-make-api-url (&optional sitename)
  "Translate SITENAME (or MEDIAWIKI-SITE if not given) to a URL."
  (format (let* ((my-parsed (url-generic-parse-url
                              (mediawiki-site-url (or sitename mediawiki-site))))
                  (my-path (url-filename my-parsed)))
	    (when (or (string= my-path "") (not (string= (substring my-path -1) "/")))
	      (setq my-path (concat my-path "/")))
	    (setf (url-filename my-parsed) (concat my-path "api.php"))
	    (url-recreate-url my-parsed))))

;;; API Error and Information Handling

(defun mediawiki-raise (result type notif)
  "Deprecated.  Error and warning handling is now done in `mediawiki-api-call'.
This function is kept for backward compatibility and does nothing."
  nil)

;;; Core API Call Function

(defun mediawiki-api-call (sitename action &optional args)
  "Wrapper for making an API call to SITENAME.
ACTION is the API action.  ARGS is a list of arguments.
Returns the full parsed JSON response as an alist.

If OAuth 2.0 is configured for SITENAME (via `mediawiki-oauth-configured-p'),
an Authorization: Bearer header is automatically added to the request."
  (when (null sitename)
    (error "No sitename given!"))
  (mediawiki-debug-line (format "\n\n----\nFor %s (action=%s):\n\n %s\n" sitename action
                          (mm-url-encode-multipart-form-data
                            (delq nil args) "==")))
  (let* ((headers
           (when (and (fboundp 'mediawiki-oauth-configured-p)
                     (mediawiki-oauth-configured-p sitename))
             (list (mediawiki-oauth-make-auth-header
                    (mediawiki-oauth-get-access-token sitename)))))
          (raw (url-http-post (mediawiki-make-api-url sitename)
                (append args (list (cons "format" "json")
                               (cons "action" action)))
                nil headers))
          (result (condition-case _err
                    (json-parse-string raw
                      :object-type 'alist
                      :array-type 'list
                      :null-object nil
                      :false-object nil)
                    (error
                     (error "There was an error parsing the result of the API call")))))
    ;; result is nil for empty {} responses (e.g. action=logout) — valid, not an error
    ;; Handle errors
    (when-let ((err (alist-get 'error result)))
      (error "(%s) %s"
        (alist-get 'code err)
        (alist-get 'info err)))
    ;; Handle warnings (log via message, do not error)
    (when-let ((warnings (alist-get 'warnings result)))
      (dolist (w warnings)
        (let* ((label (car w))
               (info (alist-get 'warnings (cdr w))))
          (when info
            (message "Warning (%s) %s" label info)))))
    result))

;;; Parameter Formatting

(defun mediawiki-api-param (value)
  "Convert VALUE into a usable form for the MediaWiki API.
* Concat a list into a bar-separated string,
* Turn an integer into a string, or
* Just return the string"
  (cond
    ((integerp value) (int-to-string value))
    ((stringp value) value)
    ((listp value) (mapconcat 'identity value "|"))
    (t (error "Don't know what to do with %s" value))))

;;; Token Management

(defun mediawiki-site-get-token (sitename type)
  "Get token(s) for SITENAME of TYPE type."
  (let* ((result (mediawiki-api-call sitename "query"
                   (list (cons "meta" "tokens")
                     (cons "type" type))))
         (tokens (alist-get 'tokens (alist-get 'query result))))
    (alist-get (intern (concat type "token")) tokens)))

;;; Query Functions

(defun mediawiki-api-query-revisions (sitename title props &optional limit)
  "Get a list of revisions and properties for a given page.
SITENAME is the site to use.  TITLE is a string containing one
title or a list of titles.  PROPS are the revision properties to
fetch.  LIMIT is the upper bound on the number of results to give."
  (when (or (eq nil title) (string= "" title))
    (error "No title passed!"))
  (let* ((result (mediawiki-api-call
                   sitename "query"
                   (list (cons "prop" (mediawiki-api-param (list "info" "revisions")))
                     (cons "titles" (mediawiki-api-param title))
                     (when limit
                       (cons "rvlimit" (mediawiki-api-param limit)))
                     (cons "rvprop" (mediawiki-api-param props))
                     (cons "rvslots" "main")
                     (cons "curtimestamp" "1")))))
    (list (cons 'curtimestamp (alist-get 'curtimestamp result))
          (cons 'pages (alist-get 'pages (alist-get 'query result))))))

(defun mediawiki-api-query-title (sitename title)
  "Query SITENAME for TITLE."
  (let* ((pagelist (mediawiki-api-query-revisions
                     sitename title
                     (list "ids" "timestamp" "flags" "comment" "user" "content"))))
    (mediawiki-pagelist-find-page pagelist title)))

;;; Page Data Extraction Functions

(defun mediawiki-page-get-title (page)
  "Given a PAGE alist, extract the title."
  (alist-get 'title page))

(defun mediawiki-page-get-revision (page revision &optional bit)
  "Given a PAGE alist, extract a REVISION.
If BIT is 'content, return the content string.
If BIT is another symbol, return that field from the revision alist.
If BIT is nil, return the whole revision alist."
  (let ((rev (nth revision (alist-get 'revisions page))))
    (cond
((eq bit 'content)
        (alist-get '*
          (alist-get 'main
            (alist-get 'slots rev))))
      (bit
       (alist-get bit rev))
      (t rev))))

;;; Page List Utilities

(defun mediawiki-pagelist-find-page (pagelist title)
  "Given PAGELIST, extract the information for TITLE."
  (let ((starttimestamp (alist-get 'curtimestamp pagelist))
        (pages (alist-get 'pages pagelist))
        page)
    (dolist (p pages)
      (let ((page-data (cdr p)))
        (when (string= (alist-get 'title page-data)
                       (mediawiki-translate-pagename title))
          (setq page page-data))))
    (when page
      (push (cons 'starttimestamp starttimestamp) page)
      page)))

;;; String Extraction Utilities

(defun mediawiki-extract-string-from-structure (structure)
  "Recursively extract the first string found in STRUCTURE."
  (cond
    ((stringp structure) structure)
    ((listp structure)
      (let ((result nil))
        (dolist (element structure)
          (when (not result)
            (setq result (mediawiki-extract-string-from-structure element))))
        result))
    (t nil)))

;;; Page History

(defun mediawiki-api-get-page-history (sitename title &optional limit)
  "Fetch revision history for TITLE on SITENAME.
LIMIT is max revisions (default 50). Returns list of alists."
  (let* ((result (mediawiki-api-call sitename "query"
                    (list (cons "titles" title)
                          (cons "prop" "revisions")
                          (cons "rvprop" "ids|timestamp|user|comment|size|flags")
                          (cons "rvslots" "main")
                          (cons "rvlimit" (number-to-string (or limit 50))))))
         (pages (alist-get 'pages (alist-get 'query result)))
         (page (cdar pages))
         (revisions (alist-get 'revisions page)))
    (mapcar (lambda (rev)
              (list (cons 'revid (alist-get 'revid rev))
                    (cons 'parentid (alist-get 'parentid rev))
                    (cons 'timestamp (alist-get 'timestamp rev))
                    (cons 'user (alist-get 'user rev))
                    (cons 'comment (alist-get 'comment rev))
                    (cons 'size (alist-get 'size rev))
                    (cons 'minor (alist-get 'minor rev))))
            revisions)))

;;; Revision Content

(defun mediawiki-api-get-revision-content (sitename title revid)
  "Fetch wikitext content of TITLE at revision REVID on SITENAME.
Returns the content string, or nil if not found."
  (let* ((result (mediawiki-api-call sitename "query"
                    (list (cons "titles" title)
                          (cons "prop" "revisions")
                          (cons "rvprop" "content")
                          (cons "rvslots" "main")
                          (cons "rvstartid" revid)
                          (cons "rvendid" revid))))
         (pages (alist-get 'pages (alist-get 'query result)))
         (page (cdar pages))
         (revisions (alist-get 'revisions page))
         (rev (car revisions)))
    (when rev
      (alist-get '*
        (alist-get 'main
          (alist-get 'slots rev))))))

(defun mediawiki-api-get-latest-revision-content (sitename title)
  "Fetch latest revision content and ID for TITLE on SITENAME.
Returns a cons cell (REVID . CONTENT), or nil if page doesn't exist."
  (let* ((result (mediawiki-api-call sitename "query"
                    (list (cons "titles" title)
                          (cons "prop" "revisions")
                          (cons "rvprop" "ids|content")
                          (cons "rvslots" "main")
                          (cons "rvlimit" 1))))
         (pages (alist-get 'pages (alist-get 'query result)))
         (page (cdar pages))
         (revisions (alist-get 'revisions page))
         (rev (car revisions)))
    (when rev
      (cons (alist-get 'revid rev)
            (alist-get '*
              (alist-get 'main
                (alist-get 'slots rev)))))))

;;; Revision Comparison

(defun mediawiki-api-compare-revisions (sitename from-rev to-rev)
  "Get diff HTML between FROM-REV and TO-REV on SITENAME.
Returns the diff body HTML as a string."
  (let* ((result (mediawiki-api-call sitename "compare"
                    (list (cons "fromrev" from-rev)
                          (cons "torev" to-rev))))
         (body (alist-get 'body (alist-get 'compare result))))
    (alist-get '* body)))

;;; Watchlist

(defun mediawiki-api-get-watchlist (sitename &optional limit days)
  "Fetch watchlist entries for SITENAME.
LIMIT is max entries (default 50). DAYS is how many days back (default 30).
Returns list of entry alists."
  (let* ((wlend (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                  (seconds-to-time (- (float-time) (* (or days 30) 86400))) t))
         (result (mediawiki-api-call sitename "query"
                    (list (cons "list" "watchlist")
                          (cons "wlprop" "ids|title|timestamp|user|comment|sizes")
                          (cons "wlallrev" "1")
                          (cons "wlshow" "unread")
                          (cons "wltype" "edit|new")
                          (cons "wllimit" (number-to-string (or limit 50)))
                          (cons "wlend" wlend))))
         (entries (alist-get 'watchlist (alist-get 'query result))))
    entries))

(defun mediawiki-api-set-watch (sitename title unwatch)
  "Add or remove TITLE from watchlist on SITENAME.
When UNWATCH is non-nil, remove; otherwise add.
Returns t on success."
  (let* ((token (or (mediawiki-site-get-token sitename "watch")
                    (mediawiki-site-get-token sitename "csrf")))
         (result (mediawiki-api-call sitename "watch"
                    (append (list (cons "titles" title)
                                  (cons "token" token))
                            (when unwatch
                              (list (cons "unwatch" "1")))))))
    (alist-get 'watch result)))

(defun mediawiki-api-mark-page-seen (sitename title)
  "Mark TITLE as seen on watchlist on SITENAME.
Returns t on success."
  (let* ((token (mediawiki-site-get-token sitename "csrf"))
         (result (mediawiki-api-call sitename "setnotificationtimestamp"
                    (list (cons "titles" title)
                          (cons "token" token)))))
    (alist-get 'setnotificationtimestamp result)))

;;; User Contributions

(defun mediawiki-api-get-user-contributions (sitename username &optional limit)
  "Fetch contributions for USERNAME on SITENAME.
LIMIT is max contributions (default 50).
Returns list of contribution alists."
  (let* ((result (mediawiki-api-call sitename "query"
                    (list (cons "list" "usercontribs")
                          (cons "ucuser" username)
                          (cons "ucprop" "ids|title|timestamp|comment|sizediff|flags")
                          (cons "uclimit" (number-to-string (or limit 50)))))
         (contribs (alist-get 'usercontribs (alist-get 'query result)))))
    contribs))

;;; Thank

(defun mediawiki-api-thank-revision (sitename revid)
  "Send thank notification for revision REVID on SITENAME.
Returns t on success."
  (let* ((token (mediawiki-site-get-token sitename "csrf"))
         (result (mediawiki-api-call sitename "thank"
                    (list (cons "rev" revid)
                          (cons "token" token)))))
    (when (alist-get 'result result)
      t)))

;;; Preview

(defun mediawiki-api-preview (sitename title wikitext)
  "Preview WIKITEXT as content of TITLE on SITENAME.
Returns parsed HTML as a string."
  (let* ((result (mediawiki-api-call sitename "parse"
                    (list (cons "title" title)
                          (cons "text" wikitext)
                          (cons "prop" "text")
                          (cons "disableeditsection" "1")
                          (cons "preview" "1"))))
         (text (alist-get 'text (alist-get 'parse result))))
    (alist-get '* text)))

;;; Edit Actions

(defun mediawiki-api-undo-revision (sitename title revid &optional summary)
  "Undo revision REVID on page TITLE on SITENAME.
Uses MediaWiki undo mechanism (server-side three-way merge).
SUMMARY is optional edit summary. Returns t on success."
  (let* ((token (mediawiki-site-get-token sitename "csrf"))
         (result (mediawiki-api-call sitename "edit"
                    (append (list (cons "title" title)
                                  (cons "undo" revid)
                                  (cons "token" token))
                            (when summary
                              (list (cons "summary" summary)))))))
    (when (alist-get 'edit result)
      t)))

(defun mediawiki-api-restore-revision (sitename title revid &optional summary)
  "Restore page TITLE to revision REVID on SITENAME.
Fetches the content at REVID and submits it as a new edit.
SUMMARY is optional edit summary. Returns t on success."
  (let* ((content (mediawiki-api-get-revision-content sitename title revid))
         (summary (or summary (format "Restored revision %d" revid)))
         (token (mediawiki-site-get-token sitename "csrf"))
         (result (when content
                   (mediawiki-api-call sitename "edit"
                     (list (cons "title" title)
                           (cons "text" content)
                           (cons "summary" summary)
                           (cons "token" token))))))
    (when (and content (alist-get 'edit result))
      t)))

;;; Async API Call

(defvar url-request-method)
(defvar url-request-extra-headers)
(defvar url-request-data)

(defun mediawiki-api-call-async (sitename action params callback)
  "Make async API call to SITENAME with ACTION and PARAMS.
CALLBACK is called with (success) where success is non-nil on success.
Returns nil."
  (let* ((url (mediawiki-make-api-url sitename))
         (all-params (append params
                             (list (cons "format" "json")
                                   (cons "action" action))))
         (data (mm-url-encode-www-form-urlencoded all-params)))
    (let ((url-request-method "POST")
          (url-request-extra-headers
           '(("Content-Type" . "application/x-www-form-urlencoded")))
          (url-request-data data))
      (url-retrieve url
        (lambda (status)
          (let ((success (not (plist-get status :error))))
            (kill-buffer (current-buffer))
            (funcall callback success)))
        nil nil t))))

(provide 'mediawiki-api)

;;; mediawiki-api.el ends here
