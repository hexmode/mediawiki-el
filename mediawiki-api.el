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
Returns the full parsed JSON response as an alist."
  (when (null sitename)
    (error "No sitename given!"))
  (mediawiki-debug-line (format "\n\n----\nFor %s (action=%s):\n\n %s\n" sitename action
                          (mm-url-encode-multipart-form-data
                            (delq nil args) "==")))
  (let* ((raw (url-http-post (mediawiki-make-api-url sitename)
                (append args (list (cons "format" "json")
                               (cons "action" action)))))
          (result (json-parse-string raw
                    :object-type 'alist
                    :array-type 'list
                    :null-object nil
                    :false-object nil)))
    (unless result
      (error "There was an error parsing the result of the API call"))
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
       (alist-get 'content
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

(provide 'mediawiki-api)

;;; mediawiki-api.el ends here
