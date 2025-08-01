;;; mediawiki-api.el --- MediaWiki API interaction functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2008, 2009, 2010, 2011, 2015 Mark A. Hershberger

;; Author: Mark A. Hershberger <mah@everybody.org>
;; Keywords: mediawiki wikipedia network wiki
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
(require 'xml)
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
  "Show a TYPE of information from the RESULT to the user using NOTIF"
  (when (assq type (cddr result))
    (mapc (lambda (err)
            (let ((label (or (cdr (assq 'code err))
                             (car err)))
                  (info (or (cdr (assq 'info err))
                            (cddr err))))
              (when info
                (funcall notif label info))))

          ;; Poor man's attempt at backward compatible xml form handling
          (if (listp (cdr (assq type (cddr result))))
              (cdr (assq type (cddr result)))
            (cddr (assq type (cddr result)))))))

;;; Core API Call Function

(defun mediawiki-api-call (sitename action &optional args)
  "Wrapper for making an API call to SITENAME.
ACTION is the API action.  ARGS is a list of arguments."
  (mediawiki-debug-line (format "\n\n----\nFor %s (action=%s):\n\n %s\n" sitename action
                                (mm-url-encode-multipart-form-data
                                 (delq nil args) "==")))
  (let* ((raw (url-http-post (mediawiki-make-api-url sitename)
                             (append args (list (cons "format" "xml")
                                                (cons "action" action)))))
         (result (assoc 'api
                        (with-temp-buffer
                          (insert raw)
                          (xml-parse-region (point-min) (point-max))))))
    (unless result
      (error "There was an error parsing the result of the API call"))

    (mediawiki-raise result 'warnings
                     (lambda (label info)
                       (message "Warning (%s) %s" label info)))
    (mediawiki-raise result 'info
                     (lambda (label info)
                       (message  "(%s) %s" label info)))
    (mediawiki-raise result 'error
                     (lambda (label info)
                       (error "(%s) %s" label info)))

    (if (cddr result)
        (let ((action-res (assq (intern action) (cddr result))))
          (unless action-res
            (error "Didn't see action name in the result list"))

          action-res)
      t)))

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
  (cdr
   (caadar
    (cddr (mediawiki-api-call sitename "query"
                              (list (cons "meta" "tokens")
                                    (cons "type" type)))))))

;;; Query Functions

(defun mediawiki-api-query-revisions (sitename title props &optional limit)
  "Get a list of revisions and properties for a given page.
SITENAME is the site to use.  TITLE is a string containing one
title or a list of titles.  PROPS are the revision properites to
fetch.  LIMIT is the upper bound on the number of results to give."
  (when (or (eq nil title) (string= "" title))
      (error "No title passed!"))
  (let ((qresult (mediawiki-api-call
         sitename "query"
         (list (cons "prop" (mediawiki-api-param (list "info" "revisions")))
               (cons "titles" (mediawiki-api-param title))
               (when limit
                 (cons "rvlimit" (mediawiki-api-param limit)))
               (cons "rvprop" (mediawiki-api-param props))
               (cons "rvslots" "main")))))
    (if (eq t qresult)
        (error "No results for revision query.")
      (cddr qresult))))

(defun mediawiki-api-query-title (sitename title)
  "Query SITENAME for TITLE."
  (let* ((pagelist (mediawiki-api-query-revisions
                    sitename title
                    (list "ids" "timestamp" "flags" "comment" "user" "content"))))
    (mediawiki-pagelist-find-page pagelist title)))

;;; Page Data Extraction Functions

(defun mediawiki-page-get-title (page)
  "Given a PAGE from a pagelist structure, extract the title."
  (cdr (assq 'title (cadr page))))

(defun mediawiki-page-get-revision (page revision &optional bit)
  "Given a PAGE, extract a REVISION from the pagelist structure.
If BIT is \='content, then return the content only.  Otherwise,
return only the items that BIT matches.  If BIT isn't given,
return the whole revision structure."
  (let ((rev (cdr (nth revision (cddr (assq 'revisions (cddr page)))))))
    (cond
     ((eq bit 'content)
      ;; Handle new slot-based format introduced with rvslots parameter
      (let ((content-data (cadr rev)))
        (cond
         ;; Check if this is the new slots format
         ((and (listp content-data) (eq (car content-data) 'slots))
          ;; New format: (slots nil (slot ((attrs...)) "content"))
          ;; Extract the content string from the slot structure
          (let ((slot-element (nth 2 content-data)))  ; Get the slot element
            (if (and (listp slot-element) (eq (car slot-element) 'slot))
                ;; The content is the last element in the slot
                (car (last slot-element))
              "")))
         ;; Check if content-data is already a string (old format)
         ((stringp content-data)
          content-data)
         ;; Fallback
         (t ""))))
     ((assoc bit (car rev))
      (cdr (assoc bit (car rev))))
     (t rev))))

;;; Page List Utilities

(defun mediawiki-pagelist-find-page (pagelist title)
  "Given PAGELIST, extract the informaton for TITLE."
  (let ((pl (cddr (assq 'pages pagelist)))
        page current)
    (while (and (not page)
                (setq current (pop pl)))
      ;; This fails when underbars are here instead of spaces,
      ;; so we make sure that it has the mediawiki pagename
      (when (string= (mediawiki-page-get-title current)
                     (mediawiki-translate-pagename title))
        (setq page current)))
    page))

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
