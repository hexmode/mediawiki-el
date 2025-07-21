;;; mediawiki-page.el --- Page operations for MediaWiki -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module provides modern page retrieval functionality for MediaWiki.
;; It implements efficient caching and supports retrieving page metadata
;; and revision information.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-api)
(require 'mediawiki-session)

;;; Configuration Variables

(defcustom mediawiki-page-cache-ttl 300
  "Time-to-live in seconds for cached page content.
Set to 0 to disable caching."
  :type 'integer
  :tag "Page Cache TTL"
  :group 'mediawiki)

(defcustom mediawiki-page-cache-size 50
  "Maximum number of pages to keep in cache."
  :type 'integer
  :tag "Page Cache Size"
  :group 'mediawiki)

(defcustom mediawiki-page-retrieve-metadata t
  "Whether to retrieve page metadata by default."
  :type 'boolean
  :tag "Retrieve Page Metadata"
  :group 'mediawiki)

(defcustom mediawiki-page-retrieve-revisions 1
  "Number of revisions to retrieve by default.
Set to 0 to disable revision retrieval."
  :type 'integer
  :tag "Retrieve Revisions"
  :group 'mediawiki)

;;; Cache Implementation

(defvar mediawiki-page-cache (make-hash-table :test 'equal)
  "Cache for page content and metadata.
Keys are (SITENAME . TITLE) cons cells.
Values are page data structures.")

(cl-defstruct mediawiki-page-data
  "Structure representing cached page data."
  title                   ; Page title
  content                 ; Page content
  metadata                ; Page metadata
  revisions               ; List of revisions
  timestamp               ; When the page was retrieved
  cache-key)              ; Cache key for this page

(defun mediawiki-page-cache-key (sitename title)
  "Create a cache key for SITENAME and TITLE."
  (cons sitename (mediawiki-normalize-title title)))

(defun mediawiki-normalize-title (title)
  "Normalize TITLE for consistent caching."
  (replace-regexp-in-string " " "_" title))

(defun mediawiki-page-cache-get (sitename title)
  "Get cached page data for SITENAME and TITLE.
Returns nil if not in cache or if cache entry has expired."
  (let* ((cache-key (mediawiki-page-cache-key sitename title))
         (cached-data (gethash cache-key mediawiki-page-cache)))
    (when cached-data
      ;; Check if cache has expired
      (if (and (> mediawiki-page-cache-ttl 0)
               (> (float-time (time-subtract (current-time)
                                            (mediawiki-page-data-timestamp cached-data)))
                  mediawiki-page-cache-ttl))
          ;; Cache expired, remove it
          (progn
            (remhash cache-key mediawiki-page-cache)
            nil)
        ;; Cache is valid
        cached-data))))

(defun mediawiki-page-cache-put (sitename title page-data)
  "Store PAGE-DATA in cache for SITENAME and TITLE."
  (let ((cache-key (mediawiki-page-cache-key sitename title)))
    ;; Set timestamp and cache key
    (setf (mediawiki-page-data-timestamp page-data) (current-time))
    (setf (mediawiki-page-data-cache-key page-data) cache-key)

    ;; Store in cache
    (puthash cache-key page-data mediawiki-page-cache)

    ;; Prune cache if needed
    (mediawiki-page-cache-prune)))

(defun mediawiki-page-cache-prune ()
  "Prune the page cache if it exceeds `mediawiki-page-cache-size'."
  (when (> (hash-table-count mediawiki-page-cache) mediawiki-page-cache-size)
    (let ((entries '()))
      ;; Collect all entries with timestamps
      (maphash (lambda (key value)
                 (push (cons key (mediawiki-page-data-timestamp value)) entries))
               mediawiki-page-cache)

      ;; Sort by timestamp (oldest first)
      (setq entries (sort entries
                          (lambda (a b)
                            (time-less-p (cdr a) (cdr b)))))

      ;; Remove oldest entries until we're under the limit
      (let ((to-remove (- (hash-table-count mediawiki-page-cache)
                          mediawiki-page-cache-size)))
        (dotimes (i to-remove)
          (when entries
            (remhash (caar entries) mediawiki-page-cache)
            (setq entries (cdr entries))))))))

(defun mediawiki-page-cache-clear (sitename &optional title)
  "Clear cache entries for SITENAME.
If TITLE is provided, only clear that specific page."
  (if title
      (remhash (mediawiki-page-cache-key sitename title) mediawiki-page-cache)
    ;; Clear all entries for this site
    (let ((keys-to-remove '()))
      (maphash (lambda (key _value)
                 (when (string= (car key) sitename)
                   (push key keys-to-remove)))
               mediawiki-page-cache)
      (dolist (key keys-to-remove)
        (remhash key mediawiki-page-cache)))))

;;; Page Retrieval Functions

(defun mediawiki-page-get (sitename title &optional options)
  "Get page content and metadata from SITENAME for TITLE.
OPTIONS is a plist with the following keys:
- :force-refresh - If non-nil, bypass cache
- :metadata - If non-nil, retrieve page metadata
- :revisions - Number of revisions to retrieve (0 for none)
- :section - Section number to retrieve (nil for whole page)
- :redirect - Whether to follow redirects (defaults to t)
- :callback - Function to call with page data (for async)

Returns a mediawiki-page-data structure or nil if page doesn't exist."
  (let ((force-refresh (plist-get options :force-refresh))
        (get-metadata (if (plist-member options :metadata)
                         (plist-get options :metadata)
                       mediawiki-page-retrieve-metadata))
        (get-revisions (if (plist-member options :revisions)
                          (plist-get options :revisions)
                        mediawiki-page-retrieve-revisions))
        (section (plist-get options :section))
        (follow-redirect (if (plist-member options :redirect)
                            (plist-get options :redirect)
                          t))
        (callback (plist-get options :callback)))

    ;; Check cache first unless force refresh
    (let ((cached-data (unless force-refresh
                         (mediawiki-page-cache-get sitename title))))
      (if cached-data
          ;; Use cached data
          (progn
            (mediawiki-debug-log "Using cached page data for %s on %s"
                                title sitename)
            (if callback
                (funcall callback cached-data)
              cached-data))

        ;; Need to fetch from API
        (if callback
            ;; Async retrieval
            (mediawiki-page-get-async sitename title get-metadata get-revisions
                                     section follow-redirect callback)
          ;; Sync retrieval
          (mediawiki-page-get-sync sitename title get-metadata get-revisions
                                  section follow-redirect))))))

(defun mediawiki-page-get-sync (sitename title get-metadata get-revisions section follow-redirect)
  "Synchronously get page from SITENAME with TITLE.
GET-METADATA, GET-REVISIONS, SECTION, and FOLLOW-REDIRECT control retrieval options."
  (let* ((params (mediawiki-page-build-query-params
                 title get-metadata get-revisions section follow-redirect))
         (response (mediawiki-api-call-sync sitename "query" params)))

    (if (mediawiki-api-response-success response)
        (let ((page-data (mediawiki-page-parse-response response title)))
          ;; Cache the result if we got valid data
          (when (and page-data
                     (mediawiki-page-data-content page-data))
            (mediawiki-page-cache-put sitename title page-data))
          page-data)

      ;; Handle error
      (mediawiki-debug-log "Failed to retrieve page %s: %s"
                          title
                          (mediawiki-api-format-error-summary response))
      nil)))

(defun mediawiki-page-get-async (sitename title get-metadata get-revisions
                                        section follow-redirect callback)
  "Asynchronously get page from SITENAME with TITLE.
GET-METADATA, GET-REVISIONS, SECTION, and FOLLOW-REDIRECT control retrieval options.
CALLBACK is called with the page data."
  (let ((params (mediawiki-page-build-query-params
                title get-metadata get-revisions section follow-redirect)))

    (mediawiki-api-call-async
     sitename "query" params
     (lambda (response)
       (let ((page-data (mediawiki-page-parse-response response title)))
         ;; Cache the result if we got valid data
         (when (and page-data
                    (mediawiki-page-data-content page-data))
           (mediawiki-page-cache-put sitename title page-data))

         ;; Call the callback with the result
         (funcall callback page-data)))

     ;; Error callback
     (lambda (response)
       (mediawiki-debug-log "Failed to retrieve page %s: %s"
                           title
                           (mediawiki-api-format-error-summary response))
       (funcall callback nil)))))

(defun mediawiki-page-build-query-params (title get-metadata get-revisions section follow-redirect)
  "Build API query parameters for page retrieval.
TITLE is the page title to retrieve.
GET-METADATA controls whether to retrieve page metadata.
GET-REVISIONS is the number of revisions to retrieve.
SECTION is the section number to retrieve (nil for whole page).
FOLLOW-REDIRECT controls whether to follow redirects."
  (let ((params '()))
    ;; Basic query parameters
    (push (cons "prop" "revisions") params)
    (push (cons "titles" title) params)
    (push (cons "rvslots" "main") params)

    ;; Content retrieval
    (push (cons "rvprop" "content") params)

    ;; Section retrieval
    (when section
      (push (cons "rvsection" (number-to-string section)) params))

    ;; Redirect handling
    (unless follow-redirect
      (push (cons "redirects" "0") params))

    ;; Metadata retrieval
    (when get-metadata
      (setcar (cdr (assoc "prop" params))
              (concat (cdr (assoc "prop" params)) "|info"))
      (push (cons "inprop" "url|displaytitle|protection|talkid|watched|watchers|notificationtimestamp|subjectid|associatedpage|url|readable|preload|displaytitle") params))

    ;; Revision retrieval
    (when (> get-revisions 0)
      (push (cons "rvlimit" (number-to-string get-revisions)) params)
      (setcar (cdr (assoc "rvprop" params))
              (concat (cdr (assoc "rvprop" params)) "|ids|timestamp|flags|comment|user")))

    ;; Return the parameters
    params))

(defun mediawiki-page-parse-response (response title)
  "Parse page data from API RESPONSE for TITLE.
Returns a mediawiki-page-data structure or nil if page doesn't exist."
  (let* ((data (mediawiki-api-response-data response))
         (query (cdr (assq 'query data)))
         (pages (cdr (assq 'pages query)))
         (redirects (cdr (assq 'redirects query)))
         (normalized (cdr (assq 'normalized query)))
         (page-data nil)
         (page-title title)
         (page-content nil)
         (page-metadata nil)
         (page-revisions nil))

    ;; Handle title normalization
    (when normalized
      (let ((norm (cl-find-if (lambda (norm)
                               (string= (cdr (assq 'from norm)) title))
                             normalized)))
        (when norm
          (setq page-title (cdr (assq 'to norm))))))

    ;; Handle redirects
    (when redirects
      (let ((redirect (cl-find-if (lambda (redir)
                                   (string= (cdr (assq 'from redir)) page-title))
                                 redirects)))
        (when redirect
          (setq page-title (cdr (assq 'to redirect))))))

    ;; Find the page in the response
    (let ((page nil))
      (dolist (p pages)
        (when (string= (cdr (assq 'title p)) page-title)
          (setq page p)))

      (when page
        ;; Check if page exists
        (if (assq 'missing page)
            (mediawiki-debug-log "Page %s does not exist" title)

          ;; Extract content
          (let* ((revisions (cdr (assq 'revisions page)))
                 (revision (car revisions)))
            (when revision
              ;; Extract content from revision
              (let* ((slots (cdr (assq 'slots revision)))
                     (main-slot (cdr (assq 'main slots))))
                (when main-slot
                  (setq page-content (cdr (assq 'content main-slot))))))

            ;; Extract revisions
            (when revisions
              (setq page-revisions
                    (mapcar (lambda (rev)
                              (let* ((slots (cdr (assq 'slots rev)))
                                     (main-slot (cdr (assq 'main slots)))
                                     (content (when main-slot
                                               (cdr (assq 'content main-slot)))))
                                (list :revid (cdr (assq 'revid rev))
                                      :parentid (cdr (assq 'parentid rev))
                                      :user (cdr (assq 'user rev))
                                      :timestamp (cdr (assq 'timestamp rev))
                                      :comment (cdr (assq 'comment rev))
                                      :content content)))
                            revisions)))

            ;; Extract metadata (remove revisions to avoid duplication)
            (let ((metadata-page (copy-alist page)))
              (setq metadata-page (assq-delete-all 'revisions metadata-page))
              (setq page-metadata metadata-page))

            ;; Create page data structure
            (setq page-data (make-mediawiki-page-data
                             :title page-title
                             :content page-content
                             :metadata page-metadata
                             :revisions page-revisions))))))

    page-data))

;;; Convenience Functions

(defun mediawiki-page-get-content (sitename title &optional options)
  "Get page content from SITENAME for TITLE.
OPTIONS is passed to `mediawiki-page-get'.
Returns the page content as a string or nil if page doesn't exist."
  (let ((page-data (mediawiki-page-get sitename title options)))
    (when page-data
      (mediawiki-page-data-content page-data))))

(defun mediawiki-page-get-metadata (sitename title &optional options)
  "Get page metadata from SITENAME for TITLE.
OPTIONS is passed to `mediawiki-page-get'.
Returns the page metadata or nil if page doesn't exist."
  (let ((page-data (mediawiki-page-get
                   sitename title
                   (plist-put options :metadata t))))
    (when page-data
      (mediawiki-page-data-metadata page-data))))

(defun mediawiki-page-get-revisions (sitename title count &optional options)
  "Get COUNT revisions for page on SITENAME with TITLE.
OPTIONS is passed to `mediawiki-page-get'.
Returns a list of revisions or nil if page doesn't exist."
  (let ((page-data (mediawiki-page-get
                   sitename title
                   (plist-put options :revisions count))))
    (when page-data
      (mediawiki-page-data-revisions page-data))))

(defun mediawiki-page-exists-p (sitename title)
  "Check if page with TITLE exists on SITENAME."
  (let ((page-data (mediawiki-page-get sitename title '(:metadata t :revisions 0))))
    (and page-data (mediawiki-page-data-content page-data) t)))

(defun mediawiki-page-get-property (sitename title property &optional options)
  "Get specific PROPERTY from page metadata on SITENAME with TITLE.
OPTIONS is passed to `mediawiki-page-get'.
Returns the property value or nil if not found."
  (let ((metadata (mediawiki-page-get-metadata sitename title options)))
    (when metadata
      (cdr (assq property metadata)))))

(defun mediawiki-page-get-url (sitename title)
  "Get the full URL for page with TITLE on SITENAME."
  (mediawiki-page-get-property sitename title 'fullurl '(:metadata t :revisions 0)))

(defun mediawiki-page-get-last-editor (sitename title)
  "Get the username of the last editor of page with TITLE on SITENAME."
  (let ((revisions (mediawiki-page-get-revisions sitename title 1)))
    (when revisions
      (plist-get (car revisions) :user))))

(defun mediawiki-page-get-last-edit-time (sitename title)
  "Get the timestamp of the last edit to page with TITLE on SITENAME."
  (let ((revisions (mediawiki-page-get-revisions sitename title 1)))
    (when revisions
      (plist-get (car revisions) :timestamp))))

(defun mediawiki-page-get-section (sitename title section &optional options)
  "Get specific SECTION from page with TITLE on SITENAME.
OPTIONS is passed to `mediawiki-page-get'.
Returns the section content as a string or nil if not found."
  (let ((options (plist-put options :section section)))
    (mediawiki-page-get-content sitename title options)))

(provide 'mediawiki-page)

;;; mediawiki-page.el ends here
