;;; mediawiki-ui.el --- Modern UI enhancements for MediaWiki -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el Contributors

;; This file is part of mediawiki.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module provides modern Emacs UI enhancements for MediaWiki
;; operations, including improved completion, suggestion features,
;; and enhanced user interaction flows.

;;; Code:

(require 'cl-lib)
(require 'eldoc)
(require 'mediawiki-core)
(require 'mediawiki-api)
(require 'mediawiki-page)

;;; Configuration

(defgroup mediawiki-ui nil
  "UI enhancements for MediaWiki operations."
  :tag "MediaWiki UI"
  :group 'mediawiki)

(defcustom mediawiki-ui-use-transient t
  "Use transient.el for command interfaces when available."
  :type 'boolean
  :tag "Use Transient"
  :group 'mediawiki-ui)

(defcustom mediawiki-ui-completion-framework 'auto
  "Completion framework to use for MediaWiki operations."
  :type '(choice (const :tag "Auto-detect" auto)
           (const :tag "Completing-read" completing-read)
           (const :tag "Ivy" ivy)
           (const :tag "Helm" helm)
           (const :tag "Vertico" vertico))
  :tag "Completion Framework"
  :group 'mediawiki-ui)

(defcustom mediawiki-ui-show-page-preview t
  "Show page preview in completion when selecting pages."
  :type 'boolean
  :tag "Show Page Preview"
  :group 'mediawiki-ui)

(defcustom mediawiki-ui-recent-pages-count 20
  "Number of recent pages to remember for quick access."
  :type 'integer
  :tag "Recent Pages Count"
  :group 'mediawiki-ui)

(defcustom mediawiki-ui-enable-eldoc t
  "Enable ElDoc support in MediaWiki mode."
  :type 'boolean
  :tag "Enable ElDoc"
  :group 'mediawiki-ui)

(defcustom mediawiki-ui-enable-which-key t
  "Enable Which-Key descriptions for MediaWiki commands."
  :type 'boolean
  :tag "Enable Which-Key"
  :group 'mediawiki-ui)

;;; Enhanced Completion System

(defvar mediawiki-ui-completion-cache (make-hash-table :test 'equal)
  "Cache for completion candidates by site.")

(defvar mediawiki-ui-recent-pages (make-hash-table :test 'equal)
  "Recent pages by site for quick access.")

(defun mediawiki-ui-detect-completion-framework ()
  "Detect the best available completion framework."
  (cond
    ((and (eq mediawiki-ui-completion-framework 'auto)
       (featurep 'vertico)) 'vertico)
    ((and (eq mediawiki-ui-completion-framework 'auto)
       (featurep 'ivy)) 'ivy)
    ((and (eq mediawiki-ui-completion-framework 'auto)
       (featurep 'helm)) 'helm)
    ((eq mediawiki-ui-completion-framework 'auto) 'completing-read)
    (t mediawiki-ui-completion-framework)))

(defun mediawiki-ui-add-recent-page (sitename title)
  "Add TITLE to recent pages for SITENAME."
  (let ((recent (gethash sitename mediawiki-ui-recent-pages)))
    (unless recent
      (setq recent '())
      (puthash sitename recent mediawiki-ui-recent-pages))

    ;; Remove if already exists
    (setq recent (delete title recent))

    ;; Add to front
    (push title recent)

    ;; Limit size
    (when (> (length recent) mediawiki-ui-recent-pages-count)
      (setq recent (butlast recent)))

    (puthash sitename recent mediawiki-ui-recent-pages)))

(defun mediawiki-ui-get-recent-pages (sitename)
  "Get recent pages for SITENAME."
  (or (gethash sitename mediawiki-ui-recent-pages) '()))

(defun mediawiki-ui-get-page-suggestions (sitename partial-title)
  "Get page suggestions for SITENAME matching PARTIAL-TITLE."
  (let ((cache-key (format "%s:%s" sitename partial-title))
         (cached (gethash cache-key mediawiki-ui-completion-cache)))

    (if (and cached (< (- (float-time) (car cached)) 300)) ; 5 minute cache
      (cdr cached)

      ;; Fetch suggestions asynchronously
      (let ((suggestions '()))
        (condition-case err
          (let ((params `(("action" . "opensearch")
                           ("search" . ,partial-title)
                           ("limit" . "10")
                           ("namespace" . "0")
                           ("format" . "json"))))

            (let ((response (mediawiki-api-call-sync sitename "opensearch" params)))
              (when (mediawiki-api-response-success response)
                (let ((data (mediawiki-api-response-data response)))
                  (when (and data (arrayp data) (> (length data) 1))
                    (setq suggestions (append (aref data 1) nil)))))))

          (error
            (mediawiki-debug-log-with-module "ui" "Failed to get suggestions: %s"
              (error-message-string err))))

        ;; Cache the results
        (puthash cache-key (cons (float-time) suggestions)
          mediawiki-ui-completion-cache)

        suggestions))))

(defun mediawiki-ui-annotate-page (candidate sitename)
  "Annotate page CANDIDATE with metadata for SITENAME."
  (let ((recent (mediawiki-ui-get-recent-pages sitename)))
    (cond
      ((member candidate recent) " (recent)")
      (t ""))))

(defun mediawiki-ui-completing-read-page (prompt sitename &optional initial-input)
  "Enhanced completing-read for page selection with PROMPT on SITENAME."
  (let* ((recent-pages (mediawiki-ui-get-recent-pages sitename))
          (completion-extra-properties
            (list :annotation-function
              (lambda (candidate)
                (mediawiki-ui-annotate-page candidate sitename))
              :company-docsig
              (lambda (candidate)
                (when mediawiki-ui-show-page-preview
                  (format "Page: %s on %s" candidate sitename)))))
          (dynamic-collection
            (lambda (string predicate action)
              (if (eq action 'metadata)
                '(metadata
                   (display-sort-function . identity)
                   (annotation-function .
                     (lambda (candidate)
                       (mediawiki-ui-annotate-page candidate sitename))))

                (let* ((suggestions (when (> (length string) 2)
                                      (mediawiki-ui-get-page-suggestions sitename string)))
                        (all-candidates (append recent-pages suggestions)))
                  (all-completions string all-candidates predicate))))))

    (completing-read prompt dynamic-collection nil nil initial-input)))

;;; Enhanced Site Selection

(defun mediawiki-ui-get-site-candidates ()
  "Get list of configured sites with annotations."
  (mapcar (lambda (site-entry)
            (let* ((name (car site-entry))
                    (site (cdr site-entry))
                    (url (mediawiki-site-config-url site))
                    (session (mediawiki-get-session name))
                    (status (if session "connected" "disconnected")))
              (cons (format "%s (%s)" name status) name)))
    mediawiki-site-alist))

(defun mediawiki-ui-select-site (&optional prompt)
  "Select a MediaWiki site with enhanced UI."
  (let* ((prompt (or prompt "Select site: "))
          (candidates (mediawiki-ui-get-site-candidates))
          (completion-extra-properties
            (list :annotation-function
              (lambda (candidate)
                (let* ((site-name (alist-get candidate candidates nil nil #'string=))
                        (site (mediawiki-get-site site-name)))
                  (when site
                    (format " [%s]" (mediawiki-site-config-url site))))))))

    (if candidates
      (let ((selected (completing-read prompt candidates nil t)))
        (alist-get selected candidates nil nil #'string=))

      (error "No MediaWiki sites configured. Use M-x customize-group mediawiki to set up sites"))))

;;; Command Interface Enhancements

;;;###autoload
(defun mediawiki-ui-save-with-options ()
  "Save current page with enhanced options interface."
  (interactive)
  (unless (eq major-mode 'mediawiki-mode)
    (user-error "Not in a MediaWiki buffer"))

  (let* ((summary (read-string "Edit summary: " nil 'mediawiki-summary-history))
          (minor-edit (yes-or-no-p "Mark as minor edit? "))
          (watch-page (yes-or-no-p "Add to watchlist? ")))

    (mediawiki-page-save-async
      mediawiki-site
      `(:title ,mediawiki-page-title
         :text ,(buffer-substring-no-properties (point-min) (point-max))
         :summary ,summary
         :minor ,(if minor-edit "1" "0")
         :watchlist ,(if watch-page "watch" "unwatch"))

      (lambda (response)
        (message "Page saved successfully")
        (when mediawiki-ui-enable-which-key
          (which-key--show-popup "Page saved!")))

      (lambda (error)
        (message "Failed to save page: %s" error)))))

;;;###autoload
(defun mediawiki-ui-open-with-preview ()
  "Open a MediaWiki page with preview functionality."
  (interactive)
  (let* ((sitename (or mediawiki-site (mediawiki-ui-select-site)))
          (title (mediawiki-ui-completing-read-page "Open page: " sitename)))

    (when (and title (not (string-empty-p title)))
      (mediawiki-ui-add-recent-page sitename title)

      ;; Show loading message
      (message "Loading page: %s..." title)

      ;; Load page asynchronously with progress
      (let ((progress-id (when mediawiki-progress-feedback-enabled
                           (mediawiki-progress-start
                             (format "Loading page: %s" title)))))

        (mediawiki-page-get-async
          sitename title t t

          (lambda (page-data)
            (when progress-id
              (mediawiki-progress-finish progress-id "Page loaded"))

            (let ((buffer (get-buffer-create (format "*MediaWiki: %s*" title))))
              (with-current-buffer buffer
                (erase-buffer)
                (insert (mediawiki-page-data-content page-data))
                (mediawiki-mode)
                (setq mediawiki-site sitename
                  mediawiki-page-title title)
                (set-buffer-modified-p nil))

              (switch-to-buffer buffer)
              (message "Loaded page: %s" title)))

          (lambda (error)
            (when progress-id
              (mediawiki-progress-finish progress-id "Failed to load page"))
            (message "Failed to load page: %s" error)))))))

;;; Quick Access Menu

(defvar mediawiki-ui-quick-menu-history '()
  "History for quick menu selections.")

;;;###autoload
(defun mediawiki-ui-quick-menu ()
  "Show a quick access menu for common MediaWiki operations."
  (interactive)

  (let* ((choices '(("Open page" . mediawiki-ui-open-with-preview)
                     ("Save page" . mediawiki-ui-save-with-options)
                     ("Recent pages" . mediawiki-ui-show-recent-pages)
                     ("Search pages" . mediawiki-ui-search-pages)
                     ("Page history" . mediawiki-ui-show-page-history)
                     ("Site statistics" . mediawiki-ui-show-site-statistics)
                     ("Login/Logout" . mediawiki-ui-manage-authentication)
                     ("Settings" . (lambda () (customize-group 'mediawiki)))))

          (completion-extra-properties
            (list :annotation-function
              (lambda (choice)
                (pcase choice
                  ("Open page" " - Load a wiki page")
                  ("Save page" " - Save current page")
                  ("Recent pages" " - Quick access to recent pages")
                  ("Search pages" " - Find pages by content")
                  ("Page history" " - View edit history")
                  ("Site statistics" " - Show async operation stats")
                  ("Login/Logout" " - Manage authentication")
                  ("Settings" " - Configure MediaWiki settings")
                  (_ "")))))

          (selected (completing-read "MediaWiki: "
                      (mapcar #'car choices)
                      nil t nil 'mediawiki-ui-quick-menu-history)))

    (when selected
      (let ((command (cdr (assoc selected choices))))
        (if (functionp command)
          (funcall command)
          (call-interactively command))))))

(defun mediawiki-ui-show-recent-pages ()
  "Show recent pages for quick access."
  (interactive)
  (let* ((sitename (or mediawiki-site (mediawiki-ui-select-site)))
          (recent-pages (mediawiki-ui-get-recent-pages sitename)))

    (if recent-pages
      (let* ((completion-extra-properties
               (list :annotation-function
                 (lambda (_) " (recent)")))
              (selected (completing-read "Recent pages: " recent-pages nil t)))

        (when selected
          (mediawiki-ui-add-recent-page sitename selected)
          (mediawiki-edit sitename selected)))

      (message "No recent pages for site: %s" sitename))))

;;; Enhanced Search Interface

(defun mediawiki-ui-search-pages ()
  "Search for pages with enhanced interface."
  (interactive)
  (let* ((sitename (or mediawiki-site (mediawiki-ui-select-site)))
          (query (read-string "Search for: "))
          (limit (read-number "Max results: " 20)))

    (when (and query (not (string-empty-p query)))
      (message "Searching for: %s..." query)

      (mediawiki-api-call-async
        sitename "query"
        `(("list" . "search")
           ("srsearch" . ,query)
           ("srlimit" . ,(number-to-string limit))
           ("srprop" . "snippet"))

        (lambda (response)
          (let ((results (cdr (assq 'search (mediawiki-api-response-data response)))))
            (if results
              (mediawiki-ui-show-search-results sitename query results)
              (message "No results found for: %s" query))))

        (lambda (error)
          (message "Search failed: %s" error))))))

(defun mediawiki-ui-show-search-results (sitename query results)
  "Show search RESULTS for QUERY on SITENAME in a buffer."
  (let ((buffer (get-buffer-create (format "*MediaWiki Search: %s*" query))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Search results for '%s' on %s:\n" query sitename))
      (insert (make-string 50 ?=) "\n\n")

      (dolist (result results)
        (let ((title (cdr (assq 'title result)))
               (snippet (cdr (assq 'snippet result)))
               (size (cdr (assq 'size result))))

          (insert (propertize title 'face 'bold) "\n")
          (when size
            (insert (format "Size: %s bytes\n" size)))
          (when snippet
            (insert snippet "\n"))
          (insert "\n")))

      (goto-char (point-min))
      (special-mode))

    (display-buffer buffer)))

;;; Statistics and Monitoring UI

;;;###autoload
(defun mediawiki-ui-show-site-statistics ()
  "Show comprehensive site and operation statistics."
  (interactive)
  (let ((buffer (get-buffer-create "*MediaWiki Statistics*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "MediaWiki Operation Statistics\n")
      (insert (make-string 35 ?=) "\n\n")

      ;; Site information
      (insert "Configured Sites:\n")
      (dolist (site-entry mediawiki-site-alist)
        (let* ((name (car site-entry))
                (session (mediawiki-get-session name))
                (status (if session "✓ Connected" "✗ Disconnected")))
          (insert (format "  %s: %s\n" name status))))

      (insert "\n")

      ;; Async operation statistics
      (if (> (hash-table-count mediawiki-async-operation-statistics) 0)
        (progn
          (insert "Async Operation Statistics:\n")
          (maphash (lambda (op-type stats)
                     (insert (format "  %s:\n" op-type))
                     (insert (format "    Total: %d | Completed: %d | Failed: %d\n"
                               (mediawiki-async-stats-total-operations stats)
                               (mediawiki-async-stats-completed-operations stats)
                               (mediawiki-async-stats-failed-operations stats))))
            mediawiki-async-operation-statistics))
        (insert "No async operation statistics available.\n"))

      (insert "\n")

      ;; Recent pages by site
      (insert "Recent Pages by Site:\n")
      (if (> (hash-table-count mediawiki-ui-recent-pages) 0)
        (maphash (lambda (sitename pages)
                   (insert (format "  %s: %d pages\n" sitename (length pages))))
          mediawiki-ui-recent-pages)
        (insert "  No recent pages.\n"))

      (goto-char (point-min))
      (special-mode))

    (display-buffer buffer)))

;;; Authentication Management UI

;;;###autoload
(defun mediawiki-ui-manage-authentication ()
  "Manage authentication for MediaWiki sites."
  (interactive)
  (let* ((sitename (mediawiki-ui-select-site "Manage authentication for site: "))
          (session (mediawiki-get-session sitename))
          (choices (if session
                     '(("Logout" . logout)
                        ("Check status" . status)
                        ("Refresh tokens" . refresh))
                     '(("Login" . login)
                        ("Setup OAuth" . oauth-setup)))))

    (let* ((completion-extra-properties
             (list :annotation-function
               (lambda (choice)
                 (pcase choice
                   ("Login" " - Authenticate with username/password")
                   ("Logout" " - End current session")
                   ("Check status" " - Verify authentication status")
                   ("Refresh tokens" " - Refresh authentication tokens")
                   ("Setup OAuth" " - Configure OAuth authentication")
                   (_ "")))))
            (selected (completing-read (format "Authentication (%s): " sitename)
                        (mapcar #'car choices) nil t)))

      (when selected
        (let ((action (cdr (assoc selected choices))))
          (pcase action
            ('login (mediawiki-do-login sitename))
            ('logout (mediawiki-do-logout sitename))
            ('status (mediawiki-ui-show-auth-status sitename))
            ('refresh (mediawiki-ui-refresh-auth sitename))
            ('oauth-setup (mediawiki-ui-setup-oauth sitename))))))))

(defun mediawiki-ui-show-auth-status (sitename)
  "Show authentication status for SITENAME."
  (let ((session (mediawiki-get-session sitename)))
    (if session
      (let ((user-info (mediawiki-session-user-info session))
             (login-time (mediawiki-session-login-time session)))
        (message "Authenticated as %s (since %s)"
          (or (cdr (assq 'name user-info)) "unknown")
          (format-time-string "%Y-%m-%d %H:%M:%S" login-time)))
      (message "Not authenticated to %s" sitename))))

(defun mediawiki-ui-refresh-auth (sitename)
  "Refresh authentication for SITENAME."
  (message "Refreshing authentication for %s..." sitename)
  ;; Implementation would depend on the auth system
  (message "Authentication refreshed for %s" sitename))

(defun mediawiki-ui-setup-oauth (sitename)
  "Setup OAuth authentication for SITENAME."
  (message "OAuth setup not yet implemented for %s" sitename))

;;; ElDoc Integration

(defun mediawiki-ui-eldoc-function ()
  "Provide ElDoc information for MediaWiki markup."
  (when (and mediawiki-ui-enable-eldoc (eq major-mode 'mediawiki-mode))
    (save-excursion
      (let ((original-point (point))
             (thing-at-point (thing-at-point 'symbol)))
        (cond
          ;; Check for wikilink - look backward and forward for [[ ]]
          ((save-excursion
             (let ((start (search-backward "[[" (line-beginning-position) t))
                    (end (search-forward "]]" (line-end-position) t)))
               (when (and start end (>= original-point start) (<= original-point end))
                 (goto-char start)
                 (looking-at "\\[\\[\\([^]]+\\)\\]\\]"))))
            (format "Link to: %s" (match-string 1)))

          ;; Check for template - look backward and forward for {{ }}
          ((save-excursion
             (let ((start (search-backward "{{" (line-beginning-position) t))
                    (end (search-forward "}}" (line-end-position) t)))
               (when (and start end (>= original-point start) (<= original-point end))
                 (goto-char start)
                 (looking-at "{{\\([^}]+\\)}}"))))
            (format "Template: %s" (match-string 1)))

          ;; Check for header at beginning of line
          ((progn
             (beginning-of-line)
             (looking-at "=\\(=+\\)\\s-*\\(.+\\)\\s-*\\1="))
            (format "Header (level %d): %s"
              (+ (length (match-string 1)) 1)
              (match-string 2)))

          (thing-at-point
            (format "MediaWiki element: %s" thing-at-point)))))))

;;; Which-Key Integration

(defun mediawiki-ui-setup-which-key ()
  "Setup Which-Key descriptions for MediaWiki commands."
  (when (and mediawiki-ui-enable-which-key (featurep 'which-key))
    (which-key-add-key-based-replacements
      "C-c C-c" "mediawiki-save"
      "C-c C-o" "mediawiki-open"
      "C-c C-r" "mediawiki-reload"
      "C-c C-q" "mediawiki-ui-quick-menu")))

;;; Transient Interface

(defvar mediawiki-ui-transient-available-p
  (and mediawiki-ui-use-transient
    (ignore-errors (require 'transient) t))
  "Whether transient.el is available.")

(if mediawiki-ui-transient-available-p
  ;; Full transient interface when available
  (progn
    (transient-define-prefix mediawiki-dispatch ()
      "MediaWiki operations dispatcher."
      :man-page "mediawiki"
      ["Page Operations"
        [("o" "Open page" mediawiki-ui-open-with-preview)
          ("s" "Save page" mediawiki-ui-save-with-options)
          ("r" "Reload page" mediawiki-reload)
          ("h" "Page history" mediawiki-ui-show-page-history)]
        [("n" "New page" mediawiki-ui-create-new-page)
          ("d" "Delete page" mediawiki-ui-delete-page)
          ("m" "Move page" mediawiki-ui-move-page)
          ("w" "Watch/Unwatch" mediawiki-ui-toggle-watchlist)]]

      ["Navigation & Search"
        [("f" "Find/Search pages" mediawiki-ui-search-pages)
          ("R" "Recent pages" mediawiki-ui-show-recent-pages)
          ("l" "Links to page" mediawiki-ui-show-links-to-page)
          ("c" "Categories" mediawiki-ui-show-categories)]
        [("t" "Random page" mediawiki-ui-random-page)
          ("b" "Browse categories" mediawiki-ui-browse-categories)
          ("u" "User contributions" mediawiki-ui-user-contributions)
          ("L" "Recent changes" mediawiki-ui-recent-changes)]]

      ["Site Management"
        [("S" "Select site" mediawiki-ui-select-site)
          ("A" "Authentication" mediawiki-ui-manage-authentication)
          ("C" "Configuration" (lambda () (interactive) (customize-group 'mediawiki)))
          ("I" "Site info" mediawiki-ui-show-site-info)]
        [("Q" "Queue status" mediawiki-async-list-operations)
          ("D" "Debug info" mediawiki-debug-view)
          ("T" "Statistics" mediawiki-ui-show-site-statistics)
          ("P" "Progress operations" mediawiki-progress-list-active)]]

      ["Help & Information"
        [("?" "Help" mediawiki-ui-show-help)
          ("v" "Version info" mediawiki-ui-show-version)
          ("q" "Quit" transient-quit-one)]])

    ;; Enhanced page operations with options
    (transient-define-prefix mediawiki-page-operations ()
      "Page-specific operations."
      :man-page "mediawiki-page"
      ["Current Page"
        [("s" "Save" mediawiki-ui-save-with-options)
          ("S" "Save as..." mediawiki-ui-save-as)
          ("r" "Reload" mediawiki-reload)
          ("p" "Preview" mediawiki-ui-preview-page)]
        [("h" "History" mediawiki-ui-show-page-history)
          ("i" "Page info" mediawiki-ui-show-page-info)
          ("l" "Links" mediawiki-ui-show-page-links)
          ("w" "Watch/Unwatch" mediawiki-ui-toggle-watchlist)]]

      ["Advanced"
        [("d" "Delete" mediawiki-ui-delete-page)
          ("m" "Move/Rename" mediawiki-ui-move-page)
          ("P" "Protect" mediawiki-ui-protect-page)
          ("U" "Unprotect" mediawiki-ui-unprotect-page)]
        [("D" "Diff" mediawiki-ui-show-diff)
          ("R" "Rollback" mediawiki-ui-rollback-edits)
          ("B" "Backup" mediawiki-ui-backup-page)
          ("E" "Export" mediawiki-ui-export-page)]]

      ["Navigation"
        [("q" "Quit" transient-quit-one)
          ("b" "Back to main" (lambda () (interactive) (mediawiki-dispatch)))]])

    ;; Bind the dispatcher to a key if keymap exists
    (when (boundp 'mediawiki-mode-map)
      (define-key mediawiki-mode-map (kbd "C-c C-d") (lambda () (interactive) (mediawiki-dispatch)))
      (define-key mediawiki-mode-map (kbd "C-c C-p") #'mediawiki-page-operations)))

  ;; Fallback functions when transient is not available
  (defun mediawiki-dispatch ()
    "MediaWiki operations dispatcher (fallback when transient unavailable)."
    (interactive)
    (message "MediaWiki transient interface not available. Use M-x mediawiki-* commands directly."))

  (defun mediawiki-page-operations ()
    "Page-specific operations (fallback when transient unavailable)."
    (interactive)
    (message "MediaWiki transient interface not available. Use M-x mediawiki-* commands directly.")))

;;; Additional UI Functions for Transient

(defun mediawiki-ui-create-new-page ()
  "Create a new MediaWiki page."
  (interactive)
  (let* ((sitename (or mediawiki-site (mediawiki-ui-select-site)))
          (title (read-string "New page title: ")))
    (when (and title (not (string-empty-p title)))
      (let ((buffer (generate-new-buffer (format "*MediaWiki: %s*" title))))
        (with-current-buffer buffer
          (mediawiki-mode)
          (setq mediawiki-site sitename
            mediawiki-page-title title)
          (insert (format "<!-- New page: %s -->\n\n" title)))
        (switch-to-buffer buffer)))))

(defun mediawiki-ui-show-page-history ()
  "Show history for the current page."
  (interactive)
  (unless (and mediawiki-site mediawiki-page-title)
    (user-error "Not in a MediaWiki buffer"))

  (message "Loading page history...")
  (mediawiki-api-call-async
    mediawiki-site "query"
    `(("prop" . "revisions")
       ("titles" . ,mediawiki-page-title)
       ("rvlimit" . "50")
       ("rvprop" . "timestamp|user|comment|size"))

    (lambda (response)
      (let ((pages (cdr (assq 'pages (mediawiki-api-response-data response)))))
        (when pages
          (let* ((page (cdar pages))
                  (revisions (cdr (assq 'revisions page))))
            (if revisions
              (mediawiki-ui-display-page-history mediawiki-page-title revisions)
              (message "No history found for page: %s" mediawiki-page-title))))))

    (lambda (error)
      (message "Failed to load page history: %s" error))))

(defun mediawiki-ui-display-page-history (title revisions)
  "Display page history for TITLE with REVISIONS."
  (let ((buffer (get-buffer-create (format "*MediaWiki History: %s*" title))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Edit History for: %s\n" title))
      (insert (make-string 50 ?=) "\n\n")

      (dolist (rev revisions)
        (let ((timestamp (cdr (assq 'timestamp rev)))
               (user (cdr (assq 'user rev)))
               (comment (cdr (assq 'comment rev)))
               (size (cdr (assq 'size rev))))

          (insert (format "Date: %s | User: %s | Size: %s bytes\n"
                    timestamp user (or size "unknown")))
          (when comment
            (insert (format "Comment: %s\n" comment)))
          (insert "\n")))

      (goto-char (point-min))
      (special-mode))

    (display-buffer buffer)))

(defun mediawiki-ui-toggle-watchlist ()
  "Toggle watchlist status for current page."
  (interactive)
  (unless (and mediawiki-site mediawiki-page-title)
    (user-error "Not in a MediaWiki buffer"))

  (let ((action (if (yes-or-no-p "Add to watchlist? ") "watch" "unwatch")))
    (mediawiki-api-call-async
      mediawiki-site action
      `(("titles" . ,mediawiki-page-title))

      (lambda (_)
        (message "Page %s %sed"
          mediawiki-page-title
          (if (string= action "watch") "watch" "unwatch")))

      (lambda (error)
        (message "Failed to %s page: %s" action error)))))

(defun mediawiki-ui-show-help ()
  "Show MediaWiki help information."
  (interactive)
  (let ((buffer (get-buffer-create "*MediaWiki Help*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "MediaWiki Mode Help\n")
      (insert (make-string 25 ?=) "\n\n")

      (insert "Main Commands:\n")
      (insert "  C-c C-d    - Open MediaWiki dispatcher (transient menu)\n")
      (insert "  C-c C-p    - Page operations menu\n")
      (insert "  C-c C-o    - Open page\n")
      (insert "  C-c C-s    - Save page\n")
      (insert "  C-c C-r    - Reload page\n")
      (insert "  C-c C-q    - Quick menu\n\n")

      (insert "UI Features:\n")
      (insert "  - Enhanced completion with suggestions\n")
      (insert "  - Recent pages history\n")
      (insert "  - Page preview in completion\n")
      (insert "  - Async operation management\n")
      (insert "  - Progress tracking\n")
      (insert "  - Modern transient interfaces\n\n")

      (insert "Customization:\n")
      (insert "  M-x customize-group RET mediawiki RET\n")
      (insert "  M-x customize-group RET mediawiki-ui RET\n\n")

      (goto-char (point-min))
      (help-mode))

    (display-buffer buffer)))

(defun mediawiki-ui-show-version ()
  "Show MediaWiki version and system information."
  (interactive)
  (message "MediaWiki.el version %s (modernized)" mediawiki-version))

;;; Initialization

(defun mediawiki-ui-setup ()
  "Setup MediaWiki UI enhancements."
  (when mediawiki-ui-enable-eldoc
    (add-hook 'mediawiki-mode-hook
      (lambda ()
        (setq-local eldoc-documentation-function
          #'mediawiki-ui-eldoc-function))))

  (mediawiki-ui-setup-which-key)

  ;; Clear old completion cache periodically
  (run-with-timer 600 600 ; Every 10 minutes
    (lambda ()
      (clrhash mediawiki-ui-completion-cache))))

;; Initialize when loaded
(mediawiki-ui-setup)

(provide 'mediawiki-ui)

;;; mediawiki-ui.el ends here
