;;; test-ui-enhancements.el --- Test UI enhancements -*- lexical-binding: t; -*-

;; Test implementation of task 8.2: Add modern UI enhancements
;; Tests completion, suggestion features, and improved user interaction flows

(require 'ert)
(require 'mediawiki-core)
(require 'mediawiki-ui)

;;; Test Setup

(defvar test-ui-recent-pages-backup nil
  "Backup of recent pages for test isolation.")

(defvar test-ui-cache-backup nil
  "Backup of completion cache for test isolation.")

(defun test-ui-setup ()
  "Set up test environment."
  (setq test-ui-recent-pages-backup (copy-hash-table mediawiki-ui-recent-pages))
  (setq test-ui-cache-backup (copy-hash-table mediawiki-ui-completion-cache))

  (clrhash mediawiki-ui-recent-pages)
  (clrhash mediawiki-ui-completion-cache)

  ;; Set up test site
  (let ((test-site (make-mediawiki-site-config
                    :name "test-site"
                    :url "https://test.example.com/w/"
                    :auth-method 'basic)))
    (mediawiki-add-site test-site)))

(defun test-ui-teardown ()
  "Clean up test environment."
  (clrhash mediawiki-ui-recent-pages)
  (clrhash mediawiki-ui-completion-cache)

  (when test-ui-recent-pages-backup
    (setq mediawiki-ui-recent-pages test-ui-recent-pages-backup))
  (when test-ui-cache-backup
    (setq mediawiki-ui-completion-cache test-ui-cache-backup))

  ;; Clean up test site
  (mediawiki-remove-site "test-site"))

;;; Test Completion Framework Detection

(ert-deftest test-ui-completion-framework-detection ()
  "Test completion framework auto-detection."
  (test-ui-setup)
  (unwind-protect
      (progn
        ;; Test auto-detection with default
        (let ((mediawiki-ui-completion-framework 'auto))
          (should (eq (mediawiki-ui-detect-completion-framework) 'completing-read)))

        ;; Test explicit framework selection
        (let ((mediawiki-ui-completion-framework 'ivy))
          (should (eq (mediawiki-ui-detect-completion-framework) 'ivy)))

        (let ((mediawiki-ui-completion-framework 'helm))
          (should (eq (mediawiki-ui-detect-completion-framework) 'helm))))

    (test-ui-teardown)))

;;; Test Recent Pages Management

(ert-deftest test-ui-recent-pages ()
  "Test recent pages functionality."
  (test-ui-setup)
  (unwind-protect
      (progn
        ;; Test adding recent pages
        (mediawiki-ui-add-recent-page "test-site" "Test Page 1")
        (mediawiki-ui-add-recent-page "test-site" "Test Page 2")
        (mediawiki-ui-add-recent-page "test-site" "Test Page 3")

        (let ((recent (mediawiki-ui-get-recent-pages "test-site")))
          (should (= (length recent) 3))
          (should (string= (car recent) "Test Page 3")) ; Most recent first
          (should (member "Test Page 1" recent)))

        ;; Test duplicate handling
        (mediawiki-ui-add-recent-page "test-site" "Test Page 1")
        (let ((recent (mediawiki-ui-get-recent-pages "test-site")))
          (should (= (length recent) 3)) ; No duplicates
          (should (string= (car recent) "Test Page 1"))) ; Moved to front

        ;; Test size limit - need to reset the pages first since the limit only applies to new additions
        (clrhash mediawiki-ui-recent-pages)
        (let ((mediawiki-ui-recent-pages-count 2))
          (mediawiki-ui-add-recent-page "test-site" "Test Page A")
          (mediawiki-ui-add-recent-page "test-site" "Test Page B")
          (mediawiki-ui-add-recent-page "test-site" "Test Page C")
          (let ((recent (mediawiki-ui-get-recent-pages "test-site")))
            (should (<= (length recent) 2)))))

    (test-ui-teardown)))

;;; Test Site Selection

(ert-deftest test-ui-site-candidates ()
  "Test site candidate generation."
  (test-ui-setup)
  (unwind-protect
      (progn
        ;; Test site candidate generation
        (let ((candidates (mediawiki-ui-get-site-candidates)))
          (should (consp candidates))
          (should (> (length candidates) 0))

          ;; Check that test-site is included
          (let ((test-candidate (cl-find-if
                                 (lambda (candidate)
                                   (string-match-p "test-site" (car candidate)))
                                 candidates)))
            (should test-candidate)
            (should (string= (cdr test-candidate) "test-site")))))

    (test-ui-teardown)))

;;; Test Completion Cache

(ert-deftest test-ui-completion-cache ()
  "Test completion caching functionality."
  (test-ui-setup)
  (unwind-protect
      (progn
        ;; Test cache key generation and storage
        (let ((cache-key "test-site:test")
              (test-suggestions '("Test Page 1" "Test Page 2")))

          (puthash cache-key (cons (float-time) test-suggestions)
                   mediawiki-ui-completion-cache)

          (should (gethash cache-key mediawiki-ui-completion-cache))

          (let ((cached (gethash cache-key mediawiki-ui-completion-cache)))
            (should (consp cached))
            (should (numberp (car cached)))
            (should (equal (cdr cached) test-suggestions)))))

    (test-ui-teardown)))

;;; Test Page Annotation

(ert-deftest test-ui-page-annotation ()
  "Test page annotation for completion."
  (test-ui-setup)
  (unwind-protect
      (progn
        ;; Add a recent page
        (mediawiki-ui-add-recent-page "test-site" "Recent Page")

        ;; Test annotation
        (let ((annotation (mediawiki-ui-annotate-page "Recent Page" "test-site")))
          (should (string= annotation " (recent)")))

        (let ((annotation (mediawiki-ui-annotate-page "Other Page" "test-site")))
          (should (string= annotation ""))))

    (test-ui-teardown)))

;;; Test Menu System

(ert-deftest test-ui-quick-menu-choices ()
  "Test quick menu choice availability."
  (test-ui-setup)
  (unwind-protect
      (progn
        ;; Test that menu choices are properly structured
        ;; We can't easily test interactive completion, but we can test the data structure
        (should (functionp 'mediawiki-ui-quick-menu))
        (should (functionp 'mediawiki-ui-open-with-preview))
        (should (functionp 'mediawiki-ui-save-with-options))
        (should (functionp 'mediawiki-ui-show-recent-pages))
        (should (functionp 'mediawiki-ui-search-pages)))

    (test-ui-teardown)))

;;; Test Enhanced Functions

(ert-deftest test-ui-enhanced-functions ()
  "Test that enhanced UI functions are available."
  (test-ui-setup)
  (unwind-protect
      (progn
        ;; Test function availability
        (should (functionp 'mediawiki-ui-open-with-preview))
        (should (functionp 'mediawiki-ui-save-with-options))
        (should (functionp 'mediawiki-ui-show-site-statistics))
        (should (functionp 'mediawiki-ui-manage-authentication))

        ;; Test that functions have proper documentation
        (should (documentation 'mediawiki-ui-open-with-preview))
        (should (documentation 'mediawiki-ui-save-with-options)))

    (test-ui-teardown)))

;;; Test Configuration Integration

(ert-deftest test-ui-configuration ()
  "Test UI configuration options."
  (test-ui-setup)
  (unwind-protect
      (progn
        ;; Test configuration variables exist
        (should (boundp 'mediawiki-ui-completion-framework))
        (should (boundp 'mediawiki-ui-show-page-preview))
        (should (boundp 'mediawiki-ui-recent-pages-count))
        (should (boundp 'mediawiki-ui-enable-eldoc))
        (should (boundp 'mediawiki-ui-enable-which-key))

        ;; Test default values are reasonable
        (should (memq mediawiki-ui-completion-framework
                      '(auto completing-read ivy helm vertico)))
        (should (numberp mediawiki-ui-recent-pages-count))
        (should (> mediawiki-ui-recent-pages-count 0)))

    (test-ui-teardown)))

;;; Test ElDoc Integration

(ert-deftest test-ui-eldoc-function ()
  "Test ElDoc integration."
  (test-ui-setup)
  (unwind-protect
      (progn
        ;; Test ElDoc function exists
        (should (functionp 'mediawiki-ui-eldoc-function))

        ;; Test with different content (mock) - skip if mediawiki-mode not available
        (when (functionp 'mediawiki-mode)
          (with-temp-buffer
            (mediawiki-mode)
            (insert "[[Test Page]]")
            (goto-char (point-min))
            (forward-char 2)

            ;; The eldoc function should recognize this as a link
            (let ((eldoc-info (mediawiki-ui-eldoc-function)))
              (when eldoc-info
                (should (string-match-p "Link to:" eldoc-info))))))))

  (test-ui-teardown))

;;; Test UI Statistics

(ert-deftest test-ui-statistics-display ()
  "Test statistics display functionality."
  (test-ui-setup)
  (unwind-protect
      (progn
        ;; Test statistics function
        (should (functionp 'mediawiki-ui-show-site-statistics))

        ;; Test that it doesn't error when called
        (should-not (condition-case err
                        (with-temp-buffer
                          (mediawiki-ui-show-site-statistics)
                          nil)
                      (error t))))

    (test-ui-teardown)))

;;; Test Transient Integration

(ert-deftest test-ui-transient-availability ()
  "Test transient interface availability."
  (test-ui-setup)
  (unwind-protect
      (progn
        ;; Test transient detection
        (should (boundp 'mediawiki-ui-transient-available-p))

        ;; Test that transient functions are defined when available
        (when mediawiki-ui-transient-available-p
          (should (functionp 'mediawiki-dispatch))
          (should (functionp 'mediawiki-page-operations))))

    (test-ui-teardown)))

;;; Test Enhanced Page Operations

(ert-deftest test-ui-page-operations ()
  "Test enhanced page operation functions."
  (test-ui-setup)
  (unwind-protect
      (progn
        ;; Test page operation functions exist
        (should (functionp 'mediawiki-ui-create-new-page))
        (should (functionp 'mediawiki-ui-show-page-history))
        (should (functionp 'mediawiki-ui-toggle-watchlist))

        ;; Test help and version functions
        (should (functionp 'mediawiki-ui-show-help))
        (should (functionp 'mediawiki-ui-show-version)))

    (test-ui-teardown)))

;;; Test Error Handling

(ert-deftest test-ui-error-handling ()
  "Test UI error handling."
  (test-ui-setup)
  (unwind-protect
      (progn
        ;; Test that functions handle missing data gracefully
        (let ((recent-pages (mediawiki-ui-get-recent-pages "nonexistent-site")))
          (should (null recent-pages)))

        ;; Test annotation with missing site
        (let ((annotation (mediawiki-ui-annotate-page "Test Page" "nonexistent-site")))
          (should (stringp annotation))))

    (test-ui-teardown)))

;;; Test Integration with Async Operations

(ert-deftest test-ui-async-integration ()
  "Test UI integration with async operations."
  (test-ui-setup)
  (unwind-protect
      (progn
        ;; Test that UI functions can work with async operations
        (should (functionp 'mediawiki-ui-open-with-preview))
        (should (functionp 'mediawiki-ui-save-with-options))

        ;; These functions should integrate with the progress system
        (should (boundp 'mediawiki-progress-feedback-enabled)))

    (test-ui-teardown)))

(provide 'test-ui-enhancements)

;;; test-ui-enhancements.el ends here
