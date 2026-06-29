;;; test-mediawiki-discussion-tools.el --- Unit tests for mediawiki-discussion-tools.el  -*- lexical-binding: t; -*-

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

;; Unit tests for mediawiki-discussion-tools.el (Phase 1 — thread listing).
;; These tests mock network interactions to avoid external dependencies.

;;; Code:

(require 'ert)
(require 'mediawiki-discussion-tools)

;;; Helpers

(defun test-mdt--parse (json-str)
  "Parse a JSON string into the API response structure."
  (json-parse-string json-str
    :object-type 'alist
    :array-type 'list
    :null-object nil
    :false-object nil))

(defmacro test-mdt-with-site (site-name &rest body)
  "Bind a test site configuration and evaluate BODY."
  `(let ((mediawiki-site-alist
          (list (list ,site-name
                      "https://test.example.org/w/"
                      "" "" ""
                      :description "Test Site"
                      :oauth-client-id "client-id"
                      :oauth-client-secret "client-secret"
                      :oauth-access-token "access-token"))))
     ,@body))

;;; Fixtures

(defvar test-mdt--header-json
  "{\"headingLevel\":null,\"name\":\"h-\",\"type\":\"heading\",\"level\":0,\"id\":\"h-\",\"replies\":[],\"html\":\"\",\"commentCount\":0,\"authorCount\":0,\"latestReplyTimestamp\":null,\"latestReply\":null,\"oldestReply\":null}")

(defun test-mdt--thread-json (id title comment-count author author-count ts)
  "Build a mock thread JSON object string."
  (let ((latest (format "{\"timestamp\":\"%s\",\"author\":\"%s\",\"type\":\"comment\",\"level\":1,\"id\":\"c-%s-latest\"}" ts author author))
        (oldest (format "{\"timestamp\":\"%s\",\"author\":\"%s\",\"type\":\"comment\",\"level\":1,\"id\":\"c-%s-oldest\"}" ts author author)))
    (concat "{"
            (format "\"headingLevel\":2,\"name\":\"h-%s\",\"type\":\"heading\",\"level\":0,\"id\":\"%s\",\"html\":\"%s\",\"commentCount\":%d,\"authorCount\":%d,\"latestReplyTimestamp\":\"%s\",\"latestReply\":%s,\"oldestReply\":%s"
                    author id title comment-count author-count ts latest oldest)
            "}")))

(defun test-mdt--thread-fresh ()
  (test-mdt--thread-json "h-Alice-fresh" "Fresh topic <b>with HTML</b>" 3 "Alice" 2 "2026-06-28T10:00:00Z"))

(defun test-mdt--thread-stale ()
  (test-mdt--thread-json "h-Bob-stale" "Stale topic" 2 "Bob" 2 "2026-01-01T00:00:00Z"))

(defun test-mdt--thread-unanswered ()
  (test-mdt--thread-json "h-Carol-unanswered" "Unanswered topic" 1 "Carol" 1 "2026-06-28T09:00:00Z"))

(defun test-mdt--make-response (thread-jsons)
  "Build a full API response JSON from THREAD-JSONS."
  (let* ((items (concat "[" test-mdt--header-json
                        (if thread-jsons
                            (concat "," (mapconcat #'identity thread-jsons ","))
                          "")
                        "]"))
         (full (concat "{\"discussiontoolspageinfo\":{\"threaditemshtml\":" items "}}")))
    (test-mdt--parse full)))

;;; Thread Parsing Tests

(ert-deftest test-mdt-parse-empty ()
  (let* ((json (test-mdt--make-response nil))
         (threads (mediawiki-discussion-tools--parse-threads json)))
    (should (null threads))))

(ert-deftest test-mdt-parse-single ()
  (let* ((json (test-mdt--make-response (list (test-mdt--thread-fresh))))
         (threads (mediawiki-discussion-tools--parse-threads json)))
    (should (= (length threads) 1))
    (should (string= (alist-get 'id (car threads)) "h-Alice-fresh"))
    (should (string= (alist-get 'title (car threads)) "Fresh topic with HTML"))
    (should (string= (alist-get 'author (car threads)) "Alice"))
    (should (= (alist-get 'reply-count (car threads)) 3))
    (should (= (alist-get 'author-count (car threads)) 2))))

(ert-deftest test-mdt-parse-multiple ()
  (let* ((json (test-mdt--make-response (list (test-mdt--thread-fresh)
                                             (test-mdt--thread-stale))))
         (threads (mediawiki-discussion-tools--parse-threads json)))
    (should (= (length threads) 2))))

(ert-deftest test-mdt-parse-missing-fields ()
  (let* ((json (test-mdt--parse "{\"discussiontoolspageinfo\":{\"threaditemshtml\":[{\"headingLevel\":null,\"name\":\"h-\"},{\"headingLevel\":2,\"id\":\"h-minimal\",\"html\":\"Bare thread\"}]}}"))
         (threads (mediawiki-discussion-tools--parse-threads json)))
    (should (= (length threads) 1))
    (let ((thread (car threads)))
      (should (= (alist-get 'reply-count thread) 0))
      (should (= (alist-get 'author-count thread) 0))
      (should (not (alist-get 'timestamp thread))))))

(ert-deftest test-mdt-parse-no-data ()
  (let ((json '((discussiontoolspageinfo . nil))))
    (should-error (mediawiki-discussion-tools--parse-threads json)
                  :type 'error)))

;;; Status Derivation Tests

(ert-deftest test-mdt-status-active ()
  (let* ((json (test-mdt--make-response (list (test-mdt--thread-fresh))))
         (threads (mediawiki-discussion-tools--parse-threads json))
         (thread (car threads)))
    (should (eq (alist-get 'status thread) 'active))))

(ert-deftest test-mdt-status-stale ()
  (let* ((json (test-mdt--make-response (list (test-mdt--thread-stale))))
         (threads (mediawiki-discussion-tools--parse-threads json))
         (thread (car threads)))
    (should (eq (alist-get 'status thread) 'stale))))

(ert-deftest test-mdt-status-unanswered ()
  (let* ((json (test-mdt--make-response (list (test-mdt--thread-unanswered))))
         (threads (mediawiki-discussion-tools--parse-threads json))
         (thread (car threads)))
    (should (eq (alist-get 'status thread) 'unanswered))))

(ert-deftest test-mdt-status-no-latest-reply ()
  (let* ((json (test-mdt--parse "{\"discussiontoolspageinfo\":{\"threaditemshtml\":[{\"headingLevel\":null,\"name\":\"h-\"},{\"headingLevel\":2,\"id\":\"h-test\",\"html\":\"Test\",\"commentCount\":2,\"latestReplyTimestamp\":null,\"latestReply\":null}]}}"))
         (threads (mediawiki-discussion-tools--parse-threads json))
         (thread (car threads)))
    (should (eq (alist-get 'status thread) 'active))))

;;; Priority Sorting Tests

(ert-deftest test-mdt-priority-ordering ()
  (let* ((json (test-mdt--make-response (list (test-mdt--thread-stale)
                                             (test-mdt--thread-fresh)
                                             (test-mdt--thread-unanswered))))
         (threads (mediawiki-discussion-tools--parse-threads json))
         (sorted (sort (copy-sequence threads)
                       (lambda (a b)
                         (< (mediawiki-discussion-tools--thread-priority a)
                            (mediawiki-discussion-tools--thread-priority b))))))
    (should (eq (alist-get 'status (nth 0 sorted)) 'unanswered))
    (should (eq (alist-get 'status (nth 1 sorted)) 'active))
    (should (eq (alist-get 'status (nth 2 sorted)) 'stale))))

(ert-deftest test-mdt-priority-values ()
  (let ((unanswered '((status . unanswered)))
        (active     '((status . active)))
        (stale      '((status . stale))))
    (should (< (mediawiki-discussion-tools--thread-priority unanswered)
               (mediawiki-discussion-tools--thread-priority active)))
    (should (< (mediawiki-discussion-tools--thread-priority active)
               (mediawiki-discussion-tools--thread-priority stale)))))

;;; ISO 8601 Parsing Tests

(ert-deftest test-mdt-parse-iso8601-valid ()
  (let ((result (mediawiki-discussion-tools--parse-iso8601 "2026-06-28T10:00:00Z")))
    (should result)
    (should (integerp (car result)))))

(ert-deftest test-mdt-parse-iso8601-invalid ()
  (should-not (mediawiki-discussion-tools--parse-iso8601 "not-a-date")))

(ert-deftest test-mdt-parse-iso8601-empty ()
  (should-not (mediawiki-discussion-tools--parse-iso8601 "")))

;;; Relative Time Formatting Tests

(ert-deftest test-mdt-format-relative-just-now ()
  (let ((ts (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))
    (should (equal "just now"
                   (mediawiki-discussion-tools--format-relative-time ts)))))

(ert-deftest test-mdt-format-relative-days ()
  (let ((ts (format-time-string "%Y-%m-%dT%H:%M:%SZ"
             (time-subtract nil (* 3 86400)) t)))
    (should (string= "3d ago"
                     (mediawiki-discussion-tools--format-relative-time ts)))))

(ert-deftest test-mdt-format-relative-hours ()
  (let ((ts (format-time-string "%Y-%m-%dT%H:%M:%SZ"
             (time-subtract nil (* 5 3600)) t)))
    (should (string= "5h ago"
                     (mediawiki-discussion-tools--format-relative-time ts)))))

(ert-deftest test-mdt-format-relative-unknown ()
  (should (string= "unknown"
                   (mediawiki-discussion-tools--format-relative-time nil)))
  (should (string= "unknown"
                   (mediawiki-discussion-tools--format-relative-time ""))))

;;; HTML Stripping Tests

(ert-deftest test-mdt-strip-tags-basic ()
  (should (string= "plain text"
                   (mediawiki-discussion-tools--strip-tags "<b>plain</b> <i>text</i>"))))

(ert-deftest test-mdt-strip-tags-mw-span ()
  (should (string= "header text"
                   (mediawiki-discussion-tools--strip-tags "<span id=\"mwAA\">header</span> text"))))

(ert-deftest test-mdt-strip-tags-no-tags ()
  (should (string= "no tags here"
                   (mediawiki-discussion-tools--strip-tags "no tags here"))))

;;; Table Entry Tests

(ert-deftest test-mdt-make-entry ()
  (let* ((json (test-mdt--make-response (list (test-mdt--thread-fresh))))
         (threads (mediawiki-discussion-tools--parse-threads json))
         (thread (car threads))
         (entry (mediawiki-discussion-tools--make-entry thread)))
    (should entry)
    (should (string= (alist-get 'id thread) (car entry)))
    (let ((vec (cadr entry)))
      (should (vectorp vec))
      (should (= (length vec) 5))
      (should (equal (get-text-property 0 'thread-id (aref vec 1))
                     "h-Alice-fresh")))))

(ert-deftest test-mdt-make-entry-no-author ()
  (let* ((thread '((id . "h-test")
                   (title . "Test")
                   (reply-count . 0)
                   (author-count . 0)
                   (status . unanswered)))
         (entry (mediawiki-discussion-tools--make-entry thread)))
    (should entry)))

;;; Status Icon Tests

(ert-deftest test-mdt-status-icon-unanswered ()
  (should (string= " ?" (mediawiki-discussion-tools--status-icon 'unanswered))))

(ert-deftest test-mdt-status-icon-active ()
  (should (string-match "."
                         (mediawiki-discussion-tools--status-icon 'active))))

(ert-deftest test-mdt-status-icon-stale ()
  (should (string-match "~"
                         (mediawiki-discussion-tools--status-icon 'stale))))

(ert-deftest test-mdt-status-icon-resolved ()
  (should (string-match "✓\\|\u2713"
                         (mediawiki-discussion-tools--status-icon 'resolved))))

;;; Full Fetch Pipeline (mocked)

(ert-deftest test-mdt-fetch-threads-mock ()
  (test-mdt-with-site "TestSite"
    (let ((mock-json (test-mdt--make-response (list (test-mdt--thread-fresh)
                                                   (test-mdt--thread-unanswered)))))
      (cl-letf (((symbol-function 'mediawiki-api-call)
                 (lambda (sitename action args) mock-json)))
        (let ((threads (mediawiki-discussion-tools--fetch-threads "TestSite" "TestPage")))
          (should (= (length threads) 2))
          (should (eq (alist-get 'status (car threads)) 'unanswered))
          (should (eq (alist-get 'status (cadr threads)) 'active)))))))

(ert-deftest test-mdt-fetch-threads-full-flag ()
  (test-mdt-with-site "TestSite"
    (let ((called-flags nil)
          (mock-json (test-mdt--make-response (list (test-mdt--thread-fresh)))))
      (cl-letf (((symbol-function 'mediawiki-api-call)
                 (lambda (sitename action args)
                   (setq called-flags (cdr (assoc "threaditemsflags" args)))
                   mock-json)))
         (mediawiki-discussion-tools--fetch-threads "TestSite" "TestPage")
         (should (string= called-flags "activity|noreplies"))
         (mediawiki-discussion-tools--fetch-threads "TestSite" "TestPage" t)
        (should (string= called-flags "activity|excludesignatures"))))))

(ert-deftest test-mdt-fetch-threads-max-limit ()
  (test-mdt-with-site "TestSite"
    (let ((mediawiki-discussion-tools-max-threads 1)
          (mock-json (test-mdt--make-response (list (test-mdt--thread-fresh)
                                                   (test-mdt--thread-stale)))))
      (cl-letf (((symbol-function 'mediawiki-api-call)
                 (lambda (sitename action args) mock-json)))
        (let ((threads (mediawiki-discussion-tools--fetch-threads "TestSite" "TestPage")))
          (should (= (length threads) 1)))))))

(ert-deftest test-mdt-fetch-threads-empty ()
  (test-mdt-with-site "TestSite"
    (let ((mock-json (test-mdt--make-response nil)))
      (cl-letf (((symbol-function 'mediawiki-api-call)
                 (lambda (sitename action args) mock-json)))
        (should-not (mediawiki-discussion-tools--fetch-threads "TestSite" "TestPage"))))))

;;; Config Defaults Tests

(ert-deftest test-mdt-config-defaults ()
  (should (= mediawiki-discussion-tools-stale-days 14))
  (should-not mediawiki-discussion-tools-max-threads)
  (should (string= mediawiki-discussion-tools-signature "~~~~")))

;;; Phase 2 — Thread Viewing Tests

(defun test-mdt--mock-reply (author timestamp html &optional children)
  "Build a mock reply alist."
  (nconc (list (cons 'author author)
               (cons 'timestamp timestamp)
               (cons 'html html))
         (when children
           (list (cons 'replies children)))))

(defun test-mdt--mock-thread (id title &optional replies)
  "Build a mock thread alist with optional REPLIES (a list)."
  (list (cons 'id id)
        (cons 'title title)
        (cons 'reply-count (if replies (1+ (length replies)) 1))
        (cons 'author-count 2)
        (cons 'timestamp "2026-06-28T10:00:00Z")
        (cons 'replies (or replies (list)))))

(ert-deftest test-mdt-format-timestamp-valid ()
  (should (string= "2026-06-28 10:00"
                   (mediawiki-discussion-tools--format-timestamp
                    "2026-06-28T10:00:00Z"))))

(ert-deftest test-mdt-format-timestamp-invalid ()
  (should (string= "bad-date"
                   (mediawiki-discussion-tools--format-timestamp "bad-date"))))

(ert-deftest test-mdt-render-replies-single-level ()
  "Test rendering a single-level reply tree."
  (let ((replies (list
                  (test-mdt--mock-reply
                   "Alice" "2026-06-28T10:00:00Z"
                   "<p>First reply</p>"))))
    (with-temp-buffer
      (mediawiki-discussion-tools--render-replies replies 0)
      (let ((content (buffer-string)))
        (should (string-match "Alice" content))
        (should (string-match "First reply" content))
        (should (string-match "2026-06-28" content))))))

(ert-deftest test-mdt-render-replies-nested ()
  "Test rendering nested replies with indentation."
  (let ((replies (list
                  (test-mdt--mock-reply
                   "Alice" "2026-06-28T10:00:00Z"
                   "<p>OP text</p>"
                   (list
                    (test-mdt--mock-reply
                     "Bob" "2026-06-28T11:00:00Z"
                     "<p>Reply</p>"))))))
    (with-temp-buffer
      (mediawiki-discussion-tools--render-replies replies 0)
      (let ((content (buffer-string)))
        (should (string-match "Alice" content))
        (should (string-match "Bob" content))
        ;; Bob's reply should be indented (2 spaces for depth 1)
        (should (string-match "  Bob" content))))))

(ert-deftest test-mdt-render-replies-empty ()
  "Test rendering an empty reply list."
  (with-temp-buffer
    (mediawiki-discussion-tools--render-replies (list) 0)
    (should (string-empty-p (buffer-string)))))

(ert-deftest test-mdt-render-thread ()
  "Test full thread rendering with title separator."
  (let ((thread (test-mdt--mock-thread
                 "h-test" "Test Thread"
                 (list (test-mdt--mock-reply
                        "Carol" "2026-06-28T09:00:00Z"
                        "<p>Question body</p>")))))
    (with-temp-buffer
      (mediawiki-discussion-tools--render-thread thread)
      (let ((content (buffer-string)))
        (should (string-match "Test Thread" content))
        (should (string-match "Carol" content))
        (should (string-match "Question body" content))))))

(ert-deftest test-mdt-render-thread-no-replies ()
  "Test rendering a thread with no replies."
  (let ((thread (test-mdt--mock-thread "h-test" "Empty Thread")))
    (with-temp-buffer
      (mediawiki-discussion-tools--render-thread thread)
      (let ((content (buffer-string)))
        (should (string-match "Empty Thread" content))))))

(ert-deftest test-mdt-strip-tags-multiline ()
  "Test stripping HTML from multi-line reply text."
  (with-temp-buffer
    (let ((replies (list
                    (test-mdt--mock-reply
                     "Dave" "2026-06-28T12:00:00Z"
                     "<p>line one</p>\n<p>line two</p>"))))
      (mediawiki-discussion-tools--render-replies replies 0)
      (let ((content (buffer-string)))
        (should (string-match "line one" content))
        (should (string-match "line two" content))))))

(ert-deftest test-mdt-thread-at-point ()
  "Test that --thread-at-point returns nil with no entries."
  (with-temp-buffer
    (mediawiki-discussion-tools-list-mode)
    (should-not (mediawiki-discussion-tools--thread-at-point))))

(ert-deftest test-mdt-refresh-table-preserves-sort-order ()
  "Tabulated-list entries and --threads share the same order."
  (let ((threads (list (test-mdt--mock-thread "h-unanswered" "Unanswered")
                       (test-mdt--mock-thread "h-active" "Active")
                       (test-mdt--mock-thread "h-stale" "Stale"))))
    (setf (alist-get 'status (nth 0 threads)) 'unanswered)
    (setf (alist-get 'status (nth 1 threads)) 'active)
    (setf (alist-get 'status (nth 2 threads)) 'stale)
    (sort threads (lambda (a b)
                    (< (mediawiki-discussion-tools--thread-priority a)
                       (mediawiki-discussion-tools--thread-priority b))))
    (with-temp-buffer
      (mediawiki-discussion-tools-list-mode)
      (setq mediawiki-discussion-tools--threads threads)
      (mediawiki-discussion-tools--refresh-table)
      (let ((entry-ids (mapcar #'car tabulated-list-entries))
            (thread-ids (mapcar (lambda (t) (alist-get 'id t)) threads)))
        (should (equal entry-ids thread-ids))))))

(ert-deftest test-mdt-refresh-table-no-column-sort ()
  "A nil tabulated-list-sort-key does not reorder entries."
  (let ((threads (list (test-mdt--mock-thread "h-A" "A")
                       (test-mdt--mock-thread "h-B" "B"))))
    (setf (alist-get 'status (nth 0 threads)) 'active)
    (setf (alist-get 'status (nth 1 threads)) 'unanswered)
    (sort threads (lambda (a b)
                    (< (mediawiki-discussion-tools--thread-priority a)
                       (mediawiki-discussion-tools--thread-priority b))))
    (should (string= "h-B" (alist-get 'id (car threads))))
    (with-temp-buffer
      (mediawiki-discussion-tools-list-mode)
      (should (null tabulated-list-sort-key))
      (setq mediawiki-discussion-tools--threads threads)
      (mediawiki-discussion-tools--refresh-table)
      (should (string= "h-B" (car (car tabulated-list-entries)))))))

(ert-deftest test-mdt-follow-point-updates-view ()
  "Moving point in the list buffer updates --last-viewed-id and the view."
  (let ((t1 (test-mdt--mock-thread "h-A" "Thread A"))
        (t2 (test-mdt--mock-thread "h-B" "Thread B")))
    (setf (alist-get 'status t1) 'active)
    (setf (alist-get 'status t2) 'unanswered)
    (let ((threads (list t1 t2)))
      (with-temp-buffer
        (mediawiki-discussion-tools-list-mode)
        (setq mediawiki-discussion-tools--threads threads)
        (mediawiki-discussion-tools--refresh-table)
        ;; Point at row 0
        (goto-char (point-min))
        (let ((pair (mediawiki-discussion-tools--thread-at-point)))
          (should pair)
          (should (string= "h-A" (alist-get 'id (cdr pair))))
          (should (= (car pair) 0)))
        ;; Move to row 1
        (forward-line 1)
        (let ((pair (mediawiki-discussion-tools--thread-at-point)))
          (should pair)
          (should (string= "h-B" (alist-get 'id (cdr pair))))
          (should (= (car pair) 1)))))))

(ert-deftest test-mdt-follow-point-sets-last-viewed-id ()
  "--follow-point updates --last-viewed-id when point moves to a new thread."
  (let ((t1 (test-mdt--mock-thread "h-A" "Thread A"))
        (t2 (test-mdt--mock-thread "h-B" "Thread B")))
    (setf (alist-get 'status t1) 'active)
    (setf (alist-get 'status t2) 'unanswered)
    (let ((threads (list t1 t2)))
      (with-temp-buffer
        (mediawiki-discussion-tools-list-mode)
        (setq mediawiki-discussion-tools--threads threads
              mediawiki-discussion-tools--last-viewed-id "h-A")
        (mediawiki-discussion-tools--refresh-table)
        ;; Point on row 0: --last-viewed-id already h-A, no change
        (goto-char (point-min))
        (let ((before mediawiki-discussion-tools--last-viewed-id))
          (mediawiki-discussion-tools--follow-point)
          (should (equal before mediawiki-discussion-tools--last-viewed-id)))
        ;; Move to row 1: should update --last-viewed-id to h-B
        ;; Mock --show-thread since it would try an API call
        (cl-letf (((symbol-function 'mediawiki-discussion-tools--show-thread)
                   (lambda (_delta) t)))
          (forward-line 1)
          (mediawiki-discussion-tools--follow-point)
          (should (string= "h-B" mediawiki-discussion-tools--last-viewed-id))
          (should (= mediawiki-discussion-tools--view-index 1)))))))

(ert-deftest test-mdt-move-to-row-updates-overlay ()
  "--move-to-row moves point and hl-line-overlay to the target row."
  (let ((t1 (test-mdt--mock-thread "h-A" "Thread A"))
        (t2 (test-mdt--mock-thread "h-B" "Thread B")))
    (setf (alist-get 'status t1) 'active)
    (setf (alist-get 'status t2) 'unanswered)
    (let ((threads (list t1 t2)))
      (with-temp-buffer
        (mediawiki-discussion-tools-list-mode)
        (setq mediawiki-discussion-tools--threads threads)
        (mediawiki-discussion-tools--refresh-table)
        (setq hl-line-overlay (make-overlay 1 1))
        (let ((moved-beg nil)
              (moved-end nil)
              (window-prop nil)
              (win-point nil))
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (_buf) (selected-window)))
                    ((symbol-function 'move-overlay)
                     (lambda (ov beg end)
                       (setq moved-beg beg moved-end end)))
                    ((symbol-function 'overlay-put)
                     (lambda (ov prop _val)
                       (when (eq prop 'window)
                         (setq window-prop t))))
                    ((symbol-function 'set-window-point)
                     (lambda (win pos)
                       (setq win-point pos))))
            (mediawiki-discussion-tools--move-to-row 1))
          (should moved-beg)
          (should (> moved-end moved-beg))
          (should win-point)
          (should window-prop))))))

(ert-deftest test-mdt-move-to-row-moves-point ()
  "--move-to-row positions point on the correct data row."
  (let ((t1 (test-mdt--mock-thread "h-A" "Thread A"))
        (t2 (test-mdt--mock-thread "h-B" "Thread B")))
    (setf (alist-get 'status t1) 'active)
    (setf (alist-get 'status t2) 'unanswered)
    (let ((threads (list t1 t2)))
      (with-temp-buffer
        (mediawiki-discussion-tools-list-mode)
        (setq mediawiki-discussion-tools--threads threads)
        (mediawiki-discussion-tools--refresh-table)
        ;; Check point at each row
        (mediawiki-discussion-tools--move-to-row 0)
        (should (string= "h-A" (tabulated-list-get-id)))
        (mediawiki-discussion-tools--move-to-row 1)
        (should (string= "h-B" (tabulated-list-get-id)))))))

(provide 'test-mediawiki-discussion-tools)

;;; test-mediawiki-discussion-tools.el ends here
