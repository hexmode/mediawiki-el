;;; test-mediawiki-cache.el --- Unit tests for mediawiki-cache.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 MediaWiki.el contributors

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

;; Unit tests for mediawiki-cache.el module.

;;; Code:

(require 'ert)
(require 'mediawiki-cache)

;;; Test Cache Operations

(ert-deftest test-mediawiki-cache-get-unknown ()
  "Test `mediawiki-cache-get' returns nil for unknown revids."
  (mediawiki-cache-clear)
  (should (null (mediawiki-cache-get 99999)))
  (should (null (mediawiki-cache-get -1))))

(ert-deftest test-mediawiki-cache-put-and-get ()
  "Test `mediawiki-cache-put' stores and `mediawiki-cache-get' retrieves."
  (mediawiki-cache-clear)
  (mediawiki-cache-put 42 "Hello, World!")
  (should (string= "Hello, World!" (mediawiki-cache-get 42)))
  (mediawiki-cache-put 99 "Another entry")
  (should (string= "Another entry" (mediawiki-cache-get 99)))
  (should (string= "Hello, World!" (mediawiki-cache-get 42))))

(ert-deftest test-mediawiki-cache-has-p ()
  "Test `mediawiki-cache-has-p' returns non-nil for cached revids."
  (mediawiki-cache-clear)
  (should-not (mediawiki-cache-has-p 1))
  (mediawiki-cache-put 1 "content")
  (should (mediawiki-cache-has-p 1))
  (should (string= "content" (mediawiki-cache-has-p 1))))

(ert-deftest test-mediawiki-cache-clear ()
  "Test `mediawiki-cache-clear' removes all entries."
  (mediawiki-cache-clear)
  (mediawiki-cache-put 1 "one")
  (mediawiki-cache-put 2 "two")
  (should (= 2 (mediawiki-cache-size)))
  (mediawiki-cache-clear)
  (should (= 0 (mediawiki-cache-size)))
  (should (null (mediawiki-cache-get 1)))
  (should (null (mediawiki-cache-get 2))))

;;; Test LRU Eviction

(ert-deftest test-mediawiki-cache-lru-eviction ()
  "Test LRU eviction: oldest entry removed when cache exceeds max-size."
  (mediawiki-cache-clear)
  (let ((mediawiki-cache-max-size 3))
    (mediawiki-cache-put 1 "first")
    (mediawiki-cache-put 2 "second")
    (mediawiki-cache-put 3 "third")
    ;; Cache now has 3 entries — at capacity
    (should (string= "first" (mediawiki-cache-get 1)))
    (should (= 3 (mediawiki-cache-size)))

    ;; Adding a 4th entry evicts the oldest (revid 1)
    (mediawiki-cache-put 4 "fourth")
    (should (= 3 (mediawiki-cache-size)))
    (should (null (mediawiki-cache-get 1)))   ; evicted
    (should (string= "second" (mediawiki-cache-get 2)))
    (should (string= "fourth" (mediawiki-cache-get 4)))))

(ert-deftest test-mediawiki-cache-lru-reorder-on-put ()
  "Test re-putting a revid moves it to front (not double-counted)."
  (mediawiki-cache-clear)
  (let ((mediawiki-cache-max-size 2))
    (mediawiki-cache-put 1 "old")
    (mediawiki-cache-put 2 "second")
    ;; Re-put revid 1 — should refresh its position
    (mediawiki-cache-put 1 "updated")
    (should (= 2 (mediawiki-cache-size)))
    ;; Adding a 3rd entry should evict revid 2 (now the oldest)
    (mediawiki-cache-put 3 "third")
    (should (= 2 (mediawiki-cache-size)))
    (should (string= "updated" (mediawiki-cache-get 1)))
    (should (null (mediawiki-cache-get 2)))   ; evicted
    (should (string= "third" (mediawiki-cache-get 3)))))

;;; Test Async Prefetch

(ert-deftest test-mediawiki-cache-prefetch-watchlist-diffs ()
  "Test `mediawiki-cache-prefetch-watchlist-diffs' populates cache."
  (mediawiki-cache-clear)
  (cl-letf (((symbol-function 'mediawiki-api-call-async)
             (lambda (_sitename _action _params callback)
               ;; Mock a response whose alist structure is traversable by
               ;; the code in `mediawiki-cache-prefetch' callback.
               (let ((mock-result
                      '((query
                        (pages
                         ("1"
                          (title . "Test")
                          (revisions
                           ((slots
                             (* . "mock revision content"))))))))))
                 (funcall callback mock-result)))))

    (mediawiki-cache-prefetch-watchlist-diffs
     "enwiki"
     '(((revid . 101) (old_revid . 100))))

    ;; Both old and new revisions should be cached
    (should (string= "mock revision content" (mediawiki-cache-get 100)))
    (should (string= "mock revision content" (mediawiki-cache-get 101)))))

(provide 'test-mediawiki-cache)

;;; test-mediawiki-cache.el ends here
