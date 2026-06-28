;;; test-mediawiki-support-desk.el --- Unit tests for mediawiki-support-desk.el  -*- lexical-binding: t; -*-

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

;; Unit tests for the mediawiki-support-desk.el convenience wrapper.
;; The underlying logic is tested in test-mediawiki-discussion-tools.el.

;;; Code:

(require 'ert)
(require 'mediawiki-support-desk)

(ert-deftest test-sd-config-defaults ()
  (should (string= mediawiki-support-desk-page "Project:Support_desk")))

(ert-deftest test-sd-calls-discussion-tools ()
  "Test that mediawiki-support-desk delegates to mediawiki-discussion-tools."
  (let ((called-page nil)
        (called-site nil))
    (cl-letf (((symbol-function 'mediawiki-discussion-tools)
               (lambda (page &optional sitename)
                 (setq called-page page
                       called-site sitename))))
      (mediawiki-support-desk "TestSite")
      (should (string= called-page "Project:Support_desk"))
      (should (string= called-site "TestSite")))))

(provide 'test-mediawiki-support-desk)

;;; test-mediawiki-support-desk.el ends here
