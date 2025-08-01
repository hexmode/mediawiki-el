;;; test-json-api-framework.el --- Test JSON API communication framework -*- lexical-binding: t; -*-

;; Test script for the JSON-based MediaWiki API communication framework

(require 'ert)
(require 'mediawiki-api)
(require 'mediawiki-core)

(ert-deftest test-api-response-creation ()
  "Test creating API response structures."
  (let ((response (make-mediawiki-api-response
                   :success t
                   :data '((query . ((pages . ((12345 . ((title . "Test Page"))))))))
                   :warnings nil
                   :errors nil)))
    (should (mediawiki-api-response-success response))
    (should (mediawiki-api-response-data response))))

(ert-deftest test-error-creation ()
  "Test MediaWiki API error creation."
  (let ((error (mediawiki-api-create-error "badtoken" "Invalid token")))
    (should (string= (plist-get error :code) "badtoken"))
    (should (string= (plist-get error :info) "Invalid token"))))

(ert-deftest test-parameter-building ()
  "Test API parameter building."
  (let ((params (mediawiki-api-build-params
                 "action" "query"
                 "titles" "Main Page"
                 "prop" "info")))
    (should (= (length params) 3))
    (should (string= (cdr (assoc "action" params)) "query"))
    (should (string= (cdr (assoc "titles" params)) "Main Page"))))

(ert-deftest test-version-comparison ()
  "Test MediaWiki version comparison."
  (should (= (mediawiki-api-version-compare '(1 35 0) '(1 35 0)) 0))
  (should (= (mediawiki-api-version-compare '(1 35 0) '(1 34 0)) 1))
  (should (= (mediawiki-api-version-compare '(1 34 0) '(1 35 0)) -1)))

(ert-deftest test-error-classification ()
  "Test error code classification."
  (should (eq (mediawiki-api-classify-error-code "badtoken") 'authentication))
  (should (eq (mediawiki-api-classify-error-code "ratelimited") 'rate-limit))
  (should (eq (mediawiki-api-classify-error-code "badtitle") 'validation))
  (should (eq (mediawiki-api-classify-error-code "editconflict") 'conflict)))

(ert-deftest test-response-validation ()
  "Test API response validation."
  (let ((valid-response (make-mediawiki-api-response
                         :success t
                         :data '((query . ((pages . nil))))
                         :warnings nil
                         :errors nil)))
    (should (null (mediawiki-api-validate-response valid-response)))))

(provide 'test-json-api-framework)

;;; test-json-api-framework.el ends here
