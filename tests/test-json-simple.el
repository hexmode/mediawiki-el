;;; test-json-simple.el --- Simple JSON test -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)

(ert-deftest test-json-functions-available ()
  "Test that required JSON functions are available."
  (should (fboundp 'json-read-from-string))
  (should (fboundp 'json-parse-string)))

(ert-deftest test-json-parsing-basic ()
  "Test basic JSON parsing functionality."
  (let ((json-object-type 'plist)
        (json-array-type 'vector))
    (let ((result (json-read-from-string "{\"test\": \"value\"}")))
      (should (plistp result))
      (should (string= (plist-get result :test) "value")))))

(ert-deftest test-json-parsing-complex ()
  "Test parsing of more complex JSON structures."
  (let ((json-object-type 'plist)
        (json-array-type 'vector))
    (let ((result (json-read-from-string "{\"array\": [1, 2, 3], \"nested\": {\"key\": \"value\"}}")))
      (should (plistp result))
      (should (vectorp (plist-get result :array)))
      (should (= (aref (plist-get result :array) 0) 1))
      (should (plistp (plist-get result :nested)))
      (should (string= (plist-get (plist-get result :nested) :key) "value")))))

;;; test-json-simple.el ends here