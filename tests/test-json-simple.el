;;; test-json-simple.el --- Simple JSON test

(require 'json)

(message "JSON functions available:")
(message "  json-read-from-string: %s" (fboundp 'json-read-from-string))
(message "  json-parse-string: %s" (fboundp 'json-parse-string))

(let ((json-object-type 'plist)
      (json-array-type 'vector))
  (let ((result (json-read-from-string "{\"test\": \"value\"}")))
    (message "Parsed result: %s" result)
    (message "Test value: %s" (plist-get result :test))))

;;; test-json-simple.el ends here