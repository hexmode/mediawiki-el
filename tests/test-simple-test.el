;; Simple test to check if async callbacks  -*- lexical-binding: t; -*-
(require 'ert)

(ert-deftest test-async-callback ()
  "Test that async callbacks work."
  (let ((called nil))
    (run-with-timer 0.1 nil (lambda () (setq called t)))
    (sit-for 0.2)
    (should called)))
