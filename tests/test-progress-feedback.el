;;; test-progress-feedback.el --- Test progress feedback functionality -*- lexical-binding: t; -*-

;; Test implementation of task 7.2: Add progress feedback for operations
;; Tests progress indicators, cancellation support, and status updates

(require 'ert)
(require 'mediawiki-core)

;;; Test Setup

(defvar test-progress-operations-backup nil
  "Backup of progress operations for test isolation.")

(defun test-progress-setup ()
  "Set up test environment."
  (setq test-progress-operations-backup (copy-hash-table mediawiki-progress-operations))
  (clrhash mediawiki-progress-operations)
  (setq mediawiki-progress-feedback-enabled t))

(defun test-progress-teardown ()
  "Clean up test environment."
  (clrhash mediawiki-progress-operations)
  (when test-progress-operations-backup
    (setq mediawiki-progress-operations test-progress-operations-backup)))

;;; Test Progress Tracking

(ert-deftest test-progress-start-and-finish ()
  "Test starting and finishing progress operations."
  (test-progress-setup)
  (unwind-protect
      (progn
        ;; Test starting progress
        (let ((id (mediawiki-progress-start "Test operation" 10)))
          (should id)
          (should (stringp id))
          (should (gethash id mediawiki-progress-operations))

          ;; Check operation structure
          (let ((operation (gethash id mediawiki-progress-operations)))
            (should (string= (mediawiki-progress-operation-description operation) "Test operation"))
            (should (= (mediawiki-progress-operation-total operation) 10))
            (should (= (mediawiki-progress-operation-completed operation) 0)))

          ;; Test finishing progress
          (mediawiki-progress-finish id "Operation completed")
          (should-not (gethash id mediawiki-progress-operations))))

    (test-progress-teardown)))

(ert-deftest test-progress-update ()
  "Test updating progress operations."
  (test-progress-setup)
  (unwind-protect
      (progn
        (let ((id (mediawiki-progress-start "Test operation" 10)))
          ;; Test progress update
          (mediawiki-progress-update id 5 "Halfway done")

          (let ((operation (gethash id mediawiki-progress-operations)))
            (should (= (mediawiki-progress-operation-completed operation) 5))
            (should (string= (mediawiki-progress-operation-status operation) "Halfway done")))

          ;; Test progress increment
          (mediawiki-progress-increment id 2 "Making progress")

          (let ((operation (gethash id mediawiki-progress-operations)))
            (should (= (mediawiki-progress-operation-completed operation) 7))
            (should (string= (mediawiki-progress-operation-status operation) "Making progress")))

          (mediawiki-progress-finish id)))

    (test-progress-teardown)))

(ert-deftest test-progress-cancellation ()
  "Test progress operation cancellation."
  (test-progress-setup)
  (unwind-protect
      (progn
        (let ((cancelled nil))
          ;; Create cancellable operation
          (let ((id (mediawiki-progress-start "Cancellable operation" 10 t
                                             (lambda () (setq cancelled t)))))

            ;; Test that operation is cancellable
            (let ((operation (gethash id mediawiki-progress-operations)))
              (should (mediawiki-progress-operation-cancellable operation)))

            ;; Test cancellation
            (should (mediawiki-progress-cancel id))
            (should cancelled)
            (should-not (gethash id mediawiki-progress-operations))))

        ;; Test non-cancellable operation
        (let ((id (mediawiki-progress-start "Non-cancellable operation" 10 nil)))
          (should-not (mediawiki-progress-cancel id))
          (should (gethash id mediawiki-progress-operations))
          (mediawiki-progress-finish id)))

    (test-progress-teardown)))

(ert-deftest test-progress-cancel-all ()
  "Test cancelling all operations."
  (test-progress-setup)
  (unwind-protect
      (progn
        (let ((cancelled-count 0))
          ;; Create multiple operations
          (let ((id1 (mediawiki-progress-start "Op 1" 10 t (lambda () (setq cancelled-count (1+ cancelled-count)))))
                (id2 (mediawiki-progress-start "Op 2" 20 nil))
                (id3 (mediawiki-progress-start "Op 3" 30 t (lambda () (setq cancelled-count (1+ cancelled-count))))))

            ;; Verify all operations exist
            (should (= (hash-table-count mediawiki-progress-operations) 3))

            ;; Cancel all
            (mediawiki-progress-cancel-all)

            ;; Check that cancellable operations were cancelled
            (should (= cancelled-count 2))

            ;; Check that only non-cancellable operation remains
            (should (= (hash-table-count mediawiki-progress-operations) 1))
            (should (gethash id2 mediawiki-progress-operations))

            ;; Clean up remaining operation
            (mediawiki-progress-finish id2))))

    (test-progress-teardown)))

(ert-deftest test-progress-with-feedback ()
  "Test progress with feedback helper function."
  (test-progress-setup)
  (unwind-protect
      (progn
        (let ((work-completed 0))
          (mediawiki-progress-with-feedback
           "Test work" 5
           (lambda (progress-fn)
             (dotimes (i 5)
               (setq work-completed (1+ work-completed))
               (funcall progress-fn work-completed (format "Step %d" (1+ i)))
               (sit-for 0.01)))) ; Small delay to simulate work

          ;; Verify work was completed
          (should (= work-completed 5))

          ;; Verify no operations remain active
          (should (= (hash-table-count mediawiki-progress-operations) 0))))

    (test-progress-teardown)))

(ert-deftest test-progress-indeterminate ()
  "Test indeterminate progress (no total)."
  (test-progress-setup)
  (unwind-protect
      (progn
        (let ((id (mediawiki-progress-start "Indeterminate operation")))
          (should id)

          (let ((operation (gethash id mediawiki-progress-operations)))
            (should-not (mediawiki-progress-operation-total operation))
            (should (= (mediawiki-progress-operation-completed operation) 0)))

          ;; Test incrementing without total
          (mediawiki-progress-increment id 3)

          (let ((operation (gethash id mediawiki-progress-operations)))
            (should (= (mediawiki-progress-operation-completed operation) 3)))

          (mediawiki-progress-finish id)))

    (test-progress-teardown)))

(ert-deftest test-progress-disabled ()
  "Test progress tracking when disabled."
  (test-progress-setup)
  (unwind-protect
      (progn
        ;; Disable progress feedback
        (setq mediawiki-progress-feedback-enabled nil)

        ;; Try to start progress - should return nil
        (let ((id (mediawiki-progress-start "Test operation" 10)))
          (should-not id)
          (should (= (hash-table-count mediawiki-progress-operations) 0)))

        ;; Test that update/finish operations are safe with nil id
        (mediawiki-progress-update nil 5)
        (mediawiki-progress-increment nil 2)
        (mediawiki-progress-finish nil))

    (test-progress-teardown)
    (setq mediawiki-progress-feedback-enabled t)))

(ert-deftest test-progress-display-functions ()
  "Test progress display and management functions."
  (test-progress-setup)
  (unwind-protect
      (progn
        ;; Create some test operations
        (let ((id1 (mediawiki-progress-start "Operation 1" 10))
              (id2 (mediawiki-progress-start "Operation 2" nil t)))

          (mediawiki-progress-update id1 5)
          (mediawiki-progress-increment id2 3)

          ;; Test that operations are tracked
          (should (= (hash-table-count mediawiki-progress-operations) 2))

          ;; Test list active operations (just verify no errors)
          (should (mediawiki-progress-list-active))

          ;; Clean up
          (mediawiki-progress-finish id1)
          (mediawiki-progress-finish id2)))

    (test-progress-teardown)))

(ert-deftest test-progress-time-formatting ()
  "Test time formatting utility."
  (test-progress-setup)
  (unwind-protect
      (progn
        (let ((start-time (current-time)))
          ;; Test different time ranges
          (let ((formatted (mediawiki-progress-format-time-elapsed start-time)))
            (should (string-match-p "\\.[0-9]s$" formatted))) ; Should show seconds with decimal

          ;; Test with artificial older time
          (let* ((old-time (time-subtract start-time '(0 70 0))) ; 70 seconds ago
                 (formatted (mediawiki-progress-format-time-elapsed old-time)))
            (should (string-match-p "1m [0-9]+s$" formatted))))) ; Should show minutes and seconds

    (test-progress-teardown)))

(provide 'test-progress-feedback)

;;; test-progress-feedback.el ends here
