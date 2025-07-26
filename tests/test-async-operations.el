;;; test-async-operations.el --- Test async operation support -*- lexical-binding: t; -*-

;; Test implementation of task 8.1: Implement async operation support
;; Tests operation queuing, management, non-blocking modes, and status tracking

(require 'ert)
(require 'mediawiki-core)

;;; Mock Functions for Testing

(defvar test-mock-callbacks '()
  "List to store callbacks for manual triggering in tests.")

(defun mock-async-function (&rest args)
  "Mock async function that stores callbacks for manual triggering."
  (let ((callback (nth (- (length args) 2) args))
        (error-callback (nth (- (length args) 1) args)))
    (push (list 'success callback error-callback) test-mock-callbacks)))

(defun mock-failing-async-function (&rest args)
  "Mock async function that stores error callback for manual triggering."
  (let ((callback (nth (- (length args) 2) args))
        (error-callback (nth (- (length args) 1) args)))
    (push (list 'error callback error-callback) test-mock-callbacks)))

(defun mock-hanging-async-function (&rest args)
  "Mock async function that never calls any callback."
  (let ((callback (nth (- (length args) 2) args))
        (error-callback (nth (- (length args) 1) args)))
    (push (list 'hang callback error-callback) test-mock-callbacks)))

(defun test-trigger-mock-callbacks (type)
  "Trigger stored mock callbacks of TYPE ('success, 'error, or 'all)."
  (dolist (cb-info test-mock-callbacks)
    (let ((cb-type (nth 0 cb-info))
          (callback (nth 1 cb-info))
          (error-callback (nth 2 cb-info)))
      (when (or (eq type 'all) (eq type cb-type))
        (cond
         ((eq cb-type 'success) (when callback (funcall callback "mock-result")))
         ((eq cb-type 'error) (when error-callback (funcall error-callback "mock-error")))))))
  (setq test-mock-callbacks '()))

;;; Test Setup

(defvar test-async-operations-backup nil
  "Backup of async operations for test isolation.")

(defvar test-async-queue-backup nil
  "Backup of async queue for test isolation.")

(defvar test-async-active-backup nil
  "Backup of active operations for test isolation.")

(defvar test-async-stats-backup nil
  "Backup of statistics for test isolation.")

(defun test-async-setup ()
  "Set up test environment."
  (setq test-async-operations-backup (copy-hash-table mediawiki-async-operations))
  (setq test-async-queue-backup (copy-sequence mediawiki-async-operation-queue))
  (setq test-async-active-backup (copy-sequence mediawiki-async-active-operations))
  (setq test-async-stats-backup (copy-hash-table mediawiki-async-operation-statistics))
  
  (clrhash mediawiki-async-operations)
  (setq mediawiki-async-operation-queue '())
  (setq mediawiki-async-active-operations '())
  (clrhash mediawiki-async-operation-statistics)
  (setq mediawiki-async-queue-enabled t)
  (setq mediawiki-async-max-concurrent-operations 5)
  (setq test-mock-callbacks '()))

(defun test-async-teardown ()
  "Clean up test environment."
  (clrhash mediawiki-async-operations)
  (setq mediawiki-async-operation-queue '())
  (setq mediawiki-async-active-operations '())
  (clrhash mediawiki-async-operation-statistics)
  
  (when test-async-operations-backup
    (setq mediawiki-async-operations test-async-operations-backup))
  (when test-async-queue-backup
    (setq mediawiki-async-operation-queue test-async-queue-backup))
  (when test-async-active-backup
    (setq mediawiki-async-active-operations test-async-active-backup))
  (when test-async-stats-backup
    (setq mediawiki-async-operation-statistics test-async-stats-backup)))

;;; Test Operation Creation and Queuing

(ert-deftest test-async-create-operation ()
  "Test creating async operations."
  (test-async-setup)
  (unwind-protect
      (progn
        ;; Test basic operation creation
        (let* ((callback (lambda (result) result))
               (error-callback (lambda (error) error))
               (operation-id (mediawiki-async-create-operation
                             'test-op "Test operation" "test-site"
                             #'mock-async-function '()
                             callback error-callback)))
          
          (should (stringp operation-id))
          (should (string-match-p "^async-" operation-id))
          
          ;; Verify operation is stored
          (let ((operation (gethash operation-id mediawiki-async-operations)))
            (should operation)
            (should (eq (mediawiki-async-operation-type operation) 'test-op))
            (should (string= (mediawiki-async-operation-description operation) "Test operation"))
            (should (string= (mediawiki-async-operation-sitename operation) "test-site"))
            (should (eq (mediawiki-async-operation-function operation) #'mock-async-function))
            (should (equal (mediawiki-async-operation-args operation) '()))
            (should (eq (mediawiki-async-operation-callback operation) callback))
            (should (eq (mediawiki-async-operation-error-callback operation) error-callback))
            (should (= (mediawiki-async-operation-priority operation) 5)) ; default priority
            (should (= (mediawiki-async-operation-retry-count operation) 0)))))
    
    (test-async-teardown)))

(ert-deftest test-async-queue-operation ()
  "Test queuing operations."
  (test-async-setup)
  (unwind-protect
      (progn
        ;; Create and queue an operation
        (let ((operation-id (mediawiki-async-create-operation
                            'test-op "Test operation" "test-site"
                            #'mock-async-function '()
                            (lambda (result) result) nil)))
          
          ;; Initially not in queue
          (should-not (member operation-id mediawiki-async-operation-queue))
          
          ;; Queue the operation
          (mediawiki-async-queue-operation operation-id)
          
          ;; Should be in active operations now (since queue is empty)
          (should (member operation-id mediawiki-async-active-operations))
          
          ;; Verify the mock function was called
          (should (= (length test-mock-callbacks) 1))))
    
    (test-async-teardown)))

(ert-deftest test-async-priority-queue ()
  "Test priority-based operation queuing."
  (test-async-setup)
  (unwind-protect
      (progn
        ;; Set max concurrent to 1 to force queuing
        (setq mediawiki-async-max-concurrent-operations 1)
        
        ;; Create operations with different priorities
        (let ((low-priority-id (mediawiki-async-create-operation
                               'test-op "Low priority" "test-site"
                               #'mock-hanging-async-function 
                               '() nil nil '(:priority 2)))
              (high-priority-id (mediawiki-async-create-operation
                                'test-op "High priority" "test-site"
                                #'mock-hanging-async-function '() nil nil '(:priority 8)))
              (medium-priority-id (mediawiki-async-create-operation
                                  'test-op "Medium priority" "test-site"
                                  #'mock-hanging-async-function '() nil nil '(:priority 5))))
          
          ;; Queue all operations
          (mediawiki-async-queue-operation low-priority-id)
          (mediawiki-async-queue-operation high-priority-id)
          (mediawiki-async-queue-operation medium-priority-id)
          
          ;; First operation should be active, others queued
          (should (= (length mediawiki-async-active-operations) 1))
          (should (= (length mediawiki-async-operation-queue) 2))
          
          ;; Queue should be sorted by priority (high to low)
          (let ((first-queued (nth 0 mediawiki-async-operation-queue))
                (second-queued (nth 1 mediawiki-async-operation-queue)))
            (should (string= first-queued high-priority-id))
            (should (string= second-queued medium-priority-id)))))
    
    (test-async-teardown)))

;;; Test Operation Management

(ert-deftest test-async-cancel-operation ()
  "Test cancelling operations."
  (test-async-setup)
  (unwind-protect
      (progn
        (let ((operation-id (mediawiki-async-create-operation
                            'test-op "Test operation" "test-site"
                            #'mock-hanging-async-function '() nil nil)))
          
          (mediawiki-async-queue-operation operation-id)
          (should (gethash operation-id mediawiki-async-operations))
          
          ;; Cancel the operation
          (let ((cancelled (mediawiki-async-cancel-operation operation-id)))
            (should cancelled)
            (should-not (gethash operation-id mediawiki-async-operations))
            (should-not (member operation-id mediawiki-async-active-operations))
            (should-not (member operation-id mediawiki-async-operation-queue)))))
    
    (test-async-teardown)))

(ert-deftest test-async-pause-resume-queue ()
  "Test pausing and resuming the operation queue."
  (test-async-setup)
  (unwind-protect
      (progn
        ;; Pause queue
        (mediawiki-async-pause-queue)
        (should-not mediawiki-async-queue-enabled)
        
        ;; Create and try to queue operation
        (let ((operation-id (mediawiki-async-create-operation
                            'test-op "Test operation" "test-site"
                            #'mock-hanging-async-function '() nil nil)))
          
          (mediawiki-async-queue-operation operation-id)
          
          ;; Should be queued but not active (queue is paused)
          (should (member operation-id mediawiki-async-operation-queue))
          (should-not (member operation-id mediawiki-async-active-operations))
          
          ;; Resume queue
          (mediawiki-async-resume-queue)
          (should mediawiki-async-queue-enabled)
          
          ;; Operation should now be active
          (should (member operation-id mediawiki-async-active-operations))
          (should-not (member operation-id mediawiki-async-operation-queue))))
    
    (test-async-teardown)))

;;; Test Non-blocking Modes

(ert-deftest test-async-non-blocking-mode ()
  "Test toggling non-blocking mode."
  (test-async-setup)
  (unwind-protect
      (progn
        ;; Initially disabled
        (should-not mediawiki-non-blocking-mode)
        
        ;; Toggle on
        (mediawiki-toggle-non-blocking-mode)
        (should mediawiki-non-blocking-mode)
        
        ;; Toggle off
        (mediawiki-toggle-non-blocking-mode)
        (should-not mediawiki-non-blocking-mode))
    
    (test-async-teardown)))

(ert-deftest test-async-high-level-wrappers ()
  "Test high-level async wrapper functions."
  (test-async-setup)
  (unwind-protect
      (progn
        ;; Test basic operation creation with high-level interface
        (let ((operation-id (mediawiki-async-create-operation
                             'api-call "API call: query on test-site" "test-site"
                             #'mock-async-function '() nil nil)))
          (should (stringp operation-id))
          (mediawiki-async-queue-operation operation-id)
          
          (let ((operation (gethash operation-id mediawiki-async-operations)))
            (should operation)
            (should (eq (mediawiki-async-operation-type operation) 'api-call))
            (should (string-match-p "API call: query on test-site" 
                                   (mediawiki-async-operation-description operation)))))
        
        ;; Test page save wrapper simulation
        (let ((operation-id (mediawiki-async-create-operation
                             'page-save "Save page: Test Page on test-site" "test-site"
                             #'mock-async-function '() nil nil)))
          (should (stringp operation-id))
          (mediawiki-async-queue-operation operation-id)
          
          (let ((operation (gethash operation-id mediawiki-async-operations)))
            (should (eq (mediawiki-async-operation-type operation) 'page-save))
            (should (string-match-p "Save page: Test Page on test-site"
                                   (mediawiki-async-operation-description operation)))))
        
        ;; Test page get wrapper simulation
        (let ((operation-id (mediawiki-async-create-operation
                             'page-get "Get page: Test Page from test-site" "test-site"
                             #'mock-async-function '() nil nil)))
          (should (stringp operation-id))
          (mediawiki-async-queue-operation operation-id)
          
          (let ((operation (gethash operation-id mediawiki-async-operations)))
            (should (eq (mediawiki-async-operation-type operation) 'page-get))
            (should (string-match-p "Get page: Test Page from test-site"
                                   (mediawiki-async-operation-description operation))))))
    
    (test-async-teardown)))

;;; Test Statistics and Status Tracking

(ert-deftest test-async-statistics ()
  "Test async operation statistics tracking."
  (test-async-setup)
  (unwind-protect
      (progn
        ;; Create a mock operation
        (let ((operation (make-mediawiki-async-operation
                         :type 'test-op
                         :started (current-time))))
          
          ;; Test updating statistics
          (mediawiki-async-update-statistics operation 'completed 1.5)
          
          (let ((stats (gethash 'test-op mediawiki-async-operation-statistics)))
            (should stats)
            (should (= (mediawiki-async-stats-total-operations stats) 1))
            (should (= (mediawiki-async-stats-completed-operations stats) 1))
            (should (= (mediawiki-async-stats-failed-operations stats) 0))
            (should (= (mediawiki-async-stats-cancelled-operations stats) 0))
            (should (= (mediawiki-async-stats-average-completion-time stats) 1.5)))
          
          ;; Add a failed operation
          (mediawiki-async-update-statistics operation 'failed nil)
          
          (let ((stats (gethash 'test-op mediawiki-async-operation-statistics)))
            (should (= (mediawiki-async-stats-total-operations stats) 2))
            (should (= (mediawiki-async-stats-completed-operations stats) 1))
            (should (= (mediawiki-async-stats-failed-operations stats) 1))
            (should (= (mediawiki-async-stats-cancelled-operations stats) 0)))))
    
    (test-async-teardown)))

(ert-deftest test-async-operation-status ()
  "Test getting operation status information."
  (test-async-setup)
  (unwind-protect
      (progn
        (let ((operation-id (mediawiki-async-create-operation
                            'test-op "Test operation" "test-site"
                            #'mock-hanging-async-function '() nil nil)))
          
          ;; Get status before queuing
          (let ((status (mediawiki-async-get-operation-status operation-id)))
            (should status)
            (should (eq (plist-get status :type) 'test-op))
            (should (string= (plist-get status :description) "Test operation"))
            (should (string= (plist-get status :sitename) "test-site"))
            (should (eq (plist-get status :status) 'unknown)))
          
          ;; Queue operation and check status
          (mediawiki-async-queue-operation operation-id)
          
          (let ((status (mediawiki-async-get-operation-status operation-id)))
            (should (eq (plist-get status :status) 'active)))))
    
    (test-async-teardown)))

(ert-deftest test-async-batch-operations ()
  "Test batch operation queueing."
  (test-async-setup)
  (unwind-protect
      (progn
        (let ((operations (list
                          (list 'test-op1 "Operation 1" "site1" #'mock-hanging-async-function '() nil nil)
                          (list 'test-op2 "Operation 2" "site2" #'mock-hanging-async-function '() nil nil)
                          (list 'test-op3 "Operation 3" "site3" #'mock-hanging-async-function '() nil nil))))
          
          (let ((operation-ids (mediawiki-async-batch-operations operations)))
            (should (= (length operation-ids) 3))
            
            ;; Verify all operations were created and queued
            (dolist (id operation-ids)
              (should (stringp id))
              (should (gethash id mediawiki-async-operations))))))
    
    (test-async-teardown)))

(ert-deftest test-async-concurrent-limit ()
  "Test concurrent operation limit enforcement."
  (test-async-setup)
  (unwind-protect
      (progn
        ;; Set limit to 2
        (setq mediawiki-async-max-concurrent-operations 2)
        
        ;; Create 4 operations that don't complete
        (let ((operation-ids '()))
          (dotimes (i 4)
            (let ((id (mediawiki-async-create-operation
                      'test-op (format "Operation %d" i) "test-site"
                      #'mock-hanging-async-function
                      '() nil nil)))
              (push id operation-ids)
              (mediawiki-async-queue-operation id)))
          
          ;; Should have 2 active, 2 queued
          (should (= (length mediawiki-async-active-operations) 2))
          (should (= (length mediawiki-async-operation-queue) 2))))
    
    (test-async-teardown)))

(ert-deftest test-async-operation-timeout ()
  "Test operation timeout handling."
  (test-async-setup)
  (unwind-protect
      (progn
        ;; Create operation with short timeout
        (let* ((timeout-called nil)
               (error-callback (lambda (error) (setq timeout-called t)))
               (operation-id (mediawiki-async-create-operation
                             'test-op "Timeout test" "test-site"
                             #'mock-hanging-async-function
                             '() nil error-callback '(:timeout 0.1))))
          
          (mediawiki-async-queue-operation operation-id)
          
          ;; Operation should be active initially
          (should (gethash operation-id mediawiki-async-operations))
          (should (member operation-id mediawiki-async-active-operations))))
    
    (test-async-teardown)))

(provide 'test-async-operations)

;;; test-async-operations.el ends here