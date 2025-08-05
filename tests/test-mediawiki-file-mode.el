;;; test-mediawiki-file-mode.el --- Test mediawiki-file-mode functionality  -*- lexical-binding: t; -*-

;; This is a simple test file to verify mediawiki-file-mode works correctly
;; Run with: emacs -batch -L . -l mediawiki-autoloads.el -l test-mediawiki-file-mode.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'mediawiki-mode)

(ert-deftest test-mediawiki-file-mode-exists ()
  "Test that mediawiki-file-mode can be defined and called."
  (should (fboundp 'mediawiki-file-mode))

  ;; Test that we can enter the mode without error
  (with-temp-buffer
    (mediawiki-file-mode)
    (should (eq major-mode 'mediawiki-file-mode))
    (should (string= mode-name "MW-File"))))

(ert-deftest test-mediawiki-file-mode-keybindings ()
  "Test that mediawiki-file-mode has the correct keybindings."
  (with-temp-buffer
    (mediawiki-file-mode)

    ;; Test that file operation keys are bound correctly
    (should (eq (key-binding "\C-x\C-s") 'save-buffer))
    (should (eq (key-binding "\C-c\C-c") 'kill-buffer-and-window))
    (should (eq (key-binding "\C-x\C-w") 'write-file))
    (should (eq (key-binding "\C-c\C-o") 'find-file))

    ;; Test that the wiki reload key is not bound to mediawiki-reload
    (should-not (eq (key-binding "\M-g") 'mediawiki-reload))))

(ert-deftest test-mediawiki-file-mode-inherits-from-mediawiki-mode ()
  "Test that mediawiki-file-mode properly inherits from mediawiki-mode."
  (with-temp-buffer
    (mediawiki-file-mode)

    ;; Should have MediaWiki text formatting functions
    (should (fboundp 'mediawiki-insert-bold))
    (should (fboundp 'mediawiki-insert-italics))
    (should (fboundp 'mediawiki-insert-link))

    ;; Should have MediaWiki navigation functions
    (should (fboundp 'mediawiki-next-header))
    (should (fboundp 'mediawiki-prev-header))

    ;; Test that some MediaWiki keybindings still work
    (should (eq (key-binding "\C-c\C-f\C-b") 'mediawiki-insert-bold))
    (should (eq (key-binding "\C-c\C-f\C-i") 'mediawiki-insert-italics))
    (should (eq (key-binding "\C-c\C-f\C-l") 'mediawiki-insert-link))))

(ert-deftest test-mediawiki-file-mode-auto-mode-alist ()
  "Test that .wiki and .mediawiki files automatically use mediawiki-file-mode."
  ;; Test .wiki extension
  (should (eq (cdr (assoc "\\.wiki\\'" auto-mode-alist)) 'mediawiki-file-mode))

  ;; Test .mediawiki extension
  (should (eq (cdr (assoc "\\.mediawiki\\'" auto-mode-alist)) 'mediawiki-file-mode)))

;;; test-mediawiki-file-mode.el ends here
