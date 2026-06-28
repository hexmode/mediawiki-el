;;; init.el --- Per-invocation init for mediawiki.el testing  -*- lexical-binding: t; -*-

;; Usage:
;;   emacs --init-directory=./emacs.d
;;
;; Requires Emacs 30+ for the :vc keyword.

;;; Code:

;; `package-vc-install' (used by `:vc') compiles every .el file it
;; finds, including tests.  The test directory intentionally triggers
;; compilation warnings (mock lambdas with unused args, setting internal
;; variables).  Suppress native compilation of test files to avoid ~200
;; warnings on startup.
(defvar native-comp-jit-compilation-deny-list)
(add-to-list 'native-comp-jit-compilation-deny-list
             (expand-file-name "mediawiki/tests" package-user-dir))

(require 'use-package)

(use-package mediawiki
  :vc (:url "https://github.com/hexmode/mediawiki-el" :rev :latest)
  :bind (([(control c) ?m] . mediawiki-site))
  :custom
  (mediawiki-debug t)
  (mediawiki-site-default "MediaWiki")
  (mediawiki-site-alist
   '(("MediaWiki"
      "https://www.mediawiki.org/w/"
      :first-page "Main Page"
      :oauth-client-id "ee568ec9d0e5251c4aba251e1baf8fe8"))))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((mediawiki :url "https://github.com/hexmode/mediawiki-el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
