;;; init.el --- Per-invocation init for mediawiki.el testing  -*- lexical-binding: t; -*-

;; Usage:
;;   emacs --init-directory=./emacs.d
;;
;; Requires Emacs 30+ for the :vc keyword.

;;; Code:
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
