;;; check-parens.el --- precise parenthesis check for Emacs Lisp files  -*- lexical-binding: t; -*-
;;
;; `check-parens' reports the point where it gave up, which is often not
;; where the mistake is, and in batch it can look fine on a file the byte
;; compiler then rejects.  This walks the buffer with the Lisp parser and
;; reports the depth and the position of the innermost unclosed form.
;;
;; Usage:
;;   emacs -Q --batch -l tools/check-parens.el [-- FILE.el ...]
;;
;; With no files it checks *.el in the current directory and tests/*.el.
;; Exit status is 1 when any file is unbalanced.

(defun check-parens--report (file state)
  "Report STATE for FILE.  Return non-nil when FILE is balanced."
  (let ((depth (nth 0 state))
        (open (nth 1 state)))
    (cond
     ((> depth 0)
      (goto-char (or open (point-min)))
      (princ (format "%s: %d unclosed form(s), innermost opened at line %d:\n    %s\n"
                     file depth (line-number-at-pos)
                     (string-trim
                      (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position)))))
      nil)
     ((< depth 0)
      (princ (format "%s: %d extra closing paren(s) at end of file\n" file (- depth)))
      nil)
     (t t))))

(defun check-parens-file (file)
  "Check FILE.  Return non-nil when its parentheses balance."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (let ((state (condition-case err
                     (parse-partial-sexp (point-min) (point-max))
                   (error
                    (princ (format "%s: cannot parse: %s\n" file err))
                    nil))))
      (when state
        (check-parens--report file state)))))

(let* ((args (cdr command-line-args-left))
       (files (or args
                  (append (file-expand-wildcards "*.el")
                          (file-expand-wildcards "tests/*.el"))))
       (failed 0))
  (unless files
    (princ "check-parens: no files to check\n")
    (kill-emacs 1))
  (dolist (file files)
    (unless (check-parens-file file)
      (setq failed (1+ failed))))
  (princ (format "check-parens: %d file(s) checked, %d unbalanced\n"
                 (length files) failed))
  (kill-emacs (if (> failed 0) 1 0)))
