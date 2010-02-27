;;; mediawiki.el --- mediawiki frontend

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Original Author: Jerry <unidevel@yahoo.com.cn>
;; Updated for Emacs22 url.el: Mark A. Hershberger <mah@everybody.org>
;; Keywords: extensions, convenience, lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Howto: 
;;  (add-to-list 'load-path "path to mediawiki.el")
;;  (require 'mediawiki)
;;  M-x customize-group RET mediawiki RET
;;  *dink* *dink*
;;  M-x mediawiki-site RET Wikipedia RET
;;
;; Open a wiki file:    M-x mediawiki-open 
;; Save a wiki buffer:  C-x C-s 
;; Save a wiki buffer with a different name:  C-x C-w

;; TODO:
;;  * pcomplete for site alist
;;  * Optionally use org-mode formatting for editing and translate that to mw

;; Latest version can be found at
;;   http://meta.wikimedia.org/wiki/mediawiki.el
;;   http://www.emacswiki.org/cgi-bin/emacs/mediawiki.el

;;; Code:

(require 'url-http)

(when (not (fboundp 'url-hexify-string))
  (defconst url-unreserved-chars
    '(
      ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
      ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
      ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
      ?- ?_ ?. ?! ?~ ?* ?' ?\( ?\))
    "A list of characters that are _NOT_ reserved in the URL spec.
This is taken from RFC 2396.")

  (defun url-hexify-string (string)
  "Return a new string that is STRING URI-encoded.
First, STRING is converted to utf-8, if necessary.  Then, for each
character in the utf-8 string, those found in `url-unreserved-chars'
are left as-is, all others are represented as a three-character
string: \"%\" followed by two lowercase hex digits."
  ;; To go faster and avoid a lot of consing, we could do:
  ;; 
  ;; (defconst url-hexify-table
  ;;   (let ((map (make-vector 256 nil)))
  ;;     (dotimes (byte 256) (aset map byte
  ;;                               (if (memq byte url-unreserved-chars)
  ;;                                   (char-to-string byte)
  ;;                                 (format "%%%02x" byte))))
  ;;     map))
  ;;
  ;; (mapconcat (curry 'aref url-hexify-table) ...)
  (mapconcat (lambda (byte)
               (if (memq byte url-unreserved-chars)
                   (char-to-string byte)
                 (format "%%%02x" byte)))
             (string-to-list string)
             "")))

(defun url-http-get (url &optional headers bufname callback cbargs)
  "Convenience method to use method 'GET' to retrieve URL"
  (let* ((url-request-extra-headers (if headers headers
                                      (if url-request-extra-headers url-request-extra-headers
                                        (cons nil nil))))
         (buf (if bufname bufname
                (current-buffer)))
         (url-request-method "GET"))

    (url-retrieve
     url
     'url-http-get-post-process
     (list buf callback cbargs))))

(defun url-http-get-post-process (status bufname callback cbargs)
  "Post process an HTTP GET."
  (when bufname
    (goto-char url-http-end-of-headers)
    (let ((old-buf (current-buffer))
          (str (decode-coding-string
                (buffer-substring-no-properties
                 (point) (point-max))
                'utf-8)))
      (kill-buffer old-buf)
      (with-current-buffer bufname
        (insert-string str)
        (when callback
          (apply callback cbargs))))))


(require 'mml)
(require 'mm-url)

(defvar url-http-post-post-process 'url-http-post-post-process)
(defun url-http-post (url parameters &optional multipart headers bufname
                          callback cbargs)
  "Convenience method to use method 'POST' to retrieve URL"

  (let* ((url-request-extra-headers (if headers headers
                                      (if url-request-extra-headers url-request-extra-headers
                                        (cons nil nil))))
         (boundary (int-to-string (random)))
         (cs 'utf-8)
         (content-type (if multipart
                           (concat "multipart/form-data, boundary=" boundary)
                         (format "application/x-www-form-urlencoded; charset=%s" cs)))
         (url-request-method "POST")
         (url-request-coding-system cs)
         (url-request-data (if multipart
                               (mm-url-encode-multipart-form-data parameters boundary)
                             (mm-url-encode-www-form-urlencoded parameters))))
    (mapcar
     (lambda (pair)
       (let ((key (car pair))
             (val (cdr pair)))
         (if (assoc key url-request-extra-headers)
             (setcdr (assoc key url-request-extra-headers) val)
           (add-to-list 'url-request-extra-headers
                        (cons key val)))))
     (list 
      (cons "Connection" "close")
      (cons "Content-Type" content-type)))

    (url-retrieve
     url
     url-http-post-post-process
     (list bufname callback cbargs))))

(defun url-http-post-post-process (status bufname &optional callback cbargs)
  "Post process on HTTP POST."
  (let ((kill-this-buffer (current-buffer)))
    (when (and (integerp status) (not (< status 300)))
      (kill-buffer kill-this-buffer)
      (error "Oops! Invalid status: %d" status))

    (when (not url-http-end-of-headers)
      (kill-buffer kill-this-buffer)
      (error "Oops! Don't see end of headers!"))

    (goto-char url-http-end-of-headers)
    (let ((str (decode-coding-string
                (buffer-substring-no-properties (point) (point-max))
                'utf-8)))
      (kill-buffer (current-buffer))
      (if (not bufname)
          (when callback
            (apply callback (list str cbargs)))
        (set-buffer bufname)
        (insert-string str)
        (set-buffer-modified-p nil)
        (pop-to-buffer bufname)))))

(defun mm-url-encode-multipart-form-data (pairs &optional boundary)
  "Return PAIRS encoded in multipart/form-data."
  ;; RFC1867

  ;; Get a good boundary
  (unless boundary
    (setq boundary (mml-compute-boundary '())))
      
  (concat

   ;; Start with the boundary
   "--" boundary "\r\n"

   ;; Create name value pairs
   (mapconcat

    ;; Delete any returned items that are empty
    (delq nil
          (lambda (data)
            (when (car data)
              ;; For each pair
              (concat

               ;; Encode the name
               "Content-Disposition: form-data; name=\"" (car data) "\"\r\n"
               "Content-Type: text/plain; charset=utf-8\r\n"
               "Content-Transfer-Encoding: binary\r\n\r\n"

               (cond ((stringp (cdr data))
                      (cdr data))
                     ((integerp (cdr data))
                      (int-to-string (cdr data))))

               "\r\n"))))

    ;; use the boundary as a separator
    pairs (concat "--" boundary "\r\n"))

   ;; put a boundary at the end.
   "--" boundary "--\r\n"))

(defgroup mediawiki nil
  "A mode for editting pages on MediaWiki sites."
  :tag "MediaWiki")

(defcustom mediawiki-site-default "Wikipedia"
  "The default mediawiki site to point to.  Set here for the
default and use `mediawiki-site' to set it per-session
later."
  :type 'string
  :tag "MediaWiki Site Default"
  :group 'mediawiki)

(defcustom mediawiki-site-alist '(("Wikipedia"
                                   "http://en.wikipedia.org/wiki/index.php"
                                   "username"
                                   "password"))
  "A list of MediaWiki websites."
  :group 'mediawiki
  :type '(alist :tag "Site Name"
                :key-type (string :tag "Site Name")
                :value-type (list :tag "Parameters"
                                  (string :tag "URL")
                                  (string :tag "Username")
                                  (string :tag "Password")
                                  (string :tag "First Page"
                                          :description "First page to open when `mediawiki-site' is called for this site"))))

(defvar mediawiki-login-success "You are now logged in"
  "A string that should be present on login success on all
mediawiki sites.")

(defvar mediawiki-permission-denied "The action you have requested is limited"
  "A string that will indicate permission has been denied.")

(defvar mediawiki-site nil
  "The current mediawiki site from `mediawiki-site-alist'.  If
not set, defaults to `mediawiki-site-default'.")

(defvar mediawiki-argument-pattern "?title=%s&action=%s"
  "Format of the string to append to URLs.  Two string arguments
are expected: first is a title and then an action.")

(defvar mediawiki-URI-pattern
  "http://\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?/"
  "Pattern matching a URI like this:
	http://mediawiki.sf.net/index.php
Password not support yet")

(defvar mediawiki-page-uri nil
  "The URI of the page corresponding to the current buffer, thus defining
the base URI of the wiki engine as well as group and page name.")

(defvar mediawiki-page-title nil
  "The title of the page corresponding to the current buffer")

(defvar mediawiki-url nil
  "The URL of the current Mediawiki site for the current buffer")

(defvar mediawiki-page-ring nil
  "Ring that holds names of buffers we navigate through.")

(defvar mediawiki-page-ring-index 0)

(defun mediawiki-set-url (&optional url)
  (interactive "sURL: ")
  (if (string-match mediawiki-URI-pattern url) 
      (setq mediawiki-url url)
    (error (concat "Error: [" url "] is not a valid mediawiki url."))))

(defun mediawiki-make-url (title action)
  (format (concat mediawiki-url mediawiki-argument-pattern)
	  (url-hexify-string title)
	  action))

(defun mediawiki-open (name)
  "Open a wiki page specified by NAME from the mediawiki engine"
  (interactive "sWiki Page: ")
  (when (or (not (stringp name))
            (string-equal "" name))
    (error "Need to specify a name."))
  (when (not mediawiki-url)
    (mediawiki-set-url))
  (mediawiki-edit name "raw"))

(defun mediawiki-reload ()
  (interactive)
  (let ((title mediawiki-page-title))
    (if title
	(mediawiki-open title)
      (error "Error: %s is not a mediawiki document." (buffer-name)))))


(defun mediawiki-edit (title action)
  "Edit wiki file with the name of title"
  (when (not (ring-p mediawiki-page-ring))
    (setq mediawiki-page-ring (make-ring 30)))

  (ring-insert mediawiki-page-ring
               (get-buffer-create
                (concat mediawiki-site ": " title)))
  (mediawiki-get title (ring-ref mediawiki-page-ring 0)))

(defvar mediawiki-edit-form-vars nil)
(defun mediawiki-get-edit-form-vars (bufname)
  "Extract the form variables from a page.  This should only be
called from a buffer in mediawiki-mode as the variables it sets
there will be local to that buffer."

  ;; Assume UTF-8, put contents in str
  (let ((str (decode-coding-string
              (buffer-substring-no-properties (point) (point-max))
              'utf-8))
        form (start 0))

    ;; Find the form
    (if (string-match "<form [^>]*id=[\"']editform['\"][^>]*>\\(\\(.\\|\n\\)*?\\)</form>" str)

        (progn
          (setq form (match-string 1 str))

          ;; find the first input element in the form
          (setq start (string-match "<input \\([^>]*name=[\"']\\([^\"']+\\)['\"][^>]*\\)>" form start))

          ;; clear the list of (buffer-local) form variables. Have to
          ;; switch like this because we're in a buffer containing the
          ;; results of the post.
          (with-current-buffer bufname
            (setq mediawiki-edit-form-vars '(nil)))

          ;; Continue until we can't find any more input elements
          (while start

            ;; First, capture the place where we'll start next.  Have
            ;; to do this here since match-end doesn't seem to let you
            ;; specify the string you were matching against, unlike
            ;; match-string
            (setq start (match-end 0))

            ;; Capture the string that defines this element
            (let ((el (match-string 1 form))
                  ;; get the element name
                  (el-name (match-string 2 form)))

              ;; figure out if this is a submit button and skip it if it is.
              (unless (string-match "type=[\"']submit['\"]" el)
                (string-match "value=[\"']\\([^\"']*\\)['\"]" el)

                ;; set the buffer-local form variables
                (with-current-buffer bufname
                  (add-to-list 'mediawiki-edit-form-vars
                               (cons el-name (match-string 1 el))))))
            (setq start (string-match
                         "<input \\([^>]*name=[\"']\\([^\"']+\\)['\"][^>]*\\)>"
                                      form start))))
      (if (string-match mediawiki-permission-denied str)
          (if (mediawiki-logged-in-p)
              (error "Permission Denied.")
            (error "Log in first and try again.")
          (error "Permission Denied. Login and try again")
        (error "No edit form found!"))))))

(defun mediawiki-logged-in-p ()
  "Returns t if we are logged in already."
  (not (eq nil mediawiki-site)))         ; FIXME should check cookies

(defun mediawiki-get (title bufname)
  (let ((page-uri (mediawiki-make-url title "raw")))

    (with-current-buffer bufname
      (delete-region (point-min) (point-max))
      (mediawiki-mode)
      (setq mediawiki-page-title title)
      (setq mediawiki-page-uri page-uri)

      (setq url-request-extra-headers 
            (list (cons "Connection" "close"))))

    (url-http-get page-uri         ; Source URI
                  nil              ; headers
                  bufname          ; output buffer
                  ;; callback on result buf
                  'mediawiki-setup-buffer-for-edit
                  (list title))))

(defun mediawiki-setup-buffer-for-edit (title)
  (url-http-get
   (mediawiki-make-url title "edit")
   nil (get-buffer-create " *mediawiki-form*")
   'mediawiki-get-edit-form-vars
   (list (current-buffer)))
  (set-buffer-file-coding-system 'utf-8)
  (goto-char (point-min))
  (pop-to-buffer bufname)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list t)
  (buffer-enable-undo bufname))

(defun mediawiki-save (&optional summary)
  (interactive "sSummary: ")
  (if mediawiki-page-title
      (mediawiki-save-page
       mediawiki-page-title
       summary
       (buffer-substring-no-properties (point-min) (point-max)))
    (error "Error: %s is not a mediawiki document." (buffer-name))))

(defun mediawiki-save-as (&optional title summary)
  (interactive "sTitle: "
               "sSummary: ")
  (when (and title (not (string= title "")))
    (mediawiki-save-page title summary (buffer-string-no-properties))
    (mediawiki-open title)))

(defun mediawiki-site-extract (sitename index)
  (nil-blank-string (nth index (assoc sitename mediawiki-site-alist))))

(defun mediawiki-site-url (sitename)
  "Get the url for a given site."
  (mediawiki-site-extract sitename 1))

(defun mediawiki-site-username (sitename)
  "Get the username for a given site."
  (mediawiki-site-extract sitename 2))

(defun mediawiki-site-password (sitename)
  "Get the password for a given site."
  (mediawiki-site-extract sitename 3))

(defun mediawiki-site-first-page (sitename)
  "Get the password for a given site."
  (mediawiki-site-extract sitename 4))

(defun mediawiki-do-login (sitename &optional username password)
  "Use USERNAME and PASSWORD to log into the MediaWiki site and
get a cookie."
  (interactive "sSitename: ")

  (setq mediawiki-site nil)             ; This wil be set once we are logged in

  ;; Possibly save info once we have it, eh?
  (let ((url (or (mediawiki-site-url sitename)
                 (read-string "URL: ")))
        (user (or (mediawiki-site-username sitename)
                  username
                  (read-string "Username: ")))
        (pass (or (mediawiki-site-password sitename)
                  password
                  (read-passwd "Password: "))))
    (mediawiki-set-url url)

    ;; Set the wpName and wpPassword on the proper submit page.
    (url-http-post (mediawiki-make-url "Special:Userlogin" "submitlogin")
                   (list (cons "wpName" user)
                         (cons "wpPassword" pass))
                   nil nil nil
                   'mediawiki-check-login
                   (list sitename))))

(defun mediawiki-check-login (str args)
  "Check that the password was accepted."
  (if (not (string-match mediawiki-login-success str))
      (error "Invalid Login!")
    (setq mediawiki-site (car args))
    (message (format "Login to MediaWiki site '%s' successful." (car args)))
    (when (mediawiki-site-first-page mediawiki-site)
      (mediawiki-open (mediawiki-site-first-page mediawiki-site)))))

(defun mediawiki-save-page (&optional title summary content)
  "Save the current page to a MediaWiki wiki."
  (let ((page-uri (mediawiki-make-url title "submit&externaledit=true"))
        (document (if content content
                      (buffer-substring)))
        (args mediawiki-edit-form-vars))

    (mapcar
     (lambda (pair)
       (let ((key (car pair))
             (val (cdr pair)))
         (if (assoc key args)
             (setcdr (assoc key args) val)
           (add-to-list 'args
                        (cons key val)))))
     (list (cons "wpSummary" summary)
           (cons "wpTextbox1" content)
           (cons "wpSave" 1)))

    (setq url-request-extra-headers 
          (list (cons "Connection" "Close")))

    (url-http-post
     page-uri  ;; Get destination
     args      ;; Form elements
     t)        ;; This is multipart

    (set-buffer-modified-p nil)))

(defun mediawiki-browse (&optional buf)
  "Open the buffer BUF in a browser. If BUF is not given,
the current buffer is used."
  (interactive)
  (if mediawiki-page-title 
      (browse-url (mediawiki-make-url mediawiki-page-title "view"))))

(defun mediawiki-site (&optional site)
  "Set up mediawiki.el for a site.  Without an argument, use
`mediawiki-site-default'.  Interactively, prompt for a site."
  (interactive "sSitename: ")
  (when (or (eq nil mediawiki-site)
            (not (string-equal site mediawiki-site)))
    (mediawiki-do-login site)))

(defun mediawiki-open-page-at-point ()
  "Open a new buffer with the page at point."
  (interactive)
  (mediawiki-open (mediawiki-page-at-point)))

(defun mediawiki-page-at-point ()
  "Return the page name under point.  Typically, this means
anything enclosed in [[PAGE]]."
  (let ((pos (point))
        (eol (point-at-eol))
        (bol (point-at-bol))
        page)
    (save-excursion
      (let ((end   (search-forward  "]]" eol t))
            (start (search-backward "[[" bol t)))
        (when (and 
               (not (eq nil start))
               (not (eq nil end))
               (<= pos end)
               (>= pos start))
          (buffer-substring-no-properties
           (+ start 2) (- end 2)))))))

(defmacro mediawiki-goto-relative-page (direction)
  `(let ((buff (ring-ref mediawiki-page-ring
                        (setq mediawiki-page-ring-index
                              (,direction mediawiki-page-ring-index 1)))))
     (while (not (buffer-live-p buff))
       (setq buff (ring-ref mediawiki-page-ring
                            (setq mediawiki-page-ring-index
                                  (,direction mediawiki-page-ring-index 1)))))
     (pop-to-buffer buff)))

(defun mediawiki-goto-previous-page ()
  "Pop up the previous page being editted."
  (interactive)
  (mediawiki-goto-relative-page -))

(defun mediawiki-goto-next-page ()
  "Pop up the previous page being editted."
  (interactive)
  (mediawiki-goto-relative-page +))

(define-derived-mode mediawiki-mode text-mode "MediaWiki Mode"
  (progn
    (make-variable-buffer-local 'mediawiki-page-title)
    (make-variable-buffer-local 'mediawiki-site)
    (make-variable-buffer-local 'mediawiki-url)
    (make-variable-buffer-local 'mediawiki-edit-form-vars)

    (define-key mediawiki-mode-map "\M-g"     'mediawiki-reload)
    (define-key mediawiki-mode-map "\C-x\C-s" 'mediawiki-save)
    (define-key mediawiki-mode-map "\C-c\C-c" 'mediawiki-save-and-bury)
    (define-key mediawiki-mode-map "\C-x\C-w" 'mediawiki-save-as)
    (define-key mediawiki-mode-map "\C-c\C-o" 'mediawiki-open)
    (define-key mediawiki-mode-map "\M-p"     'mediawiki-goto-previous-page)
    (define-key mediawiki-mode-map "\M-n"     'mediawiki-goto-next-page)
    (define-key mediawiki-mode-map [(control return)]
      'mediawiki-open-page-at-point)))

(provide 'mediawiki)

;;; mediawiki.el ends here