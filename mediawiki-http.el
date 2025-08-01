;;; mediawiki-http.el --- HTTP utilities and compatibility functions for MediaWiki  -*- lexical-binding: t; -*-

;; Copyright (C) 2008, 2009, 2010, 2011, 2015 Mark A. Hershberger

;; Author: Mark A. Hershberger <mah@everybody.org>
;; Keywords: mediawiki wikipedia network wiki
;; URL: https://github.com/hexmode/mediawiki-el

;; This file is NOT (yet) part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides HTTP utilities and compatibility functions for
;; the MediaWiki package. It includes URL compatibility functions,
;; HTTP request/response handling, multipart form data encoding,
;; and legacy HTTP function definitions.

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-utils)
(require 'url-http)
(require 'mml)
(require 'mm-url)
(require 'cl-macs)

(eval-when-compile
  ;; Below copied from url-http to avoid compilation warnings
  (defvar url-http-extra-headers)
  (defvar url-http-target-url)
  (defvar url-http-proxy)
  (defvar url-http-connection-opened))

;;; URL Compatibility Functions

;; As of 2010-06-22, these functions are in Emacs
(unless (fboundp 'url-bit-for-url)
  (defun url-bit-for-url (method lookfor url)
    (when (fboundp 'auth-source-user-or-password)
      (let* ((urlobj (url-generic-parse-url url))
             (bit (funcall method urlobj))
             (methods (list 'url-recreate-url
                            'url-host)))
        (if (and (not bit) (> (length methods) 0))
            (auth-source-user-or-password
             lookfor (funcall (pop methods) urlobj) (url-type urlobj))
          bit)))))

(unless (fboundp 'url-user-for-url)
  (defun url-user-for-url (url)
    "Attempt to use .authinfo to find a user for this URL."
    (url-bit-for-url 'url-user "login" url)))

(unless (fboundp 'url-password-for-url)
  (defun url-password-for-url (url)
    "Attempt to use .authinfo to find a password for this URL."
    (url-bit-for-url 'url-password "password" url)))

;;; HTTP Request Creation

(when (and (fboundp 'url-http-create-request) (boundp 'url-http-extra-headers))
  (if (string= "GET / HTTP/1.0\r\nMIME-Version: 1.0\r\nConnection: close\r\nHost: example.com\r\nAccept: */*\r\nUser-Agent: URL/Emacs\r\nContent-length: 4\r\n\r\ntest"
	       (let ((url-http-target-url (url-generic-parse-url "http://example.com/"))
		     (url-http-data "test") (url-http-version "1.0") (url-http-referer "test")
		     url-http-method url-http-attempt-keepalives url-extensions-header
		     url-http-extra-headers url-http-proxy url-mime-charset-string)
		 (url-http-create-request)))
      (defun url-http-create-request (&optional ref-url)
	"Create an HTTP request for `url-http-target-url', referred to by REF-URL."
	(let* ((extra-headers)
	       (request nil)
	       (no-cache (cdr-safe (assoc "Pragma" url-http-extra-headers)))
	       (using-proxy url-http-proxy)
	       (proxy-auth (if (or (cdr-safe (assoc "Proxy-Authorization"
						    url-http-extra-headers))
				   (not using-proxy))
			       nil
			     (let ((url-basic-auth-storage
				    'url-http-proxy-basic-auth-storage))
			       (url-get-authentication url-http-target-url nil 'any nil))))
	       (real-fname (concat (url-filename url-http-target-url)
				   (with-no-warnings
                                     (url-recreate-url-attributes url-http-target-url))))
	       (host (url-host url-http-target-url))
	       (auth (if (cdr-safe (assoc "Authorization" url-http-extra-headers))
			 nil
		       (url-get-authentication (or
						(and (boundp 'proxy-info)
						     proxy-info)
						url-http-target-url) nil 'any nil))))
	  (if (equal "" real-fname)
	      (setq real-fname "/"))
	  (setq no-cache (and no-cache (string-match "no-cache" no-cache)))
	  (if auth
	      (setq auth (concat "Authorization: " auth "\r\n")))
	  (if proxy-auth
	      (setq proxy-auth (concat "Proxy-Authorization: " proxy-auth "\r\n")))

	  ;; Protection against stupid values in the referer
	  (if (and ref-url (stringp ref-url) (or (string= ref-url "file:nil")
						 (string= ref-url "")))
	      (setq ref-url nil))

	  ;; We do not want to expose the referer if the user is paranoid.
	  (if (or (memq url-privacy-level '(low high paranoid))
		  (and (listp url-privacy-level)
		       (memq 'lastloc url-privacy-level)))
	      (setq ref-url nil))

	  ;; url-http-extra-headers contains an assoc-list of
	  ;; header/value pairs that we need to put into the request.
	  (setq extra-headers (mapconcat
			       (lambda (x)
				 (concat (car x) ": " (cdr x)))
			       url-http-extra-headers "\r\n"))
	  (if (not (equal extra-headers ""))
	      (setq extra-headers (concat extra-headers "\r\n")))

	  ;; This was done with a call to `format'.  Concatting parts has
	  ;; the advantage of keeping the parts of each header together and
	  ;; allows us to elide null lines directly, at the cost of making
	  ;; the layout less clear.
	  (setq request
		;; We used to concat directly, but if one of the strings happens
		;; to being multibyte (even if it only contains pure ASCII) then
		;; every string gets converted with `string-MAKE-multibyte' which
		;; turns the 127-255 codes into things like latin-1 accented chars
		;; (it would work right if it used `string-TO-multibyte' instead).
		;; So to avoid the problem we force every string to be unibyte.
		(mapconcat
		 ;; FIXME: Instead of `string-AS-unibyte' we'd want
		 ;; `string-to-unibyte', so as to properly signal an error if one
		 ;; of the strings contains a multibyte char.
		 'string-as-unibyte
		 (delq nil
		       (list
			;; The request
			(or url-http-method "GET") " "
			(if using-proxy (url-recreate-url url-http-target-url) real-fname)
			" HTTP/" url-http-version "\r\n"
			;; Version of MIME we speak
			"MIME-Version: 1.0\r\n"
			;; (maybe) Try to keep the connection open
			"Connection: " (if (or using-proxy
					       (not url-http-attempt-keepalives))
					   "close" "keep-alive") "\r\n"
			;; HTTP extensions we support
			(if url-extensions-header
			    (format
			     "Extension: %s\r\n" url-extensions-header))
			;; Who we want to talk to
			(if (/= (url-port url-http-target-url)
				(url-scheme-get-property
				 (url-type url-http-target-url) 'default-port))
			    (format
			     "Host: %s:%d\r\n" host (url-port url-http-target-url))
			  (format "Host: %s\r\n" host))
			;; Who its from
			(if url-personal-mail-address
			    (concat
			     "From: " url-personal-mail-address "\r\n"))
			;; Encodings we understand
			(if url-mime-encoding-string
			    (concat
			     "Accept-encoding: " url-mime-encoding-string "\r\n"))
			(if url-mime-charset-string
			    (concat
			     "Accept-charset: " url-mime-charset-string "\r\n"))
			;; Languages we understand
			(if url-mime-language-string
			    (concat
			     "Accept-language: " url-mime-language-string "\r\n"))
			;; Types we understand
			"Accept: " (or url-mime-accept-string "*/*") "\r\n"
			;; User agent
			(url-http-user-agent-string)
			;; Proxy Authorization
			proxy-auth
			;; Authorization
			auth
			;; Cookies
			(url-cookie-generate-header-lines host real-fname
							  (equal "https" (url-type url-http-target-url)))
			;; If-modified-since
			(if (and (not no-cache)
				 (member url-http-method '("GET" nil)))
			    (let ((tm (url-is-cached url-http-target-url)))
			      (if tm
				  (concat "If-modified-since: "
					  (url-get-normalized-date tm) "\r\n"))))
			;; Whence we came
			(if ref-url (concat
				     "Referer: " ref-url "\r\n"))
			extra-headers
			;; Length of data
			(if url-http-data
			    (concat
			     "Content-length: " (number-to-string
						 (length url-http-data))
			     "\r\n"))
			;; End request
			"\r\n"
			;; Any data
			url-http-data "\r\n"))
		 ""))
	  (url-http-debug "Request is: \n%s" request)
	  request))))

;;; Multipart Form Data Encoding

;; There are some versions of emacs that don't have
;; mm-url-encode-multipart-form-data.
;; See https://lists.gnu.org/archive/html/emacs-diffs/2014-11/msg00167.html
(unless (fboundp 'mm-url-encode-multipart-form-data)
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
      'identity
      ;; Delete any returned items that are empty
      (delq nil
            (mapcar (lambda (data)

                      (cond
                       ((consp (car data))
                        (let ((fieldname (cadar data))
                              (filename  (caadar data))
                              (mimetype  (car (caadar data)))
                              (content   (caar (caadar data))))

                          (concat
                           ;; Encode the name
                           "Content-Disposition: form-data; name=\"" fieldname "\"\r\n"
                           "Content-Type: " mimetype "\r\n"
                           "Content-Transfer-Encoding: binary\r\n\r\n"
                           content
                           "\r\n")))

                       ((stringp (car data))
                        ;; For each pair

                        (concat
                         ;; Encode the name
                         "Content-Disposition: form-data; name=\""
                         (car data) "\"\r\n"
                         "Content-Type: text/plain; charset=utf-8\r\n"
                         "Content-Transfer-Encoding: binary\r\n\r\n"

                         (cond ((stringp (cdr data))
                                (cdr data))
                               ((integerp (cdr data))
                                (int-to-string (cdr data))))

                         "\r\n"))
                       (t (error "I don't handle this"))))
                    pairs))
      ;; use the boundary as a separator
      (concat "--" boundary "\r\n"))

     ;; put a boundary at the end.
     "--" boundary "--\r\n")))

;;; URL Retrieval Functions

(defun url-compat-retrieve (url post-process buffer callback cbargs)
  "Function to compatibly retrieve a URL.
Some provision is made for different versions of Emacs version.
POST-PROCESS is the function to call for post-processing.
BUFFER is the buffer to store the result in.  CALLBACK will be
called in BUFFER with CBARGS, if given."
  (let ((url-user-agent (concat
                         (if (not (eq url-user-agent 'default))
                             (string-trim (if (functionp url-user-agent)
                                              (funcall url-user-agent)
                                            url-user-agent))
                           "")
                         " mediawiki.el " mediawiki-version "\r\n")))
    (cond ((boundp 'url-be-asynchronous) ; Sniff w3 lib capability
           (if callback
               (setq url-be-asynchronous t)
             (setq url-be-asynchronous nil))
           (url-retrieve url t)
           (when (not url-be-asynchronous)
             (let ((result (funcall post-process buffer)))
               result)))
          (t (if callback
                 (url-retrieve url post-process
                               (list buffer callback cbargs))
               (with-current-buffer (url-retrieve-synchronously url)
                 (funcall post-process buffer)))))))

;;; HTTP GET and POST Functions

(defvar url-http-get-post-process 'url-http-response-post-process)
(defun url-http-get (url &optional headers buffer callback cbargs)
  "Convenience method to use method \='GET' to retrieve URL.
HEADERS is the optional headers to send.  BUFFER is where the
result will be stored.  CALLBACK will be called in BUFFER with
CBARGS, if given."
  (let* ((url-request-extra-headers (if headers headers
                                      (if url-request-extra-headers
                                          url-request-extra-headers
                                        (cons nil nil))))
         (url-request-method "GET"))

    (when (url-basic-auth url)
      (add-to-list 'url-request-extra-headers
                   (cons "Authorization" (url-basic-auth url))))
    (url-compat-retrieve url url-http-get-post-process
			 buffer callback cbargs)))

(defvar url-http-post-post-process 'url-http-response-post-process)
(defun url-http-post (url parameters &optional multipart headers buffer
                          callback cbargs)
  "Convenience method to use method \='POST' to retrieve URL.
PARAMETERS are the parameters to put the the body.  If MULTIPART
is t, then multipart/form-data will be used.  Otherwise,
applicaton/x-www-form-urlencoded is used.  HEADERS is the
optional headers to send.  BUFFER is where the result will be
stored.  CALLBACK will be called in BUFFER with CBARGS, if
given."

  (let* ((url-request-extra-headers
          (if headers headers
            (when url-request-extra-headers url-request-extra-headers)))
         (boundary (int-to-string (random)))
         (cs 'utf-8)
         (content-type
          (if multipart
              (concat "multipart/form-data, boundary=" boundary)
            (format "application/x-www-form-urlencoded; charset=%s" cs)))
         (url-request-method "POST")
         (url-request-coding-system cs)
         (url-request-data
          (if multipart
              (mm-url-encode-multipart-form-data
               parameters boundary)
            (mm-url-encode-www-form-urlencoded (delq nil parameters)))))
    (mapc
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

    (url-compat-retrieve url url-http-post-post-process
			 buffer callback cbargs)))

;;; Response Processing

(defun url-http-response-post-process (status &optional buffer
                                              callback cbargs)
  "Default post-processor for an HTTP POST request response.
STATUS is the HTTP status code.  BUFFER, CALLBACK, and CBARGS are
passed as well from the original POST request."
  (let ((kill-this-buffer (current-buffer)))
    (when (and (integerp status) (not (< status 300)))
      (mediawiki-debug kill-this-buffer "url-http-response-post-process:1")
      (error "Oops! Invalid status: %d" status))

    (when (or (not (boundp 'url-http-end-of-headers))
              (not url-http-end-of-headers))
      (mediawiki-debug kill-this-buffer "url-http-response-post-process:2")
      (error "Oops! Don't see end of headers!"))

    ;; FIXME: need to limit redirects
    (if (and (listp status)
             (eq (car status) :redirect))
        (progn
          (message (concat "Redirecting to " (cadr status)))
          (url-http-get (cadr status) nil buffer callback cbargs))

      (goto-char url-http-end-of-headers)
      (forward-line)

      (let ((str (decode-coding-string
                  (buffer-substring-no-properties (point) (point-max))
                  'utf-8)))
        (mediawiki-debug (current-buffer) "url-http-response-post-process:3")
        (when buffer
          (set-buffer buffer)
          (insert str)
          (goto-char (point-min))
          (set-buffer-modified-p nil))
        (when callback
          (apply callback (list str cbargs)))
        (when (not (or callback buffer))
          str)))))

(provide 'mediawiki-http)

;;; mediawiki-http.el ends here
