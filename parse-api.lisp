;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: PARSE; Base: 10 -*-
;;
;; Copyright (c) 2017, Paul Meurer, University of Bergen
;; http://clarino.uib.no
;; All rights reserved.
;; 

;; A REST/JSON API for GNC parsing

;; See api-client.lisp for an example client in Common Lisp

(in-package :parse)

;; request: http://clarino.uib.no/gnc/parse-api?command=<command>&…
;; examples:

;; http://clarino.uib.no/gnc/parse-api?command=get-session
;; returns: 
;; {"session-id":"242097204858072","user-id":[],"user-name":[]}

;; http://clarino.uib.no/gnc/parse-api?session-id=242097204858072&command=parameter&attribute=features&value=false
;; returns:
;; {"success":true}

;; http://clarino.uib.no/gnc/parse-api?session-id=242097204858072&command=parse&text=გამარჯობა.
;; returns:
;; {"tokens":[{"word":"გამარჯობა","msa":[{"lemma":"გამარჯობა","features":"Interj"}]},{"word":".","msa":[{"lemma":".","features":"Punct Period"}]}]}

(define-url-function parse-api-json
    (request ((session-id integer)
	      ;;(login-code string nil :global t)
	      (session-index string nil :global t t) ;; t t: cookie-only = t, get-only = nil
	      login-index
	      login-code
	      (command keyword)
	      (variety keyword)
	      text
	      ;; filters
	      (attribute keyword)
	      value
	      )
	     :path "/parse-api"
	     :base-url (base-url *framework*)
	     :type :json)
  (debug (request-query request))
  (with-database-connection ()
    (block rest
      (multiple-value-bind (user-id user-name login-type expired-p)
	  (fw::session-index->user :session-index (or login-index session-index) :login-code login-code)
	(declare (ignore #+debug user-name login-type))
	(when expired-p
	  (json "error" "Session has expired." "session-expired" t)
	  (return-from rest))
	(let ((session (get-session :gnc-text session-id :user-id user-id)))
	  (handler-case
	      (case command
		(:get-session
		 (setf (session-av-list session)
		       (list :readings t :lemma t :disambiguate t :features t :dependencies nil
			     :suppress-discarded-p t))
		 (json "session-id" (write-to-string (session-id session))
		       "user-id" user-id
		       "user-name" user-name))
		(:parameter
		 (debug value)
		 (when attribute
		   (setf (getf (session-av-list session) attribute)
			 (if (string-equal value "true") t nil))
		   (debug (session-av-list session))
		   (json "success" t)))
		(:parse
		 (let ((gnc-text (parse::parse-text
				  text
				  :variety (or variety :ng)
				  :load-grammar nil)))
		   ;;(setf gnc.text::*text* gnc-text)
		   (setf (getf (session-av-list session) :gnc-text) gnc-text)
		   (apply #'parse::write-gnc-text-json
			  gnc-text
			  stream
			  (session-av-list session))))
		(otherwise
		 (json "error" (format nil "The command ~a is not implemented." command))))
	    (error (cond)
	      (json "error" (setf *error-string* (format nil "~a" cond)))))))))
  (terpri stream))


#+test
(print (run-command *api-session* :fetch-word-list :attribute :word))



;;; client

(defclass api-session ()
  ((session-id :initform nil :initarg :session-id :reader session-id)))

(defparameter *server-uri* "http://localhost/gnc/parse-api")
#+test
(defparameter *server-uri* "http://clarino.uib.no/gnc/parse-api")

(defparameter *api-session* nil)

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))

(defmethod run-parse-command ((session api-session) command &key text variety attribute value)
  (when (and (null (session-id session))
	     (not (eq command :get-session)))
    (error "Session is not initialized."))
  (multiple-value-bind (response status-code)
      (drakma:http-request
       *server-uri*
       :method :post
       :parameters `(("command" . ,(format nil "~(~a~)" command))
		     ,@(when (session-id session) `(("session-id" . ,(write-to-string (session-id session)))))
		     ,@(when text `(("text" . ,(encoding:utf-8-encode text))))
		     ,@(when attribute `(("attribute" . ,attribute)))
		     ,@(when value `(("value" . ,(encoding:utf-8-encode value))))
		     )
       :cookie-jar *cookie-jar*
       :content-type "application/json"
       :want-stream nil)
    (cond ((eq status-code 200)
	   (let* ((response (encoding:utf-8-decode-octets response))
		  (json (st-json:read-json response)))
	     (write-line response)
	     (when (st-json:getjso "error" json)
	       (error "Server error: ~a" (st-json:getjso "error" json)))
	     json))
	  (t
	   (error "Status code: ~a" status-code)))))

(defun get-api-session ()
  (let* ((session (make-instance 'api-session))
	 (result (run-parse-command session :get-session))
	 (session-id (parse-integer (st-json:getjso "session-id" result))))
    (debug result)
    (setf (slot-value session 'session-id) session-id)
    session))

#+test
(setf *api-session* (get-api-session))

#+test
(run-parse-command *api-session* :parameter :attribute "dependencies" :value "true")

#+test
(run-parse-command *api-session* :parse :text "ეს მართლად არ ვარგა.")


;; 242091309200147

:eof
