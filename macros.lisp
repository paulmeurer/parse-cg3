;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: PARSE; Base: 10 -*-
;;
;; Copyright (c) 2017, Paul Meurer, University of Bergen
;; http://clarino.uib.no
;; All rights reserved.
;; 

(in-package :parse)

(defvar *in-json* nil)

;; captures stream
(defmacro json (&rest args)
  `(cond (*in-json*
	  (st-json:jso ,@args))
	 (t
	  (let ((*in-json* t))
	    (st-json:write-json
	     (st-json:jso ,@args)
	     stream)))))

:eof
