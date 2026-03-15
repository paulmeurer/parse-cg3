;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: PARSE; Base: 10 -*-
;;
;; Copyright (c) 2017, Paul Meurer, University of Bergen
;; http://clarino.uib.no
;; All rights reserved.
;;

;;;; ====================================================================
;;;; macros.lisp — Utility macros for the PARSE package
;;;; ====================================================================

(in-package :parse)

(defvar *in-json* nil)

;; Dual-purpose JSON macro: when called at top level (not nested),
;; writes a JSON object directly to the captured `stream` variable.
;; When called inside another json form (*in-json* is true), returns
;; a st-json:jso object instead, so nested structures compose correctly.
(defmacro json (&rest args)
  `(cond (*in-json*
	  (st-json:jso ,@args))
	 (t
	  (let ((*in-json* t))
	    (st-json:write-json
	     (st-json:jso ,@args)
	     stream)))))

:eof
