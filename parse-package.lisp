;;; -*- Mode: LISP; Package: CL-USER; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;
;; Copyright (c) 2021, Paul Meurer, University of Bergen
;; https://clarino.uib.no
;; All rights reserved.
;;

;;;; ====================================================================
;;;; parse-package.lisp — PARSE package definition (cl-fst backend)
;;;; ====================================================================
;;;;
;;;; Defines the PARSE package used by all parse-cg3 source files.
;;;; This variant uses CL-FST for finite-state transducer operations;
;;;; parse-package-foma.lisp is the alternative using CL-FOMA.
;;;; ====================================================================

(in-package "CL-USER")

(defpackage "PARSE"
  (:use "COMMON-LISP"
        "CL-FST"
	"ACL-COMPAT.MP"
	"ASERVE"
	"NET.ASERVE"
        ;"FRAMEWORK"
        "GRAPH"
        )
  (:export "PARSED-TEXT"
           "TEXT-ARRAY"))

:eof
