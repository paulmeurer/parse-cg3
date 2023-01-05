;;; -*- Mode: LISP; Package: CL-USER; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;
;; Copyright (c) 2021, Paul Meurer, University of Bergen
;; https://clarino.uib.no
;; All rights reserved.
;; 

(in-package "CL-USER")

(defpackage "PARSE"
  (:use "COMMON-LISP"
        "CL-FOMA"
	"ACL-COMPAT.MP"
	"ASERVE"
	"NET.ASERVE"
        ;"FRAMEWORK"
        "GRAPH"
        )
  (:export "PARSED-TEXT"
           "TEXT-ARRAY"))

:eof
