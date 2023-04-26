;;;   -*- Mode: LISP; Package: COMMON-LISP-USER; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

(in-package :cl-user)

(asdf:defsystem :parse-cg3
  :name "parse-cg3"
  :author "Paul Meurer <paul.meurer@uib.no>"
  :maintainer "Paul Meurer <paul.meurer@uib.no>"
  :licence "BSD 3 term"
  :description "FST/CG morphosyntax parse code"
  :depends-on (:cl-fst
	       :acl-compat
               :graph
               :st-json
               :framework-1-2
               :aserve
               :aserve-custom
	       :cl-vislcg3)
  :serial t
  :components
  ((:file "parse-package")
   (:file "macros")
   (:file "parse-text")
   (:file "cg3-disambiguate")
   (:file "parse-api")
   ))

:eof
