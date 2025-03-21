;;;   -*- Mode: LISP; Package: COMMON-LISP-USER; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

(in-package :cl-user)

(asdf:defsystem :parse-cg3-foma
  :name "parse-cg3-foma"
  :author "Paul Meurer <paul.meurer@uib.no>"
  :maintainer "Paul Meurer <paul.meurer@uib.no>"
  :licence "BSD 3 term"
  :description "FST/CG morphosyntax parse code"
  :depends-on (:cl-foma
	       :acl-compat
               :graph
               :st-json
               ;:framework-shib
               :aserve
               :aserve-custom
	       :cl-vislcg3)
  :serial t
  :components
  ((:file "parse-package-foma")
   (:file "macros")
   (:file "parse-text")
   (:file "cg3-disambiguate")
   ;;(:file "parse-api")
   ))

:eof
