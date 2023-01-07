;;;   -*- Mode: LISP; Package: COMMON-LISP-USER; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

(in-package :cl-user)

(asdf:defsystem :parse-cg3-foma-kat
  :name "parse-cg3-foma-kat"
  :author "Paul Meurer <paul.meurer@uib.no>"
  :maintainer "Paul Meurer <paul.meurer@uib.no>"
  :licence "BSD 3 term"
  :description "FST/CG morphosyntax parse code"
  :depends-on (:parse-cg3-foma)
  :serial t
  :components
  ((:file "parse-kat")))

:eof
