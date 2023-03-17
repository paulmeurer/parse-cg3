;;   -*- Mode: LISP; Package: COMMON-LISP-USER; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

(in-package :cl-user)

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

#+openmcl
(setf ccl:*default-file-character-encoding* :utf-8)

(defparameter *lisp-root* (namestring (merge-pathnames "lisp" (user-homedir-pathname))))
(defparameter *quicklisp-root* (namestring (merge-pathnames "quicklisp" (user-homedir-pathname))))

#+(or :allegro :openmcl)
(setf (logical-pathname-translations "quicklisp")
      `(("**;*.*" ,(concatenate 'string *quicklisp-root* "/**/*.*"))))

#+(or :allegro :openmcl)
(setf (logical-pathname-translations "lisp")
      `(("**;*.*" ,(concatenate 'string *lisp-root* "/**/*.*"))))

#+(or :allegro :sbcl :openmcl)
(setf (logical-pathname-translations "lib")
      '(("**;*.*" "lisp:lib;**;*.*")))

#+(or :allegro :sbcl :openmcl)
(setf (logical-pathname-translations "projects")
      '(("**;*.*" "quicklisp:local-projects;**;*.*")))

(push #.(concatenate 'string *lisp-root* "systems/") asdf:*central-registry*)

:eof
