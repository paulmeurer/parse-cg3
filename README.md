
FST/CG morphosyntax parse code (for Georgian and Abkhaz)

This code runs on Linux and Mac OS X/Intel

Installation:

* Common Lisp:

Install Clozure Common Lisp from ccl.clozure.com

Preferably use Emacs/slime as programming environment.

Your /home/<user>/.emacs file should look like dot.emacs, but make necessary adjustments, as detailed in the file

Get slime from https://slime.common-lisp.dev

* Libraries:

* Constraint Grammar:

Install CG3 from visl.sdu.dk/cg3.html

* Finite state calculus:

You need either Xerox FST (which is proprietary software), or (easier to install, open source) foma.

FST: Get it e.g. from Konstanz

foma: Install from fomafst.github.io

* Third-party Lisp libraries:

Install Quicklisp from quicklisp.org

* Installation of Lisp code and libraries:

When you install quicklisp, this directory will be created for you:

     ~/quicklisp/local-projects/

In this directory, clone the following git projects:

* clone git@git.app.uib.no:clarino-uib/lisp/parse-cg3.git
* clone git@git.app.uib.no:clarino-uib/lisp/foma.git
* clone git@git.app.uib.no:clarino-uib/lisp/utilities.git
* clone git@git.app.uib.no:clarino-uib/lisp/zebu.git
* clone git@git.app.uib.no:clarino-uib/lisp/graph.git
* clone git@git.app.uib.no:clarino-uib/lisp/aserve.git
* clone git@git.app.uib.no:clarino-uib/lisp/vislcg3.git
* clone git@git.app.uib.no:clarino-uib/lisp/xml.git
* clone git@git.app.uib.no:clarino-uib/lisp-lib/dat.git
* clone git@git.app.uib.no:clarino-uib/lisp-lib/encoding.git
* clone git@git.app.uib.no:clarino-uib/lisp-lib/libxslt.git

If you do use Xerox FST you will instead of foma.git need

* clone git@git.app.uib.no:clarino-uib/lisp-lib/fst.git

After that, when you have started Clozure Common Lisp, evaluate

      (ql:register-local-projects)

in the REPL (the Emacs buffer called *slime-repl clozure*). This is needed only once.

Then you can start the Georgian parser by evaluating

     (ql:quickload :parse-cg3-foma-kat)

or (in case of FST)

     (ql:quickload :parse-cg3-kat)

Third-party Common Lisp libraries are installed automatically (but you will have to install possibly missing Lunux libraries).

When everything loads fine, try it out by evaluating the code at the bottom of parse-kat.lisp.




