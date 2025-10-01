
FST/CG morphosyntax parse code (for Georgian and Abkhaz)

This code runs on Linux and Mac OS X/Intel

Installation:

* Common Lisp:

Install Clozure Common Lisp from ccl.clozure.com:

see https://github.com/Clozure/ccl/releases/tag/v1.13
Tested for AlmaLinux 8.8

Preferably use Emacs/slime as programming environment.

Your /home/<user>/.emacs file should look like dot.emacs, but make necessary adjustments, as detailed in the file

Get slime from https://slime.common-lisp.dev:

* git clone https://github.com/slime/slime.git

* Libraries:

* Constraint Grammar:

Install CG3 from visl.sdu.dk/cg3.html (https://github.com/GrammarSoft/cg3)

need: yum install rapidjson

* Finite state calculus:

You need either Xerox FST (which is proprietary software), or (easier to install, open source) foma.

FST: Get it e.g. from Konstanz

foma: Install from fomafst.github.io

* Third-party Lisp libraries:

Install Quicklisp from quicklisp.org:

* curl -O https://beta.quicklisp.org/quicklisp.lisp

* run (load "quicklisp.lisp"), then follow instructions

* Installation of Lisp code and libraries:

When you install quicklisp, this directory will be created for you:

```
~/quicklisp/local-projects/
```

In this directory, clone the following git projects:

* git clone https://github.com/paulmeurer/parse-cg3.git
* git clone https://github.com/paulmeurer/foma.git
* git clone https://github.com/paulmeurer/utilities.git
* git clone https://github.com/paulmeurer/zebu.git
* git clone https://github.com/paulmeurer/graph.git
* git clone https://github.com/paulmeurer/aserve.git
* git clone https://github.com/paulmeurer/vislcg3.git
* git clone https://github.com/paulmeurer/xml.git
* git clone https://github.com/paulmeurer/dat.git
* git clone https://github.com/paulmeurer/encoding.git
* git clone https://github.com/paulmeurer/libxslt.git

etc., or

* git clone git@git.app.uib.no:clarino-uib/lisp/parse-cg3.git
* git clone git@git.app.uib.no:clarino-uib/lisp/foma.git
* git clone git@git.app.uib.no:clarino-uib/lisp/utilities.git
* git clone git@git.app.uib.no:clarino-uib/lisp/zebu.git
* git clone git@git.app.uib.no:clarino-uib/lisp/graph.git
* git clone git@git.app.uib.no:clarino-uib/lisp/aserve.git
* git clone git@git.app.uib.no:clarino-uib/lisp/vislcg3.git
* git clone git@git.app.uib.no:clarino-uib/lisp/xml.git
* git clone git@git.app.uib.no:clarino-uib/lisp-lib/dat.git
* git clone git@git.app.uib.no:clarino-uib/lisp-lib/encoding.git
* git clone git@git.app.uib.no:clarino-uib/lisp-lib/libxslt.git

If you do use Xerox FST you will instead of foma.git need

* git clone git@git.app.uib.no:clarino-uib/lisp-lib/fst.git

After that, when you have started Clozure Common Lisp, evaluate

```
(ql:register-local-projects)
```

in the REPL (the Emacs buffer called *slime-repl clozure*). This is needed only once.

To get the foma morphology files, do this:

```
cd parse-cg3
mkdir regex
cd regex
curl https://clarino.uib.no/iness/resources/misc/georgian-morph-foma.tar.gz --output foma.tar.gz
tar -xvf foma.tar.gz
```

If you can use the Xerox fst files, you can get them in a similar manner:

```
cd parse-cg3
mkdir regex
cd regex
curl https://clarino.uib.no/iness/resources/misc/georgian-morph-fst.tar.gz --output fst.tar.gz
tar -xvf fst.tar.gz
```


Then you can start the Georgian parser by evaluating

```
(ql:quickload :parse-cg3-foma-kat)
```

or (in case of FST)

```
(ql:quickload :parse-cg3-kat)
```

Third-party Common Lisp libraries are installed automatically (but you will have to install possibly missing Linux libraries).

When everything loads fine, try it out by evaluating the code at the bottom of parse-kat.lisp.




