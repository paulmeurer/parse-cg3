;;;  -*- Mode: Emacs-Lisp -*-

(global-font-lock-mode t)

(normal-erase-is-backspace-mode 1)

(setq-default indent-tabs-mode nil)

(setq default-process-coding-system '(utf-8 . utf-8))

;; key bindings for terminal mode
(define-key input-decode-map "[1;5A" [(control up)])
(define-key input-decode-map "[1;5B" [(control down)])
(define-key input-decode-map "[1;5C" [(control right)])
(define-key input-decode-map "[1;5D" [(control left)])
(define-key input-decode-map "[1;5H" [(control home)])
(define-key input-decode-map "[1;5F" [(control end)])

(setq default-enable-multibyte-characters t)

(global-set-key [C-right]  'forward-sexp)
(global-set-key [C-left]   'backward-sexp)
(global-set-key [S-right]   'mark-sexp)

;; install slime in /usr/local/slime, then this will work:
(add-to-list 'load-path "/usr/local/slime")
(require 'slime)

;; Important: modify this code for your settings:

;; the location of the Clozure Common Lisp code:
;; /usr/local/ccl

;; the dot.ccl-init.lisp file:
;; /home/paul/dot.ccl-init.lisp

;; use: ‘M-x slime’ to start lisp
(setq slime-lisp-implementations
      '((clozure ("/usr/local/ccl/lx86cl64" "--load" "/home/paul/dot.ccl-init.lisp"))
	))

(slime-setup '(slime-repl))

:eof
