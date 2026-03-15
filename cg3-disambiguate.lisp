;; ====================================================================
;; cg3-disambiguate.lisp — CG3 grammar loading and disambiguation
;; ====================================================================
;;
;; This file bridges the parse-cg3 system with the VISL CG-3 engine
;; (via the cl-vislcg3 FFI bindings).  It handles:
;;
;;   1. Grammar management — loading, splitting, and caching CG3
;;      grammars for different Georgian varieties (OG/MG/NG) and Abkhaz.
;;      The unified grammar files (geo-dis.cg3, geo-dep.cg3) are split
;;      into variety-specific files at load time because CG3 needs a
;;      single coherent grammar; the split is driven by variety prefixes
;;      (o-, m-, n-, on-, om-, etc.) at the start of rule lines.
;;
;;   2. Text disambiguation — cg3-disambiguate-text walks the token
;;      array sentence by sentence, builds CG3 cohorts from the
;;      morphological readings, runs the CG3 rules, and writes back
;;      the selected/discarded status and rule traces into the token
;;      plists.  It also extracts dependency parent pointers when the
;;      grammar includes SETPARENT/SETCHILD rules.
;;
;;   3. Chunk disambiguation — cg3-disambiguate-chunk and
;;      cg3-parse-sentence provide a simpler interface for running CG3
;;      on isolated token lists (used for NE classification and testing).
;;
;; Thread safety: all CG3 operations are serialized via
;; +disambiguate-lock+ because the CG3 C library is not reentrant.
;; ====================================================================

(in-package :vislcg3)

;; ====================================================================
;; Grammar parameters and loading
;; ====================================================================
;;
;; Each variety has its own compiled grammar pointer.  load-grammar
;; splits the unified grammar, validates it via the vislcg3 binary,
;; and loads it into the CG3 library.  Grammars are cached and only
;; reloaded when :force is true (or on first use).

(defparameter *og-grammar*
  nil #+ignore
  (print (vislcg3::cg3-grammar-load
	  (namestring (translate-logical-pathname
		       "projects:parse-cg3;cg3;geo-og-dis.cg3")))))

(defparameter *mg-grammar*
  nil #+ignore
  (print (vislcg3::cg3-grammar-load
	  (namestring (translate-logical-pathname
		       "projects:parse-cg3;cg3;geo-mg-dis.cg3")))))

(defparameter *ng-grammar*
  nil #+ignore
  (print (vislcg3::cg3-grammar-load
	  (namestring (translate-logical-pathname
		       "projects:parse-cg3;cg3;geo-ng-dis.cg3")))))

(defparameter *abk-grammar* nil)

(defparameter *non-grammar* nil)

(defparameter *grammar-path* 
  (translate-logical-pathname "projects:parse-cg3;cg3;"))

#+test
(print (grammar-initialization-error-message "geo-og-dis.cg3"))

(defun grammar-initialization-error-message (grammar &optional path)
  #+ccl
  (let* ((output
	  (substitute #\newline #\
		      (u:trim-whitespace
		       (with-output-to-string (stream)
			 (ccl::run-program "vislcg3"
					   (list "--grammar"
						 (u:concat (or path (namestring *grammar-path*))
							   grammar)
						 "--grammar-only")
					   :output stream)))))
	 (error-pos (search "Error" output)))
    (write-line output)
    (when error-pos (subseq output error-pos))))

#+test
(load-grammar :ng)

;; Splits a unified grammar (geo-dis.cg3 or geo-dep.cg3) into three
;; variety-specific files (og, mg, ng).  Rules prefixed with variety
;; codes (e.g. "o-SELECT", "mn-REMOVE") are included only in the
;; corresponding output files; unprefixed rules go into all three.
;; This allows maintaining a single source file while CG3 requires
;; a monolithic grammar.
(defun split-grammar-file (&key (type :dis)) ;; or :dep
  (let ((og-file (format nil "projects:parse-cg3;cg3;geo-og-~(~a~).cg3" type))
	(mg-file (format nil "projects:parse-cg3;cg3;geo-mg-~(~a~).cg3" type))
	(ng-file (format nil "projects:parse-cg3;cg3;geo-ng-~(~a~).cg3" type))
	(in-og t)
	(in-mg t)
	(in-ng t)
        (in-curly nil)
	(in-rule nil))
    (with-open-file (og-stream og-file :direction :output :if-exists :supersede)
      (with-open-file (mg-stream mg-file :direction :output :if-exists :supersede)
	(with-open-file (ng-stream ng-file :direction :output :if-exists :supersede)
	  (u:with-file-lines (line (format nil "projects:parse-cg3;cg3;geo-~(~a~).cg3" type))
            ;; (print (list (if in-curly :c) (if in-rule :r) (if in-og :og) (if in-ng :ng) line))
	    (cond (in-rule
		   (write-line (if in-og line "") og-stream)
		   (write-line (if in-mg line "") mg-stream)
		   (write-line (if in-ng line "") ng-stream)
                   (when (find #\{ line)
                     (setf in-curly t))
                   (when (find #\} line)
                     (setf in-curly nil))
		   (when (and (find #\; line :end (position #\# line))
                              (not in-curly))
		     (setf in-rule nil in-og t in-mg t in-ng t)))
		  ((and (not (eq 0 (position #\# (string-trim  " 	" line))))
			(or (search "o-" line :end2 (min 4 (length line)))
			    (search "m-" line :end2 (min 4 (length line)))
			    (search "n-" line :end2 (min 4 (length line)))))
		   (setf in-rule (or (not (find #\; line)) in-curly)
                         in-curly (find #\{ line)
                         in-og nil
			 in-mg nil
			 in-ng nil)
		   (let ((var-end (position #\- line :end (min 4 (length line)))))
		     (when (find #\o line :end var-end) (setf in-og t))
		     (when (find #\m line :end var-end) (setf in-mg t))
		     (when (find #\n line :end var-end) (setf in-ng t))
		     (setf line (subseq line (1+ var-end)))
		     (write-line (if in-og line "") og-stream)
		     (write-line (if in-mg line "") mg-stream)
		     (write-line (if in-ng line "") ng-stream)))
		  (t
                   (when (find #\{ line)
                     (setf in-curly t))
                   (when (find #\} line)
                     (setf in-curly nil))
		   (write-line line og-stream)
		   (write-line line mg-stream)
		   (write-line line ng-stream)))))))))

#+test
(split-grammar-file :type :dis)
#+test
(split-grammar-file :type :dep)

(defun load-grammar (variety &key (force t))
  (when (or force
	    (not (ecase variety
		   ((:og :xm :hm :oge) *og-grammar*)
		   (:mg *mg-grammar*)
		   ((:ng :jg)
		    *ng-grammar*)
		   (:abk
		    *abk-grammar*)
                   (:non
		    *non-grammar*))))
    (when (find variety '(:og :xm :hm :mg :ng :jg :oge))
      (split-grammar-file :type :dis)
      (split-grammar-file :type :dep))
    (let ((grammar-file
	   (ecase (debug variety)
	     ((:og :xm :hm :oge)
	      "geo-og-dis.cg3")
	     (:mg
	      "geo-mg-dis.cg3")
	     ((:ng :jg)
	      "geo-ng-dis.cg3")
	     (:abk
	      "abk-dis.cg3")
             (:non
	      "non-dis.cg3"))))
      (let ((error-message (grammar-initialization-error-message grammar-file)))
	(when error-message
	  (error error-message)))
      (ecase variety
	((:og :xm :hm :oge)
	 (when *og-grammar* (cg3-grammar-free *og-grammar*))
	 (setf *og-grammar*
	       (cg3-grammar-load (u:concat (namestring *grammar-path*) grammar-file))))
	(:mg
	 (when *mg-grammar* (cg3-grammar-free *mg-grammar*))
	 (setf *mg-grammar*
	       (cg3-grammar-load (u:concat (namestring *grammar-path*) grammar-file))))
	((:ng :jg)
	 (when *ng-grammar* (cg3-grammar-free *ng-grammar*))
	 (setf *ng-grammar*
	       (cg3-grammar-load (u:concat (namestring *grammar-path*) grammar-file))))
	(:abk
	 (when *abk-grammar* (cg3-grammar-free *abk-grammar*))
	 (setf *abk-grammar*
	       (cg3-grammar-load (u:concat (namestring *grammar-path*) grammar-file))))
        (:non
	 (when *non-grammar* (cg3-grammar-free *non-grammar*))
	 (setf *non-grammar*
	       (cg3-grammar-load (u:concat (namestring *grammar-path*) grammar-file))))))))

;; Maps CG3 internal rule-type integers to human-readable names for
;; rule trace display.  The integers come from the CG3 library's
;; internal enum.
(defun rule-name (ruletype &optional line discarded)
  (let ((name (ecase ruletype
		(15 "add")
		(16 "map")
		(17 "replace")
		(18 "select")
		(19 "remove")
		(20 "iff")
		(21 "append")
		(22 "substitute")
                (28 "remvariable")
                (29 "setvariable")
                (32 "setparent")
		(33 "setchild")
		(35 "setrelation")
		(50 "addcohort")
                (45 "remcohort")
                (47 "unmap")
                (48 "copy")
                (60 "protect")
                (64 "with"))))
    (if line
	(format nil "~:[~;; ~]~a:~d" discarded name line)
	name)))

(defun name-type-to-prop-tag (name-type)
  (ecase (intern (string-upcase name-type) :keyword)
    (:zoonym "Zoon")
    ((:ethnonym :people) "Ethn")
    ((:anthroponym :person) "Anthr")
    ((:toponym :place) "Top")
    (:hydronym "Top Hydr")
    (:other "Name")))

;; When a word falls inside a TEI <name type="…"> element, we enrich
;; its morphological readings with the corresponding proper-noun tag
;; (Anthr, Top, Ethn, etc.) and a <Name> marker.  This lets the CG3
;; grammar treat named-entity context as a feature for disambiguation.
(defun augment-morphology (word morphology &key name-type)
  (let* ((prop-tag (name-type-to-prop-tag name-type))
	 (punct-p
	  (find-if (lambda (reading)
		     (search "Punct " (cadr reading)))
		   morphology))
	 (name-readings-p
	  (find-if (lambda (reading)
		       (search prop-tag (cadr reading)))
		   morphology))
	 (augmentable-readings
	  (unless name-readings-p
	    (find-if (lambda (reading)
		       (or (eq 0 (search "N " (cadr reading)))
			   (eq 0 (search "A " (cadr reading)))))
		     morphology))))
    #+debug
    (when (null morphology)
      (print (list :word word :prop prop-tag)))
    (cond ((null morphology)
	   (list (list "[??]" (format nil "N Prop ~a <Name>" prop-tag) nil nil)))
	  (punct-p
	   morphology)
	  (name-readings-p
	   (dolist (reading morphology)
	     (when (and (search prop-tag (cadr reading))
			(not (search "<Name>" (cadr reading))))
	       (setf (cadr reading) (u:concat (cadr reading) " <Name>"))))
	   morphology)
	  (augmentable-readings
	   (u:collecting
	     (dolist (reading morphology)
	       (when (or (eq 0 (search "N " (cadr reading)))
			 (eq 0 (search "A " (cadr reading))))
		 (u:collect (list (car reading)
				  (u:concat "N Prop "
					    prop-tag
					    (subseq (cadr reading)
						    (if (search "N Prop " (cadr reading))
							6
							1))
					     " <Name>")
				  nil nil)))
	       (u:collect reading))))
	  (t
	   (cons (list "[??]" (format nil "N Prop ~a <Name>" prop-tag) nil nil)
		 morphology)))))

#+test
(:TMESIS-MSA
 (("ხოლო" ("ხოლო" "Cj" NIL NIL) ("ხოლ-ი" "N Voc Sg NonStand" NIL NIL) ("ხოლ-ი" "N Abs Sg NonStand Encl:IndSp3" NIL NIL))
  ("თუ" ("თუ" "Cj Sub" NIL NIL))
  ("ვითარ" ("ჳთარ-ი" "Pron Int Abs Sg" NIL NIL) ("ჳთარ" "Adv Rel" NIL NIL) ("ჳთარ" "Adv Int" NIL NIL))
  ("განიკურნოს" ("გან-კურნებ[ა]-ჲ/კურნ" "V Pass Conj-II Pv <S> <S:Nom> S:3Sg" NIL NIL) ("გან-კურნებ[ა]-ჲ/კურნ" "V Act Conj-II Pv SV <S-DO> <S:Erg> <DO:Nom> S:3Sg DO:3Sg" NIL NIL))))

(defparameter +disambiguate-lock+ (make-process-lock :name "disambiguate-lock"))

(defun lemma-unique-frequency (l f &rest rest)
  (declare (ignore l f rest))
  nil)

(defparameter *sentence-end-strings* '("." "?" "!" "…" ";" ":"))

(defparameter *text* nil)

#+test
(cg3-disambiguate-text *text* :variety :ng :load-grammar nil :variables '("wiki"))

(defmethod parse::is-quote-verb ((text parse::parsed-text) token)
  (declare (ignore token))
  nil)

;; ====================================================================
;; cg3-disambiguate-text — main disambiguation entry point
;; ====================================================================
;;
;; Walks the token array, grouping tokens into sentences (delimited by
;; sentence-ending punctuation or structural XML elements like </p>).
;; For each sentence:
;;   1. Build CG3 cohorts from the morphological readings
;;   2. Run the CG3 grammar rules
;;   3. Read back the selected/discarded readings and rule traces
;;   4. Optionally read back dependency parent pointers
;;
;; Sentence segmentation has special handling for right punctuation
;; ("»", """, etc.), quotation dashes followed by quote verbs, and
;; an optional uppercase-start heuristic (used for Abkhaz).
;; The 100-word safety limit prevents runaway sentences from blocking
;; the CG3 engine.

;; TODO: check if grammar has changed
(defmethod cg3-disambiguate-text ((text parse::parsed-text)
				  &key (variety :og) (tracep t) (load-grammar t) mode remove-brackets
				    (sentence-end-strings *sentence-end-strings*)
                                    ;; if true sentence is not ended if lowercase follows
                                    variables
                                    dependencies
                                    sentence-start-is-uppercase
                                    guess-scope guess-table)
  (setf *text* text) ;; debug
  ;; (debug sentence-end-strings)
  (unless guess-scope (setf guess-table nil))
  (with-process-lock (+disambiguate-lock+)
    (load-grammar variety :force load-grammar)
    (let* ((grammar (ecase variety
		      ((:og :xm :hm :oge) *og-grammar*)
		      (:mg *mg-grammar*)
		      ((:ng :jg) *ng-grammar*)
		      (:abk *abk-grammar*)
                      (:non *non-grammar*)))
           (language (case variety
                       (:abk :abk)
                       (:non :non)
                       (otherwise :kat)))
	   (applicator (cg3-applicator-create grammar))
	   (sentence nil)
	   (word-list ()) ;; for debugging
	   (prev-pos 0)
	   (pos 0)
	   (token-array (parse::text-array text))
	   (name-type nil))
      (cg3-applicator-setflags applicator (if tracep (ash 1 7) 0))
      (unwind-protect
	   (loop with mwe-length = 0
	      while (< prev-pos (length token-array))
	      do
	      (unwind-protect
		   (progn
		     (setf sentence (cg3-sentence-new applicator))

                     (when variables
                       (let ((cohort (cg3-cohort-create sentence))
			     (tag (cg3-tag-create-u8
				   applicator
                                   (concat "_VAR_"))))
		         (cg3-cohort-setwordform cohort tag)
                         (let ((reading (cg3-reading-create cohort)))
                           (dolist (var variables)
			     (let ((tag (cg3-tag-create-u8 applicator (format nil "\"~a\"" var))))
			       (cg3-reading-addtag reading tag)))
			   (cg3-cohort-addreading cohort reading))
                         (cg3-sentence-addcohort sentence cohort)))
                     
                     (setf word-list ())
		     (loop with word-seen = nil ;; and w = nil
			and end-punct-found = nil
			for i from prev-pos below (length token-array)
			for token = (aref token-array i)
			do (setf pos i)
			(case (car token)
			  (:start-element
			   (when (eq (cadr token) :|name|) ;; extract name type
			     (setf name-type (getf token :|type|))))
			  (:end-element
			   (when (eq (cadr token) :|name|)
			     (setf name-type nil)))
			  (:word
			   (cond ((> mwe-length 1)
				  (decf mwe-length))
				 (t
				  (destructuring-bind (&key word dipl facs norm morphology tmesis-msa mwe
							    &allow-other-keys)
                                      token
                                    (push word word-list)
				    (when mwe (setf mwe-length mwe))
				    (let ((wordform
                                           (cond ((and word remove-brackets)
                                                  (delete-if (lambda (c) (find c "{}[]|/\\‹›")) word))
                                                 (word
                                                  word)
                                                 (t
					          (let ((str ""))
						    (dolist (elt (or dipl facs norm))
						      (when (eq (car elt) :characters)
						        (setf str (u:concat str (cadr elt)))))
						    (delete-if (lambda (c) (find c "{}[]|/\\‹›")) str))))))
                                      ;; (debug wordform)
				      (setf word-seen (if mwe nil wordform))
				      (cond (tmesis-msa ;; each tmesis element is a separate cohort
					     (loop for (segment . msa) in tmesis-msa
						do (let ((cohort (cg3-cohort-create sentence))
						         (tag (cg3-tag-create-u8 applicator (format nil "\"<~a>\"" segment))))
						  (cg3-cohort-setwordform cohort tag)
						  (loop for l.f in msa
						     for i from 0
						     do (let ((reading (cg3-reading-create cohort))
							      (tag (cg3-tag-create-u8 applicator (format nil "\"~a\"" (car l.f)))))
							  (cg3-reading-addtag reading tag)
							  (cg3-reading-addtag
                                                           reading (cg3-tag-create-u8 applicator (format nil "[~d]" i)))
							  (dolist (tag (u:split (cadr l.f) #\space nil nil t))
							    (let ((tag (cg3-tag-create-u8 applicator tag)))
							      (cg3-reading-addtag reading tag)))
							  (cg3-cohort-addreading cohort reading)))
						  (cg3-sentence-addcohort sentence cohort))))
					    (t
					     (when (null morphology)
					       (setf morphology
						     (case variety
						       (:abk (list (list "??" "Unrecognized" nil nil)))
						       (:non (list (list "??" "unknown" nil nil)))
						       (otherwise
							(list (list "??" "Unrecognized" nil nil)))))
					       (if (find :morphology token)
						   (setf (getf token :morphology) morphology)
						   (setf (cddr token)
							 (append (cddr token) (list :morphology morphology)))))
					     (when (and name-type (not (eq variety :abk)))
					       (setf morphology (augment-morphology wordform morphology
										    :name-type name-type))
					       (setf (getf token :morphology) morphology))
					     (let ((cohort (cg3-cohort-create sentence))
						   (tag (cg3-tag-create-u8
							 applicator
                                                         (concat "\"<" wordform ">\""))))
                                               ;;(debug wordform)
					       (cg3-cohort-setwordform cohort tag)
					       (loop for l.f in morphology
						  for i from 0
						  do (let ((reading (cg3-reading-create cohort))
                                                           ;; lemma
							   (tag (cg3-tag-create-u8 applicator (format nil "\"~a\"" (car l.f)))))
						       (cg3-reading-addtag reading tag)
						       (cg3-reading-addtag
							reading (cg3-tag-create-u8 applicator (format nil "[~d]" i)))
						       (let ((uf (apply #'lemma-unique-frequency l.f)))
                                                         (when uf
                                                           (cg3-reading-addtag
                                                            reading (cg3-tag-create-u8
                                                                     applicator
                                                                     (format nil "<lemma:~d>" uf)))))
                                                       ;; features
                                                       (let ((tags (u:split (cadr l.f) #\space nil nil t)))
                                                         (when (eq mode :redisambiguate)
                                                           ;; remove syntactic and subtoken tags if redisambiguating
                                                           (setf tags (delete-if 
                                                                       (lambda (tag)
                                                                         (or (char= (char tag 0) #\>)
                                                                             (char= (char tag 0) #\$)))
                                                                       tags)
                                                                 (cadr l.f) (format nil "~{~a~^ ~}" tags)))
                                                         (dolist (tag tags)
                                                           (let ((tag (cg3-tag-create-u8 applicator tag)))
                                                             (cg3-reading-addtag reading tag))))
						       (cg3-cohort-addreading cohort reading)))
					       (cg3-sentence-addcohort sentence cohort))))))))))
			;; sentence end condition
			until (or end-punct-found
				  (and word-seen
				       (or (and (> (- pos prev-pos) 100)
						(progn (format t "100 word limit reached!~%Sentence: ~{~a ~}~%"
							       (loop for i from prev-pos to pos
								  for token = (aref token-array i)
								  when (eq (car token) :word)
								  collect (cadr token)))
						       t))
					   (and (eq (car token) :end-element)
						(find (cadr token)
                                                      '(:|p| :|div| :|head| :|lg| :|li| :|title|
                                                        "p" "div" "head" "lg" "li" "title")
                                                      :test #'equal))
					   (and (eq (car token) :word)
						(find word-seen sentence-end-strings :test #'equal)
						;; look one ahead to see if there is more right punctuation
						(let ((next-token
						       (when (< (1+ i) (length token-array))
							 (aref token-array (1+ i))))
                                                      (next-is-lowercase
                                                       (and sentence-start-is-uppercase
                                                            (loop for id from (1+ i) below (length token-array)
                                                                  for (type word) = (aref token-array id)
                                                                  when (and (eq type :word)
                                                                         (not (find word '("”" "»" "–" ")" "]" "!" "?")
                                                                                    :test #'string=)))
                                                                  return (lower-case-p (char word 0)))))
                                                      ;; no sentence end if dash followed by quote verb
                                                      (next-is-quotation-dash
                                                       (loop for id from (1+ i) below (length token-array)
                                                             for (type word) = (aref token-array id)
                                                             when (eq type :word)
                                                             return (and (equal word "–")
                                                                         (parse::is-quote-verb
                                                                          text (aref token-array (1+ id)))))))
                                                  (unless (or next-is-lowercase next-is-quotation-dash)
                                                    (setf end-punct-found t)
                                                    (or (null next-token)
                                                        (not (eq (car next-token) :word))
                                                        (not (find (getf next-token :word)
                                                                    ;; right punctuation
                                                                   '("”" "»" ")" "]" "?" "!")
                                                                   :test #'string=))))))))))
		     #+debug(format t "~&~{~a ~}~%" (nreverse word-list))
		     (cg3-sentence-runrules applicator sentence)
                     (loop with coh = 0 and mwe-count = 0 and added = nil and added-count = 0
			for i from prev-pos to pos ;; the sentence range
			for token = (aref token-array i)
			while (< coh (cg3-sentence-numcohorts sentence))
			when (eq (car token) :word)
			do (cond ((> mwe-count 1)
				  (decf mwe-count))
				 (added
                                  (if (zerop (decf added-count))
				      (setf added nil)
                                      (decf i))
				  (let* ((cohort (cg3-sentence-getcohort sentence coh))
					 (tag (cg3-cohort-getwordform cohort))
					 (word (cg3-tag-gettext-u8 tag))
                                         #+ccl(self (ccl::%new-gcable-ptr 4 t))
					 #+ccl(parent (ccl::%new-gcable-ptr 4 t))
					 ;; there is only one
					 (reading (cg3-cohort-getreading cohort 0))
					 (word (cg3-tag-gettext-u8
						(cg3-reading-gettag reading 0)))
					 (word (subseq word 2 (- (length word) 2)))
                                         (wid (getf token :wid))
                                         )
                                    ;;(debug wid)
				    #+ccl(cg3-cohort-getdependency cohort self parent)
                                    (setf (getf (cddr token) :subtokens)
                                          (append (getf (cddr token) :subtokens)
                                                  (list
                                                   (list :word word
						         :parent (ccl:%get-signed-long parent)
						         :self (ccl:%get-signed-long self)
						         :morphology
						         (list
						          (let* ((traces (cg3-reading-numtraces reading))
							         (lemma (cg3-tag-gettext-u8 (cg3-reading-gettag reading 1)))
							         (tags
							          (format nil "~{~a~^ ~}"
								          (loop for i from 3 below (cg3-reading-numtags reading)
								                for tag = (cg3-tag-gettext-u8 (cg3-reading-gettag reading i))
								                collect tag))))
						            (list (subseq lemma 1 (1- (length lemma)))
							          tags
							          nil
							          (u:collecting
							            (dotimes (tr traces)
							              (let ((trace (cg3-reading-gettrace reading tr))
								            (ruletype (cg3-reading-gettrace-ruletype
                                                                                       reading tr)))
							                (u:collect (rule-name ruletype trace)))))
							          nil)))
                                                         :wid (when wid
                                                                (format nil "w~a"
                                                                        (+ (parse-integer wid :start 1)
                                                                           (1+ (length (getf (cddr token) :subtokens))))))
                                                         ;; we assume that stored-parent
                                                         ;; always coincide with parent
                                                         ;; :stored-parent (ccl:%get-signed-long parent)
						         :stored-parent nil;; #+ccl(ccl:%get-signed-long parent)
                                                         :stored-label nil
                                                         :parent-token token)))))
				  (incf coh))
				 (t
                                  (setf mwe-count (getf token :mwe 0)) 
				  (let* ((cohort (cg3-sentence-getcohort sentence coh))
					 (tag (cg3-cohort-getwordform cohort))
					 (word (cg3-tag-gettext-u8 tag)))
                                    (when (string= word ">>>")
				      (incf coh)
				      (setf cohort (cg3-sentence-getcohort sentence coh)
					    tag (cg3-cohort-getwordform cohort)
					    word (cg3-tag-gettext-u8 tag)))
                                    (when (string= word "_VAR_")
				      (incf coh)
				      (setf cohort (cg3-sentence-getcohort sentence coh)
					    tag (cg3-cohort-getwordform cohort)
					    word (cg3-tag-gettext-u8 tag)))
                                    ;; lemma + features
				    (destructuring-bind (&key morphology tmesis-msa &allow-other-keys) token
				      (cond (tmesis-msa
					     (loop for ((segment . msa) . rest) on tmesis-msa
						   do (msa-set-disambiguation word cohort
                                                                              msa language guess-table)
						   when rest
						   do (incf coh)
						   (setf cohort (cg3-sentence-getcohort sentence coh)
						         tag (cg3-cohort-getwordform cohort)
						         word (cg3-tag-gettext-u8 tag))))
					    (t
                                             (multiple-value-setq (added added-count)
					       (msa-set-disambiguation
                                                word cohort morphology language guess-table)))))
				    ;; dependencies
				    #+ccl ;; *text*
                                    (when dependencies
                                      (let ((self (ccl::%new-gcable-ptr 4 t))
					    (parent (ccl::%new-gcable-ptr 4 t)))
                                        (cg3-cohort-getdependency cohort self parent)
				        (setf (getf (cddr token) :self)
					      (ccl:%get-signed-long self)
					      (getf (cddr token) :parent)
					      (ccl:%get-signed-long parent))))
				    (incf coh)
                                    (when added ;; same token again, take care of added cohort
				      (decf i)))))))
		(cg3-sentence-free sentence))
	      (setf prev-pos (1+ pos)))
	(cg3-applicator-free applicator))
      ;; adjust dependency ids for addcohort
      (let ((ids (collecting
                   (loop for token across token-array
		         for id = (getf token :self)
		         for subtokens = (getf token :subtokens)
		         when id
		         do (collect id)
		         (dolist (subtoken subtokens)
                           (collect (getf subtoken :self)))))))
	(loop for token across token-array
	      for self = (getf token :self)
	      for parent = (getf token :parent)
	      for subtokens = (getf token :subtokens)
	      when self
	      ;; or … -1 can only happen if token has been deleted
	      do (setf (getf token :self) (1+ (or (position self ids) -1)))
              when parent
	      do (setf (getf token :parent) (1+ (or (position parent ids) -2)))
	      (dolist (subtoken subtokens)
	        (setf (getf subtoken :self) (1+ (or (position (getf subtoken :self) ids) -1)))
	        (when (getf subtoken :parent)
	          (setf (getf subtoken :parent) (1+ (or (position (getf subtoken :parent) ids) -2)))))))
      text)))

;; ====================================================================
;; msa-set-disambiguation — map CG3 output back to reading plists
;; ====================================================================
;;
;; After CG3 has run, each cohort's readings are either selected or
;; deleted.  This function matches them back to the original morphology
;; list (by reading-id encoded as a [N] tag), records the rule traces,
;; and handles appended readings (from APPEND/ADDCOHORT rules).
;; For Georgian (:kat), it also updates feature strings from the CG3
;; output, since MAP/SUBSTITUTE rules may have modified them.
;; Returns (values added-p added-count) when ADDCOHORT created new
;; subtokens (e.g. splitting an enclitic copula).

(defun msa-set-disambiguation (word cohort morphology language guess-table)
  (let* ((added nil)
         (added-count 0)
	 (reading-count (length morphology))
         (set-tags-from-cg-output (eq language :kat))
         (numreadings (cg3-cohort-numreadings cohort))
	 (selected
	  (u:collecting
	    (dotimes (re numreadings)
	      (let* ((reading (cg3-cohort-getreading cohort re))
		     (rid (cg3-reading-gettag reading 2))
		     (reading-id (cg3-tag-gettext-u8 rid))
		     (appended-p (char/= (char reading-id 0) #\[)) ;; appended reading
		     ;; the reading id is coded as an extra tag, e.g. [0], [1] etc.
		     (reading-id (if appended-p
				     reading-count
				     (parse-integer reading-id
						    :start 1
						    :end (1- (length reading-id)))))
		     (traces (cg3-reading-numtraces reading))
		     (appended-tags (when appended-p
				      (loop for i from 1 below (cg3-reading-numtags reading)
					 collect (cg3-tag-gettext-u8 (cg3-reading-gettag reading i)))))
		     ;; alternative: MWE tags (e.g. "2:Adv") are added to an existing reading 
		     #+disabled
		     (mwe-tags (loop for i from 2 below (cg3-reading-numtags reading)
				  for tag = (cg3-tag-gettext-u8 (cg3-reading-gettag reading i))
				  when (and (> (length tag) 1)
					    (find (char tag 0) "23456789")
					    (char= (char tag 1) #\:))
				  collect tag))
		     (dependency-labels
		      (loop for i from 1 below (cg3-reading-numtags reading)
			    for tag = (cg3-tag-gettext-u8 (cg3-reading-gettag reading i))
			    when (find (char tag 0) ">@")
			    collect tag
			    when (or (equal tag "$ADDED")
                                     (equal tag "$ADDED_PP")
                                     (equal tag "$ADDED_Ind"))
			    do (setf added t)
                            ;; incf only for first reading!
                            (when (zerop re) (incf added-count)))))
                (if set-tags-from-cg-output
                    (let ((rlist (nth reading-id morphology))
                          (guessed nil))
                      (when rlist ;; NIL should not happen!
		        (setf (cadr rlist)
			      (format nil "~{~a~^ ~}"
                                      (loop for i from 4 ;; 3 ;; 4
                                            below (cg3-reading-numtags reading)
                                            for tag = (cg3-tag-gettext-u8 (cg3-reading-gettag reading i)) 
		                            collect tag
                                            when (and guess-table (string= tag "Guess"))
                                            do (setf guessed t)))))
                      (when (and guess-table guessed)
                        
                        (let* ((word (subseq word 2 (- (length word) 2)))
                               (lemma (cg3-tag-gettext-u8 (cg3-reading-gettag reading 1)))
                               (lemma (subseq lemma 1 (1- (length lemma))))
                               (pos (cg3-tag-gettext-u8 (cg3-reading-gettag reading 4)))
                               (amb-table (car guess-table))
                               (unamb-table (cdr guess-table))
                               )
                          (unless (gethash lemma amb-table)
                            (setf (gethash lemma amb-table) (dat:make-string-tree)))
                          (incf (dat:string-tree-get
                                 (gethash lemma amb-table)
                                 (u:concat word "+" pos)
                                 0))
                          (when (= numreadings 1)
                            (unless (gethash lemma unamb-table)
                              (setf (gethash lemma unamb-table) (dat:make-string-tree)))
                            (incf (dat:string-tree-get
                                   (gethash lemma unamb-table) 
                                   (format nil "~a	~{~a~^ ~}"
                                           word
                                           ;; numreadings
                                           (loop for i from 4
                                                 below (cg3-reading-numtags reading)
                                                 for tag = (cg3-tag-gettext-u8 (cg3-reading-gettag reading i))
                                                 until (eq (char tag 0) #\>)
		                                 collect tag))
                                   0))))))
		    (when dependency-labels
		      (let ((rlist (nth reading-id morphology)))
		        (when rlist
		          (setf (cadr rlist)
			        (format nil "~a~{ ~a~}" (cadr rlist) dependency-labels))))))
		(when appended-p
		  (setf (cdr morphology)
			(append (cdr morphology)
				(list (list (subseq (car appended-tags) 1 (1- (length (car appended-tags))))
					    (format nil "~{~a~^ ~}" (cdr appended-tags))
					    nil nil))))
		  (incf reading-count))
		(u:collect
		    (list reading-id
			  (u:collecting
			    (dotimes (tr traces)
			      (let ((trace (cg3-reading-gettrace reading tr))
				    (ruletype (cg3-reading-gettrace-ruletype reading tr)))
				(u:collect (rule-name ruletype trace)))))
			  nil ;; mwe-tags
			  ))))))
	 (discarded
	  (u:collecting
	    (dotimes (re (cg3-cohort-numdelreadings cohort))
	      (let* ((reading (cg3-cohort-getdelreading cohort re))
		     (rid (cg3-reading-gettag reading 2))
		     (reading-id (cg3-tag-gettext-u8 rid))
		     (reading-id (parse-integer reading-id
						:start 1
						:end (1- (length reading-id))))
		     (traces (cg3-reading-numtraces reading)))
		(u:collect
		    (cons reading-id
			  (u:collecting
			    (dotimes (tr traces)
			      (let ((trace (cg3-reading-gettrace reading tr))
				    (ruletype (cg3-reading-gettrace-ruletype reading tr)))
				(u:collect (rule-name ruletype trace t))))))))))))
    (loop for reading in morphology
       for rid from 0
       for sel = (find rid selected :key #'car)
       for disc = (unless sel (find rid discarded :key #'car))
       do (cond (sel
		 ;;(debug reading)
		 (setf (caddr reading) :selected-cg
		       (cadddr reading) (cadr sel))
		 ;; add MWE readings
		 (when (caddr sel)
		   (setf (cadr reading)
			 (format nil "~a~{ ~a~}" (cadr reading) (caddr sel)))))
		(t
		 (setf (caddr reading) :discarded-cg
		       (cadddr reading) (cdr disc)))))
    (values added added-count)))

;; ====================================================================
;; Chunk disambiguation — for isolated token lists
;; ====================================================================
;;
;; cg3-disambiguate-chunk and cg3-parse-sentence provide a simpler
;; interface for running CG3 on a list of (word lemmas features) tuples
;; rather than a full parsed-text.  Used for NE classification (guessing
;; POS and proper-noun type for unknown words) and testing.


#+test
(cg3-disambiguate-chunk
 '(("შეხვედრა" "¦შეხვედრ[ა]¦" "¦ N Nom Sg ¦")
   ("ჰოლანდიელთა" "¦ჰოლანდიელ-ი¦" "¦ N Gen Pl OldPl ¦ N Dat Pl OldPl ¦")
   ("წინააღმდეგ" "¦წინააღმდეგ¦" "¦ Pp <Gen> ¦ Adv ¦")
   ("," "¦,¦" "¦ Punct Comma ¦")
   ("შესაძლოა" "¦შე-საძლო/ძლ¦" "¦ A Part FutPart Pv Nom Sg Encl:Aux ¦")
   ("ქალაქ" "¦ქალაქ-ი¦" "¦ N Qual Dat Att ¦")
   ("ტირასპოლში" "¦ტირასპოლ-ი¦" "¦ Target ¦")
   ("ბატონი" "¦ბატონ-ი¦" "¦ N Qual ¦")
   ("ბაიევსკი" "¦ბაიევსკი¦" "¦ Target ¦")
   ("," "¦,¦" "¦ Punct Comma ¦")
   ("„" "¦„¦" "¦ Punct Quote ¦")
   ("შერიფის" "¦შერიფ¦" "¦ N Prop Anthr FirstName Gen ¦")
   ("”" "¦”¦" "¦ Punct Quote ¦")
   ("ახლადაშენებულ" "" ""))
 :grammar-file "ng-guess-pos-ne.cg3")

#+test
(grammar-initialization-error-message "ng-guess-pos-ne.cg3")

;; Convenience macro: loads a grammar, creates an applicator, runs body,
;; and ensures cleanup.  Used by code that needs a one-shot grammar
;; (e.g. NE classification with a specialized grammar file).
(defmacro with-cg3-grammar ((APPLICATOR &key grammar-file (tracep nil))
			    &body body)
  (with-gensyms (error-message grammar)
    `(with-process-lock (+disambiguate-lock+)
       (let ((,error-message (grammar-initialization-error-message ,grammar-file)))
	 (when ,error-message
	   (error ,error-message)))
       (let* ((,grammar (cg3-grammar-load (u:concat (namestring *grammar-path*) ,grammar-file)))
	      (,APPLICATOR (cg3-applicator-create ,grammar)))
	 (cg3-applicator-setflags ,APPLICATOR (if ,tracep (ash 1 7) 0))
	 (unwind-protect
	      (progn ,@body)
	   (cg3-applicator-free ,APPLICATOR))))))

(defun cg3-parse-sentence (applicator token-list FUN)
  (let ((sentence (cg3-sentence-new applicator)))
    (unwind-protect
	 (progn
	   (loop
	      for token in token-list
	      do
	      (destructuring-bind (word lemmas features) token
		(let ((lemmas (u:split lemmas #\¦ nil nil t))
		      (features (u:split features #\¦ nil nil t)))
		  (let ((cohort (cg3-cohort-create sentence))
			(tag (cg3-tag-create-u8 applicator (format nil "\"<~a>\"" word))))
		    (cg3-cohort-setwordform cohort tag)
		    (loop for l in lemmas for f in features
		       for i from 0
		       do (let ((reading (cg3-reading-create cohort))
				(tag (cg3-tag-create-u8 applicator (format nil "\"~a\"" l))))
			    (cg3-reading-addtag reading tag)
			    (dolist (tag (u:split f #\space nil nil t))
			      (let ((tag (cg3-tag-create-u8 applicator tag)))
				(cg3-reading-addtag reading tag)))
			    (cg3-cohort-addreading cohort reading)))
		    (cg3-sentence-addcohort sentence cohort)))))
	     
	   (cg3-sentence-runrules applicator sentence)
	   
	   (loop for coh below (cg3-sentence-numcohorts sentence)
	      do (let* ((cohort (cg3-sentence-getcohort sentence coh))
			(tag (cg3-cohort-getwordform cohort))
			(word (cg3-tag-gettext-u8 tag)))
		   (when (and (string/= word ">>>") (string/= word "_VAR_"))
		     (setf cohort (cg3-sentence-getcohort sentence coh)
			   tag (cg3-cohort-getwordform cohort)
			   word (cg3-tag-gettext-u8 tag))
		     (funcall
		      FUN
		      word
		      (loop for re below (cg3-cohort-numreadings cohort)
			 for reading = (cg3-cohort-getreading cohort re)
			 for numtags = (cg3-reading-numtags reading)
			 collect (loop for i from 1 below numtags
				    collect (cg3-tag-gettext-u8 (cg3-reading-gettag reading i)))))))))
      (cg3-sentence-free sentence))))

;; test
(defmethod cg3-disambiguate-chunk ((chunk list)
				  &key grammar-file (tracep t) (load-grammar t))
  (with-process-lock (+disambiguate-lock+)
    (when load-grammar
      (let ((error-message (grammar-initialization-error-message grammar-file)))
	(when error-message
	  (error error-message))))
    (let* ((grammar (cg3-grammar-load (u:concat (namestring *grammar-path*) grammar-file)))
	   (applicator (cg3-applicator-create grammar))
	   (sentence nil)
	   (token-list chunk))
      (cg3-applicator-setflags applicator (if tracep (ash 1 7) 0))
      (unwind-protect
	   (progn
	     (setf sentence (cg3-sentence-new applicator))
	     (loop
		for token in chunk
		do
		(destructuring-bind (word lemmas features) token
		  (let ((lemmas (u:split lemmas #\¦ nil nil t))
			(features (u:split features #\¦ nil nil t)))
		    (let ((cohort (cg3-cohort-create sentence))
			  (tag (cg3-tag-create-u8 applicator (format nil "\"<~a>\"" word))))
		      (cg3-cohort-setwordform cohort tag)
		      (loop for l in lemmas for f in features
			 for i from 0
			 do (let ((reading (cg3-reading-create cohort))
				  (tag (cg3-tag-create-u8 applicator (format nil "\"~a\"" l))))
			      (cg3-reading-addtag reading tag)
			      (dolist (tag (u:split f #\space nil nil t))
				(let ((tag (cg3-tag-create-u8 applicator tag)))
				  (cg3-reading-addtag reading tag)))
			      (cg3-cohort-addreading cohort reading)))
		      (cg3-sentence-addcohort sentence cohort)))))
	     
	     (cg3-sentence-runrules applicator sentence)
		     
	     (loop with coh = 0
		while (< coh (cg3-sentence-numcohorts sentence))
		do (let* ((cohort (cg3-sentence-getcohort sentence coh))
			  (tag (cg3-cohort-getwordform cohort))
			  (word (cg3-tag-gettext-u8 tag)))
		     (when (string= word ">>>")
		       (incf coh)
		       (setf cohort (cg3-sentence-getcohort sentence coh)
			     tag (cg3-cohort-getwordform cohort)
			     word (cg3-tag-gettext-u8 tag)))
		     ;; lemma + features
		     ;;(write-line word)
		     (dotimes (re (cg3-cohort-numreadings cohort))
		       (let* ((reading (cg3-cohort-getreading cohort re))
			      (numtags (cg3-reading-numtags reading)))
			 (format t "     ~{~a~^ ~}~%"
				 (loop for i from 1 below numtags
				    collect (cg3-tag-gettext-u8 (cg3-reading-gettag reading i))))))
		     (incf coh))))
	(cg3-sentence-free sentence)
	(cg3-applicator-free applicator)))))


:eof
