;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: PARSE; Base: 10 -*-

(in-package :parse)

(defmethod init-transducers ((language (eql :kat))
                             &key ng-only
                               ;; symlinked to georgian-morph/regex
                               (transducer-dir "projects:gnc;morphology;fst;")
                               &allow-other-keys)
  (let* ((ng-file (u:concat transducer-dir "georgian-morph-ng" ".fst"))
	 (og-file (u:concat transducer-dir "georgian-morph-og" ".fst"))
	 (xm-file (u:concat transducer-dir "georgian-morph-xanmeti" ".fst"))
	 (hm-file (u:concat transducer-dir "georgian-morph-haemeti" ".fst"))
	 (ng-net nil)
	 (comp-ng-net nil)
	 (og-net nil)
	 (xm-net nil)
	 (hm-net nil)
	 )
    (format t "~&Loading transducers for ~a…~%" language)
    (labels ((load-morph (file-list)
	       (u:collecting
		 (loop for name in file-list
		    do ;; (print name)
		      (format t "~&loading: ~s~%" name) 
		      (case name
			(:georgian-morph-ng
			 (u:collect (or ng-net
					(setf ng-net (make-instance 'cl-fst:fst-net
								    :file ng-file :name :ng)))))
			(:georgian-comp-ng
			 (u:collect (or comp-ng-net
					(setf comp-ng-net
					      (make-instance 'cl-fst:fst-net
							     :file og-file :name :comp-ng)))))
			(:georgian-morph-og
			 (u:collect (or og-net
					(setf og-net (make-instance 'cl-fst:fst-net
								    :file og-file :name :og)))))
			(:georgian-morph-xanmeti
			 (u:collect (or xm-net
					(setf xm-net (make-instance 'cl-fst:fst-net
								    :file xm-file :name :xm)))))
			(:georgian-morph-haemeti
			 (u:collect (or hm-net
					(setf hm-net (make-instance 'cl-fst:fst-net
								    :file hm-file :name :hm)))))
                        (otherwise
			 (let ((file (u:concat transducer-dir name ".fst")))
			   (format t "~&loading: ~s~%" file) 
			   (if (probe-file file)
			       (u:collect
				   (make-instance
				    'cl-fst:fst-net
				    :file file
				    :name (intern (string-upcase name) :keyword)))
			       (warn "File not found: ~a" file)))))))))
      (setf *ng-tokenizer*
	    (make-instance 'cl-fst:fst-tokenizer :token-boundary #.(string #\newline)
			   :file (u:concat transducer-dir "georgian-tokenize-ng.fst")))
      (setf *tokenizer*
	    (make-instance 'cl-fst:fst-tokenizer :token-boundary #.(string #\newline)
			   :file (u:concat transducer-dir "geo-tokenize.fst")))
      (setf *og-analyzer*
            (load-morph '(:georgian-morph-og
                          :georgian-morph-ng
                          :georgian-comp-ng
                          ;;"georgian-noun-guessed"
                          )))
      (unless ng-only
        (setf *xm-analyzer*
              (load-morph '(:georgian-morph-xanmeti
                            :georgian-morph-haemeti
                            :georgian-morph-og
                            ;;:georgian-morph-ng
                            ;;:georgian-comp-ng
                            ;;"georgian-noun-guessed"
                            )))
        (setf *hm-analyzer*
              (load-morph '(:georgian-morph-haemeti
                            :georgian-morph-xanmeti
                            :georgian-morph-og
                            ;;:georgian-morph-ng
                            ;;"georgian-comp-ng"
                            ;;"georgian-noun-guessed"
                            )))
        #-lokokina
        (setf *mg-analyzer*
              #-lokokina
              (load-morph '("georgian-morph-mg"
                            :georgian-morph-ng
                            :georgian-morph-og
                            :georgian-comp-ng
                            ))))
      (setf *ng-analyzer*
            (load-morph '(:georgian-morph-ng
                          "georgian-redup-ng"
                          "georgian-morph-ng-pv"
                          ;;"georgian-verb-ng-guessed" ; has to be improved
                          :georgian-comp-ng ;; taken out because of many (?) wrong composita
                          :georgian-morph-og
                          #+ignore "georgian-noun-guessed"
                          "anthr-coll"
                          "foreign-morph"))))
    )
  (print :done))

(defparameter *kat-feature-name-table* (make-hash-table :test #'equal))

(defparameter *kat-gnc-to-ud-table* (make-hash-table :test #'equal))

(defun load-feature-names (file)
  (clrhash *kat-feature-name-table*)
  (clrhash *kat-gnc-to-ud-table*)
  (block read
    (u:with-file-fields ((f &optional ud variety posp used-with eng-desc kat-code kat-desc comment)
                         file :empty-to-nil t)
      (when (equal f "###") (return-from read))
      (unless (char= (char f 0) #\#)
        (setf (gethash f *kat-feature-name-table*)
              (mapcar (lambda (str) (unless (equal str "") str))
                      (list ud kat-code variety posp used-with eng-desc kat-desc comment)))))))


#+test
(print (parse-text "ძალიან კარგი" :variety :ng :disambiguate t :lookup-guessed nil))

#+test
(write-gnc-text-json (parse-text "ძალიან კარგი" :variety :ng)
                     *standard-output*)

#+test
(write-gnc-text-json (parse-text "Аӡҕаб бзиоуп." :variety :abk)
                     *standard-output*)


;; remove subsumption:
;; removes reading with specific preverb if equal form with preverb wildcard * is found
(defun remove-subsumed-preverbs (readings)
  (let ((wc-readings
	 (loop for r in readings
	    when (char= (char r 0) #\*)
	    collect r)))
    (cond (wc-readings
	   (loop for r in readings
	      unless (and (char/= (char r 0) #\*)
			  (let ((pv-end (1+ (or (position #\· r) -1))))
			    (and pv-end
				 (find-if (lambda (wc-r)
					    (string= wc-r r :start1 2 :start2 pv-end))
					  wc-readings))))
	      collect r))
	  (t
	   readings))))

(defmethod computed-norm ((language (eql :kat)) word)
  ;; normalized stuttering, e.g. არაფ…ფფერი -> არაფერი
  (when word
    (let* ((ell-pos (and (> (length word) 3) (position #\… word :start 1 :end (1- (length word)))))
	   (c (when ell-pos (char word (- ell-pos 1)))))
      (cond ((and ell-pos
		  (char= c (char word (+ ell-pos 1))))
	     (cond ((and (> ell-pos 1) ;; არაფფ…ფერი
			 (char= c (char word (- ell-pos 2))))
		    (u:concat (subseq word 0 (1- ell-pos)) (subseq word (+ ell-pos 2))))
		   ((and (< ell-pos (- (length word) 2)) ;; არაფ…ფფერი
			 (char= c (char word (+ ell-pos 2))))
		    (u:concat (subseq word 0 ell-pos) (subseq word (+ ell-pos 3))))
		   (t			; არაფ…ფერი
		    (u:concat (subseq word 0 ell-pos) (subseq word (+ ell-pos 2))))
		   ))
	    ((and (> (length word) 2)
		  (find #\_ word)
		  (find-if-not (lambda (c) (char= c #\_)) word))
	     (delete #\_ word))
	    (t
	     nil)))))

#+moved
(defmethod process-gnc-text ((text text) (mode (eql :analyze))
			     &key (variety :og) correct-spelling-errors
			       (normalize t) (lookup-guessed t)
			     ;; experimental; keeps MWE and non-MWE readings;
			     ;; MWE second (third) word are accessible in CG
			     ;; Therefore, some rules will have to be adapted
			     keep-non-mwe-readings
			       ;; lexicon
			       unknown-only-p &allow-other-keys)
  ;;(print (list :process-gnc-text :mode :analyze :variety variety))
  (setf (text-lexicon text) (dat:make-string-tree))
  (let ((token-array (text-array text))
	(norm-table (make-hash-table :test #'equal)) ;; calculated anew from .lex file
	;;(lexicon (or lexicon (text-lexicon text)))
	(lang-stack (list variety))
	(lexicon (text-lexicon text))
	(extracted-table (make-hash-table :test #'equal))) ;; table of all words that have been treated
    ;;#+test ;; *text*
    ;; (describe lexicon)
    (let ((lex-file (when (location text) (merge-pathnames ".lex" (location text)))))
      ;;(debug lex-file)
      (when (and lex-file (probe-file lex-file))
	(Print :lex-file-found)
	;; corr is treated like norm if not used as base for text correction
	(u:with-file-fields ((token norm lemma code pos features
				    &optional class present future aorist
				    perfect
				    corr comment
				    approved
				    )
			     lex-file)
	  (declare (ignore lemma code pos features class present future aorist perfect comment))
	  ;;(print (list token norm lemma))
	  (unless (equal approved "false")
	    (setf (gethash token extracted-table)
		  t
		  #+ignore
		  (list norm lemma pos code features
			class present future aorist perfect corr comment approved)))
	  (unless (and (or (null norm) (equal norm "-") (equal norm ""))
		       (or (null corr) (equal corr "-") (equal corr "")))
	    ;;(debug norm)
	    (setf (gethash token norm-table) (or norm corr)))))
      (labels ((node-token (node)
		 (let* ((word (cadr node))
			(norm-list (getf (cddr node) :norm))
			(norm (when norm-list
				(assert (null (cddr norm-list)))
				(getf (car norm-list) :characters)))
			(dipl-list (getf (cddr node) :dipl))
			(dipl (when dipl-list
				(assert (null (cddr dipl-list)))
				(getf (car dipl-list) :characters)))
			(lex-norm (gethash (or word dipl) norm-table)) ;; from .lex-file
			(computed-norm (unless lex-norm
					 ;; normalize ellipsis in stuttering etc.
					 (computed-norm word)))
			)
		   (when (and (null norm) (or lex-norm computed-norm))
		     (setf (getf (cddr node) :norm)
			   (list (list :characters (or lex-norm computed-norm)))))
		   ;;(when lex-norm (debug lex-norm))
		   (values (or norm
			       lex-norm
			       computed-norm
			       ;; not sure this is correct
			       (let ((dipl (getf (cddr node) :dipl)))
				 (when dipl
				   (let ((str ""))
				     (dolist (elt dipl)
				       (when (eq (car elt) :characters)
					 (setf str (u:concat str (cadr elt)))))
				     (delete-if (lambda (c) (find c "{}[]|/\\‹›")) str))))
			       word
			       dipl)
			   (or word dipl)
			   lex-norm))))
	(loop with mwe-positions = ()
	   for node across token-array
	   for i from 0
	   do (case (car node)
		(:start-element
		 (push (let ((lang (getf node :|xml:lang|)))
			 (cond ((null lang)
				nil)
			       ((equal lang "oge")
				:og)
			       ((or (equal lang "mge") (equal lang "kat-mg"))
				:mg)
			       ((equal lang "kat")
				:ng)
			       ((equal lang "jge")
				:jg)
			       (t
				(intern (string-upcase lang) :keyword))))
			lang-stack))
		(:end-element
		 (pop lang-stack))
		(:word
		 (multiple-value-bind (token word lex-norm) (node-token node)
		   (declare (ignore lex-norm))
		   (let* ((next-token+j2 (loop for j from (1+ i) below (length token-array)
					    for node = (aref token-array j)
					    when (eq (car node) :word)
					    do (return (list (node-token node) j))))
			  (next-token+j3 (when next-token+j2
					   (loop for j from (1+ (cadr next-token+j2))
					      below (length token-array)
					      for node = (aref token-array j)
					      when (eq (car node) :word)
					      do (return (list (node-token node) (cadr next-token+j2) j)))))
			  (variety (find-if-not #'null lang-stack))
			  (normalized-token (when token
					      (transliterate-to-mkhedruli
					       (cond ((and normalize (> (length token) 1))
							   (remove-if (lambda (c) (find c "()[]/\\")) token))
						     (t
						      token)))))
			  (readings (when token ;; TODO: use second value (norm)
				      (lookup-morphology normalized-token
							 :lookup-guessed lookup-guessed 
							 :variety variety
							 :orthography (orthography text))))
			  #+not-used
			  (word-readings (when (and token (string/= token word))
					   (lookup-morphology word
							      :lookup-guessed lookup-guessed 
							      :variety variety)))
			  ;; fetch from DB; used in parsing interface
			  (new-morphology (when (and #+gekko nil (null readings)
						     (null (location text)))
					    (get-word-codes token :source "parse" :variety variety)))
			  (ignore (and (null readings)
				       (find-if (lambda (c) (or (char<= #\a c #\z)
								(char<= #\A c #\Z)
								(char<= #\0 c #\9)))
						(cadr node))))
			  (segments (unless readings (split-tmesis token))))
		     (multiple-value-bind (guess eq)
			 (when (and correct-spelling-errors
				    (> (length token) 6)
				    (or (null readings)
					(find-if (lambda (r) (search " Comp " r))
						 readings :key #'cadr))
				    (null (getf (cddr node) :norm)))
			   (guess-correct-spelling token))
		       (let* ((guess-readings (when (and guess (not eq))
						(lookup-morphology guess
								   :variety variety
								   :lookup-guessed lookup-guessed 
								   :guess t)))
			      (mwe2-reading (when (and (car next-token+j2)
						       (not (find (char token 0) ",;.:?!“”«»–"))
						       (not (find (char (car next-token+j2) 0)
								  ",;.:?!“”«»")))
					     (lookup-morphology (u:concat token " " (car next-token+j2))
								:lookup-guessed lookup-guessed 
								:variety variety :mwe t
								:orthography (orthography text))))
			      (mwe3-reading (when (and (not mwe2-reading)
						       (not (find (char token 0) ",;.:?!“”«»–"))
						       (car next-token+j3)
						       (not (find (char (car next-token+j2) 0)
								  ",;.:?!“”«»–"))
						       (not (find (char (car next-token+j3) 0)
								  ",;.:?!“”«»–")))
					      (lookup-morphology (u:concat token
									   " " (car next-token+j2)
									   " " (car next-token+j3))
								 :lookup-guessed lookup-guessed 
								 :variety variety :mwe t
								 :orthography (orthography text))))
			      (mwe-reading (or mwe2-reading mwe3-reading))
			      (mwe-length (if mwe3-reading 3 2))
			      (next-token+j (if mwe3-reading next-token+j3 next-token+j2)))
			 (when mwe-reading
			   (setf readings (if keep-non-mwe-readings
					      (progn (mapc (lambda (r)
							     (setf (cadr r) (u:concat (cadr r) " MWE")))
							   mwe-reading)
						     (append mwe-reading readings))
					      mwe-reading)) ;; discard regular readings
			   (unless keep-non-mwe-readings
			     (setf (getf (cddr node) :mwe) mwe-length))
			   (dolist (i (cdr next-token+j)) (push i mwe-positions)))
			 ;;(debug readings)
			 (cond ((find i mwe-positions)
				(setf (getf (cddr node) :morphology)
				      (if keep-non-mwe-readings
					  (cons (list "" "<MWE>" nil nil) readings)
					  (list (list "" "<MWE>" nil nil)))))
			       (guess-readings
				(setf (getf (cddr node) :norm)
				      (list (list :characters  guess))
				      (getf (cddr node) :morphology) guess-readings))
			       (readings
				(setf (getf (cddr node) :morphology) readings))
			       (new-morphology
				(setf (getf (cddr node) :new-morphology) new-morphology))
			       ((find variety '(:og :xm :hm :mg))
				(setf (getf (cddr node) :tmesis-msa)
				      (loop for (segment . rest) on segments
					 collect (cons segment
						       (lookup-morphology
							segment
							:lookup-guessed nil
							:tmesis-segment (if rest :infix :verb)
							:variety variety))))))
			 (when (gethash word extracted-table)
			   (setf (getf (dat:string-tree-get lexicon word) :extracted) t))
			 ;; need to know which lexicon words have been corrected and are no longer in the text
			 (setf (getf (dat:string-tree-get lexicon word) :in-text) t)
			 (unless (and unknown-only-p
				      (or readings segments))
			   (cond (ignore
				  nil)
				 (readings
				  (setf (getf (dat:string-tree-get lexicon word)
					      :morphology)
					readings))
				 (segments
				  (setf (getf (dat:string-tree-get lexicon word)
					      :tmesis-msa)
					(getf (cddr node) :tmesis-msa))))
			   ;; ("ბატონ-ყმიანა" "w4960") 
			   ;; (print (list word (getf node :|xml:id|)))
			   (push (getf node :|xml:id|) ;; concordance of the token
				 (getf (dat:string-tree-get lexicon word)
				       :wids))
			   (incf (getf (dat:string-tree-get lexicon word)
				       :count 0))))))))))
	(when (and lex-file (probe-file lex-file))
	  (u:with-file-fields ((word norm lemma code pos features
				      &optional class present future aorist perfect corr comment
				      approved)
			       lex-file)
	    (when t ;; (dat:string-tree-get lexicon token)
	      (pushnew (list norm lemma code pos features class 
			     present future aorist perfect corr comment
			     approved)
		       (getf (dat:string-tree-get lexicon word)
			     ;; was: (cddr (print (dat:string-tree-get lexicon token)))
			     :new-morphology)))))))))

(defmethod lookup-morphology ((language (eql :kat)) word
                              &key (variety :ng) tmesis-segment guess mwe
                                (lookup-guessed t)
                                &allow-other-keys)
  ;;(print (list word variety))
  (let ((lemmas+features ())
	(stripped-word
	 (remove-if (lambda (c) 
		      (find c "ՙ‹›{}\\|")) ;; #\Armenian_Modifier_Letter_Left_Half_Ring in numbers
		    word))
	(analyzer (case variety
		    (:og *og-analyzer*)
		    (:xm *xm-analyzer*)
		    (:hm *hm-analyzer*) ;; redundant?
		    (:mg *mg-analyzer*)
		    (:ng *ng-analyzer*)
		    (:jg *ng-analyzer*) ;; ?
		    (otherwise nil)
		    )))
    (cond (analyzer
           (cl-fst:fst-lookup ;; cl-foma:foma-lookup
            analyzer
            stripped-word
            (lambda (w l+f net)
              (declare (ignore w))
              ;; strip some markers and features
              (let* ((readings (or (u:split l+f #\newline nil nil t)
                                   #+gekko
                                   (and lookup-guessed
                                        (lookup-guessed stripped-word))))
                     (prev "")
                     (og-readings
                      (when (find variety '(:xm :hm))
                        (block og
                          (cl-fst:fst-lookup *og-analyzer*
                                             stripped-word
                                             (lambda (w l+f net)
                                               (declare (ignore w net))
                                               (return-from og l+f)))))))
                (dolist (reading (remove-subsumed-preverbs (sort readings #'string<)))
                  (when (and net (eq (cl-fst::name net) :georgian-redup-ng))
                    (setf reading (normalize-reduplication-reading reading))
                    #+debug(debug reading))
                  (unless (and (eq (string< prev reading) (length prev))
                               (equal (subseq reading (length prev)) "+NonStand"))
                    (let* ((f-start nil)
                           (xm-only (when (and (find variety '(:xm :hm))
                                               (find #\ხ stripped-word)
                                               (eq (cl-fst::name net) :xm))
                                      (not (search reading og-readings))))
                           (hm-only (when (and (find variety '(:xm :hm))
                                               (find #\ჰ stripped-word)
                                               (eq (cl-fst::name net) :hm))
                                      (not (search reading og-readings)))))
                      (loop for i from 0
                         for c across reading
                         when (char= c #\+) do (setf f-start i)
                         when (and f-start
                                   (= i (1+ f-start))
                                   (char<= #\A c #\Z))
                         do (return))
                      (let* ((features (when f-start (subseq reading f-start)))
                             (features (if features (subseq (substitute #\space #\+ features) 1) ""))
                             (reading (subseq reading 0 f-start)))
                        (cond (xm-only
                               (setf features (u:concat features " Xan")))
                              (hm-only
                               (setf features (u:concat features " Hae")))
                              (guess
                               (setf features (u:concat features " LevGuess"))))
                        (unless (and (eq tmesis-segment :infix)
                                     (search "N " features))
                          (pushnew (list reading
                                         (cond ((eq tmesis-segment :infix)
                                                (u:concat features " <TmInfix>"))
                                               ((eq tmesis-segment :verb)
                                                (u:concat features " <TmV>"))
                                               (t
                                                features))
                                         nil ;; flag
                                         nil) ;; trace (used CG rules)
                                   lemmas+features :test #'equal)))))
                  (setf prev reading))))))
	  (mwe ;; don’t recognize foreign stuff as mwe
	   nil)
	  (t
	   (setf lemmas+features (list (list "-" (format nil "Foreign ~a" variety) nil nil)))))
    #+test
    (cl-fst::fst-lookup *georgian-guessed* w
			(lambda (w l+f net)
			  (declare (ignore w net))
			  (unless lemmas+features
			    (dolist (reading (u:split l+f #\newline nil nil t))
			      (let* ((f-start (position #\+ reading))
				     (features (subseq reading f-start))
				     (reading (subseq reading 0 f-start)))
				(pushnew (list reading (substitute #\space #\+ features))
					 lemmas+features :test #'equal))))))
    (values lemmas+features stripped-word)))

#+moved
(defmethod process-gnc-text ((text text) (mode (eql :disambiguate))
			     &key (variety :og) (load-grammar t)
			       (sentence-end-strings vislcg3::*sentence-end-strings*)
			       &allow-other-keys)
  (vislcg3::cg3-disambiguate-text text :variety variety :load-grammar load-grammar
				  :sentence-end-strings sentence-end-strings))

(defconstant +asomtavruli+ "ႠႡႢႣႤႥႦჁႧႨႩႪႫႬჂႭႮႯႰႱႲჃႳႴႵႶႷႸႹႺႻႼႽႾჄႿჀჅ")
(defconstant +nuskhuri+ "ⴀⴁⴂⴃⴄⴅⴆⴡⴇⴈⴉⴊⴋⴌⴢⴍⴎⴏⴐⴑⴒⴣⴓⴔⴕⴖⴗⴘⴙⴚⴛⴜⴝⴞⴤⴟⴠⴥ")
(defconstant +mkhedruli+ "აბგდევზჱთიკლმნჲოპჟრსტჳუფქღყშჩცძწჭხჴჯჰჵ")
(defconstant +mtavruli+ "ᲐᲑᲒᲓᲔᲕᲖᲱᲗᲘᲙᲚᲛᲜᲲᲝᲞᲟᲠᲡᲢᲳᲣᲤᲥᲦᲧᲨᲩᲪᲫᲬᲭᲮᲴᲯᲰᲵ")

#+test
(u:with-file-fields ((asomtavruli nuskhuri mkhedruli  mtavruli &rest rest)
		   "projects:gnc;data;character-table1.tsv")
  (write-string mtavruli))

(defmethod transliterate ((language (eql :kat)) str)
  (transliterate-to-mkhedruli str))

(defun transliterate-to-mkhedruli (str)
  (loop for c across str for i from 0
     for pos = (or (position c +asomtavruli+)
		   (position c +nuskhuri+)
		   (position c +mtavruli+))
     when pos
     do (setf (char str i) (char +mkhedruli+ pos)))
  str)


(defparameter *project* :gnc) ;; :abnc)

(define-url-function parse-text-json
    (request ((login-code string nil :global t)
	      (session-index string nil :global t t)
	      (session-id integer)
	      text
	      (variety keyword)
	      (show-all-readings boolean)
	      (show-rules boolean))
	     :type :json
	     :path "/parse-api"
	     :base-url (base-url *framework*))
  ;;(print (request-query request))
  (when (eq *project* :abnc) (setf variety :abk))
  (let* ((session (get-session :gnc-text session-id))
	 ;;(gnc-text (make-instance 'gnc-text))
	 (*tagset* :full-tagset))
    (setf (getf (session-av-list session) :parse-show-word-ids) nil)
    (handler-case
	(progn
	  (if text
	      (setf (getf (session-av-list session) :parse-text) text)
	      (setf text (getf (session-av-list session) :parse-text)))
	  (if variety
	      (setf (getf (session-av-list session) :variety) variety)
	      (setf variety (getf (session-av-list session) :variety :ng)))
	  (let ((gnc-text (parse-text text
				      :variety (or (getf (session-av-list session) :variety) :ng)
				      :load-grammar nil)))
	    ;;(setf *text* gnc-text)
	    (setf (getf (session-av-list session) :gnc-text) gnc-text)
	    (write-gnc-text-json gnc-text stream)))
      (error (cond)
	(format t "~a" cond)
	))))

;; *text*

(defmethod write-gnc-text-json ((text text) stream
				&key tracep split-trace
				  (suppress-discarded-p t)
				  (lemma t)
				  (features t)
				  (disambiguate t)
				  (dependencies nil)
				  show-rules
				  rid ;; whether to show rid
				  manual ;; for manual disambiguation
				  &allow-other-keys)
  (let ((start-cpos nil)
	(end-cpos nil))
    (json
     "tokens"
     (loop for node across (text-array text)
	for id from 0
	for cpos = (getf node :cpos)
	when (eq (car node) :word)
	collect (progn
		  (setf end-cpos cpos)
		  (unless start-cpos (setf start-cpos cpos))
		  (write-word-json node
				   :id id
				   :tracep tracep
				   :split-trace split-trace
				   :suppress-discarded-p suppress-discarded-p
				   :lemma lemma
				   :features features
				   :disambiguate disambiguate
				   :dependencies dependencies
				   :show-rules show-rules
				   :rid rid
				   :manual manual)))
     "startCpos" start-cpos ;; not needed?
     "endCpos" end-cpos ;; not needed?
     )))

(defun write-word-json (node &key id
			       tracep ;; traces in view mode?
			       split-trace
			       suppress-discarded-p
			       (lemma t)
			       (features t)
			       (disambiguate t)
			       (dependencies nil)
			       show-rules
			       rid
			       manual
			       error
			       (count t)
			       no-morphology
			       no-newlines)
  (declare (ignore tracep split-trace no-newlines error disambiguate))
  (ecase (car node)
    (:word
     (destructuring-bind (&key word morphology facs dipl norm |id| cpos comment
			       &allow-other-keys) node
       (declare (ignore morphology comment |id| norm dipl facs))
       ;;(debug node)
       (let* ((morphology (unless no-morphology (getf (cddr node) :morphology)))
	      (tmesis-msa (unless no-morphology (getf (cddr node) :tmesis-msa))))
         (declare (ignore tmesis-msa))
	 (apply
	  #'st-json::jso
	  `("word"
	    ,word
	    ,@(when dependencies 
		    `("self"
		      ,(unless (eql (getf node :self) 0) (getf node :self))
		      "parent"
		      ,(unless (eql (getf node :parent) -1) (getf node :parent))))
	    ,@(when count `("count" ,(length morphology)))
	    ,@(when cpos `("cpos" ,cpos))
	    "id" ,id
	    ,@(when (and lemma features)
		    `("msa"
		      ,(cond (morphology
			      (write-msa-json morphology
					      :node node
					      :lemma lemma
					      :features features
					      :suppress-discarded-p suppress-discarded-p
					      :show-rules show-rules
					      :rid rid
					      :manual manual))
			     #+ignore-yet
			     (tmesis-msa
			      (loop for ((segment . msa) . rest) on tmesis-msa
				 do (write-msa-xml
				     msa stream
				     :node node
				     :mode mode
				     :no-newlines no-newlines
				     :suppress-discarded-p suppress-discarded-p
				     :tracep tracep
				     :split-trace split-trace)
				 when rest
				 do #m(gnc:tmesis/)))))))))))))

(defun gnc-to-ud-features (features)
  (format nil "~{~a~^ ~}"
	  (loop for f in (u:split features #\space)
	     for ud = (car (gethash f *kat-feature-name-table*))
	     when (and ud (not (equal ud "-")))
	     collect (string-left-trim "*" ud))))

(defun remove-dep-edge-features (features)
  (format nil "~{~a~^ ~}"
	  (loop for f in (u:split features #\space)
	     unless (find (char f 0) ">@")
	     collect f)))

(defun load-suppressed-features (&optional file)
  (setf *suppressed-features*
	(u:collecting
	  (u:with-file-lines (line (or file "projects:gnc;text-tool;static;suppressed-features.txt"))
	    (let ((f (string-trim '(#\space #\tab) line)))
	      (unless (or (string= f "")
			  (char= (char f 0) #\#))
		(u:collect f)))))))

(defvar *tagset*)

(load-suppressed-features)

(defun pos2 (features)
  (let* ((flist (u:split features #\Space))
	 (filtered-list
	  (cons (car flist)
		(delete-if (lambda (f)
			     (or (find f *suppressed-features* :test #'string=)
				 (eq (string< "PP:" f) 3)
				 ))
			   (cdr flist)))))
    (format nil "~{~a~^ ~}" filtered-list)))

(defun filter-morphology (readings &key tagset)
  (case tagset
    (:reduced-tagset
     (let ((freadings ()))
       (loop for (lemma features flag trace ids trans) in readings
	  for pos2 = (pos2 features)
	  for id from 0
	  do (let ((freading (find-if (lambda (fr)
					(and (equal (car fr) lemma)
					     (equal (cadr fr) pos2)))
				      freadings)))
	       (cond (freading
		      (when (find flag '(:selected :selected-cg))
			(setf (caddr freading) flag))
		      (push id (caddr (cddr freading)))
		      (dolist (rule trace)
			(pushnew rule (cadddr freading) :test #'equal)))
		     (t
		      (push (list lemma pos2 flag trace (list id)) freadings)))))
       freadings))
    (:pos
     (let ((freadings ()))
       (loop for (lemma features flag trace) in readings
	  for pos2 = (car (u:split features #\Space))
	  for id from 0
	  do (let ((freading (find-if (lambda (fr)
					(and (equal (car fr) lemma)
					     (equal (cadr fr) pos2)))
				      freadings)))
	       (cond (freading
		      (when (find flag '(:selected :selected-cg))
			(setf (caddr freading) flag))
		      (push id (caddr (cddr freading)))
		      (dolist (rule trace)
			(pushnew rule (cadddr freading) :test #'equal)))
		     (t
		      (push (list lemma pos2 flag trace (list id)) freadings)))))
       freadings))
    (:pos1 ;; first feature
     (loop for (lemma features flag trace ids trans) in readings
	for pos = (car (u:split features #\Space nil nil t))
	when (find flag '(:selected :selected-cg))
	collect (list lemma pos flag trace ids trans)))
    (:pos2 ;; first, evtl. second feature
     (loop for (lemma features flag trace ids trans) in readings
	for f-list = (u:split features #\Space nil nil t)
	for pos = (car f-list)
	for pos2 = (find-if (lambda (f)
			      (find f '("Prop" "Tr" "Intr" "Caus" "Stat" "StatPass")
				    :test #'string=))
			    f-list :from-end t) ;; want Caus > Stat
	when (find flag '(:selected :selected-cg))
	collect (list lemma (if pos2 (u:concat pos " " pos2) pos)
		      flag trace ids trans)))
    (:ud
     (loop for (lemma features flag trace ids trans) in readings
	for id from 0
	collect (list lemma (gnc-to-ud-features features) flag trace ids trans)))
    (:msa ;; i.e., no dependency edge labels
     (loop for (lemma features flag trace ids trans) in readings
	for id from 0
	collect (list lemma (remove-dep-edge-features features) flag trace ids trans)))
    (otherwise
     readings)))

(defun write-msa-json (morphology &key node
				    lemma
				    features
				    (suppress-discarded-p t)
				    rid
				    manual
				    show-rules)
  (let ((morphology (filter-morphology morphology :tagset :full-tagset #+orig *tagset*)))
    (u:collecting
      (loop for reading in morphology
	 for rid from 0
	 do (destructuring-bind (l f &optional flag trace ids) reading
	      (declare (ignore ids)) ;; equivalent readings after filtering; not used here
	      (unless (and suppress-discarded-p (eq flag :discarded-cg))
		(u:collect (apply #'st-json::jso
				  `(,@(when lemma `("lemma" ,l))
				      ,@(unless suppress-discarded-p
						`("status" ,(string-downcase flag)))
				      ,@(when features `("features" ,f))
				      ,@(when rid `("rid" ,rid))
				      ,@(when manual `("manual" :null))
				      ,@(when show-rules `("rules" ,trace)))))))))))


(defmethod get-token-table ((text text) &key node-id &allow-other-keys)
  (assert node-id)
  (let* ((id-table (make-hash-table :test #'equal))
	 (secedge-table (make-hash-table :test #'equal))
	 (token-table (make-hash-table :test #'equal))
	 (parent-id nil)
	 (word nil)
	 (token-array (token-array text))
	 (parents ())
	 (subtoken-count (count-if (lambda (token) (getf token :subtoken)) token-array))
	 (pre-subtoken-ids (loop for token across token-array
			      when (getf token :subtoken)
			      collect (cons (getf token :self)
					    (getf (getf token :subtoken) :self)))))
    (declare (ignore id-table))
    (unless nil	;(depid-table text) ;; maps dep node-ids to token-table ids
      (setf (depid-array text)
	    (make-array (+ 1 (length token-array) subtoken-count) :initial-element ()))
      (loop for i from 0
	 for token across token-array
	 when (and (eq (car token) :word) (getf token :self))
	 do (setf (getf (aref (depid-array text) (getf token :self)) :token) i)
	 unless (or (null (getf token :parent)) (= (getf token :parent) -1))
	 do (pushnew (getf token :self)
		     (getf (aref (depid-array text)
				 (getf token :parent)) :children))
	 when (getf token :subtoken)
	 do (let ((subtoken (getf token :subtoken)))
	      (setf (getf (aref (depid-array text) (getf subtoken :self)) :token) (- -1 i))
	      (unless (or (null (getf subtoken :parent)) (= (getf subtoken :parent) -1))
		(pushnew (getf subtoken :self)
			 (getf (aref (depid-array text)
				     (getf subtoken :parent)) :children))))))
    (labels ((walk (node-id)
	       (let* ((t-ch-list (aref (depid-array text) node-id))
		      (token-id (getf t-ch-list :token))
		      (token (if (< token-id 0)
				 (getf (aref token-array (- -1 token-id)) :subtoken)
				 (aref token-array token-id)))
		      (children (getf t-ch-list :children))
		      (parent (getf token :parent))
		      (msa (getf token :morphology))
		      ;;(reading (find-if-not (lambda (r) (find :discarded-cg r)) msa))
		      (reading (or (find-if (lambda (reading)
					      (and (not (find :discarded-cg reading))
						   (search " >" (cadr reading) :from-end t)))
					    msa)
				   (find-if-not (lambda (r) (find :discarded-cg r)) msa)))
		      (lemma (car reading)) ;; only first reading is considered!
		      (features (cadr reading))
		      (pos (subseq features 0 (position #\space features)))
		      (rel-start (search " >" features :from-end t))
		      (rel-end (when rel-start (position #\space features :start (+ 2 rel-start))))
		      (relation (when rel-start (subseq features (+ 2 rel-start) rel-end)))
		      (word (getf token :word))
		      (mwe (when (>= token-id 0)
			     (loop for id from (1+ token-id) below (length token-array)
				for token = (aref token-array id)
				while (equal "<MWE>" (cadar (getf token :morphology)))
				collect (getf token :word)))))
		 (setf (gethash node-id token-table)
		       (make-token-list :terminal-p t
					:id node-id
					:word (cond (mwe
						     (format nil "~a~{ ~a~}" word mwe))
						    ((getf token :subtoken)
						     (subseq word 0 (1- (length word))))
						    (t
						     word))
					:atts (list :|lemma| lemma :|pos| pos :|morph| features)
					:relation relation
					:head parent))
		 (mapc #'walk children))))
      (walk node-id))
    (let ((ids (sort (u:collecting
		       (maphash (lambda (id tl)
				  (declare (ignore id))
				  (u:collect (token-list-id tl)))
				token-table))
		     #'<)))
      ;;(setf *token-table* token-table)
      (maphash (lambda (id tl)
		 (declare (ignore id))
		 (let ((pos (position (token-list-id tl) ids)))
		   (when pos
		     (setf (token-list-id tl) (1+ pos))
		     (unless (= (token-list-head tl) -1)
		       (let ((pos (position (token-list-head tl) ids)))
			 (when pos
			   (setf (token-list-head tl)
				 (1+ (position (token-list-head tl) ids)))))))))
	       token-table)
      (values token-table (1+ (position node-id ids))))))

;;  *text*

(defparameter *root* nil)

#+test
(print (build-dep-graph *text* :node-id 6))

(defstruct token-list
  terminal-p
  id
  word
  head
  relation
  slashee-ids
  slash-relations
  atts)

(time (init-transducers :kat))

#+test
(print (parse-text "ძალიან კარგი" :variety :ng :disambiguate t :lookup-guessed nil))

#+test
(write-text-json (parse-text "ძალიან კარგი" :variety :ng)
                     *standard-output*)

:eof
