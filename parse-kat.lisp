;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: PARSE; Base: 10 -*-

(in-package :parse)

(defparameter *tokenizer* nil)
(defparameter *ng-tokenizer* nil)
(defparameter *og-analyzer* nil)
(defparameter *xm-analyzer* nil)
(defparameter *hm-analyzer* nil)
(defparameter *mg-analyzer* nil)
(defparameter *ng-analyzer* nil)

(defparameter *ng-guess-verb-analyzer*
  (make-instance 'fst-net
		 :file "projects:georgian-morph;regex;guessed-verb.fst"
                 :name :ng-guess-verb))

(defmethod init-transducers ((language (eql :kat))
                             &key ng-only plain-og (guess t)
                               (transducer-dir (ecase +fst+
                                                 (:foma "projects:parse-cg3;regex;")
                                                 (:fst "projects:georgian-morph;regex;")))
                               &allow-other-keys)
  (let* ((type (string-downcase +fst+))
         (ng-file (u:concat transducer-dir "georgian-morph-ng." type))
	 (og-file (u:concat transducer-dir "georgian-morph-og." type))
	 (xm-file (u:concat transducer-dir "georgian-morph-xanmeti." type))
	 (hm-file (u:concat transducer-dir "georgian-morph-haemeti." type))
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
                    when name
		    do ;; (print name)
		      (format t "~&loading: ~s~%" name) 
		      (case name
			(:georgian-morph-ng
			 (u:collect (or ng-net
					(setf ng-net (make-instance 'fst-net
								    :file ng-file :name :ng)))))
                        #+what-is-that?
			(:georgian-comp-ng
			 (u:collect (or comp-ng-net
					(setf comp-ng-net
					      (make-instance 'fst-net
							     :file og-file :name :comp-ng)))))
			(:georgian-morph-og
			 (u:collect (or og-net
					(setf og-net (make-instance 'fst-net
								    :file og-file :name :og)))))
			(:georgian-morph-xanmeti
			 (u:collect (or xm-net
					(setf xm-net (make-instance 'fst-net
								    :file xm-file :name :xm)))))
			(:georgian-morph-haemeti
			 (u:collect (or hm-net
					(setf hm-net (make-instance 'fst-net
								    :file hm-file :name :hm)))))
                        (otherwise
			 (let ((file (u:concat transducer-dir name "." type)))
			   ;;(format t "~&loading: ~s~%" file) 
			   (if (probe-file file)
			       (u:collect
				   (make-instance
				    'fst-net
				    :file file
				    :name (intern (string-upcase name) :keyword)))
			       (warn "File not found: ~a" file)))))))))
      (setf *ng-tokenizer*
	    (make-instance 'fst-tokenizer :token-boundary #.(string #\newline)
			   :file (u:concat transducer-dir "georgian-tokenize-ng." type)))
      (setf *tokenizer*
	    (make-instance 'fst-tokenizer :token-boundary #.(string #\newline)
			   :file (u:concat transducer-dir "geo-tokenize." type)))
      (unless ng-only
        (setf *og-analyzer*
              (if plain-og
                  (load-morph '(:georgian-morph-og
                                ;; :georgian-comp-ng
                                ))
                  (load-morph '(:georgian-morph-og
                            :georgian-morph-ng
                            ;; :georgian-comp-ng
                            ;;"georgian-noun-guessed"
                            ))))
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
                            ;; :georgian-comp-ng
                            ))))
      (setf *ng-analyzer*
            (load-morph (list :georgian-morph-ng
                              "georgian-redup-ng"
                              "georgian-morph-ng-pv"
                              ;;"georgian-verb-ng-guessed" ; has to be improved
                              ;; :georgian-comp-ng ;; taken out because of many (?) wrong composita
                ;;              :georgian-morph-og
                              "ng-prefix-noun-adj"
                              #+ignore "georgian-noun-guessed"
                              "anthr-coll"
                              "foreign-morph"
                              (when guess "georgian-guessed")
                              ))))
    )
  (print :done))

(defun load-feature-names (file)
  (clrhash *feature-name-table*)
  (block read
    (u:with-file-fields ((f &optional ud variety posp used-with eng-desc kat-code kat-desc comment)
                         file :empty-to-nil t)
      (when (equal f "###") (return-from read))
      (unless (char= (char f 0) #\#)
        (setf (gethash f *feature-name-table*)
              (mapcar (lambda (str) (unless (equal str "") str))
                      (list ud kat-code variety posp used-with eng-desc kat-desc comment)))))))

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

(defparameter *redup-scanner* ;; splits like: 0/1=2/3+4
  (cl-ppcre:create-scanner
   "^([^\\/\\=\\+]+)/([^\\/\\=\\+]+)=([^\\/\\=\\+]+)/([^\\/\\=\\+]+)\\+(.*)$"))

#+test
(debug
 (cl-ppcre:scan
  *redup-scanner*
  "გა-ჭენება/ჭენ=გამო-ჭენებ[ა]/ჭენ+V+Act+Aor+Pv+Redup+<S-DO>+<S:Erg>+<DO:Nom>+S:3Sg+DO:3+NonStand"
  ))
#+test
(debug
 (normalize-reduplication-reading
  "გა-ჭენება/ჭენ=გამო-ჭენებ[ა]/ჭენ+V+Act+Aor+Pv+Redup+<S-DO>+<S:Erg>+<DO:Nom>+S:3Sg+DO:3+NonStand"))

(defun normalize-reduplication-reading (reading)
  (multiple-value-bind (start end match-starts match-ends)
      (cl-ppcre:scan *redup-scanner* reading)
    (declare (ignore start end))
    (flet ((seg (i)
	     (subseq reading (aref match-starts i) (aref match-ends i))))
      (if (string= (seg 1) (seg 3))
	  (format nil "~a=~a/~a+~a" (seg 0) (seg 2) (seg 1) (seg 4))
	  (format nil "~a=~a/~a=~a+~a" (seg 0) (seg 2) (seg 1) (seg 3) (seg 4))))))

#+test
(pprint (lookup-morphology :kat "K-ჯგუფებისათვის"))

#+test
(pprint (lookup-morphology :kat "ენტომოფაგები"))

#+test
(pprint (lookup-morphology :kat "მ³/წმ"))
#+test
(pprint (lookup-morphology :kat "+14°C"))

#+test
(pprint (guess-ng-verb-morphology "ვავარეპარირებ"))

(defun guess-ng-verb-morphology (word &optional guess-table)
  (let ((lemmas+features ())
        (prev nil))
    ;; try analysing word by verb guesser, for any possible segmentation into prefix, root, suffix
    ;; root is replaced by $
    (loop for start from 0 below (1- (length word))
          do (loop for end from (1+ start) below (1- (length word))
                   for prefix = (subseq word 0 start)
                   for root = (subseq word start end)
                   for suffix = (subseq word end)
                   do (fst-lookup *ng-guess-verb-analyzer*
                                  (u:concat prefix "$" suffix)  
                                  (lambda (w l+f net)
                                    ;;(declare (ignore w net))
                                    (when (string/= l+f "") (list w l+f))
                                    (when (> (length l+f) 0)
                                      (let ((exp-l+f ""))
                                        (labels ((insert-root (pos)
                                                   (let ((dpos (position #\$ l+f :start pos)))
                                                     (setf exp-l+f
                                                           (u:concat exp-l+f (subseq l+f pos dpos)))
                                                     (when dpos
                                                       (setf exp-l+f (u:concat exp-l+f root))
                                                       (insert-root (1+ dpos))))))
                                          (insert-root 0)
                                          (let ((readings (u:split exp-l+f #\newline nil nil t)))
                                            (multiple-value-setq (lemmas+features prev)
                                              (append-readings lemmas+features
                                                               word
                                                               (mapcar (lambda (r)
                                                                         (u:concat r " GuessV Guess"))
                                                                       readings)
                                                               :variety :ng
                                                               :prev prev
                                                               :guess-table guess-table))))))))))
    lemmas+features))

(defmethod lookup-morphology ((language (eql :kat)) word
                              &key (variety :ng) guess-table tmesis-segment mwe
                                &allow-other-keys)
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
           (fst-lookup
            analyzer
            stripped-word
            (lambda (w l+f net)
              (declare (ignore w))
              ;; strip some markers and features
              (let* ((readings (u:split l+f #\newline nil nil t))
                     (prev "")
                     (og-readings
                      (when (find variety '(:xm :hm))
                        (block og
                          (fst-lookup *og-analyzer*
                                      stripped-word
                                      (lambda (w l+f net)
                                        (declare (ignore w net))
                                        (return-from og l+f)))))))
                (multiple-value-setq (lemmas+features prev)
                  (append-readings lemmas+features stripped-word readings
                                   :variety variety
                                   :og-readings og-readings
                                   :tmesis-segment tmesis-segment
                                   :net net
                                   :prev prev
                                   :guess-table guess-table)))))
           (when (and (eq variety :ng)
                      (not (find #\space stripped-word))
                      (or (null lemmas+features)
                          (search "Guess" (cadar lemmas+features))))
             (setf lemmas+features
                   (append lemmas+features
                           (guess-ng-verb-morphology stripped-word guess-table)))))
          (mwe ;; don’t recognize foreign stuff as mwe
	   nil)
	  (t
	   (setf lemmas+features (list (list "-" (format nil "Foreign ~a" variety) nil nil)))))
    (values lemmas+features stripped-word)))

(defun append-readings (lemmas+features stripped-word readings
                        &key (variety :ng) og-readings tmesis-segment net guess-table prev)
  (let* ((readings (remove-subsumed-preverbs (sort readings #'string<)))
         (is-guessed (and guess-table (search "Guess" (car readings))))
         (wf-table (car guess-table))
         (max-wf-count 0))
    (when is-guessed
      ;; find the maximal number of attested wordforms for any of the lemmas + Pos
      (dolist (reading readings)
        (let* ((pos-start (position #\+ reading))
               (pos-end (position #\+ reading :start (1+ pos-start)))
               (lemma (subseq reading 0 pos-start))
               (wf-count 0)
               (amb-guess-readings (gethash lemma wf-table)))
          (when amb-guess-readings
            (dat:do-string-tree (word count amb-guess-readings)
              (when (string= reading word ;; word is word+Pos; compare Pos
                             :start1 pos-start
                             :end1 pos-end
                             :start2 (position #\+ word :from-end t))
                (incf wf-count))))
          (setf max-wf-count (max max-wf-count wf-count)))))
    
    (dolist (reading readings)
      (when (and net (eq (net-name net) :georgian-redup-ng))
        (setf reading (normalize-reduplication-reading reading)))
      (unless (and (eq (string< prev reading) (length prev))
                   (equal (subseq reading (length prev)) "+NonStand"))
        (let* ((f-start nil)
               (xm-only (when (and (find variety '(:xm :hm))
                                   (find #\ხ stripped-word)
                                   (eq (net-name net) :xm))
                          (not (search reading og-readings))))
               (hm-only (when (and (find variety '(:xm :hm))
                                   (find #\ჰ stripped-word)
                                   (eq (net-name net) :hm))
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
                 (lemma (subseq reading 0 f-start)))
            (when (and guess-table (search "Guess" features))
              ;;(print (list :l lemma :f features))
              (let* ((wf-count 0)
                     (unamb-freq 0)
                     (wf-table (car guess-table))
                     (unamb-table (cdr guess-table))
                     (amb-guess-readings (gethash lemma wf-table))
                     (unamb-guess-readings (gethash lemma unamb-table)))
                (when amb-guess-readings
                  (dat:do-string-tree (word count amb-guess-readings)
                    (when (string= features word
                                   :end1 (position #\space features)
                                   :start2 (1+ (position #\+ word :from-end t)))
                      (incf wf-count))))
                (when (> wf-count 0)
                  (setf features (u:concat features (format nil " <WFGuess> <guess=~a>" wf-count)))
                  (when (= wf-count max-wf-count)
                    (setf features (u:concat features " <WFMax>"))))
                (when unamb-guess-readings
                  (dat:do-string-tree (word count unamb-guess-readings)
                    (incf unamb-freq count)))
                (when (> unamb-freq 0)
                  (setf features (u:concat features (format nil " <GlobalGuess> <uaguess=~a>" unamb-freq)))
                  (setf features (u:concat features " <UnambGuess>")))))
            (cond (xm-only
                   (setf features (u:concat features " Xan")))
                  (hm-only
                   (setf features (u:concat features " Hae")))
                  #+ignore
                  (guess
                   (setf features (u:concat features " LevGuess"))))
            (unless (and (eq tmesis-segment :infix)
                         (search "N " features))
              (pushnew (list lemma
                             (cond ((eq tmesis-segment :infix)
                                    (u:concat features " <TmInfix>"))
                                   ((eq tmesis-segment :verb)
                                    (u:concat features " <TmV>"))
                                   (t
                                    features))
                             nil ;; flag
                             nil) ;; trace (used CG rules)
                       lemmas+features :test #'equal)))))
      (setf prev reading)))
  (values lemmas+features prev))

(defconstant +asomtavruli+ "ႠႡႢႣႤႥႦჁႧႨႩႪႫႬჂႭႮႯႰႱႲჃႳႴႵႶႷႸႹႺႻႼႽႾჄႿჀჅ")
(defconstant +nuskhuri+ "ⴀⴁⴂⴃⴄⴅⴆⴡⴇⴈⴉⴊⴋⴌⴢⴍⴎⴏⴐⴑⴒⴣⴓⴔⴕⴖⴗⴘⴙⴚⴛⴜⴝⴞⴤⴟⴠⴥ")
(defconstant +mkhedruli+ "აბგდევზჱთიკლმნჲოპჟრსტჳუფქღყშჩცძწჭხჴჯჰჵ")
(defconstant +mtavruli+ "ᲐᲑᲒᲓᲔᲕᲖᲱᲗᲘᲙᲚᲛᲜᲲᲝᲞᲟᲠᲡᲢᲳᲣᲤᲥᲦᲧᲨᲩᲪᲫᲬᲭᲮᲴᲯᲰᲵ")

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

(defparameter *project* :gnc)

#+obsolete??
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

#+obsolete
(defmethod write-gnc-text-json ((text parsed-text) stream
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
  ;;(debug (list :disambiguate disambiguate :dependencies dependencies :rid rid))
  (let ((start-cpos nil)
	(end-cpos nil))
    (json
     `("tokens"
       ,(loop for node across (text-array text)
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
       ,@(when start-cpos `("startCpos" start-cpos)) ;; not needed?
       ,@(when start-cpos `("endCpos" end-cpos)) ;; not needed?
       ))))

#+obsolete ;; see parse-text.lisp
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
			       &allow-other-keys)
         node
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
		    `("self" ,(unless (eql (getf node :self) 0) (getf node :self))
		      "parent" ,(or (unless (eql (getf node :parent) -1) (getf node :parent)) :null)
                      "label" ,(or (getf node :label) :null)
                      "stored_parent" ,(or (getf node :stored-parent) :null)
                      "stored_label" ,(or (getf node :stored-label) :null)))
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

#+moved
(defun gnc-to-ud-features (features)
  (format nil "~{~a~^ ~}"
	  (loop for f in (u:split features #\space)
	     for ud = (car (gethash f *feature-name-table*))
	     when (and ud (not (equal ud "-")))
	     collect (string-left-trim "*" ud))))

#+moved
(defun remove-dep-edge-features (features)
  (format nil "~{~a~^ ~}"
	  (loop for f in (u:split features #\space)
	     unless (find (char f 0) ">@")
	     collect f)))

#+moved
(defun load-suppressed-features (&optional file)
  (setf *suppressed-features*
	(u:collecting
	  (u:with-file-lines (line (or file "projects:gnc;text-tool;static;suppressed-features.txt"))
	    (let ((f (string-trim '(#\space #\tab) line)))
	      (unless (or (string= f "")
			  (char= (char f 0) #\#))
		(u:collect f)))))))

(defvar *tagset*)

#+moved
(when (probe-file "projects:gnc;text-tool;static;suppressed-features.txt")
  (load-suppressed-features))

#+moved
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

#+moved
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
    (:full-tagset
     (loop for (lemma features flag trace ids trans) in readings
	for id from 0
	   collect (list lemma (u:split features #\+) flag trace ids trans)))
    (otherwise
     readings)))

#+obsolete? ;; see parse-text.lisp
(defmethod get-token-table ((text parsed-text) &key node-id &allow-other-keys)
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

(defmethod write-text-tsv ((text parsed-text) stream
                           &key tracep (suppress-discarded-p t)
                             write-xml-id
                             write-rule
                             write-rest
                             dependencies
                             &allow-other-keys)
  (declare (ignore tracep))
  (labels ((filter (features)
	     (cond (dependencies
		    features)
		   (t
		    (let ((end (or (search " @" features) (search " >" features))))
		      (if end (subseq features 0 end) features))))))
    (let (#+ignore
          (stream *standard-output*)
          (format-string "~a	¦~{~a¦~}	¦~{ ~a ¦~}"))
      (when dependencies
        (setf format-string (u:concat format-string "	~{~a~^->~}")))
      (when write-rule
        (setf format-string (u:concat format-string "	¦~{~a¦~}")))
      (when write-xml-id
        (setf format-string (u:concat "~a	" format-string)))      
      (when write-rest
        (setf format-string (u:concat "~a	" format-string)))      
      (setf format-string (u:concat format-string "~%"))
      (labels ((write-node (node &optional is-subtoken)
                 (destructuring-bind (&optional type word &key dipl norm
                                                morphology tmesis-msa subtoken
                                                |xml:id| comment rest parent self
                                                &allow-other-keys) node
                   (declare (ignore dipl))
                   (when (eq type :word)
                     (when comment
                       (setf comment (substitute #\¶ #\newline
                                                 (substitute #\¶ #\return (substitute #\space #\tab
                                                                                      (u:trim comment))))))
                     (cond (tmesis-msa
                            (apply #'format stream format-string
                                   (u:collecting
                                     (when write-xml-id
                                       (u:collect (or |xml:id| "")))
                                     (u:collect word)
                                     (when write-rest
                                       (u:collect (format nil "~{~a~^	~}" rest)))
                                     (u:collect
                                         (loop for (token . msa) in tmesis-msa
                                            do (loop for (lemma features flag) in msa
                                                  unless (or (find flag '(:discarded-cg :discarded))
                                                             ;; don't record lemmas for clitic tmesis infixes,
                                                             ;; parallel to treatment of word-final clitics
                                                             (search "Tm:" features))
                                                  collect lemma)))
                                     (u:collect
                                         (loop for (token . msa) in tmesis-msa
                                            do (loop for (lemma features flag) in msa
                                                  ;; unless (find flag '(:discarded-cg :discarded))
                                                  collect (filter features))))
                                     (when dependencies
                                       (u:collect (if parent (list parent self) (list self))))
                                     (when write-rule
                                       (u:collect
                                           (loop for (token . msa) in tmesis-msa
                                              do (loop for (lemma features flag) in msa
                                                    ;; unless (find flag '(:discarded-cg :discarded))
                                                    collect flag))))
                                     #+ignore
                                     (or comment "")))
                            nil)
                           (t
                            (apply #'format stream format-string
                                   (u:collecting
                                     (when write-xml-id
                                       (u:collect (or |xml:id| "")))
                                     (u:collect
                                         (or (when is-subtoken "")
                                             (when norm
                                               (assert (null (cdr norm)))
                                               (getf (car norm) :characters))
                                             (let ((dipl (getf (cddr node) :dipl)))
                                               (when dipl
                                                 (let ((str ""))
                                                   (dolist (elt dipl)
                                                     (when (eq (car elt) :characters)
                                                       (setf str (u:concat str (cadr elt)))))
                                                   (let ((norm (delete-if (lambda (c) (find c "{}[]|/\\‹›/")) str)))
                                                     (if (> (length norm) 0)
                                                         norm
                                                         str)))))
                                             (and (cadr node)
                                                  (let ((norm (delete-if (lambda (c) (find c "{}[]|/\\‹›/")) (cadr node))))
                                                    (if (> (length norm) 0)
                                                        norm
                                                        (cadr node))))
                                             ""))
                                     (when write-rest
                                       (u:collect (format nil "~{~a~^	~}" rest)))
                                     ;; (delete-duplicates ;; no!
                                     (u:collect
                                         (loop for (lemma features flag) in morphology
                                            unless (and suppress-discarded-p (find flag '(:discarded-cg :discarded)))
                                            collect lemma))
                                     ;; :test #'string=)
                                     (u:collect
                                         (loop for (lemma features flag) in morphology
                                            unless (and suppress-discarded-p (find flag '(:discarded-cg :discarded)))
                                            collect (filter features)))
                                     (when dependencies
                                       (u:collect (if parent (list parent self) (list self))))
                                     (when write-rule
                                       (u:collect
                                           (loop for (lemma features flag) in morphology
                                              unless (and suppress-discarded-p (find flag '(:discarded-cg :discarded)))
                                              collect flag)))
                                     #+ignore
                                     (or comment "")))
                            subtoken))))))
        (loop for node across (token-array text)
           for id from 0
           for subtoken = (write-node node)
           when subtoken
           do (write-node subtoken t))))))

#+ignore ;; see parse-text.lisp
(defstruct token-list
  terminal-p
  id
  word
  head
  relation
  slashee-ids
  slash-relations
  atts)

#+disabled
(if (eq +fst+ :fst)
    ;; Initialize the transducers
    (init-transducers :kat)
    ;; if you are interested in Modern Georgian only use this instead:
    (init-transducers :kat :ng-only t))

;; Takes a txt file as input and outputs a json array.
;; Make sure there are no line breaks in sentences.
;; Each element in the array is a parsed sentence.
;; If is :file parsing is run in two passes, in the first one frequency information
;; is collected for guessed (and other?) words, which is used in the second
;; pass to disambiguate guesses
(defun parse-kat-file (file &key (format :json) (input-format :txt) (write-xml-id t)
                              write-rest dependencies write-unknown guess-scope)
  (let ((out-file (merge-pathnames (format nil ".~(~a~)" format) file))
        (unknown-file (merge-pathnames ".unk" file))
        (unknown-tree (dat:make-string-tree))
        (count (list 0)))
    (ecase input-format
      (:txt
       (with-open-file (stream out-file :direction :output :if-exists :supersede)
         (ecase format
           (:json
            (write-string "[ " stream)
            (u:with-file-lines (line file)
              (write-text-json (parse-text line
                                           :variety :ng
                                           :disambiguate t
                                           :lookup-guessed nil
                                           :unknown-tree unknown-tree
                                           :cg3-variables '("guess" "rank")
                                           :guess-scope guess-scope
                                           :count count)
                               stream)
              (terpri stream))
            (write-string "]" stream))
           (:tsv
            (let ((guess-table (cons (make-hash-table :test #'equal)
                                     (make-hash-table :test #'equal))))
              (when (eq guess-scope :file)
                (print :first-pass)
                (u:with-file-lines (line file)
                  (parse-text line
                              :variety :ng
                              :disambiguate t
                              :guess-scope guess-scope
                              :guess-table guess-table
                              :count count
                              :cg3-variables '("guess")
                              ))
                (maphash (lambda (lemma readings)
                           (write-line lemma)
                           (dat:do-string-tree (word count readings)
                             (format t "~a	~a~%" count word)))
                         (car guess-table))
                (maphash (lambda (lemma readings)
                           (write-line lemma)
                           (dat:do-string-tree (word count readings)
                             (format t "~a	~a~%" count word)))
                         (cdr guess-table)))
              (print :second-pass)
              (u:with-file-lines (line file)
                (write-text-tsv (parse-text line
                                            :variety :ng
                                            :disambiguate t
                                            :guess-table guess-table
                                            :unknown-tree unknown-tree
                                            :cg3-variables '("guess" "rank")
                                            :count count)
                                stream
                                :write-xml-id write-xml-id
                                :dependencies dependencies)
                (terpri stream)))))))
      (:tsv
       (with-open-file (stream out-file :direction :output :if-exists :supersede)
         (ecase format
           #+not-yet
           (:json
            (write-string "[ " stream)
            (u:with-file-lines (line file)
              (write-text-json (parse-text line
                                           :variety :ng
                                           :disambiguate t
                                           :unknown-tree unknown-tree
                                           :cg3-variables cg3-variables
                                           :guess-scope guess-scope
                                           :count count)
                               stream :dependencies dependencies)
              (terpri stream))
            (write-string "]" stream))
           (:tsv
            (let ((tokens ()))
              (u:with-file-fields ((&rest tl) file :end-line :eof)
                (let ((is-dot (equal (car tl) "."))
                      (is-eof (eq (car tl) :eof)))
                  (cond ((or is-dot is-eof)
                         (when is-dot
                           (push tl tokens))
                         (write-text-tsv (parse-text (nreverse tokens)
                                                     :variety :ng
                                                     :disambiguate t
                                                     :unknown-tree unknown-tree)
                                         stream
                                         :write-xml-id write-xml-id
                                         :write-rest write-rest
                                         :dependencies dependencies)
                         (when is-dot
                           (setf tokens ())))
                        #+old
                        ((eq (car tl) :eof)
                         (write-text-tsv (parse-text (nreverse tokens)
                                                     :variety :ng
                                                     :disambiguate t
                                                     :unknown-tree unknown-tree
                                                     :count count)
                                         stream
                                         :write-xml-id write-xml-id
                                         :write-rest write-rest
                                         :dependencies dependencies))
                        #+old
                        ((equal (car tl) ".")
                         (push tl tokens)
                         (write-text-tsv (parse-text (nreverse tokens)
                                                     :variety :ng
                                                     :disambiguate t
                                                     :unknown-tree unknown-tree)
                                         stream
                                         :write-xml-id write-xml-id
                                         :write-rest write-rest
                                         :dependencies dependencies)
                         (setf tokens ()))
                        (t
                         (push tl tokens)))))
              (terpri stream)))))))
    (when write-unknown
      (let ((unknown-count 0))
        (with-open-file (stream unknown-file :direction :output :if-exists :supersede)
          (dat:do-string-tree (word count unknown-tree)
            (incf unknown-count count)
            (format stream "~a	~a~%" count word)))
        (debug unknown-count)))
    (print (car count))
    (print :done)))

(process-run-function "init-gnc-transducers"
                      (lambda ()
                        (init-transducers :kat)))

;; Example:

#+test
(parse-kat-file "projects:parse-cg3;example-text-kat.txt" :format :tsv :dependencies t :write-rest t)
#+test
(parse-kat-file "projects:georgian-morph;eval.txt" :format :tsv)

#+test
(parse-kat-file "projects:parse-cg3;data;ka-10000-clean.txt" :format :tsv :dependencies nil :write-unknown t
                :write-xml-id nil)

#+test
(parse::write-text-json (parse::parse-text "აბა" :variety :ng :disambiguate t :lookup-guessed nil) *standard-output*)

:eof
