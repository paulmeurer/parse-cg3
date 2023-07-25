;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: PARSE; Base: 10 -*-

(in-package :parse)

(defclass parsed-text ()
  ((name :initform nil :initarg :name :reader name)
   (variety :initform :ng :initarg :variety :accessor variety)
   (orthography :initform :standard :initarg :orthography :reader orthography)
   (location :initform nil :initarg :location :reader location)
   (corpus :initform nil :initarg :corpus :reader corpus)
   (text-array :initform (make-array 0 :adjustable t :fill-pointer t) :accessor text-array)
   ;; word-id -> index in text-array
   (word-id-table :initform (make-hash-table :test #'equal) :reader word-id-table) 
   (lexicon :initform (dat:make-string-tree) :accessor text-lexicon)
   (disambiguation-table :initform (make-hash-table :test #'equal) :reader disambiguation-table) 
   (depid-array :initform nil :accessor depid-array) ;; dependency node ids
   (start-wid :initform nil :initarg :start-wid :accessor start-wid)
   (end-wid :initform nil :initarg :end-wid :accessor end-wid)
   ;; stored reading by wid, where xml:id = "w<wid>"
   (wid-table :initform (make-hash-table) :reader wid-table)
   ))

(defmethod token-array ((text parsed-text))
  (text-array text))

(defmethod transliterate ((language t) str)
  str)

;; to be overridden
(defun text-class ()
  'parsed-text)

(defconstant +wrong-abk-chars+ "ԥԤӷӶӌӋ")
(defconstant +correct-abk-chars+ "ҧҦҕҔҷҶ")

(defun normalize-characters (text)
  (loop for c across text for i from 0
     for pos = (position c +wrong-abk-chars+)
     when pos
     do (setf (char text i) (char +correct-abk-chars+ pos))))

(defmethod parse-text ((text string) &key variety load-grammar (disambiguate t)
                                       lookup-guessed orthography unknown-tree count &allow-other-keys)
  (assert variety)
  (when (eq variety :kat) (setf variety :ng))
  (when (eq variety :abk)
    (normalize-characters text))
  (let* ((parsed-text (make-instance (text-class) :orthography orthography))
         (word-id-table (word-id-table parsed-text))
         (id 0))
    (setf *text* parsed-text)
    (fst-map-tokens
     (if (eq variety :abk)
	 *abk-tokenizer*
	 *ng-tokenizer*)
     text
     (lambda (token)
       (unless (or (equal token "@@@")
		   (u:null-or-empty-string-p token))
         (when count (incf (car count)))
	 (setf token (normalize-token token))
         (let* ((wid (format nil "w~a" (incf id)))
                (token (list :word token :wid wid)))
           (vector-push-extend token (text-array parsed-text))
           (setf (gethash wid word-id-table) token)))))
    (process-text parsed-text :analyze
                  :variety variety
                  :lookup-guessed lookup-guessed
                  :orthography (orthography parsed-text)
                  :unknown-tree unknown-tree
                  ;;:correct-spelling-errors (eq variety :ng)
                  )
    (when disambiguate
      (process-text parsed-text :disambiguate
                    :variety variety
                    :lookup-guessed lookup-guessed
                    :load-grammar load-grammar
                    :sentence-end-strings (if (eq variety :abk)
                                              '("." "?" "!" "…" ";")
                                              '("." "?" "!" "…"))))
    parsed-text))

;; pre-tokenized text, given as list of tokens, where each token is (word . rest). rest is kept unchanged.
(defmethod parse-text ((tokens list) &key variety load-grammar (disambiguate t) lookup-guessed unknown-tree
                                       &allow-other-keys)
  (assert variety)
  (when (eq variety :kat) (setf variety :ng))
  (let* ((parsed-text (make-instance (text-class))))
    (dolist (tl tokens)
      (destructuring-bind (token . rest) tl
        (unless (or (equal token "@@@")
                    (u:null-or-empty-string-p token))
          (setf token (normalize-token token))
          (vector-push-extend (list :word token :rest rest) (text-array parsed-text))
          )))
    (process-text parsed-text :analyze
                  :variety variety
                  :lookup-guessed lookup-guessed
                  :unknown-tree unknown-tree
                  ;;:correct-spelling-errors (eq variety :ng)
                  )
    (when disambiguate
      (process-text parsed-text :disambiguate
                    :variety variety
                    :lookup-guessed lookup-guessed
                    :load-grammar load-grammar
                    :sentence-end-strings  (if (eq variety :abk)
                                              '("." "?" "!" "…" ";")
                                              '("." "?" "!" "…"))))
    parsed-text))

(defun normalize-token (token)
  (setf token (string-trim #(#\Space #\Tab) token))
  (cond ((string= token "-")
	 "–")
	((string= token "—")
	 "–")
	((> (length token) 1)
	 (substitute #\- #\— (substitute #\- #\– token)))
	(t
	 token)))

;; debug
(defparameter *text* nil)

(defmethod parse-text ((text parsed-text) &key variety mode load-grammar (disambiguate t) lookup-guessed
                                            wid-table unknown-tree &allow-other-keys)
  (when (eq variety :kat) (setf variety :ng))
  (setf *text* text)
  (unless (eq mode :redisambiguate)
    (process-text text :analyze
                  :variety variety
                  :lookup-guessed lookup-guessed
                  :unknown-tree unknown-tree
                  ;;:correct-spelling-errors (eq variety :ng)
                  ))
  (when disambiguate
    (process-text text :disambiguate
                  :variety variety
                  :load-grammar load-grammar
                  :mode mode
                  :sentence-end-strings (if (eq variety :abk) ;; put this in to text object!
                                            '("." "?" "!" "…" ";")
                                            '("." "?" "!" "…"))))
  text)

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

;; to be overridden
(defun guess-correct-spelling (token)
  (declare (ignore token))
  nil)

(defmethod computed-norm ((language t) word)
  (declare (ignore word))
  nil)

(defparameter *infix-table* (make-hash-table :test #'equal))

(when (probe-file "projects:gnc;morphology;fst;tmesis.txt")
  (u:with-file-lines (infix "projects:gnc;morphology;fst;tmesis.txt")
    (setf (gethash infix *infix-table*) t)))

#+test
(print (split-tmesis "გან-ხოლო-თუ-ვითარ-იკურნოს"))

#+test
(print (split-tmesis "ცასა-მე-ა")) ;; -ა should be Q


;; splits tmesis into list of infixes plus concatenation of prefix and suffix
;; if infixes are in *infix-table*.
;; returns NIL if no tmesis found
(defun split-tmesis (word)
  (when (find #\- word)
    (let ((segments (u:split word #\-)))
      (when (string= (nth (1- (length segments)) segments) "ა")
	(setf (nth (1- (length segments)) segments) "-ა"))
      (when (> (length segments) 2)
	(let* ((prefix (pop segments))
	       (non-prefixes segments)
	       (infix-list
		(loop for (infix . rest) on non-prefixes
		   while (and rest (gethash infix *infix-table*))
		   collect infix
		   do (pop segments))))
	  (when infix-list
	    (append infix-list (list (apply #'concatenate 'string prefix segments)))))))))

;; TODO: use variety
(defmethod get-word-codes (token (variety t) &key (source "parse"))
  (declare (ignore source token))
  nil)

(defmethod load-wid-table ((text parsed-text))
  nil)

;; *text*

;; lexicon is stored in .lex file in save-all-new-words() (obsolete!)
;; it stores the values of :new-morphology
(defmethod process-text ((text parsed-text) (mode (eql :analyze))
                         &key (variety :og) correct-spelling-errors
                           (normalize t) (lookup-guessed t) unknown-tree
                           ;; experimental; keeps MWE and non-MWE readings;
                           ;; MWE second (third) word are accessible in CG
                           ;; Therefore, some rules will have to be adapted
                           keep-non-mwe-readings
                           ;; lexicon
                           unknown-only-p &allow-other-keys)
  ;;(print (list :process-text :mode :analyze :variety variety))
  (setf (text-lexicon text) (dat:make-string-tree))
  (let ((language (if (eq variety :abk) :abk :kat))
        (token-array (text-array text))
	(norm-table (make-hash-table :test #'equal)) ;; calculated anew from .lex file
	;;(lexicon (or lexicon (text-lexicon text)))
	(lang-stack (list variety))
	(lexicon (text-lexicon text))
        (wid-table (wid-table text))
        (extracted-table (make-hash-table :test #'equal))) ;; table of all words that have been treated
    ;;#+test ;; *text*
    ;; (describe lexicon)
    (load-wid-table text)
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
			(dipl (when (and (not norm) dipl-list)
				(assert (null (cddr dipl-list)))
				(getf (car dipl-list) :characters)))
			(lex-norm (gethash (or word dipl) norm-table)) ;; from .lex-file
			(computed-norm (unless lex-norm
					 ;; normalize ellipsis in stuttering etc.
					 (computed-norm language word)))
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
					      (transliterate
                                               language
					       (cond ((and normalize (> (length token) 1))
                                                      (remove-if (lambda (c) (find c "()[]/\\")) token))
						     (t
						      token)))))
			  (readings (when token ;; TODO: use second value (norm)
				      (lookup-morphology language normalized-token
							 :lookup-guessed lookup-guessed 
							 :variety variety
							 :orthography (orthography text))))
			  #+not-used
			  (word-readings (when (and token (string/= token word))
					   (lookup-morphology language word
							      :lookup-guessed lookup-guessed 
							      :variety variety)))
			  ;; fetch from DB; used in parsing interface
			  (new-morphology (when (and #+gekko nil (null readings)
						     (null (location text)))
					    (get-word-codes token variety :source "parse")))
			  (ignore (and (null readings)
				       (find-if (lambda (c) (or (char<= #\a c #\z)
								(char<= #\A c #\Z)
								(char<= #\0 c #\9)))
						(cadr node))))
			  (segments (unless readings (split-tmesis token))))
                     (when (and (null readings) unknown-tree)
                       (incf (dat:string-tree-get unknown-tree normalized-token 0))) 
		     (multiple-value-bind (guess eq)
			 (when (and correct-spelling-errors
				    (> (length token) 6)
				    (or (null readings)
					(find-if (lambda (r) (search " Comp " r))
						 readings :key #'cadr))
				    (null (getf (cddr node) :norm)))
			   (guess-correct-spelling token))
		       (let* ((guess-readings (when (and guess (not eq))
						(lookup-morphology language guess
								   :variety variety
								   :lookup-guessed lookup-guessed 
								   :guess t)))
			      (mwe2-reading (when (and (car next-token+j2)
						       (not (find (char token 0) ",;.:?!“”«»–"))
						       (not (find (char (car next-token+j2) 0)
								  ",;.:?!“”«»")))
                                              (lookup-morphology language
                                                                 (u:concat token " " (car next-token+j2))
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
					      (lookup-morphology language
                                                                 (u:concat token
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
                                (when wid-table
                                  (let ((wid (getf node :wid)))
                                    (when wid
                                      (let* ((id (parse-integer wid :start 1))
                                             (reading (gethash id wid-table)))
                                        (when reading
                                          (destructuring-bind (&optional w l fl label parent status comment) reading
                                            (declare (ignore w))
                                            (let ((reading1 (find-if (lambda (r)
                                                                       ;; compare lemma and features,
                                                                       (and (string= (car r) l)
                                                                            (or (string= (cadr r) fl)
                                                                                (let ((s-f (u:split fl #\space))
                                                                                      (d-f (u:split (cadr r) #\space)))
                                                                                  (loop for f in s-f
                                                                                        always (or (find f '("<Relax>" "<NoLex>")
                                                                                                         :test #'string=)
                                                                                                   (find f d-f :test #'string=)))))))
                                                                     readings)))
                                              (when reading1
                                                ;; mark found reading as <Sel>
                                                (setf (cadr reading1)
                                                      (u:concat (cadr reading1) " <Sel>")))
                                              (when status
                                                (setf (getf (cddr node) :status) status))
                                              (when comment
                                                (setf (getf (cddr node) :comment) comment))
                                              (when parent ;; preliminarily store wid here; has to be changed to node id
                                                ;; after disambiguation
                                                (setf (getf (cddr node) :stored-parent) (list parent)))
                                              (when label
                                                (setf (getf (cddr node) :stored-label) label)))
                                            readings))))))
                                (setf (getf (cddr node) :morphology) readings))
			       (new-morphology
				(setf (getf (cddr node) :new-morphology) new-morphology))
			       ((find variety '(:og :xm :hm :mg))
				(setf (getf (cddr node) :tmesis-msa)
				      (loop for (segment . rest) on segments
					 collect (cons segment
						       (lookup-morphology
							language segment
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
                                     &optional class present future aorist
                                     perfect corr comment
                                     approved)
			       lex-file)
	    (when t ;; (dat:string-tree-get lexicon token)
	      (pushnew (list norm lemma code pos features class 
			     present future aorist perfect corr comment
			     approved)
		       (getf (dat:string-tree-get lexicon word)
			     ;; was: (cddr (print (dat:string-tree-get lexicon token)))
			     :new-morphology)))))))))

(defmethod process-text ((text parsed-text) (mode (eql :disambiguate))
                         &key (variety :og) (load-grammar t) mode
                           (sentence-end-strings vislcg3::*sentence-end-strings*)
                           &allow-other-keys)
  (vislcg3::cg3-disambiguate-text text
                                  :variety variety
                                  :mode mode
                                  :load-grammar load-grammar
				  :sentence-end-strings sentence-end-strings
                                  :sentence-start-is-uppercase (eq variety :abk))

  (let ((token-array (text-array text))
        (wid-table (word-id-table text)))
    (loop for node across token-array
          do (let* ((msa (getf node :morphology))
                    (reading (or (find-if (lambda (reading)
					    (and (not (find :discarded-cg reading))
						 (search " >" (cadr reading) :from-end t)))
					  msa)
				 (find-if-not (lambda (r) (find :discarded-cg r)) msa)))
		    (features (cadr reading))
		    (pos (subseq features 0 (position #\space features)))
		    (rel-start (search " >" features :from-end t))
		    (rel-end (when rel-start (position #\space features :start (+ 2 rel-start))))
	            (relation (when rel-start (subseq features (+ 2 rel-start) rel-end)))
                    )
               (setf (getf (cddr node) :label) relation)))
    ;; fix stored-parent
    (loop for node across token-array
          for p = (getf node :stored-parent)
          when (listp p) ;; list ensures that this is done only once, but not on redisambiguation
          do (setf (getf node :stored-parent)
                   (getf (gethash (format nil "w~a" (car p)) wid-table) :self)))))

;; *text*

(defmethod write-text-json ((text parsed-text) stream
                            &key tracep split-trace
                              (suppress-discarded-p t)
                              (lemma t)
                              (features t)
                              (disambiguate t)
                              (dependencies nil)
                              start
                              end
                              show-rules
                              rid ;; whether to show rid
                              manual ;; for manual disambiguation
                              &allow-other-keys)
  (let ((st-json:*output-literal-unicode* t)
        (left-context ())
        (right-context ())
        (window 20))
    (u:collecting-into (left tokens right)
      (loop with id = 0
         for node across (text-array text)
         ;; for id from 0
	 for cpos = (getf node :cpos)
	 do (cond ((if (and start end)
                       (<= start id end)
                       t)
      	           (u:collect-into tokens
                                   (ecase (car node)
                                     (:word
                                      (prog1 (write-word-json node
                                                              :id id ;; (1+ id)
                                                              :tracep tracep
                                                              :split-trace split-trace
                                                              :suppress-discarded-p suppress-discarded-p
                                                              :lemma lemma
                                                              :features features
                                                              :disambiguate disambiguate
                                                              :dependencies dependencies
                                                              :show-rules show-rules
                                                              :rid rid
                                                              :manual manual)
                                        (incf id)))
                                     (:start-element
                                      (st-json:jso "struct" (getf node :start-element)
                                                   "type" "start-element"))
                                     (:end-element
                                      (st-json:jso "struct" (getf node :end-element)
                                                   "type" "end-element"))
                                     (:empty-element
                                      (st-json:jso "struct" (getf node :empty-element)
                                                   "type" "empty-element")))))
                  ((and start end (< (- start window) id start))
                   (ecase (car node)
                     (:word
                      (u:collect-into left (cadr node))
                      (incf id))
                     (:start-element
                      (u:collect-into left (u:concat "‹" (cadr node) "›")))
                     (:end-element
                      (u:collect-into left (u:concat "‹/" (cadr node) "›")))
                     (:empty-element
                      (u:collect-into left (u:concat "‹" (cadr node) "/›")))))
                  ((and start end (< end id (+ end window)))
                   (ecase (car node)
                     (:word
                      (u:collect-into right (cadr node))
                      (incf id))
                     (:start-element
                      (u:collect-into right (u:concat "‹" (cadr node) "›")))
                     (:end-element
                      (u:collect-into right (u:concat "‹/" (cadr node) "›")))
                     (:empty-element
                      (u:collect-into right (u:concat "‹" (cadr node) "/›")))))
                  ((eq (car node) :word)
                   (incf id))))
      (json
       "tokens" tokens
       "leftContext" left
       "rightContext" right
       "tokenCount" (count-if (lambda (node) (eq (car node) :word)) (text-array text))))))

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
     (destructuring-bind (&key word morphology facs dipl norm dipl-xml norm-xml stored-norm computed-norm
                               |id| cpos comment wid status &allow-other-keys)
         node
       (declare (ignore morphology |id| dipl facs))
       ;;(print (list :word word :morphology morphology))
       (let* ((morphology (unless no-morphology morphology)) ;; (getf (cddr node) :morphology)))
	      (tmesis-msa (unless no-morphology (getf (cddr node) :tmesis-msa))))
         (declare (ignore tmesis-msa))
	 (apply
	  #'st-json::jso
	  `("word"
	    ,word
	    ,@(when dependencies 
		    `("self" ,(or (unless (eql (getf node :self) 0) (getf node :self)) :null)
		      "parent" ,(or (unless (eql (getf node :parent) -1) (getf node :parent)) :null)
                      "label" ,(or (getf node :label) :null)
                      "stored_parent" ,(or (getf node :stored-parent) :null)
                      "stored_label" ,(or (getf node :stored-label) :null)))
	    ,@(when count `("count" ,(length morphology)))
	    ,@(when cpos `("cpos" ,cpos))
            "norm" ,(or norm :null)
            "dipl_xml" ,(or dipl-xml :null)
            "norm_xml" ,(or norm-xml :null)
            "stored_norm" ,(if stored-norm :true :false)
            "computed_norm" ,(if computed-norm :true :false)
	    "id" ,id
            "wid" ,(or wid :null)
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
				 do #m(gnc:tmesis/))))))
            "status" ,(or status :null)
            "comment" ,(or comment :null))))))))

(defparameter *feature-name-table* (make-hash-table :test #'equal))

(defun gnc-to-ud-features (features)
  (format nil "~{~a~^ ~}"
	  (loop for f in (u:split features #\space)
	     for ud = (car (gethash f *feature-name-table*))
	     when (and ud (not (equal ud "-")))
	     collect (string-left-trim "*" ud))))

(defun remove-dep-edge-features (features)
  (format nil "~{~a~^ ~}"
	  (loop for f in (u:split features #\space)
	     unless (find (char f 0) ">@")
	     collect f)))

(defparameter *suppressed-features* nil)

(defun load-suppressed-features (&optional file)
  (setf *suppressed-features*
	(u:collecting
	  (u:with-file-lines (line (or file "projects:gnc;text-tool;static;suppressed-features.txt"))
	    (let ((f (string-trim '(#\space #\tab) line)))
	      (unless (or (string= f "")
			  (char= (char f 0) #\#))
		(u:collect f)))))))

(defvar *tagset*)

(when (probe-file "projects:gnc;text-tool;static;suppressed-features.txt")
  (load-suppressed-features))

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
	 do (destructuring-bind (l f &optional flag trace ids nix count) reading
	      (declare (ignore ids nix)) ;; equivalent readings after filtering; not used here
	      (unless (and suppress-discarded-p (eq flag :discarded-cg))
		(u:collect (apply #'st-json::jso
				  `(,@(when lemma `("lemma" ,l))
				      ,@(unless suppress-discarded-p
						`("status" ,(string-downcase flag)))
				      ,@(when features `("features" ,(u:split f #\Space)))
				      ,@(when rid `("rid" ,rid))
				      ,@(when manual `("manual" :null))
				      ,@(when show-rules `("rules" ,trace))
                                      ,@(when count `("count" ,count))
                                      ,@(when trace `("trace" ,trace)))))))))))

(defmethod select-reading ((text parsed-text) &key wid rid)
  (let* ((token (gethash wid (parse::word-id-table text)))
         (readings (getf token :morphology))
         (reading (nth rid readings)))
    (dolist (reading readings)
      (when (eq (caddr reading) :selected-manually)
        (setf (caddr reading) :discarded-manually)))
    (setf (caddr reading) :selected-manually
          (cadr reading) (u:concat (cadr reading) " <Sel>"))))

;; *text*

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
				 (getf token :parent))
                           :children))
	 when (getf token :subtoken)
	 do (let ((subtoken (getf token :subtoken)))
	      (setf (getf (aref (depid-array text) (getf subtoken :self)) :token) (- -1 i))
	      (unless (or (null (getf subtoken :parent)) (= (getf subtoken :parent) -1))
		(pushnew (getf subtoken :self)
			 (getf (aref (depid-array text)
				     (getf subtoken :parent))
                               :children))))))
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
		      #|(rel-start (search " >" features :from-end t))
		      (rel-end (when rel-start (position #\space features :start (+ 2 rel-start))))
		      (relation (when rel-start (subseq features (+ 2 rel-start) rel-end)))|#
                      (relation (getf token :label))
		      (word (getf token :word))
		      (mwe (when (>= token-id 0)
			     (loop for id from (1+ token-id) below (length token-array)
				   for token = (aref token-array id)
				   while (equal "<MWE>" (cadar (getf token :morphology)))
				   collect (getf token :word)))))
		 (setf (gethash node-id token-table)
		       (make-token-list :terminal-p t
					:id node-id
                                        :wid (getf token :wid)
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

(defstruct token-list
  terminal-p
  id
  word
  head
  relation
  slashee-ids
  slash-relations
  atts
  wid)

;; *text*
;; kp::*session*

(defmethod build-dep-graph ((text parsed-text) &key node-id (add-root t) &allow-other-keys)
  (assert (not (null node-id)))
  (multiple-value-bind (token-table node-id) (get-token-table text :node-id node-id)
    (let ((node-table (make-hash-table :test #'equal)) ;; rehashed in display-dep-graph() below; populated where?
	  (children-table (make-hash-table :test #'equal))
	  (slash-nodes (make-hash-table :test #'equal))
	  (roots ()))
      (maphash (lambda (token-id tl)
		 ;; id might be different from token-list-id when there are gaps in original ids; don't use it!
                 (let* ((id (token-list-id tl)) ;; this is different from build-dep-graph() for dep-parsed-sentence!
			(children (gethash id children-table))
			(node (when (listp children) ;; if not, token describes existing node with new slash
				(make-instance 'dep-linear-node
					       :id id
                                               :token-id token-id
                                               :wid (token-list-wid tl)
                                               :node-table node-table
					       ;;  all nodes are in text order (= id)
					       :node-id (or (token-list-id tl) id)
					       :type (if (token-list-terminal-p tl)
							 :surface-form
							 :node)
					       :relation (or (token-list-relation tl) "--") 
					       :label (or (token-list-word tl) "--")
					       :atts (token-list-atts tl)
					       :children children)))
			(parent (when node (gethash (or (token-list-head tl) -1) children-table))))
		   ;;(debug node)
		   (when node
		     (setf (gethash id children-table) node
			   (gethash id node-table) node)
		     (when (= id node-id) (push node roots))
		     (cond ((listp parent)
			    (u:append* (gethash (or (token-list-head tl) -1) children-table) node))
			   (t
			    (u:append* (node-children parent) node))))
		   (mapc (lambda (slashee-id slash-relation)
			   (let ((slash-node (make-instance 'dep-linear-slash-node
							    :id slashee-id
							    :node-table node-table
							    :relation slash-relation
							    :label "/"
							    :atts (list :|lemma| "/")
							    :slashee slashee-id)))
			     (if node
				 (push slash-node (gethash node slash-nodes))
				 (push slash-node (gethash children slash-nodes))))) ;; children is node!
			 (token-list-slashee-ids tl)
			 (token-list-slash-relations tl))))
	       token-table)
      (maphash (lambda (node slash-node-list)
		 (declare (ignore node))
		 (dolist (slash-node slash-node-list)
		   (when (slashee slash-node)
		     (when (gethash (slashee slash-node) node-table)
		       (setf (slashee slash-node) (gethash (slashee slash-node) node-table))
		       (push (cons :parent slash-node) (slash-parents (slashee slash-node)))))))
	       slash-nodes)
      ;; *node-table*
      (let ((root (if (and (not add-root) roots (null (cdr roots)))
		      (car roots)
		      (setf (gethash 0 node-table)
			    (make-instance 'dep-linear-node
					   :id 0
					   :node-id 0
					   :node-table node-table
					   :label "ROOT"
					   :sort "root" ;, ??
					   :children roots)))))
	(labels ((set-parents (node &optional parent)
		   #+debug(print (list :parent parent))
		   (when (node-parents node) (warn "setting ~s to ~s" (node-parents node) parent))
		   (when parent (setf (node-parents node) (list parent)))
		   (setf (slot-value node 'graph::level)
			 (if parent (1+ (graph::node-level parent)) 0))
		   (mapc (lambda (child) (set-parents child node)) (node-children node))
		   ;; set left-leaf-id; used to sort children
		   (setf (left-leaf-id node)
			 (if (node-children node)
			     (reduce #'min (node-children node) :key #'left-leaf-id)
			     (node-id node))
			 (node-children node) (sort (node-children node) #'<  :key #'left-leaf-id)))
		 (set-slash-nodes (node)
		   (dolist (slash-node (gethash node slash-nodes))
		     (setf (node-parents slash-node) (list node))
		     (push slash-node (slash-nodes node))
		     (if (node< (slashee slash-node) node)
			 (push slash-node (node-children node))
			 (u:append* (node-children node) slash-node)))
		   (mapc #'set-slash-nodes (node-children node))))
	  #+debug(print :set-parents)
	  (set-parents root)
	  #+debug(print :set-slash-nodes)
	  (set-slash-nodes root)
	  #+debug(print :done))
	(setf *root* root)
	root))))

#| CONLL-X (http://ilk.uvt.nl/conll/#dataformat)

Field number:	Field name:	Description:
1	ID	Token counter, starting at 1 for each new sentence.
2	FORM	Word form or punctuation symbol.
3	LEMMA	Lemma or stem (depending on particular data set) of word form, or an underscore if not available.
4	CPOSTAG	Coarse-grained part-of-speech tag, where tagset depends on the language.
5	POSTAG	Fine-grained part-of-speech tag, where the tagset depends on the language, or identical to the coarse-grained part-of-speech tag if not available.
6	FEATS	Unordered set of syntactic and/or morphological features (depending on the particular language), separated by a vertical bar (|), or an underscore if not available.
7	HEAD	Head of the current token, which is either a value of ID or zero ('0'). Note that depending on the original treebank annotation, there may be multiple tokens with an ID of zero.
8	DEPREL	Dependency relation to the HEAD. The set of dependency relations depends on the particular language. Note that depending on the original treebank annotation, the dependency relation may be meaningful or simply 'ROOT'.
9	PHEAD	Projective head of current token, which is either a value of ID or zero ('0'), or an underscore if not available. Note that depending on the original treebank annotation, there may be multiple tokens with an ID of zero. The dependency structure resulting from the PHEAD column is guaranteed to be projective (but is not available for all languages), whereas the structures resulting from the HEAD column will be non-projective for some sentences of some languages (but is always available).
10	PDEPREL	Dependency relation to the PHEAD, or an underscore if not available. The set of dependency relations depends on the particular language. Note that depending on the original treebank annotation, the dependency relation may be meaningful or simply 'ROOT'.
|#

;; *text*

#+test
(write-dependencies-conll *text* *standard-output*)

(defparameter *graph* nil)

(defmethod write-dependencies-conll ((text parsed-text) stream &key (start 1) &allow-other-keys)
  (let ((token-array (text-array text)))
    (decf start)
    (loop for node across token-array
          when (and (getf node :stored-label)
                    (not (getf node :stored-parent)))
          do (let ((graph (build-dep-graph text :node-id (getf node :self))))
               (setf *graph* graph)
               (write-dependency-conll graph stream :sentence-id (incf start))))))

#+test
(write-dependency-conll *graph* *standard-output*)

(defun morph-to-ud (morph &key drop-pos)
  (let ((features (u:split morph #\space)))
    (setf features (delete-if (lambda (f) (find #\> f)) features))
    (when drop-pos (pop features))
    (format nil "~{~a~^ ~}" features)))

(defmethod write-dependency-conll ((graph dep-node) stream &key text sentence-id &allow-other-keys)
  (let ((nodes ()))
    (labels ((walk (node)
               (let ((atts (node-atts node))
                     (parent (if (node-parents node)
                                 (node-id (car (node-parents node)))
                                 0)))
                 (unless (zerop (node-id node))
                   (push (list (node-id node)
                               parent
                               (node-label node)
                               (relation node) 
                               (getf atts :|lemma|)
                               (getf atts :|pos|)
                               (morph-to-ud (getf atts :|morph|) :drop-pos t))
                         nodes)))
               (mapc #'walk (node-children node))))
      (walk graph)
      (setf nodes (sort nodes #'< :key #'car))
      (format stream "# sent_id: ~a~%# sentence: ~{~a~^ ~}~%"
              sentence-id (mapcar #'caddr nodes))
      (loop for (id parent word label lemma pos morph) in nodes
            do (format stream "~a	~a	~a	~a	~a	~a	~a	~a	_	_~%"
                       id word lemma pos pos morph parent (string-downcase label)))
      (terpri stream))))


:eof
