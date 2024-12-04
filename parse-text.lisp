;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: PARSE; Base: 10 -*-

(in-package :parse)

(defclass parsed-text ()
  ((name :initform nil :initarg :name :reader name)
   (variety :initform :ng :initarg :variety :accessor variety)
   (orthography :initform :standard :initarg :orthography :reader orthography)
   (location :initform nil :initarg :location :reader location) ;; path to doc location
   (document :initform nil :initarg :document :reader document) ;; document name in corpus
   (corpus :initform nil :initarg :corpus :reader corpus)
   (text-array :initform (make-array 0 :adjustable t :fill-pointer t) :accessor text-array)
   ;; word-id (as 'w<wid>') -> index in text-array
   (word-id-table :initform (make-hash-table :test #'equal) :reader word-id-table) 
   (lexicon :initform (dat:make-string-tree) :accessor text-lexicon)
   (disambiguation-table :initform (make-hash-table :test #'equal) :reader disambiguation-table) 
   (depid-array :initform nil :accessor depid-array) ;; dependency node ids
   (start-wid :initform nil :initarg :start-wid :accessor start-wid)
   (end-wid :initform nil :initarg :end-wid :accessor end-wid)
   ;; stored reading by wid, where xml:id = "w<wid>"
   (wid-table :initform (make-hash-table) :reader wid-table)
   ;; (guess-table :initform (make-hash-table :test #'equal) :reader guess-table)
   ))

(defmethod token-array ((text parsed-text))
  (text-array text))

(defmethod transliterate ((language t) str)
  str)

;; change this
(defmethod transliterate ((language (eql :abk)) str)
  str
  (kp::transliterate str :standard :abkhaz))


;; to be overridden
(defmethod text-class ((variety t))
  'parsed-text)

(defconstant +wrong-abk-chars+ "ԥԤӷӶӌӋ")
(defconstant +correct-abk-chars+ "ҧҦҕҔҷҶ")

(defun normalize-characters (text)
  (loop for c across text for i from 0
     for pos = (position c +wrong-abk-chars+)
     when pos
     do (setf (char text i) (char +correct-abk-chars+ pos))))

#+test
(parse-text "ала" :variety :abk)

(defmethod parse-text ((text string) &key variety load-grammar (disambiguate t) corpus
                                       orthography
                                       dependencies
                                       unknown-tree ;; obsolete?
                                       guess-scope guess-table interactive
                                       count cg3-variables &allow-other-keys)
  (assert variety)
  (when (eq variety :kat) (setf variety :ng))
  (when (eq variety :abk)
    (normalize-characters text))
  (let* ((parsed-text (make-instance (text-class variety)
                                     :orthography orthography
                                     :corpus corpus))
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
                  :orthography (orthography parsed-text)
                  :guess-table guess-table
                  :dependencies dependencies
                  ;;:unknown-tree unknown-tree
                  :interactive interactive)
    (when disambiguate
      (process-text parsed-text :disambiguate
                    :variety variety
                    :load-grammar load-grammar
                    :sentence-end-strings (if (eq variety :abk)
                                              '("." "?" "!" "…" ";" ":")
                                              '("." "?" "!" "…" ";" ":"))
                    :guess-scope guess-scope
                    :guess-table guess-table
                    :interactive interactive
                    :dependencies dependencies
                    :cg3-variables cg3-variables))
    parsed-text))

;; pre-tokenized text, given as list of tokens, where each token is (word . rest). rest is kept unchanged.
(defmethod parse-text ((tokens list) &key variety load-grammar (disambiguate t)
                                       ;; unknown-tree
                                       guess-scope guess-table
                                       cg3-variables interactive dependencies
                                       &allow-other-keys)
  (assert variety)
  (when (eq variety :kat) (setf variety :ng))
  (let ((parsed-text (make-instance (text-class variety))))
    (dolist (tl tokens)
      (destructuring-bind (token . rest) tl
        (unless (or (equal token "@@@")
                    (u:null-or-empty-string-p token))
          (setf token (normalize-token token))
          (vector-push-extend (list :word token :rest rest) (text-array parsed-text)))))
    (process-text parsed-text :analyze
                  :variety variety
                  :guess-table guess-table
                  ;; :unknown-tree unknown-tree
                  :dependencies dependencies
                  :interactive interactive)
    (when disambiguate
      (process-text parsed-text :disambiguate
                    :variety variety
                    :load-grammar load-grammar
                    :guess-scope guess-scope
                    :guess-table guess-table
                    :cg3-variables cg3-variables
                    :dependencies dependencies
                    :sentence-end-strings (if (eq variety :abk)
                                              '("." "?" "!" "…" ";" ":")
                                              '("." "?" "!" "…" ";" ":"))))
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

(defmethod parse-text ((text parsed-text) &key variety mode load-grammar (disambiguate t)
                                            wid-table ;; unknown-tree
                                            guess-scope cg3-variables
                                            guess-table interactive dependencies
                                            pos-only ;; menota: use only lemma and POS tag in :analyze
                                            &allow-other-keys)
  (declare (ignore guess-scope))
  (when (eq variety :kat) (setf variety :ng))
  (setf *text* text)
  (case mode
    (:redisambiguate
     (loop for token across (text-array text)
           do (setf (getf token :subtokens) ())))
    (:renormalize
     (process-text text :normalize :variety variety))
    (otherwise
     (process-text text :analyze
                   :interactive interactive
                   :variety variety
                   :dependencies dependencies
                   ;; :unknown-tree unknown-tree
                   :guess-table guess-table
                   :pos-only pos-only)))
  (when disambiguate
    (process-text text :disambiguate
                  :interactive interactive
                  :variety variety
                  :load-grammar load-grammar
                  :mode mode
                  :cg3-variables cg3-variables
                  :dependencies dependencies
                  :sentence-end-strings (if (eq variety :abk) ;; put this in to text object!
                                            '("." "?" "!" "…" ";" ":")
                                            '("." "?" "!" "…" ";" ":"))))
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

(defparameter *break* nil)

#+test
(setf *break* t)

;; measure ambiguity
(defparameter *ambiguity-array* (make-array 500 :initial-element 0))
(defparameter *lemma-ambiguity-array* (make-array 500 :initial-element 0))

#+test
(loop for c across *ambiguity-array* do (format t "~a, " c))
#+test
(loop for c across *lemma-ambiguity-array* do (format t "~a, " c))

;; lexicon is stored in .lex file in save-all-new-words() (obsolete!)
;; it stores the values of :new-morphology
(defmethod process-text ((text parsed-text) (mode (eql :analyze))
                         &key (variety :og)
                           (normalize t) ;; unknown-tree
                           ;; experimental; keeps MWE and non-MWE readings;
                           ;; MWE second (third) word are accessible in CG
                           ;; Therefore, some rules will have to be adapted
                           keep-non-mwe-readings
                           guess-table
                           interactive
                           unknown-only-p &allow-other-keys)
  ;; (print (list :process-text :mode :analyze :variety variety))
  (setf (text-lexicon text) (dat:make-string-tree))
  (let ((language (if (eq variety :abk) :abk :kat))
        (token-array (text-array text))
	(norm-table (make-hash-table :test #'equal)) ;; calculated anew from .lex file
	(lang-stack (list variety))
	(lexicon (text-lexicon text))
        (wid-table (wid-table text)))
    ;;#+test ;; *text*
    ;; (describe lexicon)
    ;;(print :load-wid-table)
    (unless interactive
      (load-wid-table text))
    ;;(print :wid-table-loaded)
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
      (loop with mwe-positions = () and start-seen = nil
	    for node across token-array
	    for i from 0
	    do (case (car node)
		 (:start-element
                  (push (let ((lang (or (getf node :|xml:lang|)
                                        (let ((atts (getf node :atts)))
                                          (when atts
                                            (let ((start (search "xml:lang=\"" atts)))
                                              (when start
                                                ;;;(print (list start atts))
                                                (subseq atts
                                                        (+ start 10)
                                                        (position #\" atts :start (+ start 11))))))))))
			  (cond ((null lang)
				 nil)
                                ((eq variety :abk)
                                 (intern (string-upcase lang) :keyword))
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
                        lang-stack)
                  (print (list :pushed (cadr node) lang-stack))
                  (setf start-seen t))
		 (:end-element
                  (when (and start-seen (cdr lang-stack)) ;; have to avoid popping away the top lang
                    ;; this could happen when we start deeper in the tree than the level we have reached now
                    (pop lang-stack)))
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
					       (cond ((and normalize (> (length token) 1))
                                                      ;; did include / before! fix this!
                                                      (remove-if (lambda (c) (find c "()[]/\\")) token))
						     (t
						      token))))
			   (readings (when token ;; TODO: use second value (norm)
				       (lookup-morphology language normalized-token
							  :variety variety
                                                          :guess-table guess-table
							  :orthography (orthography text))))
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
                      (when (and (find-if-not (lambda (c) (find c ".:,;-+?!“”«»()[]1234567890")) token)
                                 (< (aref *ambiguity-array* 0) 100000)
                                 readings)
                        (incf (aref *ambiguity-array* 0))
                        (incf (aref *ambiguity-array* (length readings)))
                        (incf (aref *lemma-ambiguity-array* 0))
                        (incf (aref *lemma-ambiguity-array*
                                    (let ((lemmas ()))
                                      (dolist (r readings)
                                        (pushnew (car r) lemmas :test #'string=))
                                      (length lemmas)))))
                      #+ignore
                      (when (and (null readings) unknown-tree)
                        (incf (dat:string-tree-get unknown-tree normalized-token 0)))
		      (let* ((mwe2-reading (when (and (car next-token+j2)
                                                      (not (eq language :abk))
                                                      (not (and (eq language :abkx) (not (eq language variety))))
						      (not (find (char token 0) ",;.:?!“”«»–"))
						      (not (find (char (car next-token+j2) 0)
								 ",;.:?!“”«»")))
                                             (lookup-morphology language
                                                                (u:concat token " " (car next-token+j2))
                                                                :variety variety
                                                                :mwe t
                                                                :orthography (orthography text))))
			     (mwe3-reading (when (and (not mwe2-reading)
						      (not (and (eq language :abkx) (not (eq language variety))))
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
		        (when *break* (setf *break* nil) (break))
                        (cond ((find i mwe-positions)
			       (setf (getf (cddr node) :morphology)
				     (if keep-non-mwe-readings
					 (cons (list "" "<MWE>" nil nil) readings)
					 (list (list "" "<MWE>" nil nil)))))
			      ((or readings (eq language :abk)) 
                               (when wid-table
                                 (let ((wid (or (getf node :wid) ;; from corpus att
                                                (getf node :|xml:id|)))) ;; from xml text
                                   (when (and wid (string/= wid ""))
                                     (let* ((id (parse-integer wid :start 1))
                                            (reading (gethash id wid-table)))
                                       (when reading
                                         (destructuring-bind (&optional w l fl label parent status comment)
                                             reading
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
							  :tmesis-segment (if rest :infix :verb)
							  :variety variety))))))
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
				      :count 0))))))))))))

#+test
(process-text *text* :disambiguate :variety :non :mode :redisambiguate)

#+test
(process-text *text* :analyze)

(defmethod process-text ((text parsed-text) (mode (eql :disambiguate))
                         &key (variety :og) (load-grammar t) mode
                           (sentence-end-strings vislcg3::*sentence-end-strings*)
                           guess-scope guess-table cg3-variables (dependencies t) remove-brackets
                           &allow-other-keys)
  (print :cg3)
  (vislcg3::cg3-disambiguate-text text
                                  :variety variety
                                  :mode mode
                                  :load-grammar load-grammar
				  :sentence-end-strings sentence-end-strings
                                  :sentence-start-is-uppercase (eq variety :abk)
                                  :guess-scope guess-scope
                                  :guess-table guess-table
                                  :remove-brackets remove-brackets
                                  :dependencies dependencies
                                  :variables cg3-variables)
  (print :cg3-done)
  (when dependencies
    (let ((token-array (text-array text))
          (word-id-table (word-id-table text))
          (wid-table (wid-table text)))
      (labels ((add-relation (node)
                 (when node
                   (let* ((msa (getf node :morphology))
                          (reading (or (find-if (lambda (reading)
					          (and (not (find :discarded-cg reading))
						       (search " >" (cadr reading) :from-end t)))
					        msa)
				       (find-if-not (lambda (r) (find :discarded-cg r)) msa)))
		          (features (cadr reading))
		          (rel-start (search " >" features :from-end t))
		          (rel-end (when rel-start (position #\space features :start (+ 2 rel-start))))
	                  (relation (when rel-start (subseq features (+ 2 rel-start) rel-end))))
                     (setf (getf (cddr node) :label) relation)))))
        (loop for node across token-array
              do (add-relation node)
              (mapc #'add-relation (getf node :subtokens))))
      ;; fix stored-parent and subtokens
      (loop for node across token-array ;; for i from 0
            for p = (getf node :stored-parent)
            for l = (getf node :stored-label)
            when (and p (listp p)) ;; list ensures that this is done only once, but not on redisambiguation
            do (setf (getf node :stored-parent)
                     (getf (gethash (format nil "w~a" (car p)) word-id-table) :self))
            when (or (and p (listp p)) (and (null p) l))
            do
            (dolist (subnode (getf node :subtokens))
              (let ((wid-list (gethash (parse-integer (getf subnode :wid) :start 1) wid-table)))
                ;;(debug wid-list)
                (setf (getf subnode :stored-parent)
                      (getf (gethash (format nil "w~a" (nth 4 wid-list)) word-id-table) :self)
                      (getf subnode :stored-label)
                      (nth 3 wid-list))))))))

;; kp::*text*

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
                              transliterate
                              &allow-other-keys)
  (let ((st-json:*output-literal-unicode* t)
        (left-context ())
        (right-context ())
        (window 20))
    (u:collecting-into (left tokens right)
      (loop with id = 0 and sub-count = 0
         for node across (text-array text)
         for cpos = (getf node :cpos)
	 do (cond ((if (and start end)
                       (<= start id (+ end sub-count))
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
                                                              :manual manual
                                                              :transliterate transliterate)
                                        (incf id)))
                                     (:start-element
                                      (st-json:jso "struct" (getf node :start-element)
                                                   "type" "start-element"
                                                   "atts" (or (getf node :atts) :null)))
                                     (:end-element
                                      (st-json:jso "struct" (getf node :end-element)
                                                   "type" "end-element"
                                                   "atts" (or (getf node :atts) :null)))
                                     (:empty-element
                                      (st-json:jso "struct" (getf node :empty-element)
                                                   "type" "empty-element"
                                                   "atts" (or (getf node :atts) :null)))))
                   (dolist (subtoken (getf node :subtokens))
                     (u:collect-into tokens
                                     (prog1 (write-word-json subtoken
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
                                                             :manual manual
                                                             :transliterate transliterate
                                                             :subtoken t)
                                       (incf id)))
                     (incf sub-count)))
                  ((and start end (<= (- start window) id start))
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
                  ((and start end (< end id (+ end window sub-count)))
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
                               transliterate
			       no-morphology
			       no-newlines
                               subtoken)
  (declare (ignore tracep split-trace no-newlines error disambiguate))
  (ecase (car node)
    (:word
     (destructuring-bind (&key word morphology facs dipl norm dipl-xml facs-xml norm-xml
                               stored-norm computed-norm
                               stored-facs computed-facs
                               |id| cpos comment wid status token-status &allow-other-keys)
         node
       (declare (ignore |id| dipl))
       (let* ((morphology (unless no-morphology morphology))
	      (tmesis-msa (unless no-morphology (getf (cddr node) :tmesis-msa))))
         (declare (ignore tmesis-msa))
	 (apply
	  #'st-json::jso
	  `("word" ,(kp::transliterate (or dipl word) :standard transliterate)
	    ,@(when dependencies 
		    `("self" ,(or (unless (eql (getf node :self) 0) (getf node :self)) :null)
		      "parent" ,(or (unless (eql (getf node :parent) -1) (getf node :parent)) :null)
                      "label" ,(or (getf node :label) :null)
                      "stored_parent" ,(or (getf node :stored-parent) :null)
                      "stored_label" ,(or (getf node :stored-label) :null)))
	    ,@(when count `("count" ,(length morphology)))
	    ,@(when cpos `("cpos" ,cpos))
            "norm" ,(or norm :null)
            "facs" ,(or facs :null)
            "dipl_xml" ,(or dipl-xml :null)
            "facs_xml" ,(or facs-xml :null)
            "norm_xml" ,(or norm-xml :null)
            "stored_norm" ,(if stored-norm :true :false)
            "computed_norm" ,(if computed-norm :true :false)
            "stored_facs" ,(if stored-facs :true :false)
            "computed_facs" ,(if computed-facs :true :false)
	    "id" ,id
            "wid" ,(or wid :null)
            ,@(when subtoken `("subtoken" :true))
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
					      :manual manual
                                              :transliterate transliterate))
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
            "token_status" ,(if token-status (string-downcase token-status) :null)
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
				    show-rules
                                    transliterate)
  (let ((morphology (filter-morphology morphology :tagset :full-tagset #+orig *tagset*)))
    (u:collecting
      (loop for reading in morphology
	 for rid from 0
	 do (destructuring-bind (l f &optional flag trace ids nix count) reading
	      (declare (ignore ids nix)) ;; equivalent readings after filtering; not used here
	      (unless (and suppress-discarded-p (eq flag :discarded-cg))
		(u:collect (apply #'st-json::jso
				  `(,@(when lemma `("lemma" ,l))
				      ,@(when (and lemma transliterate)
                                          `("trans_lemma" ,(kp::transliterate l :standard transliterate)))
				      ,@(unless suppress-discarded-p
						`("status" ,(string-downcase flag)))
				      ,@(when features `("features" ,(u:split f #\Space)))
				      ,@(when rid `("rid" ,rid))
				      ,@(when manual `("manual" :null))
				      ,@(when show-rules `("rules" ,trace))
                                      ,@(when count `("count" ,count))
                                      ,@(when trace `("trace" ,trace)))))))))))

(defmethod select-reading ((text parsed-text) stream &key wid rid)
  (let* ((token (gethash wid (parse::word-id-table text)))
         (readings (getf token :morphology))
         (reading (nth rid readings)))
    (dolist (reading readings)
      (when (eq (caddr reading) :selected-manually)
        (setf (caddr reading) :discarded-manually))
      (setf (cadr reading)
            (format nil "~{~a~^ ~}" (delete "<Sel>" (u:split (cadr reading) #\space) :test #'string=))))
    (setf (caddr reading) :selected-manually
          (cadr reading) (u:concat (cadr reading) " <Sel>"))
    (json "token" (parse::write-word-json token))))

;; *text*
;; *root*

#+test
(parse-text "გაიშვირა მაგიდისკენ, რომელიც ეს იყო დატოვა: გატაცებაა"
            :stream *standard-output* :variety :ng :dependencies t)

;; todo: remove duplication of get-val here and in function below
(defmethod initialize-depid-array ((text parsed-text) &key (diff (list nil)) stored (subtoken-count 0))
  (let ((token-array (token-array text)))
    (labels ((get-val (token att stored-att &optional reg)
               (cond ((not stored)
                      (getf token att))
                     (t
                      (let ((stored-val (getf token stored-att))
                            (val (getf token att)))
                        (when (and reg
                                   (not (equal val stored-val))
                                   (not (and (null stored-val)
                                             (or (eq val -1) (equal val "ROOT")))))
                          (setf (car diff) t)))
                      (getf token stored-att)))))
      (setf (depid-array text)
	    (make-array (+ 1 (length token-array) subtoken-count) :initial-element ()))
      (loop for i from 0
	    for token across token-array
            for parent = (if stored
                             (get-val token :parent :stored-parent)
                             (getf token :parent))
            when (and (eq (car token) :word) (getf token :self))
	    do (setf (getf (aref (depid-array text) (getf token :self)) :token) i)
	    unless (or (null parent) (= parent -1))
	    do (pushnew (getf token :self)
		        (getf (aref (depid-array text) parent) :children))
	    do (loop for subtoken in (getf token :subtokens)
                     for sid from 1
	             do (setf (getf (aref (depid-array text) (getf subtoken :self)) :token)
                              (+ i (ash sid 32)))
                     (unless (or (null (getf subtoken :parent)) (= (getf subtoken :parent) -1))
                       (pushnew (getf subtoken :self)
			        (getf (aref (depid-array text)
				            (getf subtoken :parent))
                                      :children))))))))

(defmethod prefix-token ((text parsed-text) word subtokens)
  (u:concat (subseq word 0 (- (length word)
                              (reduce #'+ subtokens
                                 :key (lambda (token) (1- (length (getf token :word))))
                                 :initial-value 0)))
            "_"))

(defparameter *token-table* nil)

(defmethod get-token-table ((text parsed-text) &key node-id stored &allow-other-keys)
  (assert node-id)
  (let* ((id-table (make-hash-table :test #'equal))
	 (secedge-table (make-hash-table :test #'equal))
	 (token-table (make-hash-table :test #'equal))
	 (parent-id nil)
	 (word nil)
	 (token-array (token-array text))
	 (parents ())
         (diff (list nil))
	 ;; (subtoken-count (count-if (lambda (token) (getf token :subtoken)) token-array))
	 (subtoken-count (reduce #'+ token-array
                                 :key (lambda (token) (length (getf token :subtokens)))
                                 :initial-value 0)))
    (declare (ignore id-table))
    (labels ((get-val (token att stored-att &optional reg)
               (cond ((not stored)
                      (getf token att))
                     (t
                      (let ((stored-val (getf token stored-att))
                            (val (getf token att)))
                        (when (and reg
                                   (not (equal val stored-val))
                                   (not (and (null stored-val)
                                             (or (eq val -1) (equal val "ROOT")))))
                          (setf (car diff) t)))
                      (getf token stored-att)))))
      (unless nil ;; (depid-table text) ;; maps dep node-ids to token-table ids
        (initialize-depid-array text :diff diff :stored stored :subtoken-count subtoken-count))
      (labels ((walk (node-id)
	         (let* ((t-ch-list (aref (depid-array text) node-id))
		        (token-id (getf t-ch-list :token))
                        (subtoken-pos (ash token-id -32))
                        (tid (logand token-id (- (ash 1 32) 1)))
                        (base-token (aref token-array tid))
		        (token (if (zerop subtoken-pos)
				   base-token
				   (nth (1- subtoken-pos) (getf (aref token-array tid) :subtokens))))
                        (base-wid (getf base-token :wid))
                        (wid (if (zerop subtoken-pos) ;; not subtoken
                                 base-wid
                                 (format nil "w~a" (+ (parse-integer base-wid :start 1) subtoken-pos))))
		        (children (getf t-ch-list :children))
		        (parent (if stored
                                    (get-val token :parent :stored-parent t)
                                    (getf token :parent)))
		        (msa (getf token :morphology))
		        (reading (or (find-if (lambda (reading)
					        (and (not (find :discarded-cg reading))
						     (search " >" (cadr reading) :from-end t)))
					      msa)
				     (find-if-not (lambda (r) (find :discarded-cg r)) msa)))
		        (lemma (car reading)) ;; only first reading is considered!
		        (features (cadr reading))
		        (pos (subseq features 0 (position #\space features)))
                        (relation (if stored
                                      (get-val token :label :stored-label t)
                                      (getf token :label)))
		        (word (getf token :word))
		        (mwe (when (>= token-id 0)
			       (loop for id from (1+ token-id) below (length token-array)
				     for token = (aref token-array id)
				     while (equal "<MWE>" (cadar (getf token :morphology)))
				     collect (getf token :word)))))
                   (setf (gethash node-id token-table)
		         (make-token-list :terminal-p t
					  :id node-id
                                          :wid wid
                                          :word (cond (mwe
						       (format nil "~a~{ ~a~}" word mwe))
						      ((getf token :subtokens)
                                                       (prefix-token text word (getf token :subtokens))
                                                       #+old
						       (subseq word 0 (1- (length word))))
						      (t
						       word))
					  :atts (list :|lemma| lemma :|pos| pos :|morph| features)
					  :relation relation
					  :head parent))
		   (mapc #'walk children))))
        (walk node-id)))
    (let ((ids (sort (u:collecting
		       (maphash (lambda (id tl)
				  (declare (ignore id))
				  (u:collect (token-list-id tl)))
				token-table))
		     #'<)))
      (setf *token-table* token-table)
      (maphash (lambda (id tl)
		 (declare (ignore id))
		 (let ((pos (position (token-list-id tl) ids)))
		   (when pos
		     (setf (token-list-id tl) (1+ pos))
		     (unless (eq (token-list-head tl) -1)
		       (let ((pos (position (token-list-head tl) ids)))
			 (when pos
			   (setf (token-list-head tl)
				 (1+ (position (token-list-head tl) ids)))))))))
	       token-table)
      (values token-table (1+ (position node-id ids)) (car diff)))))

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

(defmethod build-dep-graph ((text parsed-text) &key node-id (add-root t) stored &allow-other-keys)
  (assert (not (null node-id)))
  (multiple-value-bind (token-table node-id diff)
      (get-token-table text :node-id node-id :stored stored)
    ;;(print (list stored :diff diff))
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
		   (when node
		     (setf (gethash id children-table) node
			   (gethash id node-table) node)
		     (when (= id node-id) (push node roots))
		     (cond ((listp parent)
                            (let ((head (gethash (or (token-list-head tl) -1) children-table)))
                              (if (listp head)
                                  ;; the normal case
			          (u:append* (gethash (or (token-list-head tl) -1) children-table) node)
                                  ;; only happens when head is not the head of the structure
                                  (setf (gethash (or (token-list-head tl) -1) children-table)
                                        (list head node)))))
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
	(values root diff)))))

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
(write-dependencies-conll *text* *standard-output* :language :kat)

(defparameter *graph* nil)

#+test
(print (graph-is-complete *graph*))

(defmethod graph-is-complete ((graph dep-node))
  (labels ((find-last (node)
             (cond ((node-children node)
                    (find-last (car (last (node-children node)))))
                   (t
                    node))))
    (let ((word (node-label (find-last graph))))
      (not (find-if-not (lambda (c) (find c ".:!?»“”«)")) word)))))

(defmethod write-dependencies-conll ((text parsed-text) stream
                                     &key (start 1)
                                       document-id language
                                       all ;; include also analyses that are not stored; used for on-the-fly parsing
                                       &allow-other-keys)
  (let ((token-array (text-array text)))
    ;;(decf start)
    (loop for node across token-array
          when (or (and all (= -1 (getf node :parent)))
                   (and (getf node :stored-label)
                        (not (getf node :stored-parent))))
          do
          (let ((graph (build-dep-graph text
                                        :node-id (getf node :self)
                                        :stored (not all))))
            (setf *graph* graph)
            (when (or all (graph-is-complete graph))
              (write-dependency-conll graph stream
                                      :sentence-id (if document-id
                                                       (let ((slash-pos (position #\/ document-id :from-end t)))
                                                         (u:concat (if slash-pos
                                                                       (subseq document-id (1+ slash-pos))
                                                                       document-id)
                                                                   "+" (getf node :wid)))
                                                       (getf node :wid))
                                      :include-postag (not all)
                                      :language language))))))

#+test
(write-dependency-conll *graph* *standard-output* :language :kat)

;; abk only

;; fine-grained postag, which is simply the bag of features, except auxiliary features
(defun morph-to-postag (morph &key drop-pos)
  (let ((features (u:split morph #\space)))
    (setf features (delete-if (lambda (f) (find #\> f)) features))
    (when drop-pos (pop features))
    (format nil "~{~a~^_~}" features)))

(defmethod write-dependency-conll ((graph dep-node) stream
                                   &key text sentence-id (include-postag t) language &allow-other-keys)
  (let ((nodes ())
        (cop-count 0)
        (cop-nodes ())) ;; enclitic copula nodes
    (debug language)
    (labels ((walk (node)
               (let ((atts (node-atts node))
                     (parent (if (node-parents node)
                                 (node-id (car (node-parents node)))
                                 0)))
                 (unless (zerop (node-id node))
                   (let* ((morph (getf atts :|morph|))
                          (lemma (getf atts :|lemma|))
                          (word (node-label node)))
                     (multiple-value-bind (word clit clit-lemma clit-morph)
                         (if (eq language :abk)
                             (abk-split-clitics word (if (and (search "Cop" morph)
                                                              (not (equal lemma "а́кә-заа-ра")))
                                                         t))
                             word)
                       (push (list (node-id node)
                                   parent
                                   (node-label node)
                                   word
                                   lemma
                                   (morph-to-ud-pos language morph (relation node))
                                   (if include-postag (morph-to-postag morph :drop-pos nil) "_")
                                   (morph-to-ud language morph :lemma lemma)
                                   (relation node))
                             nodes)
                       (when clit
                         (push (node-id node) cop-nodes)
                         (push (list (node-id node)
                                     (node-id node)
                                     nil
                                     clit
                                     clit-lemma
                                     (morph-to-ud-pos language clit-morph "cop")
                                     (if include-postag (morph-to-postag clit-morph :drop-pos nil) "_")
                                     (morph-to-ud language clit-morph :lemma clit-lemma)
                                     "cop")
                               nodes))
                       ))))
               (mapc #'walk (node-children node))))
      (walk graph)
      (setf nodes (sort nodes #'< :key #'car))
      (when cop-nodes
        (loop for cop-pos in (sort cop-nodes #'>) 
              do (loop for node in nodes
                       when (or (> (car node) cop-pos)
                                (and (= (car node) cop-pos)
                                     (not (nth 2 node))))
                       do (incf (car node))
                       when (> (cadr node) cop-pos)
                       do (incf (cadr node))))
        (setf nodes (sort nodes #'< :key #'car)))
      (let* ((orig-text (format nil "~{~a~^ ~}" (delete-if #'null (mapcar #'caddr nodes))))
             (text (u:subst-substrings (format nil "~{~a~^ ~}" (mapcar #'cadddr nodes))
                                       '("_ _" "" " _" "" " ," "," " ." "." " !" "!" " ?" "?" " ;" ";" " :" ":")))
             (trans-text (transliterate language text)))
        (format stream "# sent_id = ~a~%# text = ~a~%# text-transcription = ~a~%"
                sentence-id text trans-text)
        (loop for ((id head is-cop form lemma cpostag postag feats deprel cop) next) on nodes
              for next-lemma = (nth 4 next)
              do (format stream "~a	~a	~a	~a	~a	~a	~a	~a	_	LMSeg:~a~a~%"
                         id
                         form
                         (strip-segmentation language lemma form)
                         ;;(if (equal lemma "а́кә-заа-ра") "а́кәзаара" lemma)
                         cpostag
                         postag
                         feats
                         head
                         #-orig(string-downcase deprel)
                         #+prelim(string-downcase (subseq deprel 0 (position #\: deprel)))
                         lemma
                         (if (and (debug next-lemma)
                                  (find (char next-lemma 0) ".:,;?!_"))
                             "|SpaceAfter=No" ""))))
      (terpri stream))))


:eof
