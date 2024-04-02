;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: PARSE; Base: 10 -*-

(in-package :parse)

(defparameter *abk-analyzer* nil)
(defparameter *abk-relax-analyzer* nil)
(defparameter *abk-tokenizer* nil)
(defparameter *abk-old-to-new-orth* nil)

#+test
(time (init-transducers :abk))

#+test
(cl-fst:fst-lookup
 *abk-relax-analyzer*
 "ушыриааиуа"
 (lambda (w l+f net)
   (declare (ignore w net))
   (print (list :relax l+f)))
 :mode :union)

#+test
(setf *abk-tokenizer*
      (make-instance 'cl-fst:fst-tokenizer
		     :token-boundary #.(string #\newline)
		     :file "projects:abnc;abk-tokenize.fst"))

(defvar *transducers-initialzed* nil)

(defmethod init-transducers ((language (eql :abk))
                             &key (transducer-dir "projects:abnc;")
                               &allow-other-keys)
  (let ((abk-orth-file "projects:abnc;abkhaz-old-to-new-orth.fst")
        (abk-file "projects:abnc;abkhaz.fst")
        (abk-relax-file "projects:abnc;abkhaz-relax.fst")
        ;;(abk-net nil)
        )
    (format t "~&Loading transducers for ~a…~%" language)
    (labels ((load-morph (file-list)
	       (u:collecting
		 (loop for name in file-list
		    do ;; (print name)
		      (format t "~&loading: ~s~%" name) 
		      (case name
			(:abkhaz-morph ;; not used!
			 (u:collect (make-instance 'cl-fst:fst-net :file abk-file :name :abk)))
			(:abkhaz-relax-morph ;; arbitrarily inserted schwas, not used!
			 (u:collect (make-instance 'cl-fst:fst-net :file abk-relax-file :name :abk-relax)))
			#+test
			(:abkhaz-morph
			 (u:collect (make-instance 'cl-foma:foma-net :file abk-file :name :abk)))
			#+test
			(:abkhaz-relax-morph ;; arbitrarily inserted schwas
			 (u:collect (make-instance 'cl-foma:foma-net :file abk-relax-file :name :abk-relax)))
			(:abkhaz-old-to-new-orth
			 (u:collect (make-instance 'cl-fst:fst-net :file abk-orth-file :name :abk)))

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
      (setf *abk-tokenizer*
            (make-instance 'cl-fst:fst-tokenizer :token-boundary #.(string #\newline)
                           :file "projects:abnc;abk-tokenize.fst"))
      (setf *abk-analyzer*
            (load-morph '("abkhaz-verb" "abkhaz-noun")))
      (setf *abk-relax-analyzer*
            (load-morph '("abkhaz-verb-relax" "abkhaz-noun-relax" "abkhaz-redup")))
      (setf *abk-old-to-new-orth*
            (load-morph '(:abkhaz-old-to-new-orth))))
    )
  (setf *transducers-initialzed* t)
  (print :done))

(defparameter *use-simple-dict-p* nil)

(defparameter *abkhaz-sorted-features* (make-hash-table :test #'equal))

(when t
  (clrhash *abkhaz-sorted-features*)
  (let ((fid 0))
    (u:with-file-lines (line "projects:abnc;sorted-features.txt")
      (u:trim line)
      (unless (or (string= line "")
		  (char= (char line 0) #\#))
	(setf (gethash line *abkhaz-sorted-features*) fid)
	(incf fid)))))

(defun sort-abkhaz-features (features)
  (sort features
	(lambda (f1 f2)
	  (< (gethash f1 *abkhaz-sorted-features* 1000)
	     (gethash f2 *abkhaz-sorted-features* 1000)))))

#+test
(print (lookup-abk-coord-compound "ишнеи-шнеиуа"))
#+test
(print (lookup-abk-coord-compound "ишааи-шааиуа"))
#+test
(print (lookup-abk-coord-compound "лхы-лҿы"))
#+test
(print (lookup-abk-coord-compound "О-о"))

;; two types of coordination compounds:
;; 1. two complete words with comparable features
;;    e.g., …гьы-…гьы coordinations, ихы-игәы, иҟаз-ианиз
;; 2. verb-verb, first part is prefixes + root
(defun lookup-abk-coord-compound (word &key (orthography :abk-cyr) print)
  (let ((hyphen-pos (position #\- word)))
    (when hyphen-pos
      (let* ((w1 (subseq word 0 (1+ hyphen-pos)))
	     (w2 (subseq word (1+ hyphen-pos)))
	     (hyphen nil)
	     (morph1 (lookup-morphology :abk w1 :orthography orthography :coord t))
	     (morph1 (or morph1
			 (and (setf hyphen t)
			      (lookup-morphology :abk (subseq word 0 hyphen-pos)
						 :orthography orthography :coord t))))
	     (morph2 (lookup-morphology :abk w2 :orthography orthography :coord t)))
        ;;(print (list morph1 morph2))
	(u:collecting
	  (loop for (lemma1 features1) in morph1
	        do (loop for (lemma2 features2) in morph2
	                 ;; do (print (list lemma1 lemma2))
		         when
		         (labels ((find-feature (f &rest features)
                                    ;; find f in features (if given) or in features2
				    (find f (cond ((null features)
					           features2)
					          ((listp (car features))
					           (car features))
					          (t
					           features))
				          :test #'string=)))
		           (cond ((find "VCoord" features1 :test #'string=)
			          (and
                                   (find "V" features2 :test #'string=)
                                   ;; equal stems
                                   (equal lemma1 (subseq lemma2 0 (1+ (position #\- lemma2 :from-end t)))) ;; ??
                                   (loop for f in features1
				         always (or (equal f "VCoord")
                                                    (find-feature f)
						    (and (equal f "FPv:аа")
						         (find-feature "FPv:на"))))
				   ;; check for equal prefixes
				   
				   ))
			         (t
			          (and (loop for f in features1
				             always (or (find-feature f "CC" "<Relax>" "<Deriv>" "[Det]" "Det")
						        (find-feature f)))
				       (loop for f in features2
				             always (or (not (find-feature f "Poss:Rel" "Why"))
						        (find-feature f features1)))))))
		         do (let* ((poss-or-det (find-if (lambda (f)
                                                           (or (equal f "Det")
                                                               (search "Poss:" f)))
                                                         features1))
                                   (keep-det2 (and poss-or-det (find poss-or-det features2 :test #'string=)))
                                   (reading (list (u:concat (if hyphen
							        lemma1
							        (subseq lemma1 0 (1- (length lemma1))))
						            "="
						            (if (and (not keep-det2) ;; always T?
                                                                     (> (length lemma2) 3)
                                                                     (find #\- lemma2 :end 3)
                                                                     #+ignore
							             (find "<PreAdj>" features2 :test #'string=)
                                                                     )
							        (subseq lemma2 (1+ (position #\- lemma2)))
							        lemma2))
					          ;; features1
					          (if print
					              (format nil "~{+~a~}" (append features2 '("CC")))
					              (append features2 '("CC")))
					          nil nil)))
		              (if print
			          (u:collect (print reading))
			          (u:collect reading))))))))))

(defparameter *simple-dictionary* nil)

(defparameter *dictionary-lemma-table* (make-hash-table :test #'equal))

#+test
(print (lemma-in-dictionary "аа-ра́"))

(defun lemma-in-dictionary (lemma)
  (let ((hyph-pos (position #\- lemma :from-end t)))
    (when (and hyph-pos
               (or (>= hyph-pos 3)
                   (string= lemma "аа-ра́")) 
               (or (string= lemma "ра" :start1 (- (length lemma) 2))
                   (string= lemma "ра́" :start1 (- (length lemma) 3))))
      (setf lemma (u:concat (subseq lemma 0 hyph-pos) (subseq lemma (1+ hyph-pos)))))
    (let ((lemma (u:subst-substrings
                  lemma '("-заара" "заара" "-заара́" "заара́"
                          ;; "-ра" "ра" "-ра́" "ра́"
                          "·" "" ":" ""))))
      ;; (debug lemma)
      (or (gethash lemma *dictionary-lemma-table*)
          (and (= (count-if (lambda (c) (find c "аыеоу")) lemma) 1)
               ;; one-syllable words may have no accent in the dictionary
               (gethash (delete #\́ lemma) *dictionary-lemma-table*))))))

;; refine for lemmas with hyphen
(defun word-equals-lemma (word lemma)
  (equal word (delete-if (lambda (c) (find c '(#\- #\́ #\· #\:))) lemma)))

(defun word-equals-lemma-lc (word lemma)
  (equal (string-downcase word) (delete-if (lambda (c) (find c '(#\- #\́ #\· #\:))) lemma)))

#+test
(print (lemma-in-dictionary "а-к-ра́"))

(defun get-simple-translation (lemma features)
  (declare (ignore lemma features))
  nil)

(defmethod lookup-morphology ((language (eql :abk)) word 
                              &key mwe variety
                                coord ;; lookup abk coordinated compound segments?
                                (orthography :abk-cyr)
                                &allow-other-keys)
  ;;(print (list word language variety))
  (let ((lemmas+features ())
	(stripped-word
	 (remove-if (lambda (c) 
		      (find c "ՙ‹›{}\\|"))
		    word))
	(analyzer *abk-analyzer*))
    (cond ((and variety (not (eq variety :abk)))
           (setf lemmas+features (list (list "-" (format nil "Foreign ~a" variety) nil nil))))
          (analyzer
           (when (eq orthography :abk-cyr-old)
             (cl-fst:fst-lookup
              *abk-old-to-new-orth*
              stripped-word
              (lambda (w norm net)
                (declare (ignore w net))
                (u:trim norm)
                (setf stripped-word norm))))
           (let ((reading-tree (dat:make-string-tree)))
             (cl-fst:fst-lookup
              *abk-relax-analyzer*
              stripped-word
              (lambda (w l+f net)
                (declare (ignore w net))
                (dolist (reading (u:split l+f #\newline nil nil t))
                  (when (find #\+ reading) ;; temp. workaround
                    (setf (dat:string-tree-get reading-tree reading) :relax))))
              :mode :union)
             (cl-fst:fst-lookup
              analyzer
              stripped-word
              (lambda (w l+f net)
                (declare (ignore w net))
                ;;(print (list :base l+f))
                (dolist (reading (u:split l+f #\newline nil nil t))
                  ;; possibly overwrites :relax
                  (when (find #\+ reading) ;; temp. workaround
                    (setf (dat:string-tree-get reading-tree reading) :base))))
              :mode :union)
             (dat:do-string-tree (reading mode reading-tree)
               (let (#+test(acc-word stripped-word))
                 #+test
                 (cl-foma:foma-lookup *abk-analyzer*
                                      reading
                                      (lambda (w acc-words net)
                                        (declare (ignore w net))
                                        ;;(debug acc-words)
                                        (loop for acc-w in (u:split acc-words #\newline)
                                           until (and (find #\́ acc-w)
                                                      (string= stripped-word (delete #\́ acc-w))
                                                      (setf acc-word acc-w))))
                                      :side :upper)
                 ;; move trailing features behind the lemma
                 (let* ((lemma-start (or (position #\* reading) -1))
                        (f-start (position #\+ reading :start (max lemma-start 1)))
                        (features (if (= -1 lemma-start)
                                      (u:split (subseq reading f-start) #\+ nil nil t)
                                      (append (u:split (subseq reading f-start)
                                                       #\+ nil nil t)
                                              (u:split (subseq reading 0 lemma-start)
                                                       #\+ nil nil t))))
                        (lemma (subseq reading (1+ lemma-start) f-start))
                        (features (if (eq mode :relax)
                                      (append features (list "<Relax>"))
                                      features))
                        (features (cond (coord
                                         features)
                                        ((find "Punct" features :test #'string=)
                                         features)
                                        (t
                                         (let ((lemma-in-dict (lemma-in-dictionary lemma)))
                                           (cond ((word-equals-lemma word lemma)
                                                  (append features (list "<Lemma>")))
                                                 ((word-equals-lemma-lc word lemma)
                                                  (append features (list "<LemmaLC>")))
                                                 ((not lemma-in-dict)
                                                  (append features (list "<NoLex>")))
                                                 ((eq lemma-in-dict :hunting-lang)
                                                  (append features (list "<HuntingLang>")))
                                                 (t
                                                  features))))))
                        (features (sort-abkhaz-features features))
                        (rid -1))
                   
                   ;; add translations if needed
                   (if (and #+gekko *use-simple-dict-p* *simple-dictionary* (not coord))
                       (let ((trans-list (get-simple-translation lemma features)))
                         (dolist (trans (or trans-list (list (list "" ""))))
                           (pushnew (list lemma
                                          features
                                          #+test(u:concat "[" acc-word "] "
                                                          (substitute #\space #\+ features))
                                          nil nil (list (incf rid)) trans)
                                    lemmas+features :test #'equal)))
                       (pushnew (list lemma
                                      features
                                      #+test(u:concat "[" acc-word "] "
                                                      (substitute #\space #\+ features))
                                      nil
                                      nil #+ignore(list (incf rid))) ;; ??
                                lemmas+features :test #'equal))
                   
                   )))
             
             (setf lemmas+features
                   (sort lemmas+features
                         (lambda (lf1 lf2)
                           (or (string< (car lf1) (car lf2))
                               (and (string= (car lf1) (car lf2))
                                    (< (length (cadr lf1)) (length (cadr lf2))))
                               (and (string= (car lf1) (car lf2))
                                    (= (length (cadr lf1)) (length (cadr lf2)))
                                    (loop for f1 in (cadr lf1)
                                          for f2 in (cadr lf2)
                                          thereis (string< f1 f2)))))))
             
             ;; mark +Det reading if one without +Det exists
             (setf lemmas+features
                   (loop with prev-lemma = "" ;; and collected = nil
                      and prev-features = ()
                      and prev-filtered-features = ()
                      for l+f in lemmas+features
                      for lemma = (car l+f)
                      and features = (cadr l+f)
                      and filtered-features = (remove-if (lambda (f)
                                                           (find f '("<Relax>") :test #'string=))
                                                         (cadr l+f))
                      ;; do (print (list :pl prev-lemma :l lemma :pf prev-features :f features))
                      if (and (string= prev-lemma lemma)
                              (= (length prev-filtered-features)
                                 (1- (length filtered-features)))
                              (let ((diff (set-difference filtered-features prev-filtered-features
                                                          :test #'string=)))
                                (and (car diff)
                                     (null (cadr diff))
                                     (string= (car diff) "Det"))))
                      do (setf (cdr prev-features) (append (cdr prev-features) (list "[Det]")))
                      else if (and (string= prev-lemma lemma)
                                   (= (length prev-filtered-features) (1- (length filtered-features)))
                                   (let ((diff (set-difference filtered-features prev-filtered-features
                                                               :test #'string=)))
                                     (and (car diff)
                                          (null (cadr diff))
                                          (string= (car diff) "LO:3SgNH"))))
                      do (setf (cdr prev-features)
                               (append (cdr prev-features) (list "[LO:3SgNH]")))
                      else
                      collect l+f
                      when (or (string/= lemma prev-lemma)
                               (> (length filtered-features) (1+ (length prev-filtered-features))))
                      do (setf prev-features features
                               prev-filtered-features filtered-features
                               prev-lemma lemma))) 

             (unless coord
               (when (null lemmas+features)
                 (setf lemmas+features
                       (lookup-abk-coord-compound word :orthography orthography)))
               (loop for lf in lemmas+features
                  do (setf (cadr lf) (format nil "~{~a~^ ~}" (cadr lf)))))
	     ))
	  (mwe ;; don’t recognize foreign stuff as mwe
	   nil)
	  (t
	   (setf lemmas+features (list (list "-" (format nil "Foreign ~a" :abk) nil nil)))))
    (values lemmas+features stripped-word)))

(defvar *tagset*)

(process-run-function "init-abk-transducers"
                      (lambda ()
                        (init-transducers :abk)))

#+test
(write-text-json (parse-text "Аӡҕаб бзиоуп." :variety :abk)
                 *standard-output*)

:eof
