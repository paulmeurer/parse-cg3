;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: PARSE; Base: 10 -*-

(in-package :parse)

;; Fehler:
;; ის არგადამხდელია.
;; არა- +Prop

#+test
(u:with-file-fields ((lemma code pos features &rest rest) "projects:georgian-morph;regex;lemmas-2023-04-01.tsv")
  (when (find code '("A" "B") :test #'string=)
    (setf lemma (u:concat lemma "ი")))
  (let* ((morph (lookup-morphology :kat lemma :lookup-guessed nil))
         (valid (loop for (l f) in morph
                   unless (or (search " Voc" f)
                              (search " Erg" f)
                              (search " Abs" f)
                              (search " Dat" f)
                              (search " Gen" f)
                              (search " Inst" f)
                              (search " Advb" f)
                              (search " Trunc" f)
                              (search " Att" f)
                              (search " Encl" f)
                              (search " L" f)
                              (search "V " f)
                              (search " Pl" f))
                   collect (list l f))))
    (when valid
      (print (list* lemma features valid)))))

(defparameter *remove-table* (make-hash-table :test #'equal))

#+test
(u:with-file-lines (line "projects:georgian-morph;regex;lemmas-2023-04-01-remove.txt")
  (let* ((l+f (read-from-string line))
         (l (car l+f)))
    (setf (gethash l *remove-table*) l+f)
    (when (char= (char l (1- (length l))) #\ი)
      (setf (gethash (subseq l 0 (1- (length l))) *remove-table*) l+f))))


#+test
(with-open-file (stream "projects:georgian-morph;regex;lemmas-2023-04-01.tsv"
                        :direction :output :if-exists :supersede)
  (u:with-file-lines (line "projects:georgian-morph;regex;lemmas-2023-04-01-to-clean.tsv")
    (let ((lemma (subseq line 0 (position #\tab line))))
      (if (gethash lemma *remove-table*)
          (write-line line)
          (write-line line stream)))))

#+test
(with-open-file (stream "projects:gnc;morphology;guessed-lemma-list-verbs.txt"
                        :direction :output :if-exists :supersede)
  (u:with-file-lines (line "projects:gnc;morphology;guessed-lemma-list-done.txt")
    (when (search "\"v" line)
      (write-line line stream))))

(in-package :fst)

;; import
#+test
(with-open-file (stream "projects:georgian-morph;regex;lemmas-2023-04-01-rest.tsv"
                        :direction :output :if-exists :supersede)
  (with-database-connection ()
    (with-transaction ()
      (block test
        (u:with-file-fields ((lemma code pos f &rest rest) "projects:georgian-morph;regex;lemmas-2023-04-01.tsv")
          (let* ((features (unless (or (equal f "+Rus") (equal f "+Foreign") (equal f "")) f))
                 (style-features (if (or (equal f "+Rus") (equal f "+Foreign")) f))
                 (max-sub-id (or (car (select [max [sub-id]]
                                              :flatp t
                                              :from [morph noun-features]
                                              :where [= [stem] ?lemma]))
                                 0))
                 (rows (when (> max-sub-id 0)
                         (select [code] [pos] [features] [style-features] :from [morph noun-features]
                                 :where [= [stem] ?lemma])))
                 (found nil))
            (cond ((and (> max-sub-id 0)
                       (progn (loop for (c p f s) in rows
                                 when (and (equal code c)
                                           (string-equal pos p)
                                           (equal f features))
                                 do (setf found (list c p f s)))
                              found))
                   (print (list :found lemma code pos f)))
                  (t
                   (print (list max-sub-id lemma code pos features style-features))
                   (insert-records :into [morph noun-features]
                                   :av-pairs `(([stem] ,lemma)
                                               ([code] ,code)
                                               ([pos] ,pos)
                                               ([sub-id] ,(1+ max-sub-id))
                                               ([features] ,features)
                                               ([style-features] ,style-features)
                                               ([comment] "GNC-extracted-2023-04-01")
                                               ([author] "PM")
                                               ([date] ,(get-universal-time))))))))))))


#+test
(with-database-connection ()
  (with-transaction ()
    (block test
      (u:with-file-lines (lemma "projects:georgian-morph;wordlists;wiki-abbrev-org.txt")
        (let* ((features "+Prop+Org")
               (style-features "+Abbrev")
               (pos "n")
               (code "A")
               (max-sub-id (or #+ignore(car (select [max [sub-id]]
                                            :flatp t
                                            :from [morph noun-features]
                                            :where [= [stem] ?lemma]))
                               0)))
          (insert-records :into [morph noun-features]
                          :av-pairs `(([stem] ,lemma)
                                      ([code] ,code)
                                      ([pos] ,pos)
                                      ([sub-id] ,(1+ max-sub-id))
                                      ([features] ,features)
                                      ([style-features] ,style-features)
                                      ([comment] "Wiki-extracted-2023-04-04")
                                      ([author] "PM")
                                      ([date] ,(get-universal-time)))))))))

#+test
(with-database-connection ()
  (with-transaction ()
    (block test
      (u:with-file-lines (lemma "projects:georgian-morph;wordlists;wiki-term.txt")
        (let* ((style-features "+Term")
               (pos "n")
               (code "A")
               (max-sub-id (or #+ignore(car (select [max [sub-id]]
                                            :flatp t
                                            :from [morph noun-features]
                                            :where [= [stem] ?lemma]))
                               0)))
          (insert-records :into [morph noun-features]
                          :av-pairs `(([stem] ,lemma)
                                      ([code] ,code)
                                      ([pos] ,pos)
                                      ([sub-id] ,(1+ max-sub-id))
                                      ;; ([features] ,features)
                                      ([style-features] ,style-features)
                                      ([comment] "Wiki-extracted-2023-04-04")
                                      ([author] "PM")
                                      ([date] ,(get-universal-time)))))))))

#+test
(with-open-file (stream "projects:georgian-morph;wordlists;wiki-chem-u.txt"
                        :direction :output :if-exists :supersede)
  (u:with-file-lines (line "projects:georgian-morph;wordlists;wiki-chem1.txt")
    (trim line)
    (unless (= (length line) 0)
      (write-line (subseq line 24 (position #\space line :start 25)) stream))))

#+test
(with-transaction ()
  (u:with-file-lines (lemma "projects:georgian-morph;wordlists;wiki-chem.txt")
    (unless (string= lemma "")
      (when (char= (char lemma (1- (length lemma))) #\ი)
        (setf lemma (subseq lemma 0 (1- (length lemma)))))
      (unless (select [stem]
                      :flatp t
                      :from [morph noun-features]
                      :where [= [stem] ?lemma])
        (write-line lemma)
        (let ((pos "n")
              (code "A"))
          (insert-records :into [morph noun-features]
                          :av-pairs `(([stem] ,lemma)
                                      ([code] ,code)
                                      ([pos] ,pos)
                                      ([sub-id] 0)
                                      ;; ([features] ,features)
                                      ([style-features] "+Term")
                                      ([comment] "Wiki-extracted-2023-04-04")
                                      ([author] "PM")
                                      ([date] ,(get-universal-time))))
          )))))

#+done
(with-transaction ()
  (u:with-file-lines (lemma "projects:georgian-morph;lists;unknown-shvili.txt")
    (unless (string= lemma "")
      #+ignore
      (when (char= (char lemma (1- (length lemma))) #\ი)
        (setf lemma (subseq lemma 0 (1- (length lemma)))))
      (unless nil #+test(select [stem]
                      :flatp t
                      :from [morph noun-features]
                      :where [= [stem] ?lemma])
        (write-line lemma)
        (let ((pos "n")
              (code "A"))
          (insert-records :into [morph noun-features]
                          :av-pairs `(([stem] ,lemma)
                                      ([code] ,code)
                                      ([pos] ,pos)
                                      ([sub-id] 0)
                                      ;; ([features] ,features)
                                      ([style-features] "+Prop+Name+LastName")
                                      ([comment] "Extracted-შვილი-2023-04-08")
                                      ([author] "PM")
                                      ([date] ,(get-universal-time))))
          )))))

#+test
(with-open-file (stream "projects:georgian-morph;lists;unknown-last-names-wiki2.txt"
                        :direction :output :if-exists :supersede)
  (u:with-file-lines (line "projects:georgian-morph;lists;unknown-last-names-wiki1.txt")
    (unless (and (> (length line) 5)
                 (string= line "ელი" :start1 (- (length line) 3)))
      (write-line line stream))))

#+test
(with-open-file (stream "projects:georgian-morph;lists;unknown-iuri-iuli-wiki.txt"
                        :direction :output :if-exists :supersede)
  (u:with-file-lines (line "projects:georgian-morph;lists;unknown-iuri-wiki.txt")
    (unless (find-if-not (lambda (c) (char<= #\ა c #\ჰ)) line) 
      (write-line line stream))))

#+test
(with-open-file (stream "projects:georgian-morph;lists;unknown-shvili-tmp.txt"
                        :direction :output :if-exists :supersede)
  (u:with-file-lines (line "projects:georgian-morph;lists;unknown-shvili.txt")
    (unless (find-if-not (lambda (c) (char<= #\ა c #\ჰ)) line) 
      (write-line line stream))))


#+test
(with-open-file (stream "projects:georgian-morph;lists;unknown-iuri1.txt"
                        :direction :output :if-exists :supersede)
  (u:with-file-lines (line "projects:georgian-morph;lists;unknown-iuri.txt")
    (unless (find-if-not (lambda (c) (char<= #\ა c #\ჰ)) line) 
      (write-line line stream))))

:eof
