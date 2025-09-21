;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: PARSE; Base: 10 -*-

(in-package :parse)

;; precision calculation

#+test
(calculate-precision *text* *standard-output*)

(defmethod calculate-precision ((text parsed-text) stream
                                &key document &allow-other-keys)
  (let ((token-array (text-array text))
        (node-count 0)
        (head-diff-count 0)
        (relation-diff-count 0)
        (head+relation-diff-count 0))
    (loop for node across token-array
          when (and (getf node :stored-label)
                    (not (getf node :stored-parent)))
          do
          (let ((graph (build-dep-graph text
                                        :node-id (getf node :self)
                                        :stored t)))
            (setf *graph* graph)
            (when (graph-is-complete graph)
              (incf node-count (hash-table-count (graph::node-table graph)))
              (multiple-value-bind (hdc rdc hrdc)
                  (calculate-precision graph stream :document document)
                (incf head-diff-count hdc)
                (incf relation-diff-count rdc)
                (incf head+relation-diff-count hrdc)))))
    (values node-count head-diff-count relation-diff-count head+relation-diff-count)))

(defparameter *diff-table* (make-hash-table :test #'equal))

(defmethod calculate-precision ((graph dep-node) stream &key document &allow-other-keys)
  (let ((head-diff-count 0)
        (relation-diff-count 0)
        (head+relation-diff-count 0))
    (labels ((walk (node)
               (let ((parent (if (node-parents node)
                                 (node-id (car (node-parents node)))
                                 0)))
                 (unless (zerop (node-id node))
                   (let ((word (node-label node)))
                     (when (or (head-diff node) (relation-diff node))
                       (setf (gethash (u:concat document ":" (graph::wid node)) *diff-table*)
                             (list word parent (head-diff node) (relation node) (relation-diff node))))
                     (cond ((and (head-diff node)
                                 (relation-diff node))
                            (incf head-diff-count)
                            (incf relation-diff-count)
                            (incf head+relation-diff-count)
                            (format stream "~a:~a	~a	~a -> ~a	~a -> ~a~%"
                                    document
                                    (graph::wid node)
                                    word
                                    parent
                                    (head-diff node)
                                    (relation node)
                                    (relation-diff node)))
                           ((and (head-diff node))
                            (incf head-diff-count)
                            (format stream "~a:~a	~a	~a -> ~a	~%"
                                    document
                                    (graph::wid node)
                                    word
                                    parent
                                    (head-diff node)))
                           ((relation-diff node)
                            (incf relation-diff-count)
                            (format stream "~a:~a	~a		~a -> ~a~%"
                                    document
                                    (graph::wid node)
                                    word
                                    (relation node)
                                    (relation-diff node)))))))
               (mapc #'walk (node-children node))))
      (walk graph)
      (values head-diff-count relation-diff-count head+relation-diff-count))))

#+test
(let ((text
       (kp::parse-corpus-context (kp::get-corpus :gnc-kat) :start-cpos 114160 :end-cpos 114554)))
  (calculate-precision text *standard-output*))

(in-package :korpuskel)

(defmethod calculate-precision ((language t) stream &key write-conll &allow-other-keys)
  (with-database-connection ()
    (let ((count 0)
          (corpus nil)
          (prev-corpus-name nil)
          (prev-doc nil)
          (page-start 0)
          (page-end 0)
          (node-count 0)
          (head-diff-count 0)
          (relation-diff-count 0)
          (head+relation-diff-count 0)
          (first t))
      (write-line "# document:word	stored -> parsed relation	stored -> parsed head" stream)
      (do-query ((corpus-name document wid)
                 [select [corpus] [document] [wid]
                         :from [corpus disambiguation]
                         :where [and [like [corpus]
                                           (ecase language
                                             (:kat "g%-%")
                                             (:abk "abnc"))]
                                     [not [null [label]]]
                                     [null [parent]]]
                         :order-by '([corpus] [document] [wid])])
        ;;(print (list corpus-name document wid))
        (unless (equal prev-corpus-name corpus-name)
          (setf corpus (get-corpus corpus-name)
                page-start 0 page-end 0))
        (let* ((doc-attr (get-corpus-attribute corpus :document))
               (doc-id (get-value-id doc-attr document))
	       ;;(wattr (get-corpus-attribute corpus :word))
	       (wid-attr (get-corpus-attribute corpus :wid))
               (wid-id (get-value-id wid-attr (format nil "w~a" wid))))
          (map-attribute-block wid-attr
                               (lambda (cp cpos end target value-id attribute)
                                 (declare (ignore cp end target value-id attribute))
                                 (when (= doc-id (cpos-val-id cpos (index-vector doc-attr) (signature doc-attr)))
                                   ;; (print (list corpus-name document page-start cpos page-end))
                                   (unless (and (equal prev-corpus-name corpus-name)
                                                (equal prev-doc document)
                                                (<= page-start cpos page-end))
                                     (multiple-value-bind (start-cpos end-cpos)
                                         (get-containing-element-cpositions corpus cpos (list "pb")
                                                                            :milestonep t)
                                       (unless start-cpos
                                         ;; for rt and ci
                                         (multiple-value-setq (start-cpos end-cpos)
                                           (get-containing-element-cpositions corpus cpos (list "text")
                                                                              :milestonep nil :complete-text t)))
                                       ;;(print (list start-cpos end-cpos))
                                       (let ((text (parse-corpus-context corpus
                                                                         :start-cpos start-cpos
                                                                         :end-cpos end-cpos
                                                                         :load-grammar first
                                                                         :keep-non-mwe-readings t)))
                                         (setf page-start start-cpos page-end end-cpos first nil)
                                         (incf count)
                                         (write-line document stream)
                                         (when write-conll
                                           (let ((conll (with-output-to-string (stream)
                                                          (parse::write-dependencies-conll text stream
                                                                                           :document-id document
                                                                                           :language language)))
                                                 (page nil)
                                                 (page-att nil))
                                             ;; extract page from text-array
                                             (loop for token
                                                   across (parse::text-array text) ;; *text*
                                                   until (and (eq (car token) :empty-element)
                                                              (equal (cadr token) "pb")
                                                              (setf page-att (getf token :atts))))
                                             (when page-att
                                               (let* ((start (+ (search " n=" page-att) 4))
                                                      (end (position #\" page-att :start (1+ start))))
                                                 (setf page (subseq page-att start end))))
                                             (with-open-file (stream (debug (format nil
                                                                             (if (string= corpus-name "abnc")
                                                                                 "projects:ud;abk;~a"
                                                                                 "projects:ud;kat;~a")
                                                                             (if page
                                                                                 (substitute
                                                                                  #\; #\/
                                                                                  (normalize-name-page-nr
                                                                                   (u:concat document "_" page ".conll")))
                                                                                 (u:concat (substitute #\; #\_ document) ".conll"))))
                                                                     :direction :output :if-exists :supersede)
                                               (write-string conll stream))))
                                         (multiple-value-bind (nc hdc rdc hrdc)
                                             (parse::calculate-precision text stream :document document)
                                           (incf node-count nc)
                                           (incf head-diff-count hdc)
                                           (incf relation-diff-count rdc)
                                           (incf head+relation-diff-count hrdc)))))))
                               wid-id :start-end-block-p nil))
        (setf prev-corpus-name corpus-name
              prev-doc document))
      (format stream "date: ~a, nodes: ~a, head-diff: ~a, relation-diff: ~a, head+relation-diff: ~a~%"
              (now :format :timestamp-utc)
              node-count head-diff-count relation-diff-count head+relation-diff-count)
      (print (list :nodes node-count :head-diff head-diff-count :relation-diff relation-diff-count))
      (debug count))))

;; diff $(ls -t | head -n 2)
;; diff of 3rd newest and newest:
;; diff $(ls -t | head -n 3 | tail -n 1) $(ls -t | head -n 1)
#+main
(with-open-file (stream (format nil "projects:ud;kat;precision;precision-~a.txt"
                                (now :format :timestamp-utc))
                        :direction :output :if-exists :supersede)
  (calculate-precision :kat stream :write-conll t)
  (print :done))

#+main
(with-open-file (stream (format nil "projects:ud;abk;precision;precision-~a.txt"
                                (now :format :timestamp-utc))
                        :direction :output :if-exists :supersede)
  (calculate-precision :abk stream :write-conll nil)
  (print :done))
