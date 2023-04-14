
(in-package :fst)

(defparameter *features-table* (make-hash-table :test #'equal))

(defparameter *tag-table* (make-hash-table :test #'equal))

;; 11954


;; 6175
(progn
  (clrhash *features-table*)
  (u:with-file-lines (line "projects:parse-cg3;features.txt")
    (u:trim line)
    (unless (char= (char line 0) #\#)
      (destructuring-bind (count f) (u:split line #\space)
        (setf (gethash f *features-table*) count))))
  (clrhash *tag-table*)
  (u:with-file-lines (line "projects:parse-cg3;tagset.txt")
    (u:trim line)
    (destructuring-bind (count tag) (u:split line #\space)
      (let* ((features (delete-if (lambda (f)
                                    (not (gethash f *features-table*)))
                                  (u:split tag #\.)))
             (tag (format nil "~{~a~^.~}" features)))
        (incf (gethash tag *tag-table* 0) (parse-integer count)))))
  (print (hash-table-count *tag-table*)))


:eof
