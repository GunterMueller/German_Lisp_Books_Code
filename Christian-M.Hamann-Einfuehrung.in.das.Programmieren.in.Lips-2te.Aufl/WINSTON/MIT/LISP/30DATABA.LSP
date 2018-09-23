;;;; -*- mode:Lisp; package:user -*- ;;;;
;;;; Created: 10 December 1992

;;;; REMARKS

#|

This version is the same as the one in the book except that
a version of DB-SHOW has been added.

|#

;;;; PROCEDURES

(defun db-count (relation)
  (format t "~&~a" (length (rest relation)))
  (values))

(defun db-union (&rest records)
  (cons (first (first records))
        (remove-duplicates (apply #'append
                                  (mapcar #'rest records))
                           :test #'equal)))

(defmacro db-project (relation &optional over &rest projections)
  `(cons ',projections
         (db-project-aux (first ,relation) (rest ,relation)
                         ',projections)))

(defun db-project-aux (fields records projections)
  (remove-duplicates
    (mapcar
      #'(lambda (record)
          (mapcar
            #'(lambda (projection)
                (nth (position projection fields) record))
            projections))
      records)
    :test #'equal))

(defmacro db-select (relation &optional over &rest triples)
  `(cons (first ,relation)
         (db-select-aux (first ,relation)
                        (rest ,relation)
                        ',triples)))

(defun db-select-aux (fields records triples)
  (if (endp triples)
      records
      (remove-if-not
        #'(lambda (record)
            (funcall (second triples)
                     (nth (position (first triples) fields)
                          record)
                     (third triples)))
        (db-select-aux fields records (nthcdr 3 triples)))))

(defun db-show (arg)
  (if (numberp arg)
      (format t "~a" arg)
  (let ((widths (db-find-field-widths arg)))
    (db-show-record widths (first arg))
    (db-show-record
        widths
        (mapcar #'(lambda (width)
                    (make-string width :initial-element #\-))
                widths))
    (dolist (record (rest arg) (values))
      (db-show-record widths record)))))

(defun db-show-record (widths fields)
  "
  Remarks:	Written strangely to work with Common Lisp subsets.
  "
  (format t "~&|")
      (do ((widths widths (rest widths))
           (fields fields (rest fields)))
	  ((endp widths))
	(format t " ~a~a |"
		(first fields)
		(make-string
		  (- (first widths)
		     (length (format nil "~a" (first fields))))
		  :initial-element #\ ))))

(defun db-find-field-widths (relation &aux result)
  (setf result
        (mapcar #'(lambda (field)
                    (length (format nil "~a" field)))
                (first relation)))
  (dolist (record (rest relation) result)
    (setf result 
          (mapcar #'(lambda (number symbol)
                      (max number
                           (length (format nil "~a" symbol))))
                  result
                  record))))

(defun db-call (arg) (eval arg))

