;;;; San Marco LISP Explorer (R)
;;;; Copyright (c) 1984, 1985, 1986, 1987 San Marco Associates

;; Revised, but not tested, 7 May 91
;; Changes to STREAM-REMEMBER and STREAM-APPEND
;; MAKE-EMPTY-STREAM introduced

(defun make-empty-stream () 'empty-stream)

(defun stream-endp (stream) (eq stream (make-empty-stream))

(defmacro stream-cons (object stream)
  `(list ,object (encapsulate ,stream)))

(defun stream-first (stream)
  (first stream))

(defun stream-rest (stream)
  (funcall (second stream)))

(defmacro encapsulate (expression)
  `(let ((switch nil) (result nil))
    #'(lambda ()
	(if switch
	    result
	    (setf switch t result ,expression)))))

(defun expose (expression)
  (funcall expression))

(defun stream-transform (procedure stream)
  (if (stream-endp stream)
      (make-empty-stream)
      (stream-cons (funcall procedure (stream-first stream))
		   (stream-transform procedure (stream-rest stream)))))

(defmacro stream-append (stream1 stream2)
  `(if (stream-endp ,stream1)
       ,stream2
     (stream-cons (stream-first ,stream1)
		  (stream-append (stream-rest ,stream1) ,stream2))))

(defun stream-concatenate (streams)
  (if (stream-endp streams)
      (make-empty-stream)
      (stream-append
	(stream-first streams)
	(stream-concatenate (stream-rest streams)))))

(defun stream-member (object stream)
  (cond ((stream-endp stream) nil)
	((equal object (stream-first stream)) t)
	(t (stream-member object (stream-rest stream)))))

(defmacro stream-remember (object variable)
  `(unless (stream-member ,object ,variable)
     (if (stream-endp ,variable)
	 (setf ,variable (stream-cons ,object (make-empty-stream)))
       (setf ,variable
	     (let ((,variable ,variable) (,object ,object))
	       (stream-append
		 ,variable
		 (stream-cons ,object (make-empty-stream))))))
     ,object))

(defun remember-assertion (assertion)
  (stream-remember assertion *assertions*))

(defun remember-rule (rule)
  (stream-remember rule *rules*))

(defun rule-name (rule) (first rule))

(defun rule-ifs (rule) (butlast (rest rule) 1))

(defun rule-then (rule) (first (last rule)))

;;;; Miscellaneous

#+comment
(defun test (stream)
  (if (stream-endp stream)
      nil
      (cons (stream-first stream)
	    (test (stream-rest stream)))))