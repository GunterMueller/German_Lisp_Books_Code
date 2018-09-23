;;;; -*- mode:LISP; -*-
;;;; Copyright (c) 1984, 1985, 1986, 1987 
;;;; by Patrick H. Winston and San Marco Associates

;;This is material for forward chaining chapter of lisp book and
;;the San Marco LISP Explorer (R)

;;This is basic stream stuff, without delayed evaluation.

;; Revised, but not tested, 7 May 91

(defun make-empty-stream () 'empty-stream)

(defun stream-endp (stream) (eq stream 'empty-stream))

(defun stream-first (stream) (first stream))

(defun stream-rest (stream) (second stream))

(defun stream-cons (object stream) (list object stream))

(defun stream-append (stream1 stream2)
  (if (stream-endp stream1) stream2
      (stream-cons (stream-first stream1)
                   (stream-append (stream-rest stream1) stream2))))

(defun stream-concatenate (streams)
  (if (stream-endp streams)
      (make-empty-stream)
      (stream-append (stream-first streams)
                     (stream-concatenate (stream-rest streams)))))

(defun stream-transform (procedure stream)
  (if (stream-endp stream)
      (make-empty-stream)
      (stream-cons (funcall procedure (stream-first stream))
                   (stream-transform procedure (stream-rest stream)))))

;;;; Access Procedures for Assertions and Rules

(defun stream-member (object stream)
  (cond ((stream-endp stream) nil)
	((equal object (stream-first stream)) t)
	(t (stream-member object (stream-rest stream)))))

(defmacro stream-remember (object variable)
  `(unless (stream-member ,object ,variable)
     (setf ,variable
	   (stream-append ,variable
			  (stream-cons ,object (make-empty-stream))))
     ,object))

(defun remember-assertion (assertion)
  (stream-remember assertion *assertions*))

(defun remember-rule (rule)
  (stream-remember rule *rules*))

(defun rule-name (rule) (first rule))

(defun rule-ifs (rule) (butlast (rest rule) 1))

(defun rule-then (rule) (first (last rule)))

