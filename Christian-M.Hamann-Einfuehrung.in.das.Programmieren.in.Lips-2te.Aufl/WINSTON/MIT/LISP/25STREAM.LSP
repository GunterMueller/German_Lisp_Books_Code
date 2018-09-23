;;;; -*- mode:Lisp; package:user -*- ;;;;
;;;; Created: 10 December 1992

;;;; REMARKS

#|

This version uses delayed evaluation.  There are small, but important
improvements relative to the version provided in the book.  The
ENCAPSULATE procedure, for example, ensures that each stream element
is computed only once.  Also, MAKE-EMPTY-STREAM is introduced;
(MAKE-EMPTY-STREAM) replaces 'EMPTY-STREAM.

|#

;;;; BASIC ACCESS FUNCTIONS WITH DELAYED EVALUATION

(defun make-empty-stream () 'empty-stream)

(defun stream-endp (stream) (eq stream 'empty-stream))

(defun stream-first (stream) (first stream))

(defun stream-rest (stream)		
  (expose (second stream)))

(defmacro stream-cons (object stream)	
  `(list ,object (encapsulate ,stream)))

(defun stream-append (stream1 stream2)
  (if (stream-endp stream1)
      stream2
      (stream-cons (stream-first stream1)
                   (stream-append (stream-rest stream1)
                                  stream2))))

(defun stream-concatenate (streams)
  (if (stream-endp streams)
      'empty-stream
    (if (stream-endp (stream-first streams))
	(stream-concatenate (stream-rest streams))
      (stream-cons (stream-first (stream-first streams))
		   (stream-concatenate
		     (stream-cons (stream-rest (stream-first streams))
				  (stream-rest streams)))))))

(defun stream-transform (procedure stream)
  (if (stream-endp stream)
      'empty-stream
      (stream-cons (funcall procedure (stream-first stream))
                   (stream-transform procedure
                                     (stream-rest stream)))))

(defun stream-member (object stream)
  (cond ((stream-endp stream) nil)
        ((equal object (stream-first stream)) t)
        (t (stream-member object (stream-rest stream)))))

(defmacro stream-remember (object variable)
  `(unless (stream-member ,object ,variable)
       (setf ,variable
	     (stream-append ,variable
			    (stream-cons ,object
					 'empty-stream)))
       ,object))

;;;; AUXILIARY PROCEDURES FOR LEXICAL ENCAPSULATION

(defmacro encapsulate (form)	;From a problem solution.
  `(let ((switch nil) (result nil))
       #'(lambda ()
	   (cond (switch #+comment
			 (format t "~%Remembering ...")
			 result)
		 (t #+comment
		    (format t "~%Computing ...")
		    (setf switch t result ,form))))))

(defmacro expose (procedure)
    `(funcall ,procedure))


