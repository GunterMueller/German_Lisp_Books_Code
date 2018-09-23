#|

This file contains programs for groveling through all the files in
the lisp book with the following objectives:

GROVEL:	To locate bad references to undefined primitives or user procedures.

JUSTIFY: To typeset all the files.

|#

;;; LOAD VARIABLES

(load "grovel.var")

;;; FINDING BAD REFERENCES

(set-syntax-from-char #\} #\) )

(defun grovel (files)
  (with-open-file (output "grovel.out" :direction :output)
    (dolist (file files)
      (format t "~&~%STARTING FILE ~a.~%" file)
      (format t "~%~a used-primitives; ~a definitions; ~a questionables"
	      (length *used-primitives*)
	      (length *definitions*)
	      (length *questionables*))
      (grovel-file file))
    (format output "~%~%Totals:")
    (format output "~%~a used-primitives; ~a definitions; ~a questionables"
	    (length *used-primitives*)
	    (length *definitions*)
	    (length *questionables*))
    (format output "~%(setf *used-primitives* '~a)"
	    (reverse *used-primitives*))
    (format output "~%(setf *text-primitives* '~a)"
	    (reverse *text-primitives*))
    (format output "~%Used but not mentioned '~a"
	    (set-difference (reverse *used-primitives*)
			    (reverse *text-primitives*)))
    (format output "~%Mentioned but not used '~a"
	    (set-difference (reverse *text-primitives*)
			    (reverse *used-primitives*)))
    (format output "~%(setf *definitions* '~a)"
	    (reverse *definitions*))
    (format output "~%(setf *parameters* '~a)"
	    (reverse *parameters*))
    (format output "~%(setf *questionables* '~a)"
	    (reverse *questionables*))))
	    
(defun grovel-file (file)
  (with-open-file (input (merge-pathnames file ".tex") :direction :input)
    (do ((line (read-line input nil 'eof)
	       (read-line input nil 'eof))
	 (count 0 (1+ count)))
	((eq line 'eof))
      (analyze-line line (namestring file) count))))

(defun analyze-line (line file count)
  (dotimes (n (1- (length line)))
    (ignore-errors
      (cond ((eql #\( (aref line n))
	     (read-symbol (1+ n) line file count))
	    ((and (eql #\# (aref line n))
		  (eql #\' (aref line (1+ n))))
	     (read-symbol (+ 2 n) line file count))
	    ((and ;(eql #\\ (aref line n))
	       (eql #\s (aref line (1+ n)))
	       (eql #\y (aref line (1+ (1+ n))))
	       (eql #\{ (aref line (1+ (1+ (1+ n))))))
	     (read-symbol (+ 4 n) line file count t))))))

(defun read-symbol (position array file count &optional (sy nil))
  (when (alpha-char-p (aref array position))
    (with-open-stream (input (make-string-input-stream array position))
      (let ((s (read input)))
	(cond ((not (ignore-errors (symbolp s))))
	      ((member s '(defun defstruct defmethod defclass defmacro))
	       (let ((definition (read input)))
		 (when (and (ignore-errors (symbolp definition))
			    (not (member definition *definitions*)))
		   (pushnew definition *definitions*)
		   (if (member definition *questionables*)
		       (progn
			 (setf *questionables*
			       (remove definition
				       *questionables*))
			 #+comment
			 (format t "~%*****Delayed definition: ~
				 ~a in ~a, line ~a:~%~a"
				 definition file count array)
			 #+comment
			 (format output "~%*****Delayed definition: ~
				 ~a in ~a, line ~a:~%~a"
				 definition file count array))
		       #+comment
		       (progn
			 (format t "~%*****Prior definition: ~
				 ~a in ~a, line ~a:~%~a"
				 definition file count array)
			 (format output "~%*****Prior definition: ~
				 ~a in ~a, line ~a:~%~a"
				 definition file count array))
		       ))))
	      ((ignore-errors (symbol-function s))
	       (if sy
		   (when (not (member s *text-primitives*))
		     (pushnew s *text-primitives*)
		     (format t "~%*****Primitive mentioned: ~
			     ~a in ~a, line ~a:~%~a"
			     s file count array)
		     (format output "~%*****Primitive mentioned: ~
			     ~a in ~a, line ~a:~%~a"
			     s file count array))
		   (when (not (member s *used-primitives*))
		     (pushnew s *used-primitives*)
		     (format t "~%*****Primitive appears: ~
			     ~a in ~a, line ~a:~%~a"
			     s file count array)
		     (format output "~%*****Primitive appears: ~
			     ~a in ~a, line ~a:~%~a"
			     s file count array))))
	      (t (when (and (not (member s *definitions*))
			    (not (member s *parameters*))
			    (not (member s *questionables*)))
		   (push s *questionables*)
		   #+comment
		   (format t "~%*****Unknown: ~
			   ~a in ~a, line ~a:~%~a"
			   s file count array)
		   #+comment
		   (format output "~%*****Unknown: ~
			   ~a in ~a, line ~a:~%~a"
			   s file count array))))))))

;;; JUSTIFICATION

(defun spell (&optional (files all))
  (process-system
    files
    #'(lambda (x) (call-program "spell" nil x))))

(defun justify  (&optional (files all))
  (process-system
    files
    #'(lambda (x)
	(call-program "txt-cmdi" nil x)
	(call-program "dvi-ps"
		      nil
		      (namestring (merge-pathnames "/output/.dvi" x))))
    t))

(defun remtabs (&optional (files all))
  (process-system
    files
    #'(lambda (x)
	(call-program "remtabs" nil x))))

(defun serial-to-qms (&optional (files all))
  (process-system
    files
    #'(lambda (x)
	(call-program "ps-qms" nil 
		      (merge-pathnames "/output/.ps" x)))))

(defun chaos-to-qms (&optional (files all))
  (process-system
    files
    #'(lambda (x)
	(call-program "send" nil 
		      (merge-pathnames "/output/.ps" x)))))

(defun process-system (files procedure
			     &optional
			     all
			     &aux switch)
  (unless (listp files) (setf files (list files)))
  (dolist (l files switch)
    (let* ((source l)
	   (source-name (namestring source))
	   (destination l))
      (cd source)
      (cond ((not (probe-file source))
	     (format t "~%Sorry, there is no ~a file to process!" source-name))
	    #+comment
	    (t (with-open-file (output destination :direction :output)
		 (format output "~%")))
	    ((or all
		 (not (probe-file destination))
		 (later-file source destination))
	     (format t "~%Evidently ~a must be reprocessed." source-name)
	     (funcall procedure l)
	     (setf switch t))
	    (t (format t "~%The ~a file has been processed already."
		       source-name))))))

(defun later-file (u v)
  "T if the first time is later than the second."
  (multiple-value-bind (u1 u2 u3 u4 u5) (file-info u)
    (multiple-value-bind (v1 v2 v3 v4 v5) (file-info v)
      (if (> u4 v4)
	  t
	  (if (< u4 v4)
	      nil
	      (> u5 v5))))))






