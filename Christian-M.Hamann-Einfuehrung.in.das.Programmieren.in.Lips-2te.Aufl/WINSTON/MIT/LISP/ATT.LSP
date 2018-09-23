;;;; San Marco LISP Explorer (R)
;;;; Copyright (c) 1984, 1985, 1986, 1987 San Marco Associates

;;; This file contains a transition tree language-understanding program.

;;; The notation differs from the Second Edition as follows:

;;; Syntax flattened!

;;;				First option	Second option
;;;	PARSE			v		>
;;;	PARSE-RETURN		^		rtn
;;;	PARSE-RETURN-IF-END	if-end-^	if-end-rtn
;;;	BRANCH			BRANCH		brnchs

;;;; THE INTERPRETER

(if (member 'gclisp *features*)
    (defmacro define-tree x
	(let* ((name (first x)) (tree (second x)))
	  `(defun ,name (word-list)
	     (interpret-tree ',tree word-list))))
    (defmacro define-tree (name tree)
      `(defun ,name (word-list)
	 (interpret-tree ',tree word-list))))

(defun interpret-tree (tree word-list &optional a-list)
  (cond ((endp tree) (values t nil word-list))
	((eq (first tree) (first word-list))
	 (interpret-tree (rest tree) (rest word-list) a-list))
	((eq '> (first tree))
	 (multiple-value-bind
	     (result binding word-list)
	     (funcall (second tree) word-list)
	   (when result
	     (interpret-tree (rest (rest tree))
			    word-list
			    (cons (list (second tree) binding)
				  a-list)))))
	((eq '* (first tree))
	 (do ((word-list word-list (rest word-list))
	      (binding-list nil (cons (first word-list) binding-list)))
	     ()
	   (multiple-value-bind (result binding word-list)
	       (interpret-tree (rest (rest tree))
			      word-list 
			      (cons (list (second tree) (reverse binding-list))
				    a-list))
	     (when result (return (values result binding word-list))))
	   (when (endp word-list) (return nil))))
	((eq 'rtn (first tree))
	 (values t (evaluate-forms (rest tree) a-list) word-list))
	((and (eq 'if-end-rtn (first tree))
	      (endp word-list))
	 (values t (evaluate-forms (rest tree) a-list) nil))
	((eq 'brnchs (first tree))
	 (interpret-branches (rest tree) word-list a-list))
	(t nil)))

(defun evaluate-forms (forms a-list)
  (eval `(let ,(make-let-variables a-list)
	      (progn ,@forms))))

(defun make-let-variables (a-list)
  (mapcar #'(lambda (pair) `(,(first pair) ',(second pair))) a-list))

(defun interpret-branches (branches word-list a-list)
  (if (endp branches)
      nil
      (multiple-value-bind (result binding words-left-over)
          (interpret-tree (first branches) word-list a-list)
        (if result
            (values result binding words-left-over)
            (interpret-branches (rest branches) word-list a-list)))))

;;;; THE COMPILER

(if (member 'gclisp *features*)
    (defmacro compile-tree x
      (let* ((name (first x)) (tree (second x)))
      `(defun ,name (word-list)
	 ,(compile-elements tree))))
    (defmacro compile-tree (name tree)
      `(defun ,name (word-list)
	 ,(compile-elements tree))))

(defun compile-elements (tree)
  (cond ((endp tree) '(values t nil word-list))
        ((eq '> (first tree))
	 `(multiple-value-bind (result ,(second tree) word-list)
	      (,(second tree) word-list)
	    (when result
		  ,(compile-elements (rest (rest tree))))))
        ((eq '* (first tree))
	 `(do ((word-list word-list (rest word-list))
	       (,(second tree) nil (append ,(second tree)
					   (list (first word-list)))))
	      ()
	   (multiple-value-bind (result binding word-list)
	       ,(compile-elements (rest (rest tree)))
	     (when result
	       (return (values result binding word-list))))
	   (when (endp word-list) (return nil))))
	((eq 'brnchs (first tree))
	 (compile-branches (rest tree)))
        ((eq 'rtn (first tree))
         `(values t (progn ,@(rest tree)) word-list))
        ((eq 'if-end-rtn (first tree))
         `(when (null word-list)
	    (values t (progn ,@(rest tree)) nil)))
        (t `(let ((current-word (first word-list))
		  (word-list (rest word-list)))
	      (when (eq current-word ',(first tree))
		,(compile-elements (rest tree)))))))

(defun compile-branches (forms)
  (unless (endp forms)
    `(multiple-value-bind (result binding words-left-over)
	 ,(compile-elements (first forms))
	 (if result
	     (values result binding words-left-over)
	     ,(compile-branches (rest forms))))))






;;;; San Marco LISP Explorer (R)
;;;; Copyright (c) 1984, 1985 San Marco Associates

#|
This file contains sample data for demonstrating a simple
transition-tree language-understanding system.  The database
system is in RDB.LSP.
|#

;;;; TOP-LEVEL INTERFACE AND REPLY PROCEDURES

(defun run-interface ()
  (print '>)
  (do ((input (read-sentence) (read-sentence)))
      ((endp input)
       (format t "~&Ok, goodbye.")
       (values))
    (unless (interface input)
      (format t "~&Sorry, I can't understand that.~
	      ~&Press the return key if you want to stop."))
    (print '>)))

#|

NOTE: WITH-INPUT-FROM-STRING effort lost in this version of Lisp;
READ doesn't do the right thing at the end of the string.
ALSO: string-trim doesn't like a list of characters; only a string works.

Failed tests:

(string-trim '(#\. #\?) ".foo?")
(read-sentence)
|#

#+development
(defun read-sentence ()
  (with-input-from-string
      (input (string-trim " ().?!" (read-line)))
    (do ((word (read input nil 'end-of-string)
	       (read input nil 'end-of-string))
	 (sentence nil))
	((eq word 'end-of-string) (return (reverse sentence)))
      (push word sentence))))

#-development
(defun read-sentence ()
  (with-open-stream
      (input (make-string-input-stream (string-trim " ().?!" (read-line))))
    (do ((word (read input nil 'end-of-string)
	       (read input nil 'end-of-string))
	 (sentence nil))
	((eq word 'end-of-string) (return (reverse sentence)))
      (push word sentence)))) 

(defun report-distance (a b)
  (format t "~&~a"
          (sqrt (+ (square (- (first a) (first b)))
                   (square (- (second a) (second b)))))))

(defun square (x) (* x x))

;;;; THE OBJECT GRAMMAR'S TRANSITION TREES

(define-tree interface
  (brnchs
    (count > objects if-end-rtn (db-call `(db-count ,objects)))
    (how many > objects are there if-end-rtn (db-call `(db-count ,objects)))
    (> enumerate > objects if-end-rtn (db-call `(db-show ,objects)))
    (> present the > attributes of > objects if-end-rtn
       (db-call `(db-show (db-project ,objects over ,@attributes))))
    (how far is > object1 from > object2 if-end-rtn
        (report-distance
	  (db-call `(db-extract-value ,object1 over peg))
	  (db-call `(db-extract-value ,object2 over peg))))))

(define-tree enumerate
  (brnchs (identify)
	  (describe)))

(define-tree present
  (brnchs (show me)
	  (what is)
	  (what are)
	  (give)
	  (display)
	  (print)
	  (present)))

(define-tree object1 (> objects rtn objects))

(define-tree object2 (> objects rtn objects))

(define-tree attributes
  (brnchs (> attribute > attributes	rtn (cons attribute attributes))
	  (and > attribute		rtn (list attribute))
	  (> attribute			rtn (list attribute))))

(define-tree attribute
  (brnchs (class	rtn 'class)
	  (color	rtn 'color)
	  (size		rtn 'size)
	  (weight	rtn 'weight)
	  (position	rtn 'peg-location)
	  (location	rtn 'peg-location)))

(define-tree attribute-values
  (brnchs (> attribute-value > attribute-values
	     rtn (append attribute-value attribute-values))
	  (and > attribute-value rtn attribute-value)
	  (> attribute-value rtn attribute-value)))

(define-tree attribute-value
  (brnchs (large	rtn '(size eql large))
	  (medium	rtn '(size eql medium))
	  (small	rtn '(size eql small))
	  (long		rtn '(size eql long))
	  (short	rtn '(size eql short))
	   
	  (black	rtn '(color eql black))
	  (blue		rtn '(color eql blue))
	  (red		rtn '(color eql red))
	  (yellow	rtn '(color eql yellow))
	  (gray		rtn '(color eql gray))
	   
	  (heavy	rtn '(weight eql heavy))
	  (light	rtn '(weight eql light))))

(define-tree multiple-objects
  (brnchs (> objects > multiple-objects
	     rtn `(db-union ,objects ,multiple-objects))
	  (and > objects		rtn objects)
	  (> objects			rtn objects)))

(define-tree objects
  (brnchs (> determiner > objects rtn objects)
	  (> attribute-values > object
	     rtn `(db-select ,object where ,@attribute-values))
	  (> object rtn object)))

(define-tree determiner
  (brnchs (a) (the)))

(define-tree object
  (brnchs (hammer	rtn 'hammers)
	  (hammers	rtn 'hammers)
	  (screwdriver	rtn 'screwdrivers)
	  (screwdrivers	rtn 'screwdrivers)
	  (saw		rtn 'saws)
	  (saws		rtn 'saws)
	  (wrench	rtn 'wrenches)
	  (wrenches	rtn 'wrenches)
	  ;;(block	rtn 'blocks)
	  ;;(blocks 	rtn 'blocks)
	  ))

;;;; DOCTOR

(define-tree doctor-tree
  (brnchs (i am worried about * words if-end-rtn
	   `(how long have you been worried about ,@words))
	  (* words mother * more-words if-end-rtn
	    (setf mother t)
	    '(tell me more about your family))
	  (* words computers * more-words if-end-rtn
	    '(do machines frighten you))
	  (* words > bad-word * more-words if-end-rtn
	    '(please do not use language like that))
	  (yes if-end-rtn '(please do not be so short with me))
	  (no if-end-rtn '(please do not be so short with me))
	  (* words if-end-rtn
	    (when (and (boundp 'mother) mother)
	      (setf mother nil)
	      '(earlier you spoke of your mother)))))

(define-tree bad-word
        (brnchs (darn)
		(shucks)))

;;;; STUDENT

(defun student (words)
  (multiple-value-bind
      (result value)
      (student-tree words)
    value))

(define-tree student-tree
  (brnchs (* words is * more-words if-end-rtn
	   `(= ,(student words) ,(student more-words)))
	  (the sum of * words and * more-words if-end-rtn
	   `(+ ,(student words) ,(student more-words)))
	  (the difference between * words and * more-words if-end-rtn
	   `(- ,(student words) ,(student more-words)))
	  (* words times * more-words if-end-rtn
	   `(* ,(student words) ,(student more-words)))
	  (twice * words if-end-rtn
	   `(* 2 ,(student words)))
	  (the square of * words if-end-rtn
	   `(expt ,(student words) 2))
	  (* words squared if-end-rtn
	   `(expt ,(student words) 2))
	  (* words if-end-rtn
	   (if (numberp (first words))
	       (first words)
	       words))))

;;;; TESTS

#|

Student Tests:

()
(STUDENT '(THE NUMBER OF SALES IS TWICE THE SQUARE OF MARKETING BUDGET))
(= (THE NUMBER OF SALES) (* 2 (EXPT (MARKETING BUDGET) 2))) 

(STUDENT '(THE NUMBER OF SALES IS 3 TIMES THE SQUARE OF MARKETING BUDGET))
(= (THE NUMBER OF SALES) (* 3 (EXPT (MARKETING BUDGET) 2))) 

				Student		     Tools		
interpreted interpreter		(/ 8.90 .22)	40.4 (/ 13.02 .22) 60.4
recordd to interpreted lisp	(/ 2.47  .22)	11.2 (/ 3.96 .22)  18.0 
recordd interpreter		(/ .61 .22)	 2.7 (/ .72 .22)    3.3
recordd to recordd lisp	(/ .22 .22)	 1.0 (/ .22 .22)    1.0 

Compiling makes it go 18 times faster.

Compiling transition trees to Lisp makes it go 3 times faster.

Database tests:

(db-project (db-select (db-union saws screwdrivers)
		       where color eql red length eql long)
	    over class size length)

|#
