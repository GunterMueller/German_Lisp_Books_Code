;;;; -*- mode:LISP; -*-
;;;; Copyright (c) 1984, 1985, 1986, 1987 
;;;; by Patrick H. Winston and San Marco Associates

;;This is material for the prolog chaining chapter of lisp book and
;;the San Marco LISP Explorer (R)

;; Warning: requires three procedures from forward chainer!

;; Revised, but not tested, 7 May 91

;;;; To be used with matching and unifying.

;;;; To be used with streams.

;;;; Common Lisp Procedures

(unless (functionp 'gentemp)
  (defun gentemp (x) (gensym x)))

;;;; Basic Procedures for Prolog

(defun filter-binding-stream (pattern stream)
  (stream-concatenate
    (stream-transform
      #'(lambda (bindings)
	  (stream-concatenate
	    (stream-cons
	      (match-pattern-to-assertions pattern bindings)
	      (stream-cons (match-pattern-to-rules pattern bindings)
			   (make-empty-stream)))))
      stream)))

(defun match-pattern-to-rules (pattern bindings)
  (stream-concatenate
    (stream-transform
      #'(lambda (rule) (try-rule pattern rule bindings))
      *rules*)))

(defun try-rule (pattern rule bindings)
  (let* ((rule (make-variables-unique rule))
	 (result (unify pattern (rule-then rule) bindings)))
    (if (eq 'fail result)
	(make-empty-stream)
	(apply-filters
		 (rule-ifs rule)
		 (stream-cons result (make-empty-stream))))))

(defun backward-chain (&rest patterns)
  (let ((binding-stream (apply-filters patterns
				       (stream-cons nil (make-empty-stream))))
	(variables (list-variables patterns))
	(displayed-answers nil))
    (if (endp variables)
	(if (stream-endp binding-stream)
	    'no
	    'yes)
	(do ((binding-stream binding-stream (stream-rest binding-stream)))
	    ((stream-endp binding-stream) 'no-more)
	  (let ((answer (make-answer variables (stream-first binding-stream))))
	    (unless (member answer displayed-answers :test #'equal)
	      (display-answer answer)
	      (push answer displayed-answers)
	      (unless (char= #\; (read-char))
		(return 'no-more))))))))

(defun list-variables (tree &optional names)
  (cond ((atom tree) names)
	((eq '? (first tree))
	 (if (member (second tree) names)
	     names
	     (append names (rest tree))))
	(t (list-variables (rest tree)
			   (list-variables (first tree)
					   names)))))

(defun list-variables (tree &optional names)
  (cond ((atom tree) names)
	((eq '? (first tree))
	 (if (or (eq '_ (second tree)) (member (second tree) names))
	     names
	     (append names (rest tree))))
	(t (list-variables (rest tree)
			   (list-variables (first tree)
					   names)))))

(defun make-answer (variables bindings)
  (instantiate-variables 
    (mapcar #'(lambda (variable) (list variable (list '? variable)))
	    variables)
    bindings))

(defun instantiate-variables (tree a-list)
  (cond ((atom tree) tree)
	((eq '? (first tree))
	 (let ((binding (assoc (second tree) a-list)))
	   (if binding
	       (instantiate-variables (second binding) a-list)
	       tree)))
	(t (cons (instantiate-variables (first tree) a-list)
		 (instantiate-variables (rest tree) a-list)))))

(defun display-answer (answers)
  (format t "~&-->")
  (dolist (answer answers)
    (format t "	~a = ~a" (first answer) (second answer))))

(defun make-variables-unique (rule)
  (let ((variables (list-variables rule)))
    (dolist (variable (reverse variables) rule)
      (setf rule (instantiate-variables
		   rule
		   (list (list variable (list '? (gentemp variable)))))))))

;;;; Basic Examples

#+comment
(progn
  (setf *assertions* (make-empty-stream) *rules* (make-empty-stream))

  (remember-assertion '(bozo is a mammal))
  (remember-assertion '(deedee has hair))
  (remember-rule '(identify1
		    ((? animal) has hair)
		    ((? animal) is a mammal)))
  (backward-chain '((? x) is a mammal)))

#+comment
(progn

(backward-chain '(robbie is a mammal))
NO 
(backward-chain '((? _) is a mammal))
YES 
(backward-chain '((? _) is a (? _)))
YES 
(backward-chain '(deedee is a mammal))
YES 
(backward-chain '((? x) is a mammal))
==>	X = BOZO
==>	X = DEEDEE
NO-MORE 
)

#+comment
(progn					;Subset of rules
  (setf *assertions* (make-empty-stream) *rules* (make-empty-stream))
  (remember-assertion '(ROBBIE HAS DARK SPOTS))
  (remember-assertion '(ROBBIE HAS TAWNY COLOR))
  (remember-assertion '(ROBBIE EATS MEAT))
  (remember-assertion '(ROBBIE HAS HAIR))
  (remember-assertion '(SUZIE HAS FEATHERS))
  (remember-assertion '(SUZIE FLIES WELL))
  (remember-rule '(identify1
		    ((? animal) has hair)
		    ((? animal) is a mammal)))
  (remember-rule '(identify3
		    ((? animal) has feathers)
		    ((? animal) is a bird)))
  (remember-rule '(identify5 
		    ((? animal) eats meat)
		    ((? animal) is a carnivore)))
  (remember-rule '(identify9
		    ((? animal) is a mammal)
		    ((? animal) is a carnivore)
		    ((? animal) has tawny color)
		    ((? animal) has dark spots)
		    ((? animal) is a cheetah)))
  (remember-rule '(identify15
		    ((? animal) is a bird)
		    ((? animal) flies well)
		    ((? animal) is a albatross)))
  
  )

#+comment
(progn
(backward-chain '(robbie is a cheetah))
YES 
(backward-chain '((? x) is a cheetah))
==>	X = ROBBIE
NO-MORE 
(time (backward-chain '((? _) is a (? _))))
YES 
(time (backward-chain '((? _) is a (? x))))
==>	X = MAMMAL
==>	X = BIRD
==>	X = CARNIVORE
==>	X = CHEETAH
==>	X = ALBATROSS
NO-MORE 

(time (backward-chain '((? x) is a (? y))))
-->	X = ROBBIE	Y = MAMMAL
-->	X = SUZIE	Y = BIRD
-->	X = ROBBIE	Y = CARNIVORE
-->	X = ROBBIE	Y = CHEETAH
-->	X = SUZIE	Y = ALBATROSS
NO-MORE 
)

;;;; Bratko's example:

#+comment
(progn
  (setf *assertions* (make-empty-stream) *rules* (make-empty-stream))

  (remember-assertion '(parent pam bob))
  (remember-assertion '(parent tom bob))
  (remember-assertion '(parent tom liz))
  (remember-assertion '(parent bob ann))
  (remember-assertion '(parent bob pat))
  (remember-assertion '(parent pat jim))
  
  (remember-assertion '(female pam))
  (remember-assertion '(male tom))
  (remember-assertion '(male bob))
  (remember-assertion '(female liz))
  (remember-assertion '(female ann))
  (remember-assertion '(female pat))
  (remember-assertion '(male jim))
  
  (remember-rule '(1
		    (parent (? x) (? y))
		    (offspring (? y) (? x))))
  
  (remember-rule '(2
		    (parent (? x) (? y))
		    (female (? x))
		    (mother (? x) (? y))))
  
  (remember-rule '(3
		    (parent (? x) (? y))
		    (parent (? y) (? z))
		    (grandparent (? x) (? z))))
  
  #+comment
  (remember-rule '(4
		    (parent (? z) (? x))
		    (parent (? z) (? y))
		    (female (? x))
		    (different (? x) (? y))
		    (sister (? x) (? y))))
  
  (remember-rule '(5
		    (parent (? x) (? z))
		    (predecessor (? x) (? z))))
  
  (remember-rule '(6
		    (parent (? x) (? y))
		    (predecessor (? y) (? z))
		    (predecessor (? x) (? z))))

  )

#+comment
(progn

(backward-chain '(parent bob pat))
YES 

(backward-chain '(parent (? x) liz))
==>	X = TOM
NO-MORE 

(backward-chain '(parent bob (? x)))
==>	X = ANN
==>	X = PAT
NO-MORE 

(backward-chain '(parent (? x) (? y)))
==>	X = PAM	Y = BOB
==>	X = TOM	Y = BOB
==>	X = TOM	Y = LIZ
==>	X = BOB	Y = ANN
==>	X = BOB	Y = PAT
==>	X = PAT	Y = JIM
NO-MORE 

(backward-chain '(parent (? y) jim) '(parent (? x) (? y)))
==>	Y = PAT	X = BOB
NO-MORE 

(backward-chain '(parent tom (? x)) '(parent (? x) (? y)))
==>	X = BOB	Y = ANN
==>	X = BOB	Y = PAT
NO-MORE 

(backward-chain '(parent (? x) ann) '(parent (? x) pat))
==>	X = BOB
NO-MORE 

(backward-chain '(offspring liz tom))
YES 

(time (backward-chain '(predecessor pam (? x))))
==>	X = BOB
==>	X = ANN
==>	X = PAT
==>	X = JIM
NO-MORE 
)

;;;; Problem example:

#+comment
(progn
  (setf *assertions* (make-empty-stream) *rules* (make-empty-stream))

  (remember-assertion '(parent james robert))
  (remember-assertion '(parent robert patrick))
  
  (remember-rule '(family1
		    (parent (? x) (? y))
		    (ancestor (? x) (? y))))

  (remember-rule '(family2
		    (parent (? x) (? y))
		    (ancestor (? y) (? z))
		    (ancestor (? x) (? z)))))

#+comment
(backward-chain '(ancestor (? x) patrick))
-->	X = ROBERT
-->	X = JAMES
NO-MORE 


