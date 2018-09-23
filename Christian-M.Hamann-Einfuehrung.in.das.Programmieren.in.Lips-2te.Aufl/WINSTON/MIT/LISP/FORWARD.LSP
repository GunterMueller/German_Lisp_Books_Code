;;;; -*- mode:LISP; -*-
;;;; Copyright (c) 1984, 1985, 1986, 1987 
;;;; by Patrick H. Winston and San Marco Associates

;;This is material for the forward chaining chapter of lisp book and
;;the San Marco LISP Explorer (R)

;; Revised, but not tested, 7 May 91

;;;; To be used with matching and unifying.

;;;; To be used with streams.

;;;; Basic Procedures for Forward Chaining

(defun try-assertion (pattern assertion bindings)
  (let ((result (match pattern assertion bindings)))
    (if (eq 'fail result)
        (make-empty-stream)
        (stream-cons result (make-empty-stream)))))

(defun match-pattern-to-assertions (pattern bindings)
  (stream-concatenate
    (stream-transform
      #'(lambda (assertion) (try-assertion pattern assertion bindings))
      *assertions*)))

(defun filter-binding-stream (pattern stream)
  (stream-concatenate
    (stream-transform
      #'(lambda (bindings)
	  (match-pattern-to-assertions pattern bindings))
      stream)))

(defun apply-filters (patterns initial-input-stream)
  (if (endp patterns)
      initial-input-stream
      (apply-filters (rest patterns)
		     (filter-binding-stream (first patterns)
					    initial-input-stream))))

(defun use-rule (rule)
  (let ((binding-stream (apply-filters (rule-ifs rule)
				       (stream-cons nil (make-empty-stream)))))
    (do ((binding-stream binding-stream (stream-rest binding-stream))
	 (success-switch nil))
	((stream-endp binding-stream) success-switch)
      (let ((result (instantiate-variables (rule-then rule)
				       (stream-first binding-stream))))
	(when (remember-assertion result)
	  (format t "~%Rule ~a indicates ~a." (rule-name rule) result)
	  (setf success-switch t))))))

(defun instantiate-variables (pattern a-list)
  (cond ((atom pattern) pattern)
	((eq '? (first pattern))
	 (second (assoc (second pattern) a-list)))
	(t (cons (instantiate-variables (first pattern) a-list)
		 (instantiate-variables (rest pattern) a-list)))))

(defun forward-chain ()
    (do ((rule-stream *rules* (stream-rest rule-stream))
         (repeat-switch nil))
        ((stream-endp rule-stream)
         (if repeat-switch
             (progn (format t "~%I'm making another pass over the rules.")
                    (forward-chain))
             (progn (format t "~%Nothing new noted.")
                    'done)))
      (when (use-rule (stream-first rule-stream))
        (setf repeat-switch t))))

;;;; Examples

#+comment
(progn
  (setf *assertions* (make-empty-stream) *rules* (make-empty-stream))
  (remember-assertion '(bozo is a dog))
  (remember-assertion '(deedee is a horse))
  (remember-assertion '(deedee is a parent of sugar))
  (remember-assertion '(deedee is a parent of brassy))
  (remember-rule
    '(identify16 
       ((? animal) is a (? species))
       ((? animal) is a parent of (? child))
       ((? child) is a (? species))))
  (forward-chain))

#+comment
(progn
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
  (remember-rule '(identify2
		    ((? animal) gives milk)
		    ((? animal) is a mammal)))
  (remember-rule '(identify3
		    ((? animal) has feathers)
		    ((? animal) is a bird)))
  (remember-rule '(identify4 
		    ((? animal) flies)
		    ((? animal) lays eggs)
		    ((? animal) is a bird)))
  (remember-rule '(identify5 
		    ((? animal) eats meat)
		    ((? animal) is a carnivore)))
  (remember-rule '(identify6
		    ((? animal) has pointed teeth)
		    ((? animal) has claws)
		    ((? animal) has forward eyes)
		    ((? animal) is a carnivore)))
  (remember-rule '(identify7
		    ((? animal) is a mammal)
		    ((? animal) has hoofs)
		    ((? animal) is a ungulate)))
  (remember-rule '(identify8 
		    ((? animal) is a mammal)
		    ((? animal) chews cud)
		    ((? animal) is a ungulate)))
  (remember-rule '(identify9
		    ((? animal) is a mammal)
		    ((? animal) is a carnivore)
		    ((? animal) has tawny color)
		    ((? animal) has dark spots)
		    ((? animal) is a cheetah)))
  (remember-rule '(identify10
		    ((? animal) is a mammal)
		    ((? animal) is a carnivore)
		    ((? animal) has tawny color)
		    ((? animal) has black stripes)
		    ((? animal) is a tiger)))
  (remember-rule '(identify11
		    ((? animal) is a ungulate)
		    ((? animal) has long neck)
		    ((? animal) has long legs)
		    ((? animal) has dark spots)
		    ((? animal) is a giraffe)))
  (remember-rule '(identify12
		    ((? animal) is a ungulate)
		    ((? animal) has black stripes)
		    ((? animal) is a zebra)))
  (remember-rule '(identify13 
		    ((? animal) is a bird)
		    ((? animal) does not fly)
		    ((? animal) has long neck)
		    ((? animal) has long legs)
		    ((? animal) is black and white)
		    ((? animal) is a ostrich)))
  (remember-rule '(identify14
		    ((? animal) is a bird)
		    ((? animal) does not fly)
		    ((? animal) swims)
		    ((? animal) is black and white)
		    ((? animal) is a penguin)))
  (remember-rule '(identify15
		    ((? animal) is a bird)
		    ((? animal) flies well)
		    ((? animal) is a albatross)))
  (remember-rule '(identify16 
		    ((? animal) is a parent of (? child))
		    ((? animal) is a (? species))
		    ((? child) is a (? species)))))


