;;; -*- Mode:Lisp; Package:PCL; Base:10; -*-

;;;; San Marco LISP Explorer (R)
;;;; Copyright (c) 1987 San Marco Associates

(in-package 'pcl)

;;;; DEFINE CLASSES

(format t "~%Working on ASSERTION class.~%")

(defclass assertion ()
  ((name                :initform 'none)
   (lower-bound         :initform 0)
   (upper-bound         :initform 1)
   (lower-source        :initform nil)
   (upper-source        :initform nil)
   (constraints         :initform nil))
  (:accessor-prefix assertion-))
  
(format t "~%Working on CONSTRAINT class.~%")

(defclass constraint ()
  ((name	:initform 'none)
   (output	:initform 'unknown))
  (:accessor-prefix constraint-))

(format t "~%Working on BINARY CONSTRAINT class.~%")

(defclass binary-constraint (constraint)
  ((input	:initform 'unknown))
  (:accessor-prefix constraint-))

(format t "~%Working on TERNARY CONSTRAINT class.~%")

(defclass ternary-constraint (constraint)
  ((input-a	:initform 'unknown)
   (input-b	:initform 'unknown))
  (:accessor-prefix constraint-))

(format t "~%Working on ORDINARY NOT class.~%")

(defclass not-box (binary-constraint) ())

(format t "~%Working on ENABLEMENT class.~%")

(defclass enablement (binary-constraint)
  ((strength 	:initform 1))	  
  (:accessor-prefix constraint-))

(format t "~%Working on AND classes.~%")

(defclass and-box (ternary-constraint) ())

(defclass independant-and (ternary-constraint) ())

(format t "~%Working on OR classes.~%")

(defclass or-box (ternary-constraint) ())

(defclass independant-or (ternary-constraint) ())

#+comment
(defmethod print-object ((x constraint) stream)
  (format stream "~a" (constraint-name x)))

#+comment
(defmethod print-object ((x assertion) stream)
  (format stream "~a" (assertion-time x)))

;;;; DEFINE ASSERTION PROCEDURES

(format t "~%Working on CONNECTION methods.~%")

(defmethod connect2 ((c constraint) (i assertion) (o assertion))
  (setf (constraint-input  c) i)
  (setf (constraint-output c) o)
  (setf (assertion-constraints i) (adjoin c (assertion-constraints i)))
  (setf (assertion-constraints o) (adjoin c (assertion-constraints o))))

(defmethod connect3 ((c constraint) (a assertion) (b assertion) (o assertion))
  (setf (constraint-input-a c) a)
  (setf (constraint-input-b c) b)
  (setf (constraint-output  c) o)
  (setf (assertion-constraints a) (adjoin c (assertion-constraints a)))
  (setf (assertion-constraints b) (adjoin c (assertion-constraints b)))
  (setf (assertion-constraints o) (adjoin c (assertion-constraints o))))

;;;; PROPAGATION

(format t "~%Working on ENABLEMENT.~%")

(defmethod propagate ((constraint not-box) (source assertion))
  (let* ((i (constraint-input constraint))
	 (o (constraint-output constraint))
	 (li (assertion-lower-bound i))
	 (ui (assertion-upper-bound i))
	 (lo (assertion-lower-bound o))
	 (uo (assertion-upper-bound o)))
    (unless (eq o source)
      (propagate o constraint (- 1 ui) (- 1 li)))
    (unless (eq i source)
      (propagate i constraint (- 1 uo) (-1 lo)))))

(format t "~%Working on ENABLEMENT.~%")

(defmethod propagate ((constraint enablement) (source assertion))
  (let* ((i (constraint-input constraint))
	 (o (constraint-output constraint))
	 (s (constraint-strength constraint))
	 (li (assertion-lower-bound i))
	 (ui (assertion-upper-bound i))
	 (lo (assertion-lower-bound o))
	 (uo (assertion-upper-bound o)))
    (unless (eq o source)
      (propagate o constraint (* li s) 1))
    (unless (eq i source)
      (propagate i constraint 0 (/ uo s)))))

(format t "~%Working on ORDINARY OR.~%")

(defmethod propagate ((constraint or-box) (source assertion))
  (let* ((a (constraint-input-a constraint))
	 (b (constraint-input-b constraint))
	 (o (constraint-output constraint))
	 (la (assertion-lower-bound a))
	 (ua (assertion-upper-bound a))
	 (lb (assertion-lower-bound b))
	 (ub (assertion-upper-bound b))
	 (lo (assertion-lower-bound o))
	 (uo (assertion-upper-bound o)))
    (unless (eq o source)
      (propagate o constraint (max la lb) (+ ua ub)))
    (unless (eq a source)
      (propagate a constraint (- lo ub) uo))
    (unless (eq b source)
      (propagate b constraint (- lo ua) uo))))

(format t "~%Working on INDEPENDANT OR.~%")

(defmethod propagate ((constraint independant-or) (source assertion))
  (let* ((a (constraint-input-a constraint))
	 (b (constraint-input-b constraint))
	 (o (constraint-output constraint))
	 (la (assertion-lower-bound a))
	 (ua (assertion-upper-bound a))
	 (lb (assertion-lower-bound b))
	 (ub (assertion-upper-bound b))
	 (lo (assertion-lower-bound o))
	 (uo (assertion-upper-bound o)))
    (unless (eq o source)
      (propagate o constraint
		 (+ la lb (- (* la lb)))
		 (+ ua ub (- (* ua ub)))))
    (unless (eq a source)
      (propagate a constraint
		 (if (= 1 ub) 0 (/ (- lo ub) (- 1 ub)))
		 (if (= 1 lb) 0 (/ (- uo lb) (- 1 lb)))))
    (unless (eq b source)
      (propagate b constraint
		 (if (= 1 ua) 0 (/ (- lo ua) (- 1 ua)))
		 (if (= 1 la) 0 (/ (- uo la) (- 1 la)))))))

(format t "~%Working on ORDINARY AND.~%")

(defmethod propagate ((constraint and-box) (source assertion))
  (let* ((a (constraint-input-a constraint))
	 (b (constraint-input-b constraint))
	 (o (constraint-output constraint))
	 (la (assertion-lower-bound a))
	 (ua (assertion-upper-bound a))
	 (lb (assertion-lower-bound b))
	 (ub (assertion-upper-bound b))
	 (lo (assertion-lower-bound o))
	 (uo (assertion-upper-bound o)))
    (unless (eq o source)
      (propagate o constraint (+ la lb -1) (min ua ub)))
    (unless (eq a source)
      (propagate a constraint la (1+ (- uo lb))))
    (unless (eq b source)
      (propagate b constraint lb (1+ (- uo la))))))

(format t "~%Working on ASSERTION.~%")

(defmethod propagate ((assertion assertion) (source constraint) lower upper)
  (let* ((old-upper (assertion-upper-bound assertion))
	 (old-lower (assertion-lower-bound assertion))
	 (new-upper (max 0 (min old-upper upper)))
	 (new-lower (min 1 (max old-lower lower))))
    (unless (= old-upper new-upper)
      (setf (assertion-upper-bound assertion) new-upper)
      (setf (assertion-upper-source assertion) source))
    (unless (= old-lower new-lower)
      (setf (assertion-lower-bound assertion) new-lower)
      (setf (assertion-lower-source assertion) source))
    (when (or (/= old-lower new-lower) (/= old-upper new-upper))
      (format t "~%Constraint ~a has modified ~a's values:~
	      ~%[~4,2f, ~4,2f] --> [~4,2f, ~4,2f]"
	      (constraint-name source)
	      (assertion-name assertion) 
	      old-lower old-upper
	      new-lower new-upper)
      (dolist (constraint (remove source (assertion-constraints assertion)))
	(propagate constraint assertion)))))

(format t "~%Working on PROPAGATE with no constraint argument.~%")

(defmethod propagate ((assertion assertion) lower upper)
    (setf (assertion-upper-bound assertion) upper)
    (setf (assertion-upper-source assertion) 'user)
    (setf (assertion-lower-bound assertion) lower)
    (setf (assertion-lower-source assertion) 'user)
    (format t "~%You have initiated propagation from ~a with values:~
	      ~%[~4,2f, ~4,2f]"
	      (assertion-name assertion) lower upper)
      (dolist (constraint (assertion-constraints assertion))
	(propagate constraint assertion)))

;;;; FORGETTING

(setf dummy (make-instance 'assertion))

(defmethod forget ((assertion assertion) source)
  (when (or (eq (assertion-lower-source assertion) source)
	    (eq (assertion-upper-source assertion) source))
    (format t "~%Forgetting ~a's values because of change to ~a:~
	      ~%[~4,2f, ~4,2f] --> [~4,2f, ~4,2f]"
	    (assertion-name assertion) (constraint-name source)
	    (assertion-lower-bound assertion)
	    (assertion-upper-bound assertion)
	    0 1)
    (setf (assertion-lower-bound assertion) 0)
    (setf (assertion-upper-bound assertion) 1)
    (setf (assertion-lower-source assertion) nil)
    (setf (assertion-upper-source assertion) nil)
    (dolist (constraint (remove source (assertion-constraints assertion)))
      (forget constraint assertion))))

(defmethod forget ((constraint binary-constraint) source)
  (let* ((i (constraint-input constraint))
	 (o (constraint-output constraint)))
    (unless (eq o source) (forget o constraint))
    (unless (eq i source) (forget i constraint))
    (propagate constraint dummy)))

(defmethod forget ((constraint ternary-constraint) source)
  (let* ((a (constraint-input-a constraint))
	 (b (constraint-input-b constraint))
	 (o (constraint-output constraint)))
    (unless (eq o source) (forget o constraint))
    (unless (eq a source) (forget a constraint))
    (unless (eq b source) (forget b constraint))
    (propagate constraint dummy)))

;;;; BOOK EXAMPLE

(setf phw
 '(progn
  (let ((assertions
	  (list (make-instance 'instance 'assertion :name 'broker1)
		(make-instance 'assertion :name 'broker2)
		(make-instance 'assertion :name 'broker-opinion)
		(make-instance 'assertion :name 'mystic1)
		(make-instance 'assertion :name 'mystic2)
		(make-instance 'assertion :name 'mystic-opinion)
		(make-instance 'assertion :name 'recommendation)))	
	(constraints
	  (list (make-instance 'or-box :name 'broker-constraint)
		(make-instance 'or-box :name 'mystic-constraint)
		(make-instance 'or-box :name 'public-constraint))))

    (format t "~%Constraints and assertions made.")

    (dolist (l constraints) (set (constraint-name l) l))
    (dolist (l assertions) (set (assertion-name l) l))
    
    (format t "~%Constraints and assertions named.")
    
    (connect3 broker-constraint broker1 broker2 broker-opinion)
    (connect3 mystic-constraint mystic1 mystic2 mystic-opinion)
    (connect3 public-constraint broker-opinion mystic-opinion recommendation)
    
    (format t "~%Constraints and assertions connected."))

 (setf a '(propagate broker1 .25 .75)
      b '(propagate broker2 .33 .66)
      c '(propagate mystic1 .15 .15) 
      d '(propagate mystic2 .85 .85)
      e '(progn (change-class public-constraint 'independant-or)
		(forget public-constraint dummy))
      f '(progn (change-class broker-constraint 'independant-or)
		(forget broker-constraint dummy))
      g '(progn (change-class mystic-constraint 'independant-or)
		(forget mystic-constraint dummy))
      l '(propagate recommendation .5 1)
      m '(propagate recommendation .5 .75)
      n '(propagate recommendation .7 .75))
 t))

;;;; QUINLAN'S EXAMPLE

(setf rjq
 '(progn
  (let ((assertions (list (make-instance 'assertion :name 'a)
			  (make-instance 'assertion :name 'b)
			  (make-instance 'assertion :name 'c)
			  (make-instance 'assertion :name 'd)
			  (make-instance 'assertion :name 'e)))
	
	(constraints (list (make-instance 'and-box :name 'and)
			   (make-instance 'independant-or :name 'i-or)
			   (make-instance 'enablement :name 'enablement :strength .8))))

    (format t "~%Constraints and assertions made.")

    (dolist (l constraints) (set (constraint-name l) l))
    (dolist (l assertions) (set (assertion-name l) l))
    
    (format t "~%Constraints and assertions named.")
    
    (connect3 and a b c)
    (connect3 i-or c d e)
    (connect2 enablement b e)
    
    (format t "~%Constraints and assertions connected."))
 (setf l '(propagate c .55 .65))
 (setf m '(propagate b .9 .9))
 (setf n '(propagate e .0 .5))))










#|

;;;; SET UP EXAMPLE CLASS INSTANCES AND BASIC TEST

(progn
  (let ((assertions (list (make-instance 'assertion :name 'a)
		     (make-instance 'assertion :name 'b)
		     (make-instance 'assertion :name 'c)))
	
	(constraints (list (make-instance 'or-box :name 'abc))))

    (format t "~%Constraints and assertions made.")

    (dolist (l constraints) (set (constraint-name l) l))
    (dolist (l assertions) (set (assertion-name l) l))
    
    (format t "~%Constraints and assertions named.")
    
    (connect3 abc a b c)   
    
    (format t "~%Constraints and assertions connected.")))

(setf (assertion-lower-bound a) .25)
(setf (assertion-upper-bound a) .75)

(setf (assertion-lower-bound b) 0)
(setf (assertion-upper-bound b) 1)

(propagate abc a)

(setf (assertion-lower-bound b) .33)
(setf (assertion-upper-bound b) .5)

(progn (change-class abc 'independant-or) t)

(propagate abc a)

(progn (change-class abc 'exclusive-or) t)

(propagate abc a)

|#
