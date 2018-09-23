;;;; -*- mode:LISP; -*-
;;;; Copyright (c) 1984, 1985, 1986, 1987 
;;;; by Patrick H. Winston and San Marco Associates

;;This is material for matching chapter of lisp book and
;;the San Marco LISP Explorer (R)

;;;; Auxiliary Procedures

(defun add-binding (pattern-variable-expression datum bindings)
 (if (eq '_ (second pattern-variable-expression))
     bindings
     (cons (list (second pattern-variable-expression) datum) bindings)))

(defun find-binding (pattern-variable-expression binding)
  (unless (eq '_ (second pattern-variable-expression))
    (assoc (second pattern-variable-expression) binding)))

(defun extract-key (binding)
  (first binding))

(defun extract-value (binding)
  (second binding))

;;;; Simple Matcher

(defun match (p d &optional bindings)   ;Final version!
  (cond
    ((and (atom p) (atom d))            ;Are both arguments atoms?
     (if (eql p d) bindings 'fail))     ;If EQL, ok; if not, fail.
    ((and (listp p) (eq '? (first p)))                  ;Is pattern a variable?
     (let ((binding (find-binding p bindings)))         ;Find binding, if any.
       (if binding                                      ;Is there a binding?
           (match (extract-value binding) d bindings)   ;If so, substitute.
           (add-binding p d bindings))))                ;If not, add binding.
    ((and (listp p) (listp d))          ;Are both pattern and datum lists?
     (let ((result (match (first p)     ;Match pattern's first element
                          (first d)     ; with datum's first element
                          bindings)))   ; using current bindings.
       (if (eq 'fail result)            ;Do first elements fail to match?
           'fail                        ;If so, fail.
           (match (rest p)              ;If not, match the
                  (rest d)              ; rest of the elements
                  result))))            ; using new bindings.
    (t 'fail)))                         ;Default. Fail.

;;;; Matcher with Auxiliary Procedures

(defun match-atoms (p d bindings)
  ;;See if P and D are the same:
  (if (eql p d)
      ;;If so, return the value of BINDINGS:
      bindings
      ;;Otherwise, return FAIL.
      'fail))

(defun match-variable (p d bindings)
  (let ((binding (find-binding p bindings)))
    ;;See if the pattern variable is known:
    (if binding 
        ;;If it is, substitute its value an try again:
        (match (extract-value binding) d bindings)      
        ;;Otherwise, add new binding:
        (add-binding p d bindings))))

(defun match-pieces (p d bindings)
  (let ((result (match (first p) (first d) bindings)))
    ;;See if the first parts match producing new bindings:
    (if (eq 'fail result)
	;;If they do not match, fail.
	'fail
	;;If they do match, try the rest parts using the resulting bindings:
	(match (rest p) (rest d) result))))

(defun match (p d &optional bindings)
  (cond ((and (atom p) (atom d))
         (match-atoms p d bindings))
        ((and (listp p) (eq '? (first p)))
         (match-variable p d bindings))
        ((and (listp p) (listp d))
         (match-pieces p d bindings))
        (t 'fail)))

;;;; Unifyer

(defun unify (p1 p2 &optional bindings)
  (cond ((and (atom p1) (atom p2))              ;Are both arguments atoms?
         (unify-atoms p1 p2 bindings))          ;If yes, ok; if no, fail.
        ((and (listp p1) (eq '? (first p1)))    ;Is p1 a variable?
         (unify-variable p1 p2 bindings))       ;Unify variable using bindings.
        ((and (listp p2) (eq '? (first p2)))    ;Is p2 a variable?
         (unify-variable p2 p1 bindings))       ;Unify variable using bindings.
        ((and (listp p1) (listp p2))            ;Are both patterns lists?
         (unify-pieces p1 p2 bindings))         ;Unify pieces.
        (t 'fail)))

(defun unify-atoms (p1 p2 bindings)     ;Identical to MATCH-ATOMS.
  (if (eql p1 p2) bindings 'fail))

(defun unify-pieces (p1 p2 bindings)    ;Identical to MATCH-PIECES.
  (let ((result (unify (first p1) (first p2) bindings)))
    (if (eq 'fail result)
        'fail
        (unify (rest p1) (rest p2) result))))

(defun unify-variable (p1 p2 bindings)
  (let ((binding (find-binding p1 bindings)))           ;Find binding, if any.
    (if binding                                         ;Is there a binding?
        (unify (extract-value binding) p2 bindings)     ;If yes, use value.
        (if (insidep p1 p2 bindings)                    ;Is p1 inside p2?
            'fail                                       ;If yes, fail.
            (add-binding p1 p2 bindings)))))            ;If no, add binding.

(defun insidep (variable expression bindings)
  (if (equal variable expression)
      nil
      (inside-or-equal-p variable expression bindings)))

(defun inside-or-equal-p (variable expression bindings)
  (cond ((equal variable expression) t)
        ((atom expression) nil)
        ((eq '? (first expression))
         (let ((binding (find-binding expression bindings)))
           (when binding
             (inside-or-equal-p variable (extract-value binding) bindings))))
        (t (or (inside-or-equal-p variable (first expression) bindings)
               (inside-or-equal-p variable (rest expression) bindings)))))
