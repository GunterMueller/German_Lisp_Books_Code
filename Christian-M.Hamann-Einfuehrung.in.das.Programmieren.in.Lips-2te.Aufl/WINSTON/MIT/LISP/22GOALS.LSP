;;;; -*- mode:Lisp; package:user -*- ;;;;
;;;; Created: 10 December 1992

;;;; PROCEDURES

(defclass node ()
  ((parent :accessor node-parent :initform nil)
   (children :accessor node-children :initform nil)
   (action :accessor node-action :initform nil)))

(defvar *current-node* (make-instance 'node))

(defmethod attach-parent ((child node) (parent node))
  (setf (node-parent child) parent)     ;Attach parent to child.
  (setf (node-children parent)          ;Attach child to parent.
        (append (node-children parent)
                (list child))))

(defmethod attach-action ((node node) action)
  (setf (node-action node) action))

(defmacro define-history-method (name parameters &rest body)
  `(defmethod ,name :around ,parameters
     (let* ((parent *current-node*)
	    (*current-node* (make-instance 'node))
	    (primary-method-value (call-next-method)))
       (when primary-method-value
	 (attach-parent *current-node* parent)
	 (attach-action *current-node*
			(list ',name ,@(remove-specializers parameters))))
       primary-method-value)))

(defun remove-specializers (parameter-list)
  (mapcar #'(lambda (element) (if (listp element) (first element) element))
	  parameter-list))

(define-history-method put-on ((object movable-block)
			       (support basic-block)))

(define-history-method get-rid-of ((object movable-block)))

(define-history-method make-space ((object movable-block)
				   (support basic-block)))

(define-history-method clear-top ((support load-bearing-block)))

(define-history-method move ((object movable-block)
                             (support basic-block)))

#+:clos
(defmethod print-object ((x basic-block) stream)
  (format stream "~a" (block-name x)))

#-gclisp
(defun show-simple-tree (node &optional (indentation 0))
  (format t "~&~vt~a" 
          indentation
          (or #+:clos (node-action node)
	      #-:clos (express-node-action node)
	      'top-of-tree))
  (dolist (node (node-children node))
    (show-simple-tree node (+ 2 indentation))))

#+gclisp
(defun show-simple-tree (node &optional (indentation 0))
  (format t "~&~a~a" 
	  (make-string indentation :initial-element #\space)
	  (or (express-action (node-action node)) 'top-of-tree))
  (dolist (node (node-children node))
    (show-simple-tree node (+ 2 indentation))))

#-:clos
(defun express-action (action)
  (when action (cons (first action) (mapcar #'block-name (rest action)))))

(defun find-action (given-form &optional (node *current-node*))
  (let ((node-form (node-action node)))
    (if (equal given-form node-form)
        node
        (dolist (child (node-children node))
          (let ((result (find-action given-form child)))
            (when result (return result)))))))

(defmacro tell-why (name &rest parameters)
  `(tell-why-aux (list ',name ,@parameters)))

(defun tell-why-aux (given-action)
  (let ((node (find-action given-action)))
    (if (not (null node))
	(cond ((node-action (node-parent node))
	       (format t "~&I did ~a because I wanted to ~a."
		       #-:clos
		       (express-action given-action)
		       #+:clos
		       given-action
		       #-:clos
		       (express-action (node-action (node-parent node)))
		       (node-action (node-parent node))))
	      (t (format t "~&I did ~a because you told me to."
			 #-:clos
			 (express-action given-action)
			 #+:clos
			 given-action)))
      (format t "~&I did not ~a."
	      #-:clos
	      (express-action given-action)
	      #+:clos
	      given-action))
    'done))
