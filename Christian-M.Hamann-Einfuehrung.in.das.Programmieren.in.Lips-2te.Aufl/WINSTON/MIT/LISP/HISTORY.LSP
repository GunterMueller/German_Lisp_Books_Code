;;;; San Marco LISP Explorer (R)
;;;; Copyright (c) 1984, 1985 San Marco Associates

;;;; HISTORY STUFF

;;;; STRUCTURES

(defstruct (node (:print-function print-node))
  (parent nil)
  (children nil)
  (situation nil))

(defun print-node (structure &rest ignore)
  (format t "~a" (node-situation structure)))

(defvar current-node (make-node))

;;;; OR CLASSES

#+comment
(defclass node ()
  ((parent nil)
   (children nil)
   (situation nil)))

;;;; MACROS

(defmacro defun-with-history (name parameters &rest body)
  `(defun ,name ,parameters
     (let ((parent current-node)
           (current-node (make-node)))
       (attach-parent current-node parent)
       (attach-situation current-node (list ',name ,@parameters))
       ,@body)))

;;;; UTILITIES

(defun attach-parent (child parent)
  (setf (node-parent child) parent)	;Attach parent to child.
  (setf (node-children parent)		;Attach child to parent.
	(append (node-children parent)
		(list child))))

(defun attach-situation (node form)
    (setf (node-situation node) form))

;;;; SEARCH

(defun show-simple-tree (node &optional (indentation 0))
  (format t "~&~vt~a" 
          indentation
          (or (node-situation node) 'top-of-tree))
  (dolist (node (node-children node))
    (show-simple-tree node (+ 2 indentation))))

(defun show-tree (node &optional (leader "") (addition "") (arrow ""))
  (format t "~&~a~a~a"
	  leader arrow (or (node-situation node) 'top-of-tree))
  (when (node-children node)
    (dolist (node (butlast (node-children node) 1))
      (show-tree node
		 (concatenate 'string leader addition)
		 "  |    "
		 "  |--> "))
    (show-tree (first (last (node-children node)))
	       (concatenate 'string leader addition)
	       "       "
	       "  *--> "))
  (values))


(defun show-tree (node &optional (leader "") (arrow ""))
  (if (zerop (length leader))
      (format t "~&~a" (or (node-situation node) 'top-of-tree))
      (format t "~&~a~a~a"
              (subseq leader 0 (- (length leader) 7))
              arrow
              (node-situation node)))
  (when (node-children node)
    (dolist (node (butlast (node-children node) 1))
      (show-tree node
                 (concatenate 'string
                              leader
                              "  |    ")
                 "  |--> "))
    (show-tree (first (last (node-children node)))
               (concatenate 'string leader "       ")
               "  *--> ")))

(defun find-given-form (given-form &optional (node current-node))
  (let ((node-form (node-situation node)))
    (if (equal given-form node-form)
	node
	(dolist (child (node-children node))
	  (let ((result (find-given-form given-form child)))
	    (when result (return result)))))))

(defmacro tell-why (name &rest parameters)
  `(let* ((given-situation (list ',name ,@parameters))
	  (node (find-given-form given-situation)))
     (if (node-p node)
	 (cond ((node-situation (node-parent node))
		(format t "~&I did ~a because I wanted to ~a."
			given-situation
			(node-situation (node-parent node))))
	       (t (format t "~&I did ~a because you told me to."
			  given-situation)))
	 (format t "~&I did not ~a." given-situation))
     (values)))

(defmacro tell-how (name &rest parameters)
  `(let* ((given-situation (list ',name ,@parameters))
	  (node (find-given-form given-situation)))
     (if (node-p node)
	 (cond ((node-children node)
		(format t "~&I did ~a by the following operations:"
			given-situation)
		(dolist (child (node-children node))
		  (format t "~&  ~a" (node-situation child))))
	       (t (format t "~&I did ~a by just doing it."
			  given-situation)))
	 (format t "~&I did not ~a." given-situation))
     (values)))

(defmacro tell-when (name &rest parameters)
  `(let* ((given-situation (list ',name ,@parameters))
	  (node (find-given-form given-situation)))
     (if (node-p node)
	 (format t "~&I did it while I ~a."
		 (node-situation (find-top-node node)))
	 (format t "~&I did not ~a." given-situation))
     (values)))

(defun find-top-node (node)
  (if (node-situation (node-parent node))
      (find-top-node (node-parent node))
      node))

