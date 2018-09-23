;;; -*- Mode:Lisp -*-

;;;; San Marco LISP Explorer (R)
;;;; Copyright (c) 1984, 1985 San Marco Associates

;;;; IMPORTANT CHANGES
;;Changed things considerably by introducing movable-block and 
;;load-bearing-block.  Consistency with book not 100% certain.
;;;; ALSO
;;Got rid of constructors
;;Added -10, 0 initforms for x and y for blocks
;;Added 0, 0 initforms for x and y for table
;;Have option of initializing by putting blocks on the table,
;;as if from a parts feeder. 3/26/88

;;Got rid of *s on table
;;Retained *s on *hand* *blocks*

;;;; CLASSES

(defclass basic-block ()
	  ((name :initform 'unknown)
	   (width :initform 'unknown)
	   (height :initform 'unknown)
	   (x :initform -10)
	   (y :initform 0)
	   (home-x :initform 'unknown)
	   (home-y :initform 'unknown)
	   (supported-by :initform nil)
	   (support-for :initform nil))
	  (:accessor-prefix block-))

(defclass movable-block (basic-block) ())

(defclass load-bearing-block (basic-block) ())

(defclass brick (movable-block load-bearing-block) ())

(defclass wedge (movable-block) ())

(defclass ball (movable-block) ())

(defclass table (load-bearing-block) ())

#+comment
(defclass heavy-brick (brick)
	  ())

#+comment
(defclass cube (brick)
	  ())

#+comment
(defclass small-cube (cube)
	  ())

(defclass table (brick)
	  ((x :initform 0)
	   (y :initform 0)))

(defclass hand ()
	  ((name :initform 'unknown)
	   (grasping :initform nil)
	   (x :initform 'unknown) 
	   (y :initform 'unknown))
	  (:accessor-prefix hand-))

#+comment
(defmethod print-object ((x basic-block) stream)
	   (format stream "~a" (block-name x)))

;;;; HISTORY MACRO

;;; Following will not work until PCL is fixed:

#+comment
(defmacro defmethod-with-history (name parameters &rest body)
  `(progn
     (defmethod ,name ,parameters
	      ,@body)
     (defmethod ,name :around ,parameters
		(let ((parent *current-node*)
		      (*current-node* (make-instance 'node)))
		  (format t "~&Working in AROUND method.")
		  (attach-parent *current-node* parent)
		  (attach-action *current-node*
				    (list ',name
					  ,@(remove-specializers parameters)))
		  (call-next-method)))))

#+comment
(defun remove-specializers (parameter-list)
  (mapcar #'(lambda (element) (if (listp element) (first element) element))
          parameter-list))

;;;; PRINCIPLE PROCEDURES

;; May want to clear the top of the object to be moved first,
;; to ensure that support won't be moved latter:

(defmethod put-on ((object movable-block) (support basic-block))
  (if (get-space object support)
      (and (grasp object)
	   (move object support)
	   (ungrasp object))
      (format t "~&Sorry, there is no room for ~a on ~a."
	      (block-name object)
	      (block-name support))))

;;; As simpler procedue for the table:

#+comment
(defmethod put-on ((object movable-block) (support table))
      (and (grasp object)
	   (move object support)
	   (ungrasp object)))

#+comment
(defmethod put-on ((object heavy-brick) (support basic-block))
	   ;;This is dangerous, because other procedures will 
	   ;;assume it succeeds, probably.
	   (format t "~&Sorry, ~a is too heavy to move."
		   (block-name object))
	   nil)

(defmethod get-space ((object movable-block) (support basic-block))
	   (cond ((find-space object support))
		 ((make-space object support))))

;;; If it is the table, just use home position.  Seems to always look in
;;; plain version:

#+comment
(defmethod get-space ((object movable-block) (support table))
	   #+comment
	   (format t "~&For space on the table for ~a, ~
		   I'll just use the home position."
		   (block-name object))
	   (list (block-home-x object) (block-home-y object)))

#+comment
(defmethod get-space ((object small-cube) (support small-cube))
	   ;;If two small cubes, just a matter of whether there is
	   ;;something supported already.
	   (when (block-support-for support) (clear-top support))
	   (list (block-x support)
		 (+ (block-y support)
		    (block-height support))))

(defmethod grasp ((object movable-block))
  (unless (eq (hand-grasping *hand*) object)
    (when (block-support-for object) (clear-top object))
    (when (hand-grasping *hand*) (get-rid-of (hand-grasping *hand*)))
    (format t "~&Move hand to pick up ~a at location ~a."
	    (block-name object)
	    (top-location object))
    (setf (hand-x *hand*) (top-location object))
    (setf (hand-y *hand*) (top-location object))
    (format t "~&Grasp ~a." (block-name object))
    (setf (hand-grasping *hand*) object))
  t)

#+comment
(defmethod grasp ((object movable-block))
  (if (eq (hand-grasping *hand*) object)
      t
      (progn
	(when (block-support-for object) (clear-top object))
	(when (hand-grasping *hand*) (get-rid-of (hand-grasping *hand*)))
	(if (block-support-for object)
	    nil
	    (progn
	      (format t "~&Move hand to pick up ~a at location ~a."
		      (block-name object)
		      (top-location object))
	      (setf (hand-x *hand*) (top-location object))
	      (setf (hand-y *hand*) (top-location object))
	      (format t "~&Grasp ~a." (block-name object))
	      (setf (hand-grasping *hand*) object))))))

(defmethod grasp ((object movable-block))
  (cond ((eq (hand-grasping *hand*) object) t)
	(t (when (block-support-for object) (clear-top object))
	   (when (hand-grasping *hand*) (get-rid-of (hand-grasping *hand*)))
	   (cond ((block-support-for object) nil)
		 (t (format t "~&Move hand to pick up ~a at location ~a."
			    (block-name object)
			    (top-location object))
		    (setf (hand-x *hand*) (top-location object))
		    (setf (hand-y *hand*) (top-location object))
		    (format t "~&Grasp ~a." (block-name object))
		    (setf (hand-grasping *hand*) object)
		    t)))))

;;; Fix to remove remove-support and add-support and to add slow speed.

(defmethod move ((object movable-block) (support basic-block))
  (remove-support object)
  (let ((newplace (get-space object support)))
    (format t "~&Move ~a to top of ~a at location ~a."
	    (block-name object)
	    (block-name support)
	    newplace)
    (setf (block-x object) (first newplace))
    (setf (block-y object) (second newplace))
    (setf (hand-x *hand*) (first (top-location object)))
    (setf (hand-y *hand*) (second (top-location object))))
  (add-support object support)
  t)

(defmethod ungrasp ((object movable-block))
  (when (block-supported-by object)
    (format t "~&Ungrasp ~a." (block-name object))
    (setf (hand-grasping *hand*) nil)
    t))

(defmethod get-rid-of ((object movable-block))
  (put-on object table))

(defmethod clear-top ((object movable-block))
  (dolist (obstacle (block-support-for object))
    (get-rid-of obstacle))
  t)

;;; Remove and add should be primary methods in move befores and afters!!

(defmethod remove-support ((object movable-block))
  (let ((support (block-supported-by object)))
    (when support
      (setf (block-support-for support)
	    (remove object (block-support-for support)))
      (setf (block-supported-by object) nil)
      t)))

(defmethod remove-support ((object basic-block))
  t)

(defmethod add-support ((object movable-block)
			(support load-bearing-block))
  (setf (block-support-for support)
	(cons object (block-support-for support)))
  (setf (block-supported-by object) support)
  t)

(defmethod add-support ((object basic-block) (support basic-block))
  t)

;;;; FAKE BEFORES AND AFTERS

(defmethod remove-support ((object basic-block))
  (let ((support (block-supported-by object)))
    (when support
      (format t "~%I am removing support relations between ~a and ~a."
	      (block-name object) (block-name support))
      (setf (block-support-for support)
	    (remove object (block-support-for support)))
      (setf (block-supported-by object) nil)
      t)))

(defmethod add-support ((object basic-block) (support brick))
  (format t "~%I am adding support relations between ~a and ~a."
	  (block-name object) (block-name support))
  (setf (block-support-for support)
	(cons object (block-support-for support)))
  (setf (block-supported-by object) support)
  t)

;;;; LOCATION PRIMITIVES

(defmethod make-space ((object movable-block) (support basic-block))
  (dolist (obstruction (block-support-for support))
    (get-rid-of obstruction)
    (let ((space (find-space object support)))
      (when space (return space)))))

(defmethod find-space ((object movable-block) (support basic-block))
  (dotimes (offset (1+ (- (block-width support)
			  (block-width object))))
    (unless (intersections-p object offset
			     (block-x support)
			     (block-support-for support))
      (return (list (+ offset (block-x support))
		    (+ (block-y support)
		       (block-height support)))))))

(defun intersections-p (object offset base obstacles)
  (dolist (obstacle obstacles)
    (let* ((ls-proposed (+ offset base))
	   (rs-proposed (1- (+ ls-proposed (block-width object))))
	   (ls-obstacle (block-x obstacle))
	   (rs-obstacle (1- (+ ls-obstacle (block-width obstacle)))))
      (unless (or (> ls-proposed rs-obstacle)
		  (< rs-proposed ls-obstacle))
	(return t)))))

(defun top-location (object)
  (list (+ (block-x object)
	   (/ (block-width object) 2))
	(+ (block-y object)
	   (block-height object))))

;;;; TREE DISPLAY

(defun show-simple-tree (node &optional (leader ""))
  (format t "~&~a~a" leader (or (node-action node) 'top-of-tree))
  (dolist (node (node-children node))
    (show-simple-tree node (concatenate 'string leader "  ")))
  (values))

(defun find-action (given-form &optional (node *current-node*))
  (let ((node-form (node-action node)))
    (if (equal given-form node-form)
        node
        (dolist (child (node-children node))
          (let ((result (find-action given-form child)))
            (when result (return result)))))))

(defun tell-when-find-top (node)
  (if (node-action (node-parent node))
      (tell-when-find-top (node-parent node))
      node))

;;;; INITIALIZATION

#+comment
(defun bw-init ()
  (setf *blocks* (list 
  (make-instance 'brick :name 'b1      :width 2  :height 2 :x 0  :y 0)
  (make-instance 'brick :name 'b2      :width 2  :height 2 :x 2  :y 0)
  (make-instance 'brick :name 'b3      :width 4  :height 4 :x 4  :y 0)
  (make-instance 'brick :name 'b4      :width 2  :height 2 :x 8  :y 0)
  (make-instance 'wedge :name 'w5      :width 2  :height 4 :x 10 :y 0)
  (make-instance 'brick :name 'b6      :width 4  :height 2 :x 12 :y 0)
  (make-instance 'wedge :name 'w7      :width 2  :height 2 :x 16 :y 0)
  (make-instance 'ball  :name 'l8      :width 2  :height 2 :x 18 :y 0)
  (make-instance 'table  :name 'table :width 20 :height 0 :x 0  :y 0)))

  (setf *hand* (make-instance 'hand :name '*hand* :x 0  :y 6))
  
  (dolist (l *blocks*) (set (block-name l) l))

  (dolist (l (remove table *blocks*)) 
    (put-on l table))

  #+comment
  (progn
    (change-class b3 'heavy-brick)
    (change-class b1 'small-cube)
    (change-class b2 'small-cube)
    (change-class b4 'small-cube))

  'done)

(defun bw-init ()
 (setf *blocks* (list 
  (make-instance 'brick :name 'b1      :width 2  :height 2)
  (make-instance 'brick :name 'b2      :width 2  :height 2)
  (make-instance 'brick :name 'b3      :width 4  :height 4)
  (make-instance 'brick :name 'b4      :width 2  :height 2)
  (make-instance 'wedge :name 'w5      :width 2  :height 4)
  (make-instance 'brick :name 'b6      :width 4  :height 2)
  (make-instance 'wedge :name 'w7      :width 2  :height 2)
  (make-instance 'ball  :name 'l8      :width 2  :height 2)
  (make-instance 'table :name 'table :width 20 :height 0 :x 0 :y 0)))

  (setf *hand* (make-instance 'hand :name '*hand* :x 0  :y 6))
  
  (dolist (l *blocks*) (set (block-name l) l))

  (dolist (l (remove table *blocks*)) 
    (put-on l table))

  #+comment
  (progn
    (change-class b3 'heavy-brick)
    (change-class b1 'small-cube)
    (change-class b2 'small-cube)
    (change-class b4 'small-cube))

  'done)

(bw-init)

(setf test '(progn (bw-init) (put-on b4 b1) (put-on w7 b2) (put-on b1 b2)))
