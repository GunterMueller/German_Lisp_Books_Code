;;;; San Marco LISP Explorer (R)
;;;; Copyright (c) 1984, 1985 San Marco Associates

;;;; FOR NOW, THIS VERSION IS FOR TESTING THINGS THAT ARE NOT
;;;; AVAILABLE IN CLOS.

#-:development
(require 'cl "c:/gclisp2/modules/")

;;;; GET HISTORY STUFF

(load 'history.lsp)

;;;; STRUCTURES

(defstruct hand 
  (name 'unknown)
  (grasping nil)
  (x 0) (y 6))

(defstruct (brick (:print-function print-brick))
  (name 'unknown)
  (width 'unknown)
  (height 'unknown)
  (x -10) (y 0)
  (supporting-thing nil)
  (things-supported nil))

(defun print-brick (structure &rest ignore)
  (format t "~a" (brick-name structure)))

;;;; PRINCIPLE PROCEDURES

;; May want to clear the top of the object to be moved first,
;; to ensure that support won't be moved latter:

;history!
(defun-with-history put-on (object support)
  (if (get-space object support)
      (and (grasp object)
	   (move object support)
	   (ungrasp object))
      (format t "~&Sorry, there is no room for ~a on ~a."
	      object
	      support)))

(defun get-space (object support)
  (cond ((find-space object support))
	((make-space object support))))

(defun grasp (object)
  (unless (eq (hand-grasping *hand*) object)
    (when (brick-things-supported object) (clear-top object))
    (when (hand-grasping *hand*) (get-rid-of (hand-grasping *hand*)))
    (format t "~&Move hand to pick up ~a at location ~a."
	    object
	    (top-location object))
    (setf (hand-x *hand*) (top-location object))
    (setf (hand-y *hand*) (top-location object))
    (format t "~&Grasp ~a." object)
    (setf (hand-grasping *hand*) object))
  t)

;history!
(defun-with-history move (object support)
  (let ((newplace (get-space object support)))
    (remove-support object)
    (format t "~&Move ~a to top of ~a at location ~a."
	    object
	    support
	    newplace)
    (setf (brick-x object) (first newplace))
    (setf (brick-y object) (second newplace))
    (setf (hand-x *hand*) (first (top-location object)))
    (setf (hand-y *hand*) (second (top-location object)))
    (add-support object support))
  t)

(defun ungrasp (object)
  (when (brick-supporting-thing object)
    (format t "~&Ungrasp ~a." object)
    (setf (hand-grasping *hand*) nil)
    t))

;history!
(defun-with-history get-rid-of (object)
  (put-on object *table*))

;history!
(defun-with-history clear-top (object)
  (dolist (obstacle (brick-things-supported object))
    (get-rid-of obstacle))
  t)

(defun remove-support (object)
  (let ((support (brick-supporting-thing object)))
    (when support
      (setf (brick-things-supported support)
	    (remove object (brick-things-supported support)))
      (setf (brick-supporting-thing object) nil)
      t)))

(defun add-support (object support)
  (setf (brick-things-supported support)
	(cons object (brick-things-supported support)))
  (setf (brick-supporting-thing object) support)
  t)

;history!
(defun-with-history make-space (object support)
  (dolist (obstruction (brick-things-supported support))
    (get-rid-of obstruction)
    (let ((space (find-space object support)))
      (when space (return space)))))

;;;; LOCATION PRIMITIVES

(defun find-space (object support)
  (dotimes (offset (1+ (- (brick-width support)
			  (brick-width object))))
    (unless (intersections-p object offset
			     (brick-x support)
			     (brick-things-supported support))
      (return (list (+ offset (brick-x support))
		    (+ (brick-y support)
		       (brick-height support)))))))

(defun intersections-p (object offset base obstacles)
  (dolist (obstacle obstacles)
    (let* ((ls-proposed (+ offset base))
	   (rs-proposed (1- (+ ls-proposed (brick-width object))))
	   (ls-obstacle (brick-x obstacle))
	   (rs-obstacle (1- (+ ls-obstacle (brick-width obstacle)))))
      (unless (or (> ls-proposed rs-obstacle)
		  (< rs-proposed ls-obstacle))
	(return t)))))

(defun top-location (object)
  (list (+ (brick-x object)
	   (/ (brick-width object) 2))
	(+ (brick-y object)
	   (brick-height object))))

;;;; INITIALIZATION

(defun bw-init ()
  (defparameter *hand* (make-hand))
  (defparameter *blocks* (list 
  (make-brick :name 'b1 :width 2  :height 2)
  (make-brick :name 'b2 :width 2  :height 2)
  (make-brick :name 'b3 :width 4  :height 4)
  (make-brick :name 'b4 :width 2  :height 2)
  (make-brick :name 'b5 :width 2  :height 4)
  (make-brick :name 'b6 :width 4  :height 2)
  (make-brick :name 'b7 :width 2  :height 2)
  (make-brick :name 'b8 :width 2  :height 4)
  (make-brick :name '*table* :width 20 :height 0 :x 0 :y 0)))
  (dolist (l *blocks*) (set (brick-name l) l))
  (dolist (l (remove *table* *blocks*)) 
    (put-on l *table*))
  'done)

(bw-init)
