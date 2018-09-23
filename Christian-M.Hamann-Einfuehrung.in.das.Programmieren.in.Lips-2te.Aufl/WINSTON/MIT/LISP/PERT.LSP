;;; -*- Mode:Lisp; Package:USER; Base:10; -*-

;;;; San Marco LISP Explorer (R)
;;;; Copyright (c) 1987 San Marco Associates


;;;; DEFINE STRUCTURES

(defstruct (event (:print-function print-event))
  (time 'unknown)
  (input-task nil)
  (output-tasks nil))

(defun print-event (structure stream ignore)
  (format stream "<event structure>"))

(defstruct (task (:print-function print-task))
  (name 'unknown)
  (input-events nil)
  (output-event nil)
  (duration 'unknown))

(defun print-task (structure stream ignore)
  (format stream "<task structure for ~a>" (task-name structure)))

;;;; DEFINE CONNECTION PROCEDURES

(defun connect (task inputs output)
  (setf (task-input-events task) inputs)	;Inputs
  (dolist (event inputs)
    (push task (event-output-tasks event)))
  (setf (task-output-event task) output)	;Output
  (setf (event-input-task output) task))

;;;; SET UP CLASS INSTANCES

(setf data '
 (let ((s (make-task :duration 4 :name 'select))
      (n (make-task :duration 6 :name 'negotiate))
      (d (make-task :duration 4 :name 'design))
      (p (make-task :duration 7 :name 'prepare))
      (f (make-task :duration 8 :name 'furnish))
      (m (make-task :duration 1 :name 'move))
      (e1 (make-event :time 0))
      (e2 (make-event))
      (e3 (make-event))
      (e4 (make-event))
      (e5 (make-event))
      (e6 (make-event))
      (e7 (make-event)))
  (connect s 	(list e1)	e2)
  (connect n 	(list e2)	e3)
  (connect d 	(list e2)	e4)
  (connect p 	(list e3 e4) 	e5)
  (connect f 	(list e3 e4) 	e6)
  (connect m 	(list e5 e6) 	e7)
  (setf *design* d)
  (setf *start* e1)
  'done))

(eval data)

;;;; FORWARD PROPAGATION

;;; Propagate though task:

(defun simulate-task (task)
  (when (event-times-known-p (task-input-events task))
    (let* ((start-time (latest-time (task-input-events task)))
	   (finish-time (+ (task-duration task) start-time)))
      (add-to-event-sequence
	`(announce-start-time ,start-time ',(task-name task)))
      (add-to-event-sequence
	`(announce-finish-time ,finish-time ',(task-name task)))
      (add-to-event-sequence 
	`(simulate-event ,finish-time ,(task-output-event task))))))

(defun announce-start-time (time name)
  (format t "~%Time ~a:~9tstarting~19t~a." time name))

(defun announce-finish-time (time name)
  (format t "~%Time ~a:~9tfinishing~19t~a." time name))

(defun add-to-event-sequence (form)
  (setf
    *event-sequence*
    (sort (cons form *event-sequence*)
	  #'earlier-first-p)))
  
(defun earlier-first-p (x y)
  (cond ((< (second x) (second y)) t)
	((= (second x) (second y))
	 (cond ((eq 'announce-finish-time (first x)) t)
	       ((eq 'announce-finish-time (first y)) nil)
	       ((eq 'simulate-event (first x)) t)
	       ((eq 'simulate-event (first y)) nil)))))

;;; Propagate though event:

(defun simulate-event (time event)
  (setf (event-time event) time)
  (dolist (output-task (event-output-tasks event))
    (simulate-task output-task)))

;;;; UTILITY PROCEDURES

(defun simulate (starting-event time)
  (setf *event-sequence* nil)
  (simulate-event time starting-event)
  (do () ()
    (if (endp *event-sequence*)
	(return 'done)
        (eval (pop *event-sequence*)))))

(defun event-times-known-p (list-of-events)
  (not (find-if-not
	 #'(lambda (event) (numberp (event-time event)))
	 list-of-events)))

(defun latest-time (events)
  (apply #'max (mapcar #'event-time events)))

;;;; TEST

