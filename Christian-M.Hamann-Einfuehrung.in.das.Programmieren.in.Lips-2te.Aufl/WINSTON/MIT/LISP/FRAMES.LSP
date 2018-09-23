;;;; FRAMES

(defclass thing () ())
(defclass person (thing) () (:constructor make-person))
(defclass professor (person) () (:constructor make-professor))
(setf grumpy (make-professor))
(setf patrick (make-person))

;;;; SLOTS

(defclass slot () () (:constructor make-slot))
(defclass size (slot) () (:constructor make-size))
(defclass personality (slot) () (:constructor make-personality))

(setf slot (make-slot))
(setf size (make-size))
(setf personality (make-personality))

;;;; FACETS

(defclass facet () () (:constructor make-facet))

(defclass value (facet) () (:constructor make-value))
(defclass default (facet) () (:constructor make-default))
(defclass procedure (facet) () (:constructor make-procedure))

(setf facet (make-facet))
(setf value (make-value))
(setf value (make-default))
(setf value (make-procedure))

(defmethod fget-v-d ((frame professor) (slot personality) (facet value))
	   (and frame slot facet (list 'eccentric)))

(defmethod fget-v-d ((frame thing) (slot slot) (facet facet))
	   (and frame slot facet (list 'unknown)))

(fget-v-d grumpy personality value)

(fget-v-d patrick personality facet)


