;;;; San Marco LISP Explorer (R)
;;;; Copyright (c) 1984, 1985 San Marco Associates

#|
This file contains a fake relational data base system.  Relations
are defined by lists, the first of which contains the field
names.

|#

;;;; FAKE RELATIONAL DATABASE PROCEDURES---SEE TEXT

;;;; MAKING RELATIONS

(defmacro db-make-relation (relation &rest fields-and-records)
  `(setf ,relation
	 ',fields-and-records))

;;;; THE OBJECT DESCRIPTIONS

(db-make-relation saws
  (class	color	size	weight	peg)
  (saw		black	medium	heavy	(2 6)))

(db-make-relation hammers
  (class	color	size	weight	peg)
  (hammer	blue	large	heavy	(4 6)))

(db-make-relation wrenches
  (class	color	size	weight	peg)
  (wrench	gray	small	light	(6 6))
  (wrench	gray	large	heavy	(8 6)))
							       
(db-make-relation screwdrivers
  (class	color	size	weight	peg)
  (screwdriver	blue	long	light	(4 2))
  (screwdriver	black	long	light	(5 2))
  (screwdriver	red	long	heavy	(6 2))
  (screwdriver	red	short	light	(7 2)))

