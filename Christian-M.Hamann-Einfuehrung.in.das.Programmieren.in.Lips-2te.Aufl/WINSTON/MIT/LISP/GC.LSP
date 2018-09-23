(setf memory
      #(symbol	unmarked	x	20
	box	unmarked	8	12
	symbol	unmarked	a	unbound
	box	unmarked	16	nil
	symbol	unmarked	b	unbound
	box	unmarked	24	28
	symbol	unmarked	l	unbound
	box	unmarked	32	nil
	symbol	unmarked	m	unbound
	free	unmarked	nil	nil))

(setf next-free-chunk 36 last-free-chunk 36)

(defun place-marks (index)
  (when (numberp index)
    (unless (eq 'marked (aref memory (+ 1 index)))
      (setf (aref memory (+ 1 index)) 'marked)
      (when (eq 'box (aref memory index))
	(place-marks (aref memory (+ 2 index)))
	(place-marks (aref memory (+ 3 index)))))))

(defun sweep ()
  (do ((index 0 (+ 4 index)))
      ((= index 40))
    (unless (or (eq 'free (aref memory index))
		(eq 'marked (aref memory (+ 1 index))))
      (setf (aref memory index) 'free)
      (setf (aref memory (+ 2 index)) nil)
      (setf (aref memory (+ 3 index)) nil)
      (setf (aref memory (+ 3 last-free-chunk)) index)
      (setf last-free-chunk index))
    (setf (aref memory (+ 1 index)) 'unmarked)))

;;For plain marker
(defun mark ()
  (do ((index 0 (+ 4 index)))
      ((= index 40))
    (when (and (eq 'symbol (aref memory index))
	       (not (eq 'unbound (aref memory (+ 3 index)))))
      (setf (aref memory (+ 1 index)) 'marked)
      (place-marks (aref memory (+ 3 index))))))

;;For hairy marker
(defun nr-mark (&aux next-chunk)
  (do ((index 0 (+ 4 index)))
      ((= index 40))
    (when (and (eq 'symbol (aref memory index))
	       (not (eq 'unbound (aref memory (+ 3 index)))))
      (setf (aref memory (+ 1 index)) 'marked)
      (nr-place-marks index (aref memory (+ 3 index))))))

(defun nr-place-marks (last-chunk this-chunk &aux next-chunk (direction 'down))
  (do () ()
    (case (aref memory this-chunk)
      (box (case direction
	     (down (setf next-chunk (aref memory (+ 2 this-chunk)))
		   (setf (aref memory (+ 2 this-chunk)) last-chunk))
	     (up (case (aref memory (+ 1 this-chunk))
		   (unmarked (setf (aref memory (+ 1 this-chunk)) 'marked)
			     (setf next-chunk (aref memory (+ 3 this-chunk)))
			     (setf (aref memory (+ 3 this-chunk))
				   (aref memory (+ 2 this-chunk)))
			     (setf (aref memory (+ 2 this-chunk)) last-chunk)
			     (setf direction 'down))
		   (marked (setf next-chunk (aref memory (+ 3 this-chunk)))
			   (setf (aref memory (+ 3 this-chunk)) last-chunk)
			   (setf direction 'up))))))
      (symbol (case direction
		(down (setf (aref memory (+ 1 this-chunk)) 'marked)
		      (setf next-chunk last-chunk)
		      (setf direction 'up))
		(up (return)))))
    (when next-chunk (setf last-chunk this-chunk this-chunk next-chunk))))

(defun show (&optional last this next)
  (send *terminal-io* :clear-screen)
  (format t "~%#(")
  (dotimes (i 10)
    (let ((p (* 4 i)))
      (format t ;"~%~10a~10a~10a~10a ~a	;Elements ~a--~a."
	      "~%~a     ~a     ~a     ~a      ~a	;Elements ~a--~a."
	      (aref memory (+ 0 p))
	      (aref memory (+ 1 p))
	      (aref memory (+ 2 p))
	      (aref memory (+ 3 p))
	      (cond ((eql last p) 'l)
		    ((eql this p) 't)
		    ((eql next p) 'n)
		    (t ""))
	      p (+ p 3)) ))
  (format t "~%)"))



