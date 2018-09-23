#+comment
(push 'dynamic *features*)
#+comment
(pop *features*)

(defun bind-variables (variables values &optional a-list)
  (append (mapcar #'list variables values) a-list))

(defun micro-eval (form &optional environment)
  (if (atom form)
      (cond ((numberp form) form)
	    ((eq t form) t)
	    ((eq nil form) nil)
	    (t (let ((binding (assoc form environment)))
		 (if binding
		     (second binding)
		     (error "I could find no variable binding for ~a."
			    form)))))
      (case (first form)
	(m-quote (second form))
	(m-if (if (micro-eval (second form) environment)
		  (micro-eval (third form) environment)
		  (micro-eval (fourth form) environment)))
	(m-defun (setf (get (second form) 'm-procedure)
		       `(m-lambda ,(third form) ,(fourth form)))
		 (second form))
	(m-apply (micro-apply (micro-eval (second form) environment)
			      (micro-eval (third form) environment)
			      #+dynamic environment))
	(m-function `(m-closure ,(second (second form))
				,(third (second form))
				,environment))
	(m-setq (setf (second (assoc (second form) environment))
		      (micro-eval (third form) environment)))
	(t (micro-apply
	     (first form)                    
	     (mapcar #'(lambda (x) (micro-eval x environment))
		     (rest form))
	     #+dynamic environment)))))

(defun micro-apply (procedure argument-values #+dynamic environment)
  (if (symbolp procedure)
      (case procedure
	(m-first (first (first argument-values)))
	(m-rest (rest (first argument-values)))
	(m-cons (cons (first argument-values) (second argument-values)))
	(m-endp (endp (first argument-values)))
	(m-not (not (first argument-values)))
	(m-eq (eq (first argument-values) (second argument-values)))
	(t (let ((procedure-description (get procedure 'm-procedure)))
	     (if procedure-description
		 (micro-apply procedure-description argument-values)
		 (error "I could find no procedure description for ~a."
			procedure)))))
      (case (first procedure)
	(m-lambda (micro-eval (third procedure)
			      (bind-variables (second procedure)
					      argument-values
					      #+dynamic environment)))
	(m-closure (micro-eval (third procedure)
			       (bind-variables (second procedure)
					       argument-values
					       (fourth procedure)))))))

(micro-eval '(m-first (m-quote (a b c))))

(setf (get 'm-second 'm-procedure) 
      '(m-lambda (l) (m-first (m-rest l))))

(micro-eval '(m-second (m-quote (a b c))))

(setf (get 'm-append 'm-procedure)
      '(m-lambda (l1 l2)
	 (m-if (m-endp l1)
	       l2
	       (m-cons (m-first l1)
		       (m-append (m-rest l1) l2)))))

(micro-eval '(m-append (m-quote (a b c)) (m-quote (x y z))))

(setf (get 'toggle 'm-procedure)
      (micro-eval
	'((m-lambda (x) (m-function (m-lambda () (m-setq x (m-not x)))))
	  t)))

(micro-eval '(toggle))

