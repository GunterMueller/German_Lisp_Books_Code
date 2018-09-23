;;;; -*- mode:Lisp; package:user -*- ;;;;
;;;; Created: 10 December 1992

;;;; PROCEDURES

(defun interface (word-list)
  (interpret-tree 
    '(brnchs
       (count > objects if-end-rtn 
         (db-call `(db-count ,objects)))
       (> enumerate > objects if-end-rtn
         (db-call `(db-show ,objects))))
    word-list))

(defun objects (word-list)
  (interpret-tree
    '(the brnchs
          (saws rtn 'saws)
          (wrenches rtn 'wrenches)
          (hammers rtn 'hammers)
          (screwdrivers rtn 'screwdrivers))
    word-list))

(defun enumerate (word-list)
  (interpret-tree
    '(brnchs (identify)
             (describe))
    word-list))

(defun evaluate-forms (forms a-list)
  (eval `(let ,(make-let-variables a-list)
              ,@forms)))

(defun make-let-variables (a-list)
  (mapcar #'(lambda (pair)
              `(,(first pair) ',(second pair))) a-list))

(defun interpret-branches (branches word-list a-list)
  (if (endp branches)
      (values nil nil word-list)
      (multiple-value-bind (result binding words-left-over)
          (interpret-tree (first branches) word-list a-list)
        (if result
            (values result binding words-left-over)
            (interpret-branches (rest branches)
                                word-list
                                a-list)))))

(defun interpret-tree (tree word-list &optional a-list)
  (cond ((endp tree) (values t nil word-list))
        ((eq (first tree) (first word-list))
         (interpret-tree (rest tree) (rest word-list) a-list))
        ((eq '> (first tree))
         (multiple-value-bind
             (result binding word-list)
             (funcall (second tree) word-list)
           (when result
             (interpret-tree (rest (rest tree))
                            word-list
                            (cons (list (second tree) binding)
                                  a-list)))))
        ((eq 'rtn (first tree))
         (values t
                 (evaluate-forms (rest tree) a-list)
                 word-list))
        ((and (eq 'if-end-rtn (first tree))
              (endp word-list))
         (values t (evaluate-forms (rest tree) a-list) nil))
        ((eq 'brnchs (first tree))
         (interpret-branches (rest tree) word-list a-list))
        (t (values nil nil word-list))))

(defmacro define-tree (name-of-tree tree-description)
  `(defun ,name-of-tree (word-list)
     (interpret-tree ',tree-description word-list)))

(define-tree interface
  (brnchs
    (count > objects if-end-rtn
                     (db-call `(db-count ,objects)))
    (> enumerate > objects if-end-rtn
                           (db-call `(db-show ,objects)))))
(define-tree objects
  (the brnchs
       (saws         rtn 'saws)
       (wrenches     rtn 'wrenches)
       (hammers      rtn 'hammers)
       (screwdrivers rtn 'screwdrivers)))
(define-tree enumerate
  (brnchs (identify)
          (describe)))

(defun read-sentence ()
  (with-input-from-string
      (input (string-trim ".?!" (read-line)))
    (do ((word (read input nil)
               (read input nil))
         (sentence nil))
        ((not word) (return (reverse sentence)))
      (push word sentence))))

(defun run-interface ()
  (print '>)
  (do ((input (read-sentence) (read-sentence)))
      ((endp input)
       (format t "~&Ok, goodbye.")
       (values))
    (unless (interface input)
      (format t "~&Sorry, I can't understand that.~
              ~&Press the return key if you want to stop."))
    (print '>)))


