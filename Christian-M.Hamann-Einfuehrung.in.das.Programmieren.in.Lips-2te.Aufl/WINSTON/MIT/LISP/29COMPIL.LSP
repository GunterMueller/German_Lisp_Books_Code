;;;; -*- mode:Lisp; package:user -*- ;;;;
;;;; Created: 10 December 1992

;;;; PROCEDURES

(defun compile-branches (forms)
  (unless (endp forms)
    `(multiple-value-bind (result binding words-left-over)
         ,(compile-elements (first forms))
         (if result
             (values result binding words-left-over)
             ,(compile-branches (rest forms))))))

(defun compile-elements (tree)
  (cond ((endp tree) '(values t nil word-list))
        ((eq '> (first tree))
         `(multiple-value-bind (result ,(second tree) word-list)
              (,(second tree) word-list)
            (when result
                  ,(compile-elements (rest (rest tree))))))
        ((eq 'brnchs (first tree))
         (compile-branches (rest tree)))
        ((eq 'rtn (first tree))
         `(values t (progn ,@(rest tree)) word-list))
        ((eq 'if-end-rtn (first tree))
         `(when (null word-list)
            (values t (progn ,@(rest tree)) nil)))
        (t `(let ((current-word (first word-list))
                  (word-list (rest word-list)))
              (when (eq current-word ',(first tree))
                  ,(compile-elements (rest tree)))))))

(defmacro compile-tree (name tree)
  `(defun ,name (word-list)
     ,(compile-elements tree)))

