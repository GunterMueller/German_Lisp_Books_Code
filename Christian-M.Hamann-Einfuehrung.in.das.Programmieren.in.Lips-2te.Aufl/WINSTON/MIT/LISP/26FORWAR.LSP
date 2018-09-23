;;;; -*- mode:Lisp; package:user -*- ;;;;
;;;; Created: 10 December 1992

;;;; REMARKS

#|

This version is the same as the one in the book except that
(MAKE-EMPTY-STREAM) replaces 'EMPTY-STREAM.

|#

;;;; PROCEDURES

(defun remember-assertion (assertion)
  (stream-remember assertion *assertions*))

(defun remember-rule (rule)
  (stream-remember rule *rules*))

(defun rule-name (rule) (first rule))

(defun rule-ifs (rule) (butlast (rest rule)))

(defun rule-then (rule) (first (last rule)))

(defun try-assertion (pattern assertion bindings)
  (let ((result (match pattern assertion bindings)))
    (if (eq 'fail result)
        (make-empty-stream)
        (stream-cons result (make-empty-stream)))))

(defun match-pattern-to-assertions (pattern bindings)
  (stream-concatenate
    (stream-transform
      #'(lambda (assertion) (try-assertion pattern
                                           assertion
                                           bindings))
      *assertions*)))

(defun filter-binding-stream (pattern stream)
  (stream-concatenate
    (stream-transform
      #'(lambda (bindings)
          (match-pattern-to-assertions pattern bindings))
      stream)))

(defun apply-filters (patterns initial-input-stream)
  (if (endp patterns)
      initial-input-stream
      (apply-filters (rest patterns)
                     (filter-binding-stream (first patterns)
                                            initial-input-stream))))

(defun instantiate-variables (pattern a-list)
  (cond
    ((atom pattern) pattern)
    ((eq '? (first pattern))
     (extract-value (find-binding pattern a-list)))
    (t (cons (instantiate-variables (first pattern) a-list)
             (instantiate-variables (rest pattern) a-list)))))

(defun use-rule (rule)
  (let ((binding-stream
          (apply-filters (rule-ifs rule)
                         (stream-cons nil (make-empty-stream)))))
    (do ((binding-stream binding-stream
                         (stream-rest binding-stream))
         (success-switch nil))
        ((stream-endp binding-stream) success-switch)
      (let ((result (instantiate-variables
                      (rule-then rule)
                      (stream-first binding-stream))))
        (when (remember-assertion result)
          (format t "~%Rule ~a indicates ~a."
                  (rule-name rule) result)
          (setf success-switch t))))))

(defun forward-chain ()
    (do ((rule-stream *rules* (stream-rest rule-stream))
         (repeat-switch nil))
        ((stream-endp rule-stream)
         (if repeat-switch
             (progn
               (format t "~%I am trying the rules again.")
               (forward-chain))
             (progn
               (format t "~%Nothing new noted.")
               'done)))
      (when (use-rule (stream-first rule-stream))
        (setf repeat-switch t))))

