;; Eulisp Module
;; Author: pete broadbery
;; File: low-seman.em
;; Date: 28/jul/1991
;;
;; Project:
;; Description: 
;;  low level semantic properties: 
;;  accessing, and adding new properties
;;  should really be done with a web of defstructs,
;;  parallel to the ab-syntax stuff, but this is easier 
;;  for the moment.

(defmodule low-seman 
  (standard0
   list-fns
   abs-syntx
   )
  ()
  (defun stop (x) nil)

  ;; use a table to store properties
  (defstruct semantic-info ()
    ((props initform ()
	    accessor semantic-props))
    constructor (make-semantic-info))

  (defun semantics-ref (info name)
    (let ((xx (assq name (semantic-props info))))
      (if (null xx) nil
	(cdr xx))))
  
  (deflocal *all-properties* ())

  ((setter setter) semantics-ref 
   (lambda (info name val)
     (if (eq info ()) (stop (list info name val)) ())
     ;; assumes I never update values
     ((setter semantic-props) info (cons (cons name val)
					 (semantic-props info)))))

  ;;((setter table-ref) (semantic-props info) name val)

  (defun make-semantic-ref (name)
    (setq *all-properties* (cons name *all-properties*))
    (let ((fn (lambda (x)
		(semantics-ref (syntactic-properties x) name))))
      ((setter setter) fn 
       (lambda (x y) 
	 ((setter semantics-ref) (syntactic-properties x) name y)))
      fn))

  ;; loose end
  (defmethod make-syntactic-properties ((x syntax-obj) lst)
    ((setter syntactic-properties) x (make-semantic-info))
    nil)

  (defun print-props (obj)
    (print obj)
    (mapcar (lambda (prop)
	      (let ((xx (semantics-ref (syntactic-properties obj) prop)))
		(if (null xx) nil
		  (format t " ~a: ~a~%" prop xx))))
	    *all-properties*))
  
  (export semantics-ref make-semantic-ref print-props)

  ;; end module
  )
