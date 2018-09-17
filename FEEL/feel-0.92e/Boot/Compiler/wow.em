;; Eulisp Module
;; Author: pab
;; File: wow.em
;; Date: Mon Feb 10 15:49:32 1992
;;
;; Project:
;; Description: 
;;  Non working version of before, after, etc methods

(defmodule wow
  (standard0
   list-fns
         

   )
  ()
  
  (defclass gf-1 (generic-function) () metaclass generic-class)

  (defclass new-method (method) () metaclass method-class)
  (defclass before-method (new-method) () metaclass method-class predicate beforep)
  (defclass after-method (new-method) () metaclass method-class predicate afterp)
  (defclass around-method (new-method) () metaclass method-class predicate aroundp)
  
  (defgeneric method-priority (meth)
    methods ((((m method) 0))
	     (((m before-method)) 1)
	     (((m after-method)) -1)
	     (((m around-method)) 2)))
  
  (defmethod  compute-discriminating-function ((gf gf-1))
    (lambda (sig)
      (find-and-sort-applicable-methods gf sig)))

  (defun find-and-sort-applicable-methods (gf sig)
    (flatten-alist (fold insert-method 
			  (find-applicable-methods gf sig)
			  nil)))
		    
  (defun insert-method (meth lst)
    (let ((val (method-priority meth)))
      (cond ((null lst) (list (list val meth)))
	    ((< val (car lst))
	     (cons (list val meths)
		   lst))
	    ((= val (car lst))
	     (nconc (car lst) meth))
	    (t (cons (car lst)
		     (insert-method meth (cdr lst)))))))
	    
  (defun flatten-alist (lst)
    (fold (lambda (x l) (append (cdr x) l))
	  lst
	  nil))
	  
  ;; end module
  )
