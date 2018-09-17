;; Eulisp Module
;; Author: pab
;; File: debug.em
;; Date: Sun Jun  7 20:31:35 1992
;;
;; Project:
;; Description: 
;;

(defmodule debug
  (standard0
   list-fns
   
   )
  ()
	
  (defconstant find-debug-form (mk-finder))
  
  (defcondition Debug-Error ())
  
  (defstruct code-info ()
    ((text initarg text 
	   reader code-text)
     (pre-break initform nil
		accessor code-pre-break)
     (post-break initform nil 
		 accessor code-post-break)
     (id initarg id
	 accessor code-id))
    constructor make-code-info)

  (defun lookup-break (id)
    (let ((tree (find-debug-form (car id))))
      (if (null tree)
	  (error "no such debug" Debug-Error)
	(lookup-form id tree))))

  (defun lookup-form (id tree)
    (if (null id) 
	tree
      (let ((nt (assq id tree)))
	(if (null nt)
	    (error "couldn't find it")
	  ))))

  (defun annotate-text (code)
    (let ((name (gensym)))
      (labels ((annotate-list-1 (lst n)
				(cond ((null lst) nil)
				      (t (let ((xx (annotate-text (car lst))))
					   (cons (list n (car lst) xx)
						 (annotate-list-1 (cdr lst) (+ n 1)))))))
	       (annotate-list (lst)
			      (annotate-list-1 lst 0))
	       )
	      (cond ((atom code) code)
		    ((eq (car code) 'quote)
		     code)
		    ((eq (car code) 'lambda)
		     (cons 'lambda 
			   (cons (cadr code)
				 (annotate-list (cddr code)))))
		    (t (annotate-list code)))))
	
)
     
  (defun macro-namep (sym mod)
    (if (symbolp sym)
	(if (dynamic-accessible-p (get-module mod) sym)
	    (let ((xx (dynamic-access (get-module mod) sym)))
	      (if (macrop xx) 
		  xx
		nil))
	  nil)
      nil))
		   
  ;; end module
  )
