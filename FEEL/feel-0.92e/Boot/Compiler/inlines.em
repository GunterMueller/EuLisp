;; Eulisp Module
;; Author: pab
;; File: inlines.em
;; Date: Wed Jun  3 18:27:37 1992
;;
;; Project:
;; Description: 
;;

(defmodule inlines
  (macros0
   
   )
  ()
  
  ;; functions...
  
  (defconstant cons (compile-inline 2 (i-cons)))
  (defconstant car (compile-inline 1 (slot-ref 0)))
  (defconstant cdr (compile-inline 1 (slot-ref 1)))
  
  (defconstant set-car (compile-inline 2 (set-slot 0)))
  (defconstant set-cdr (compile-inline 2 (set-slot 1)))
  
  (defconstant vector-ref (compile-inline 2 (vref)))
  (defconstant set-vector-ref (compile-inline 3 (set-vref)))
  
  (defconstant consp (compile-inline 1 (i-consp)))
  (defconstant null (compile-inline 1 (nullp)))
  (defconstant atom (compile-inline 1 (i-consp) (nullp)))

  (defconstant list (compile-inline -1))
  (defconstant assq (compile-inline 2 (i-assq)))
  (defconstant memq (compile-inline 2 (i-memq)))

  (defconstant identity (compile-inline 1))

  (export cons car cdr set-car set-cdr vector-ref
	  set-vector-ref list null consp atom assq
	  memq identity)
  
  (defun a (x y) (assq x y))
  (defun l () (list))
  (defun l1 () (list 1))
  (defun l2 () (list 1 2))
  ;; end module
  
  ;; setters deferred until later (currently boot.em).
  )
(defun nconc (a b)
  (labels ((last-pair (a)
		      (if (consp (cdr a)) 
			  (last-pair (cdr a))
			a)))
	  (if (null a) b
	    ((setter cdr) (last-pair a) b))))

(defun myappend (a b)
  (if (null a) b
    (let ((lst (cons (car a) nil)))
      (labels ((app-aux (l end)
			(if (null l)
			    end
			  (let ((newpair (cons (car l) nil)))
			    ((setter cdr) end newpair)
			    (app-aux (cdr l) newpair)))))
	      ((setter cdr) (app-aux (cdr a) lst) b)
	      lst))))
