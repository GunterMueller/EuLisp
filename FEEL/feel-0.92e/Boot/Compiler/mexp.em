;; Eulisp Module
;; Author: pab
;; File: mexp.em
;; Date: Tue Mar  3 12:49:44 1992
;;
;; Project:
;; Description: 
;;
;; Fast macroexpansion

(defmodule mexp
  (standard0
   ;;list-fns
   
   module-operators
   )
  ()

  (defmacro expand-forms ()
    `(do-expand (car (reify-env))))

  (defun do-expand (name)
    (let ((infile (open (format nil "~a.em" name)))
	  (outfile (open (format nil "/tmp/~a.em" name) 'output t)))
      (let ((forms (read infile)))
	(let ((res (expand-forms-1 forms name)))
	  (write res outfile)
	  (format outfile "~%~%")
	  (close outfile)
	  (close infile))))
    nil)

  (defun expand-forms-1 (x m)
    (cond ((null x) nil)
	  ((atom x) x)
	  ((eq (car x) 'quote)
	   x)
	  ((eq (car x) 'lambda)
	   (cons 'lambda (cons (cadr x)
			       (mapcar (lambda (form) (expand-forms-1 form m))
				       (cddr x)))))
	  (t (let ((xx (macro-namep (car x) m)))
	       (if xx
		   (expand-forms-1 (apply xx (cdr x))
				   m)
		 (cons (expand-forms-1 (car x) m)
		       (my-mapcar (lambda (form) 
				    (expand-forms-1 form m))
				  (cdr x))))))))
			       
  
     
  (defun macro-namep (sym mod)
    (if (symbolp sym)
	(if (dynamic-accessible-p (get-module mod) sym)
	    (let ((xx (dynamic-access (get-module mod) sym)))
	      (if (macrop xx) 
		  xx
		nil))
	  nil)
      nil))
  
  (defun my-mapcar (fn l)
    (cond ((null l) nil)
	  ((atom l) l)
	  (t (cons (fn (car l)) 
		   (my-mapcar fn (cdr l))))))

  (export expand-forms expand-forms-1 reify-env do-expand)
  ;; end module
  )
