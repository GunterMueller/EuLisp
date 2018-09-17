;; arglist scanner
;; lets hope that this is compiled!
(defmodule scan-args
  ((except (scan-args) eulisp0))

  ()
  (defun scan-args (arg lst default)
    (cond ((null lst) default)
	  ((eq (car lst) arg) (car (cdr lst)))
	  ((null (cdr lst)) default)
	  (t (scan-args arg
			(cdr (cdr lst))
			default))))

  (defun map-initargs (fn lst)
    (cond ((null lst)
	   nil)
	  (t (cons (fn (car lst) (car (cdr lst)))
		   (map-initargs fn (cdr (cdr lst)))))))

  (defun fold-initargs (fn lst start)
    (if (null lst)
	nil
      (fold-initargs fn (cdr (cdr lst) )
		     (fn (car lst) (car (cdr lst)) start))))

  (export scan-args map-initargs fold-initargs)

)
