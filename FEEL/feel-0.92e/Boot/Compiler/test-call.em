;; Eulisp Module
;; Author: pab
;; File: test-call.em
;; Date: Tue Jun  2 15:31:28 1992
;;
;; Project:
;; Description: 
;;

(defmodule test-call
  (arith 
   lists
   macros0
;   list-operators
   streams 
   others
   )
  ()
  
  (defun i (x) x)

  (defun a (x) (i x))

  (defun b (x) (let ((y (i x))) y))

  (defun j (x) (+ x 1))

  (defun k (x) (let ((xx (+ x 1))) xx))

  (deflocal i1 (lambda (x) x))

  (defun a1 (x) (i1 x))

  (defun a2 (x) (let ((xx (i1 x))) xx))

  (defun acons (x) (cons x x))

  (defun bcons (x) (i (cons x x)))

  (defun fib (x)
    (if (= x 1) x (* x (fib (- x 1)))))

  (defun summit (x y)
    (if (null x) y
      (summit (cdr x) (+ (car x) y))))

  ;; mutually recursive, no envs, all tail calls
  (defun ipow (n k)
    (labels ((e0 (x k a)
		 (cond ((zerop k) a)
		       ((evenp k) 
			(e1 (* x x) (/ k 2) a))
		       (t (e0 (* x x) (/ k 2) (* x a)))))
	     (e1 (x k a)
		 (cond ((evenp k)
			(e1 (* x x) (/ k 2) a))
		       (t (e0 (* x x) (/ k 2) (* x a))))))
       (e0 n k 1)))
  
  ;; recursive, env , internal env, tail calling
  (defun messify (x n)
    (labels ((mess-aux (l r)
		       (cond ((null l) r)
			     (t (mess-aux (cdr l)
					  (cons (mapcar (lambda (b)
							  (list b (car l) n))
							(car l))
						r))))))
	    (mess-aux x nil)))
  
  ;;
  (defun rewrite-inline-lambda (lambda-term)
    (labels ((rewrite-args (args values)
			   (print (list args values))
			   (cond ((null args) nil)
				 ((atom args) 
				  (list (list args (cons 'list values))))
				 (t (cons (list (car args) (car values))
					  (rewrite-args (cdr args) (cdr values)))))))
      (lambda (tran args)
	(print `(let ,(rewrite-args (car lambda-term) args)
		  ,@(cdr lambda-term))))))

  ;; end module
  )
