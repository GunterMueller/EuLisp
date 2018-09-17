;; Eulisp Module
;; Author: pete broadbery
;; File: test.em
;; Date: 3/sep/1991
;;
;; Project:
;; Description: 
;;

(defmodule test 
  (   
   import (compile)
   )
  ()

  (defun foo ()
    "test" 7)

  '(defun bar ()
    "wibble")

)

  (defconstant abcd "")
  
  (defconstant t1 (mk-finder))
  (export t1)


  (defconstant xx-setter 1);; (compile-inline 1 (slot-ref 4)))
  (compile-declare xx-setter setter-function t)
   
  (defconstant xx-setter-setter (compile-inline 2 (set-slot 4)))
  (compile-add-callback xx-setter-setter setter-setter-function xx)
  
  (xx-setter-setter xx-setter xx-setter-setter)

  ((xx-setter xx-setter) xx-setter-setter xx-setter-setter)

  (export xx-setter xx-setter-setter)

  ;;(defun xx () 1)
  ;;(defun yy (a) a)
  ;;((xx-setter xx-setter) xx yy)

  ;;(export xx yy)


  (deflocal xx nil)
  (deflocal yy nil)
  (deflocal zz nil)

  (defun reset ()
    (setq xx nil)
    (setq yy nil)
    (setq zz nil)
    xx)
 
  (defun foo (x)
    (labels ((countdown (n)
			(if (= n 0) 0 (progn (print n) (countdown (- n 1))))))
	    (lambda ()
	      (countdown x))))
  
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

  (defun test2 (x)
    (print "before")
    (labels ((xx (n)
		 (if (= n 0) 1
		   (xx (- n 1)))))
	    (let ((aa (xx x)))
	      (print aa)
	      aa)))
		 
  (defun i (x)
    x)

  (defun d (x)
    nil)

  (defun a (x)
    (i x))

  (defun acons (x y)
    (cons x y))

  (defun test-if (x) 
    (if x x 0))

  (defun test-not-if (x)
    (if (not x) x 0))

;; ok
  (defun l (x)
    ((lambda (y) 
       y)
     (+ x 1)))

  (defun fact (x)
    (if (< x 1) 1 
      (* x (fact (- x 1)))))

  (defun test (a)
    (if (zerop a)
	(+ a 3)
      (- a 1)))

  (defun mk-conser (x)
    (lambda (a) (cons a x)))
  
  (defun g-test (x)
    (zerop x))

  (defun gc-test (x)
    (GC)
    1 2 x)
  
  (defun gc-test2 (x)
    (if (gc-test x) 1 0))
  ;;(GC)
  (defun mk-counter-1 (n)
    (let ((v n))
      (lambda ()
        ((lambda (a)
           (setq v (+ v 1))
           a)
         v))))

  (defun nary-0 n
    n)
  
  (defun nary-1 (n . m)
    m)

  (deflocal it ())

  (defun ack (n)
    (cond ((= n 0)
	   (print (it)))
	  ((= n 1)
	   (print (it 1)))
	  ((= n 2)
	   (print (it 1 2)))
	  ((= n 3)
	   (print (it 1 2 3)))
	  ((= n 4)
	   (print (it 1 2 3 4)))
	  (t "Whups")))


   
  (defun set-it (x)
    (setq it x))


  (defun set-test (a)
   (car a))

  (defun set-test-setter (a b) 
      ((setter car) a b))
  
    ((setter setter) set-test set-test-setter)
   (export set-test)

  ;; more tests...
  (defun bar (a b)
    (let ((c (- b a)))
      (cons b
	    (lambda (x) (+ c x)))))

  (defun ffib (x) (if (< x 2) 1 (+ (ffib (- x 1)) (ffib (- x 2)))))

'  (defun fib (x) 
    (if (binary-lt x 2) 
	1
        (binary-plus (fib (binary-difference x 1)) 
		     (fib (binary-difference x 2)))))

  (defun a-mapcar (f x)
    (if (null x)
	nil
      (cons (f (car x)) 
	    (a-mapcar f (cdr x)))))

  (defun afold (f l v)
    (if (null l) v
      (afold f (cdr l) (f (car l) v))))

  ;; import/export test
  ;;(print "...")
  ;;(flush (standard-output-stream))
  (defconstant x (mk-finder))
  (deflocal y (mk-finder))
  
  (export x y)

  (defun xxx (a) (mapcdr append a))

  ;; From CLTL

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

  (defun messify (x n)
    (labels ((mess-aux (l r)
		       (cond ((null l) r)
			     (t (mess-aux (cdr l)
					  (cons (mapcar (lambda (b)
							  (cons b n))
							(car l))
						r))))))
	    (mess-aux x nil)))

  ;; possibly the worst reverse in the world
 (defun rev (l)
   (cond  ((null l) l)
	  ((null (cdr l)) l)
	  ((null (cdr (cdr l)))
	   (swap l))
	  (t ((lambda (a1)
		(cons (car a1)
		      (rev (cons (car l)
				 (rev (cdr a1))))))
	      (rev (cdr l))))))

 
  (defun swap (l) (cons (car (cdr l)) (cons (car l) (cdr (cdr l)))))

  (defun myappend-1 (a b)
    (cond ((null a) b)
	  (t (cons (car a) 
		   (myappend-1 (cdr a) b)))))
  
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

  (defun repeat (f n)
    (if (= n 0) nil
      (progn (f) (repeat f (- n 1)))))

  (defun time (f)
    (let ((xx (cpu-time)))
      (f)
      (- (cpu-time) xx)))

  (defun testxx ()
    (let ((a (open "test.em" 'input t)))
      (read a)
      (close a)
      (testxx)))

)

(i 0)
(a 0)
(acons 0 1)
(ffib 21)
(rev '(0 1 2 3))
(test-if 1)
(l 2)
(fact 3)
;; also tests generic-lookup
(afold + '(0 1 2.2 3) 0)
(ipow 2 4)
(ipow 2 5)
(ipow 2 7)
(rev '(0 1 2 3 4 5 6 7 8 9 0 a))
(gc-test2 1)
(gc-test2 nil)
