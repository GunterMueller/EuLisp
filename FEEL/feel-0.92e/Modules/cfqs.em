;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;  EuLisp Module                     Copyright (C) University of Bath 1991  ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule cfqs

  (standard futures) ()

  (defun car1 (x) (if (futurep x)
		      (car (futureeval x))
		    (car x)))

  (defun cdr1 (x) (if (futurep x)
		      (cdr (futureeval x))
		    (cdr x)))

  (defun null1 (x) (if (futurep x)
		       (null (futureeval x))
		     (null x)))

  (defun defuturize (x) (if (futurep x) (futureeval x) x))

  (defun greaterp1 (x y)
    (progn
      (if (futurep x)
	  (setq x (futureeval x))
	nil)
      (if (futurep y)
	  (setq y (futureeval y))
	nil)
      (not (< x y))))

  (defun qsort (l) (qs l nil))

  (defun qs (l rest)
    (if (null1 l)
	(defuturize rest)
      (let ((parts (partition (car1 l) (cdr1 l))))
;;	(bindings 'parts) % show bindings for parts
	(qs (left-part parts)
	    (future (cons (car1 l) (qs (right-part parts) rest)))))))

  (defun partition (elt lst)
    (progn
;;      (print "Env at start of partition")
;;      (showenv thisenv)
;;      (reclaim)
      (if (null1 lst)
	  (bundle-parts nil nil)
	(let ((cdrparts (future (partition elt (cdr1 lst)))))
	  (if (greaterp1 elt (car1 lst))
	      (bundle-parts (cons (car1 lst)
				  (future (left-part cdrparts)))
			    (future (right-part cdrparts)))
	    (bundle-parts (future (left-part cdrparts))
			  (cons (car1 lst)
				(future (right-part cdrparts))))))))
    )

  (defun bundle-parts (x y) (cons x y))

  (defun left-part (p) (car1 p))

  (defun right-part (p) (cdr1 p))

  (defun wop () (print (qsort '(5 1 2 4 3))) (wop))

  (defun make-random-list (n)
    (labels
      ((aux (n l)
	 (if (= n 0) l
	   (aux (- n 1) (cons (c-rand) l)))))
      (aux n ())))

  (setq l1 (make-random-list 20))

  (setq l2 (make-random-list 40))
  
  (export qsort make-random-list)
)

