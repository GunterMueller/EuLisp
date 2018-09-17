(defmodule list-fns
  (standard0)
  ()
  ;;
  ;;  (union (expose  arith others ccc vectors defs
  ;;          extras0 macros0  lists list-operators)	
  ;;(except (null vector) (expose class-names)))

  ;; Useful function not defined EulispLISP
  (defun deleq (a b)
    (cond
     ((null b) nil)
     ((eq a (car b))
      (cdr b))
     (t (cons (car b) (deleq a (cdr b)))) ))

  (defun mapcdr (fn lst)
    (cond ((null lst) (fn nil))
	  ((atom lst) (fn lst))
	  (t (cons (fn lst)
		   (mapcdr fn (cdr lst))))))

  (export mapcdr)
    
  
  (defun mapcan (f l)
    (fold (lambda (x lst)
	    (nconc lst (f x)))
	  l
	  nil))
  
  (export mapcan)

;;  (defun map-all (fn lst)
;;    (cond ((null lst) nil)
;;          ((atom lst) lst)
;;	  ((consp (car lst))
;;	   (cons (map-all fn (car lst))
;;		 (map-all fn (cdr lst))))
;;	  (t (cons (fn (car lst))
;;		   (map-all fn (cdr lst))))))

  (defun map-all (fn lst)
    (if (atom lst) (fn lst)
      (mapcar map-all lst)))

  (defun fold (fn lst init)
    (cond ((null lst) init)
	  (t (fold fn (cdr lst) 
		   (fn (car lst) init)))))

  (defun mapvect (fn vect)
    (mapvect-aux fn (vector-length vect) (make-vector (vector-length vect) nil) vect))

  ;; work in RL direction (for peversity)
  (defun mapvect-aux (fn i new-v old-v)
    (cond ((zerop i) new-v)
	  (t ((setter vector-ref) new-v (- i 1) (fn (vector-ref old-v (- i 1))))
	     (mapvect-aux fn (- i 1) new-v old-v))))

  (defun collect (p l)
    (mapcan (lambda (x)
	      (if (p x) 
		  (list x)
		nil))
	    l))

 (defun detect (p l)
   (cond ((null l) ())
	 ((p (car l)))
	 (t (detect p (cdr l)))))

 (defun posnq (thing l)
   (let ((count 0))
     (detect (lambda (ob)
	       (if (eq ob thing)
		   count
		 (progn (setq count (+ 1 count))
			nil)))
	     l)))
 (export posnq)

 (defun nthcdr (n list)
   (cond ((= n 0) list)
	 (t (nthcdr (- n 1) (cdr list)))))
 
 (defun nth (n list)
   (car (nthcdr n list)))
 
 (export nthcdr)
 (defun mk-finder ()
   (let* ((table (make-table eq))
	  (fn (lambda (x) (table-ref table x))))
     ((setter setter) fn 
      (lambda (x v) 
	((setter table-ref) table x v)))
     fn))

 
 (defun mk-counter (n)
   (let ((v n))
     (lambda ()
       ((lambda (a)
	  (setq v (+ v 1))
	  a)
	v))))
  
 
 (defun local-var (x)
   (let ((val x))
     (let ((fn (lambda () val))
	   (set-fn (lambda (x) (setq val x) nil)))
       ((setter setter) fn set-fn)
       fn)))

 (export mapvect   fold map-all deleq collect detect nth mk-finder mk-counter local-var)

) 

