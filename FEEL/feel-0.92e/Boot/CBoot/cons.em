;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright (c) University of Bath, 1993
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Eulisp Module
;; Author: pab
;; File: cons.em
;; Date: Tue Jun 29 21:07:48 1993
;;
;; Project:
;; Description: 
;;

(defmodule cons
  (gens
   defs
   init
   extras0
   macros0
   )
  ()
  
  ;; (export <pair>)

  (export <cons>)

  (defmethod initial-state ((p <cons>)) p)

  (defmethod next-state ((c <cons>) (s <cons>)) (cdr s))

  (defmethod current-element ((c <cons>) (s <cons>)) (car s))

  (defmethod (setter current-element) ((c <cons>) (s <cons>) v)
    ((setter car) s v))

  (defmethod current-key ((c <cons>) (s <cons>))
    (labels
     ((loop (l)
	    (if (eq l s)
		0
	      (+ 1 (loop (cdr l))))))
     (loop c)))

  (defmethod element ((p <cons>) (i <fixint>))
    (labels
     ((loop (p i)
	    (cond
	     ((= i 0) (car p))
	     ((atom p) ())
	     (t (loop (cdr p) (- i 1))))))
     (loop p i)))

  (defmethod (setter element) ((p <cons>) (i <fixint>) o)
    (labels
     ((loop (p i)
	    (cond
	     ((= i 0) ((setter car) p o))
	     ((atom p) ())
	     (t (loop (cdr p) (- i 1))))))
     (loop p i)))

  (defmethod size ((c <cons>)) (length c))

  (defmethod deep-copy ((p <cons>))
    ;; create a new pair and initialize with deep copies of the car and
    ;; the cdr slots
    (cons (deep-copy (car p)) (deep-copy (cdr p))))

  ; copy the spine only
  (defmethod shallow-copy ((pair <cons>))
    (labels ((copy (p)
	       (if (atom p)
		   p
		   (cons (car p) (copy (cdr p))))))
      (copy pair)))

  (defmethod clone ((p <cons>))
    (shallow-copy p))

  (defmethod clone ((n <null>))
    ())

  ;; defined here until PAB does a better version in the kernel
  ;; Actually---This is OK. only improvement is that apply should
  ;; be cleverer...

  (defun compose (f g) (lambda l (f (apply g l))))

  (defmethod gf-map (f (c <cons>) cs)
    ;; list method for iterating over several collections
    ;; simultaneously, applying the function f to the appropriate
    ;; combinations of elements and constructing a list of the results.
    ;; generic version in collect.em
    (let ((r ()))
      (apply do (compose (lambda (x) (setq r (cons x r))) f) c cs)
      (reverse r)))

  (defmethod gf-member (v (c <cons>) f)
    ;; returns t if the application of f to v and an element of c does
    ;; see collect.em for the generic method
    (labels
     ((loop (l)
	    (cond
	     ((null l) ())
	     ((f v (car l)) l)
	     (t (loop (cdr l))))))
     (loop c)))

  (defmethod gf-fill-range ((mc <cons>) v start end)
    (labels
     ((loop (i s)
	(cond
	 ((null s) ())
	 ((> i end) ())
	 ((>= i start)
	  ((setter current-element) mc s v)
	  (loop (+ i 1) (next-state mc s)))
	 (t (loop (+ i 1) (next-state mc s))))))
     (if (and (<= 0 start) (<= start end) (< end (size mc)))
	 (loop 0 (initial-state mc))
       mc)))

  ;; end module
  )


