;; Eulisp Module
;; Author: pab
;; File: list.em
;; Date: Tue Jun 29 21:44:41 1993
;;
;; Project:
;; Description: 
;;

(defmodule list
  (init macros0 extras0 gens null cons defs vector numbers)         
  ()
  
  (expose null cons)

  (defmethod reverse ((sequence <list>))
    ;; constructs a new sequence which is the reverse of sequence
    (reverse-list sequence))

  (deflocal max-list-size 50)

  (defmethod sort ((data <list>) comp)
    ;; sorting a list returns a new list and the original is unaltered
    (cond
     ((null data) ())
     ((< (size data) max-list-size)
      (list-insertion-sort data comp))
     (t (let ((v (convert data <vector>)))
	  (convert (vector-quick-sort v comp 0 (- (size v) 1)) <list>)))))

  (defun list-insertion-sort (l f)
    (labels
     ((insert (a v)
	(cond
	 ((null a) (cons v nil))
	 ((f v (car a)) (cons v a))
	 (t (cons (car a) (insert (cdr a) v))))))
     (accumulate insert (list (car l)) (cdr l))))

  (defmethod gf-remove (object (collection <list>) comp)
    (labels
     ((loop (l)
       (cond
	((null l) nil)
	((comp object (car l)) (cdr l))
	(t (let ((rem (loop (cdr l))))
	     (if (eq rem (cdr l)) l (cons (car l) rem)))))))
     (loop collection)))

  (defmethod gf-delete (object (collection <list>) comp)
    (cond
     ((null collection) ())
     ((comp object (car collection)) (cdr collection))
     (t (labels
	 ((loop (prev next)
	    (cond
	     ((null next) collection)
	     ((comp object (car next))
	      ((setter cdr) prev (cdr next))
	      collection)
	     (t (loop (cdr prev) (cdr next))))))
	 (loop collection (cdr collection))))))

  (defmethod gf-find-key ((c <list>) f skip failure)
    (labels
      ((loop (s i j)
	 (cond
	  ((null s) failure)
	  ((zerop i)
	   (if (f (current-element c s))
	       j
	     (loop (next-state c s) i (+ j 1))))
	  (t (let ((x (f (current-element c s))))
	       (loop (next-state c s) (if x (- i 1) i) (+ j 1)))))))
      (loop (initial-state c) skip 0)))

;; end module
)
