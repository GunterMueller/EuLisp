;; Eulisp Module
;; Author: pab
;; File: vector.em
;; Date: Tue Jun 29 22:07:58 1993
;;
;; Project:
;; Description: 
;;

(defmodule vector
  (gens
   defs
   extras0
   init
   macros0
   )
  ()


;;; ----------
;;; p121
;;; default methods for initial-state, next-state, current-element and
;;; (setter current-element) work for <vector>

  (defmethod element ((v <vector>) (s <fixint>)) (vector-ref v s))

  (defmethod (setter element) ((c <vector>) (s <fixint>) v)
    ((setter vector-ref) c s v))

  (defmethod current-key ((c <vector>) (s <fixint>)) s)

  (defmethod clone ((x <vector>))
    (let* ((upb (size x))
	   (v (make <vector> 'size upb)))
      (labels ((loop (i)
		 (when (< i upb)
		   ((setter vector-ref) v i (vector-ref x i))
		   (loop (+ i 1)))))
	(loop 0))
      v))

  (deflocal max-vector-size 10)

  (defmethod sort ((data <vector>) comp)
    ;; sorting a vector returns the original vector, which is, of
    ;; course, modified.
    ;; ------------------------------------------------------------
    ;; note that due to the use of generic operations in the vector
    ;; routines, they will work equally, albeit slowly, for lists.
    (if (< (size data) max-vector-size)
	(vector-insertion-sort data comp)
      (vector-quick-sort data comp 0 (- (size data) 1))))

;  (defun vector-insertion-sort (a f)
;    ;; adapted from Sedgewick, Algorithms in C
;    (let ((n (- (size a) 1))
;	  (i 1))
;      (while (<= i n)
;	(let ((v (element a i))
;	      (j i))
;	  (while (not (or (< j 1) (f (element a (- j 1)) v)))
;	    ((setter element) a j (element a (- j 1)))
;	    (setq j (- j 1)))
;	  ((setter element) a j v))
;	(setq i (+ i 1)))
;      a))

  (defun vector-insertion-sort (a f)
    ;; adapted from Sedgewick, Algorithms in C
    (let ((n (- (size a) 1)))
      (labels ((outer-loop (i)
        (if (<= i n)
	    (let ((v (element a i)))
	      (labels ((inner-loop (j)
	        (if (not (or (j 1) (f (element a (- j 1)) v)))
		    (progn
		      ((setter element) a j (element a (- j 1)))
		      (inner-loop (- j 1)))
		  ((setter element) a j v))))
		(inner-loop i))
	      (outer-loop (+ i 1)))
	  a)))
	(outer-loop 1))))

  (defun vector-quick-sort (v f k m)
    ;; Taken from a paper by Henry Baker on linear logic and Lisp
    ;; -----------------------------------------------------------
    ;; NB this definition doesn't work for vectors of length 2 but
    ;; since that size is handled by insertion sort and that does work
    ;; I have not attempted to find out why this does not.
    (if (>= k m)
      v
    (let* ((x (element v k))
           (i (split1 v f k (- m 1) x)))
      ((setter element) v i x)
      (vector-quick-sort v f k i)
      (vector-quick-sort v f (+ i 1) m))))

  (defun split1 (v f i j x)
    (if (= i j)
	i
      (let ((vj (element v j)))
	(if (f vj x)
	    (progn ((setter element) v i vj)
		   (split2 v f (+ i 1) j x))
	  (split1 v f i (- j 1) x)))))

  (defun split2 (v f i j x)
    (if (= i j)
	i
      (let ((vi (element v i)))
	(if (f x vi)
	    (progn ((setter element) v j vi)
		   (split1 v f i (- j 1) x))
	  (split2 v f (+ i 1) j x)))))


  (export vector-quick-sort)

;;; ----------
;;; specified methods

  (defconstant vector make-initialized-vector)

  (export vector)

;;; ----------
;;; additional methods

  (defmethod size ((c <vector>)) (length c))
  
  (defmethod allocate ((x <vector-class>) args)
    (let ((size (scan-args 'size args (lambda (a b) 0)))
	  (fill (scan-args 'fill-value args null-argument)))
      (make-vector size fill)))

  ;; end module
  )
