;; TRIANGLE sequential version

(defmodule triang

  ((rename ((binary+_Integer binary+)
	    (binary=_Integer =))
	   (except (= binary+)
		   standard)))

  ()

  (deflocal answer ())
  (deflocal final ())

  (deflocal board  (make-vector 16 1)) 
  (deflocal sequence (make-vector 14 0))
  (deflocal a (make-initialized-vector 
	       1 2 4 3 5 6 1 3 6 2 5 4 11 12 13 7 8 4 4 7 
	       11 8 12 13 6 10 15 9 14 13 13 14 15 9 10 6 6))
  (deflocal b (make-initialized-vector 
	       2 4 7 5 8 9 3 6 10 5 9 8 12 13 14 8 9 5 2 
	       4 7 5 8 9 3 6 10 5 9 8 12 13 14 8 9 5 5))
  (deflocal c (make-initialized-vector
	       4 7 11 8 12 13 6 10 15 9 14 13 13 14 15 9 
	       10 6 1 2 4 3 5 6 1 3 6 2 5 4 11 12 13 7 8 4 4))
  ((setter vector-ref) board 5 0)

  (defun last-position ()
    (labels ((last-aux (i)
		       (cond ((= i (vector-length board))
			      0)
			     ((= 1 (vector-ref board i))
			      i)
			     (t (last-aux (binary+ i 1))))))
	    (last-aux 1)))

  (defun try (i depth)     
    (cond ((= depth 14)
	   (let ((lp (last-position)) )
	     (unless (memq lp final) 
		     (setq final (cons lp final))))
	   (setq answer (cons (cdr (convert-vector-list sequence)) answer))
	   (format t "Answer: ~a~%" (car answer))
	   t)
	  ((and (= 1 (vector-ref board (vector-ref a i)))
		(= 1 (vector-ref board (vector-ref b i)))
		(= 0 (vector-ref board (vector-ref c i))))
	   ((setter vector-ref) board (vector-ref a i) 0)
	   ((setter vector-ref) board (vector-ref b i) 0)
	   ((setter vector-ref) board (vector-ref c i) 1)
	   ((setter vector-ref) sequence depth i)
	   (labels ((iterate (j depth);; ((j 0 (+ j 1)) (depth (+ depth 1) depth))
			     (if (or (= j 36) (try j depth))
				 ()
			       (iterate (binary+ j 1) depth))))
		   (iterate 0 (binary+ depth 1)))
	   ((setter vector-ref) board (vector-ref a i) 1)
	   ((setter vector-ref) board (vector-ref b i) 1)
	   ((setter vector-ref) board (vector-ref c i) 0)
	   nil)
	  (t nil)))


  (defun gogogo (i)
    (try i 1))

  (defun testtriang ()
    (let ((xx (cpu-time)))
      (gogogo 22)
      (print (- (cpu-time) xx))))

  (export try gogogo testtriang)


  )
