;; PDE solving using relaxation method and Linda

(defmodule relax (eulisp0 eulinda loops) ()

  ; initial values
  (defconstant problem1
    #( #(0.0 0.0 0.0 0.0 1.0)
       #(0.0 0.0 0.0 0.0 1.0)
       #(0.0 0.0 0.0 0.0 1.0)
       #(0.0 0.0 0.0 0.0 1.0)
       #(1.0 1.0 1.0 1.0 1.0) ))

  (defconstant problem2
    #( #(2.0 1.0 1.0 2.0)
       #(1.0 0.0 0.0 1.0)
       #(1.0 0.0 0.0 1.0)
       #(2.0 1.0 1.0 2.0) ))

  (defconstant problem3
    #( #(2.0 1.0 1.0 1.0 2.0)
       #(1.0 0.0 0.0 0.0 1.0)
       #(1.0 0.0 0.0 0.0 1.0)
       #(1.0 0.0 0.0 0.0 1.0)
       #(2.0 1.0 1.0 1.0 2.0) ))
  
  (defconstant problem4 
    #( #(2.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 2.0)
       #(1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0)
       #(1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0)
       #(1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0)
       #(1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0)
       #(1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0)
       #(1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0)
       #(1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0)
       #(1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0)
       #(1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0)
       #(1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0)
       #(2.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 2.0)))

  ; (doit problem1 0.1)
  ; (doit problem2 0.1)

  ;----------------------------------------------------------------------

  (deflocal epsilon 0)
  (deflocal mesh-size 0)
  (deflocal mesh-size1 0)
  (deflocal total-count 0)
  (deflocal relax-pool ())

  (defun make-array (n m)
    (let ((arr (make-vector m))
	  (i 0))
      (for () (< i m) (setq i (+ i 1))
	   ((setter vector-ref) arr i (make-vector n)))
      arr))

  (defun aref (array x y) (vector-ref (vector-ref array x) y))

  ((setter setter) aref
   (lambda (array x y val)
     ((setter vector-ref) (vector-ref array x) y val)))

  (defun boundaryp (x y)
    (or (= x 0)
	(= y 0)
	(= x mesh-size1)
	(= y mesh-size1)))

  (defun init-mesh (vals)
    (let ((i 0) (j 0))
      (for () (< i mesh-size) (setq i (+ i 1))
	(for (setq j 0) (< j mesh-size) (setq j (+ j 1))
	  (if (boundaryp i j)
	      (progn
		(linda-out relax-pool 'value i j (aref vals i j) 'boundary)
		(linda-out relax-pool 'result i j (aref vals i j) 'conv))
	      (linda-out relax-pool 'value i j (aref vals i j) 'inner))))))

  (defun relax ()
    (let ((x ()) (y ()) (oldval ()))
      (linda-read relax-pool 'value (? x) (? y) (? oldval) 'inner)
      (format t "relax: ~a ~a ~a~%" x y oldval)
      (let ((N ()) (E ()) (S ()) (W ()))
	(linda-read relax-pool 'value x (+ y 1) (? N) ?)
	(linda-read relax-pool 'value (+ x 1) y (? E) ?)
	(linda-read relax-pool 'value x (- y 1) (? S) ?)
	(linda-read relax-pool 'value (- x 1) y (? W) ?)
	(let ((newval (/ (+ N E S W) 4.0)))
	  (linda-in relax-pool 'value x y ? 'inner)
	  (linda-out relax-pool 'value x y newval 'inner)
	  (if (< (abs (- oldval newval)) epsilon)
	      (linda-out relax-pool 'result x y newval 'conv)
	      (linda-out relax-pool 'result x y newval 'not-conv))
	  newval))))

  ; function to collect results
  (defun collector (ans)
    (let ((convgs (make-array mesh-size mesh-size)))
      (collector-loop ans convgs 0)))

  (defun collector-loop (ans convgs count)
    (if (= count total-count)
	(progn
	  (format t "*** All points converged~%")
	  ans)
	(let ((x ()) (y ()) (val ()) (conv ()))
	  (linda-in relax-pool 'result (? x) (? y) (? val) (? conv))
	  ((setter aref) ans x y val)
	  (let ((old (aref convgs x y)))
	    (if (eq conv 'conv)
		(progn
		  ((setter aref) convgs x y conv)
		  (if (null old)	; new converge
		      (progn
			(format t "collector: ~a points converged~%"
				(+ count 1))
			(collector-loop ans convgs (+ count 1)))
		      (collector-loop ans convgs count)))
		(if (null old)
		    (collector-loop ans convgs count)
		    (progn		; old unconverged
		      ((setter aref) convgs x y ())
		      (format t "collector: ~a points converged~%"
			      (- count 1))
		      (collector-loop ans convgs (- count 1)))))))))

  (defun mutator (n pool)
    (if (linda-read? pool 'stop)
	(format t "relax: finished~%")
	(progn
	  (format t "relax: cycle ~a~%" n)
	  (relax)
	  (thread-reschedule)
	  (mutator (+ n 1) pool))))

  (deflocal answer ())

  (defun doit (initial-mesh eps)
    (setq epsilon eps)
    (setq mesh-size (vector-length initial-mesh))
    (setq mesh-size1 (- mesh-size 1))
    (setq total-count (* mesh-size mesh-size))
    (setq relax-pool (make-linda-pool))
    (setq answer (make-array mesh-size mesh-size))
    (init-mesh initial-mesh)
    (format t "Init done\n")
    (linda-eval mutator 0 relax-pool)
    (collector answer)
    (linda-out relax-pool 'stop)
    answer)

)

(tril t)

(print-linda-pool relax-pool)
