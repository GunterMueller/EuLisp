;; Eulisp Module
;; Author: pab
;; File: btrace.em
;; Date: Thu Jul 16 15:09:31 1992
;;
;; Project:
;; Description: 
;;   Simple backtrace module

(defmodule btrace
  (standard0
   list-fns
         
   )
  ()

  ;; some things we want to neither print nor write
  ;; 
  (defgeneric generic-bprin (x stream))
  
  (defmethod generic-bprin ((x <object>) stream)
    (format stream "~u" x))

  (defgeneric print-fn-trace (fn env))
  (defmethod print-fn-trace ((x <i-function>) env)
    (format t "Entered: ~a~%" x)
    (setq xxx env)
    (mapc (lambda (x) 
	    (prin "  ")
	    (prin (car x))
	    (prin ":")
	    (generic-bprin (cdr x) (standard-output-stream))
	    (newline))
	  env))

  
  (defmethod generic-prin ((x <function>) stream)
    (generic-write x stream))

  (defmethod print-fn-trace ((x <object>) env)
    (format t "Entered: ~a~%Env: ~a~%" x env))

  (defun btrace ()
    (let ((vect (make-vector 3)))
      (labels ((aux ()
		    (if (null (get-backtrace-frame vect))
			nil
		      (let ((val (vector-ref vect 2)))
			(if (eq (car val) aux) nil
			  (progn (print-fn-trace (car val) (cdr val))
				 (aux)))))))
	      (aux)
	      nil)))
     
  ;; end module
  )
