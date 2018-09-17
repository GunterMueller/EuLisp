;; Eulisp Module
;; Author: pab
;; File: error0.em
;; Date: Tue Nov  3 15:02:40 1992
;;
;; Project:
;; Description: 
;;

(defmodule error0
  (init
   extras0 
   macros0
   defs
   ;; For flush
   stream 
   )
  ()

  
  (deflocal *the-cont* ())

  (defgeneric generic-error-printer (c1 c2))

  (defmethod generic-error-printer ((c <condition>) cont)
    (flush (standard-error-stream))
    (flush (standard-output-stream))
    (format (standard-error-stream) "Trapped ~a ~a!~%" 
	    (if cont "continuable" "non-continuable") 
	    (symbol-unbraced-name (class-name (class-of c))))
    (setq *the-cont* cont)
    (mapc (lambda (slot)
	    (let ((v ((slot-description-slot-reader slot) c)))
	      (if (eq v unbound-slot-value) ()
		  (format (standard-error-stream) "  ~a: ~a~%"
			  (slot-description-name slot) v))))
	  (class-slot-descriptions (class-of c))))
  
  (export generic-error-printer)
  
  (set-print-error-callback generic-error-printer)

  (defun !cont x
    (let ((cont *the-cont*))
      (setq *the-cont* nil)
      (if (null x) (cont nil)
	(cont (car x)))))
  
  (export !cont)

  (defun std-apply-any (x . args)
    (generic-apply x args))
  
  (defgeneric generic-apply (fn args))

  (set-no-function-callback std-apply-any)

  (defmethod generic-apply ((x <object>) args)
    (error "invalid operator" <invalid-operator> 'error-value x 'op x 'args args))

  (defmethod generic-apply ((gf <generic-function>) args)
    ((generic-discriminator gf) args))

  (defmethod generic-apply ((fn <function>) args)
    (apply fn args))

  (export generic-apply
	  <invalid-operator> 
	  invalid-operator-args 
	  invalid-operator-op)

;; (1 + 2) => 3
;;  (defmethod generic-apply ((x <number>) args)
;;    (if (numberp (car args))
;;	(call-next-method)
;;      (apply (car args) (cons x (cdr args)))))
  
  ;; New backtrace. 
  ;; uses nasty internal function.

  (export !B)
  (defun !B ()
    (btrace))
  
  (defgeneric print-fn-trace (fn env))

  (defmethod print-fn-trace ((fn <i-function>) env)
      (progn (format t "Entered: ~a~%" fn)
	     (mapc (lambda (x) 
		     (prin "  ")
		     (generic-prin (car x) (standard-output-stream))
		     (prin ":")
		     (generic-prin (cdr x) (standard-output-stream))
		     (newline (standard-output-stream)))
		   env)))

  (defmethod generic-prin ((x <function>) stream)
    (generic-write x stream))

  (defmethod print-fn-trace ((x <object>) env)
    (format t "Entered: inst of: ~a~%" (class-of x)))
  
  (defmethod print-fn-trace ((x <bytefunction>) env)
    (format t "Entered: ~a Env: ~a~%" 
	    x
	    (bytefunction-env x)))

  (defmethod print-fn-trace ((x <generic-function>) env)
    (format t "Entered: ~aEnv: ~a~%" x env))

  (defmethod print-fn-trace ((x <cons>) env)
    (format t "Entered method chain: ~a~" x))

  ;; NB. Can't use labels here (interpreted, the eq test will fail..)

  (defun btrace ()
    (btrace-aux (make-vector 3)))
  
  (defun btrace-aux (vect)
    (if (null (get-backtrace-frame vect))
	nil
      (let ((val (vector-ref vect 2)))
	(if (or (eq (car val) btrace)
		(eq (car val) btrace-aux))
	    nil
	  (progn (print-fn-trace (car val) (cdr val))
		 (btrace-aux vect))))))


     

  ;; end module

  )
