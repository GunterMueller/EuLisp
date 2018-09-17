;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;  EuLisp Module                     Copyright (C) University of Bath 1991  ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Simple tracing facility
;;
;; (trace-bindings symbol1 symbol2 ... symboln) causes the symbols to be
;; traced, printing arguments on entry and result on exit.
;;
;; (untrace-bindings symbol1 symbol2 ... symboln) causes the symbols to be
;; retored to their original state.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule trace

  (standard) ()

  (defstruct traced-fn ()
    ((fn 
       initarg fn
       accessor traced-fn-fn))
    constructor make-traced-fn)

  (deflocal *trace-table* (make-table eq))

  (defun trace-indent (n char)
    (if (= n 0) (prin char (trace-output-stream))
      (progn
	(prin char (trace-output-stream))
	(trace-indent (- n 1) char))))

  (defmacro trace-bindings name-list
    `(progn
       ,@(mapcar 
	   (lambda (name) 
             `(progn
		(deflocal ,name
		  (let ((value (make-traced-fn 'fn ,name))
			(level 0))
		    ((setter table-ref) *trace-table* ',name value)
		    (lambda args 
		      (let ((ret ()))
			(format (trace-output-stream) "~a:" ',name)
			(trace-indent level #\>)
			(format (trace-output-stream) " ~a~%" args)
			(setq level (+ level 1))
                        (unwind-protect
                          (progn
                            (setq ret (apply (traced-fn-fn value) args))
			    (format (trace-output-stream) "~a:" ',name)
			    (trace-indent (- level 1) #\<)
			    (format (trace-output-stream) " ~a~%" ret))
			  (setq level (- level 1))
			  ret)))))))
	     name-list)))

  (defmacro untrace-bindings name-list
    `(progn
       ,@(mapcar
	   (lambda (name)
	     `(progn
		(deflocal ,name 
		  (traced-fn-fn (table-ref *trace-table* ',name)))
		((setter table-ref) *trace-table* ',name nil)))
	   name-list)))

  (export trace-bindings untrace-bindings traced-fn 
	  traced-fn-fn make-traced-fn *trace-table* trace-indent)

)
