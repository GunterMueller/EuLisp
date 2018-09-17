;; Eulisp Module
;; Author: pab
;; File: compile.em
;; Date: Wed Jan  8 17:41:44 1992
;;
;; Project:
;; Description: 
;;   Top level interface to compiler

(defmodule compile
  (standard0
   list-fns
   
   comp-utl  ;; These two plus 1st fn should be in a module of their own
   syntax
   pass-0
   gen-code
   iface
   output
   out-fast

   rshow
   stop
   )
  ()
  
  (defun get-module-body (name)
    (let ((stream (get-module-stream name)))
      (unwind-protect 
	  (read stream)
	(close stream))))

  (defcondition Compiler-Error ())

  (deflocal compile-time-failure ())

  (defun compile-2-ast (name)
    (setq compile-time-failure nil)
    (with-handler compile-time-error-handler
     (let ((ast (translate (get-module-body name))))
       (annotate-tree ast)
       (if compile-time-failure 
	   (progn (format t "Compile failed. Abandoning...~%")
		  (error "Compiler-Error" Compiler-Error))
	 (progn (setq y ast)
		(cons ast (generate-code ast)))))))

  (defun my-handler (escape)
    (lambda (cond cont)
      (flush (standard-error-stream))
      (flush (standard-output-stream))
      (if (eq (class-of cond) Compiler-Error)
	  (escape cond)
	(progn (format t "A compiler error has occured"))
	;;(backtrace)
	(flush (standard-error-stream))
	(flush (standard-output-stream))
	)))
  
  (defgeneric compile-time-error-handler (cond cont)
    methods ((((err Module-State-Error) cont)
	      (apply format t
		     (slot-value err 'msg)
		     (slot-value err 'values))
	      (setq compile-time-failure t)
	      (cont))
	     (((err Syntax-Error) cont)
	      (apply format t
		     (slot-value err 'msg)
		     (slot-value err 'values))
	      (setq compile-time-failure t)
	      (cont))
	     ;; re-raise this 'cos I can't continue
	     (((err Compile-Time-Error) cont)
              (apply format t
                     (slot-value err 'msg)
                     (slot-value err 'values))
	      (setq compile-time-failure t)
	      (error "Compiler Failure" Compiler-Error))
	     ;; don't know what it is, so panic.
	     (((x <object>) cont)
	      nil)))

  ;; random vbles to allow debugging
  (deflocal x ())
  (deflocal y ())
  (deflocal a ())

  (defun comp2sc (module-name)
    (comp2sc-aux module-name t))

  (defun comp2rawsc (module-name)
    (comp2sc-aux module-name nil))
  
  (defun comp2sc-aux (module-name initflag)
    (let/cc outahere
	    (with-handler (my-handler outahere)
	      (let ((cs (compile-2-ast module-name)))
		(let ((ast (car cs))
		      (state (cdr cs)))
		  (let ((cu (output-sc-state ast state initflag)))
		    (write-interface-file ast)
		    (write-fastbytes cu)
		    (write-compile-unit cu)
		    cu))))))
  
  (defconstant *raw-mods* '(boot-utils newinit initmeth))

  (defun compile-boot-modules ()
    (mapc comp2rawsc *raw-mods*)
    (mapc comp2sc '(boot macros0 extras0 defs lock standard0)))

  (defun compile-module (x)
    (if (memq x *raw-mods*)
	(comp2rawsc x)
      (comp2sc x)))

  (export  comp2sc compile-module)

  ;; end module
  )
