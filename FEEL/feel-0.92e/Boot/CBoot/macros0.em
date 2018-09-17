;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;   EuLisp Module  -   Copyright (C) Codemist and University of Bath 1990   ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;

;; Change Log:
;;   Version 1.0 

;;

(defmodule macros0

  (init others)
  ()
  ;; The compiler syntax is a little different...
  
  (deflocal *defs-compile-time* ())

  (defun compile-time-p ()
    *defs-compile-time*)

  ((setter setter) compile-time-p
   (lambda (x) (setq *defs-compile-time* x)))
  
  (export compile-time-p)

  (defmacro compile-time forms
    (if (compile-time-p)
	`(progn ,@forms)
      nil))
  
  (defmacro interpret-time forms
    (if (compile-time-p)
	nil
      `(progn ,@forms)))

  (export compile-time  interpret-time)

  (defmacro method-lambda (args . junk)
     `(lambda ,(append (method-extra-args) args) ,@junk))

  (defun method-extra-args ()
    (if (compile-time-p)
	()
      (list '***method-status-handle*** '***method-args-handle***)))

  
  (export method-lambda)

  ;; Control Extentions - Conditional Extentions
  (defmacro cond body
    (if body
	(if (cdr (car body))
	    `(if ,(car (car body))
		 (progn ,@(cdr (car body)))
		 (cond ,@(cdr body)))
	    `(or ,(car (car body)) (cond ,@(cdr body))))
	()))

  ;; Control Extentions - Binding extentions
  ;; LET expands to LAMBDA
   (defmacro let (first . rest)
     (if (symbolp first)
	 (named-let first (car rest) (cdr rest))
	 (anonymous-let first rest)))

   (defun anonymous-let (bind body)
     (if (null bind)
	 `(progn ,@body)
	 `((lambda ,(letvars bind)
	     ,@body)
	   ,@(letforms bind))))

   (defun named-let (name bind body)
     `(labels ((,name ,(letvars bind)
		  ,@body))
	(,name ,@(letforms bind))))

   (defun letvars (b)
     (if b (cons (if (consp (car b))
		     (car (car b))
		     (car b))
		 (letvars (cdr b)))
	 ()))

   (defun letforms (b)
     (if b (cons (if (consp (car b))
		     (car (cdr (car b)))
		     ())
		 (letforms (cdr b)))
	 ()))

  ;; LET* expands to LET
  (defmacro let* (bind . body)
    (if bind
	`(let (,(car bind)) (let* ,(cdr bind) ,@body))
	`(progn ,@body)))

  ;; LABELS is a complex LET

   (defmacro labels (binds . body)
     `(let ,(labelsvar binds)
	,@(labelssetq binds)
	,@body))

  (defun labelsvar (binds)
    (if binds
	(cons (list (car (car binds)) ()) (labelsvar (cdr binds)))
	()))

  (defun labelssetq (binds)
    (if binds
	(cons `(setq ,(car (car binds))
		     (lambda ,(car (cdr (car binds)))
		       ,@(cdr (cdr (car binds)))))
	      (labelssetq (cdr binds)))
	()))
		     
  (defmacro and body
    (if body
	(if (cdr body)
	    `(if ,(car body)
		 (and ,@(cdr body))
		 ())
	    (car body))
	t))

  (defmacro or body
    (if body
	(if (cdr body)
	    `(let ((@@ ,(car body)))
	       (if @@ @@ (or ,@(cdr body))))
	    (car body))
	()))

  (defmacro when (pred . forms) `(if ,pred (progn ,@forms) nil))
  (defmacro unless (pred . forms) `(if ,pred nil (progn ,@forms)))
  
  (export let let* cond and or when unless labels) 
  
  (defmacro unwind-protect (prot . rest)
    `(fn-unwind-protect (lambda () ,prot)
			(lambda () (progn ,@rest))))

  (defmacro let/cc (name . forms)
    `(simple-call/cc 
      (lambda (,name) ,@forms)))

  (defmacro with-handler (fn . forms)
    `(progn (push-handler ,fn)
	    (let ((@ (progn ,@forms)))
	      (pop-handler)
	      @)))

  (export unwind-protect let/cc with-handler)
  ;; Control Extentions - Exit Extentions
  (defmacro block forms (cons 'let/cc forms))

  (defmacro return-from (name . forms)
    (list name (cons 'progn forms)))

  (export block return-from)

  (defmacro catch (tag . body)
    `(let/cc @
	     (dynamic-let ((,tag @)) ,@body)))

  (defmacro throw (tag . forms)
    `((dynamic ,tag) (progn ,@forms)))

  (export catch throw)

  (defmacro prog1 forms
    `((lambda (@prog1-handle@)
	,@(cdr forms)
	@prog1-handle@) ,(car forms)))

  (export prog1)

  ;
  ;; Multiple Values.
  ;;
  ;;  An el-cheapo pseudo implementation.
  ;

  ;;(defmacro values forms
  ;;(if (null (cdr forms)) forms
  ;;`(list ,@forms)))

  ;;(defun call/mv (f values) (apply f values))

  ;;(defmacro let/mv (vars form . body)
  ;;`(call/mv (lambda ,vars ,@body) ,form))

  ;;(export values call/mv let/mv)
  
  ;; Compiler hacks
  
  (defmacro compile-inline (n . x)
    `(%Compiler-special inline-fn ,n ,@x))
  
  (export compile-inline)

  (defmacro compile-declare (bind name value)
    `(%Compiler-special-object add-property
			       (,name ,value) ,bind))

  (defmacro compile-add-callback (bind name value)
    `(%Compiler-special-object add-callback
			       (,name ,value) ,bind))
    
  (export compile-declare compile-add-callback)

  ;; Laziness
  
  (defmacro define-simple-generic (name sig fn)
    `(progn (defconstant ,name (make <generic-function>
				     'lambda-list ',sig
				     'argtype ,(list-length sig)
				     'name ',name
				     'method-class <method>))
	    (add-method ,name (make <method>
				    'signature (list ,@sig)
				    'function ,fn))
	    (export ,name)))
  (export define-simple-generic)
)
 
