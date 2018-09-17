;; Eulisp Module
;; Author: pab
;; File: defs.em
;; Date: Fri Jun 12 18:01:37 1992
;;
;; Project:
;; Description: 
;;

(defmodule defs
  ( macros0
    (except (not) extras0)
    (except (not) init)
    )
  ()
  (defun not (x) (null x))
  
  ;; XXX: Boot problemette
  ;; 'defstruct'...

  ;; Utils... 

  (defconstant *key-list-fail* nil)
  ;;(defun not (x) (null x))

  (defconstant *nothing* (gensym))

  (defun search-key-list (l k)
    (cond ((null l) *key-list-fail*)
	  ((eqcar l k) (cadr l))
	  (t (search-key-list (cddr l) k))))

  (defconstant invalid-slot-options nil)
  '(make <condition-class>
       'name 'invalid-slot-options
       'direct-superclasses (list <condition>)
       'direct-slot-descriptions
          `((name options 
             initargs (options) 
             initform ,(lambda () ()) 
             slot-class ,local-slot-description)))

  (deflocal *name* nil)
  (deflocal *readers* nil)
  (deflocal *writers* nil)
  (deflocal *accessors* nil)
  (deflocal *initargs* nil)

  (defun reset ()
    (setq *name* nil)
    (setq *readers* nil)
    (setq *writers* nil)
    (setq *accessors* nil)
    (setq *initargs* nil))

  (defun canonicalise (ops def-slot-class)
    (when (symbolp ops) (setq ops (list ops)))
    (unless (consp ops) (error "slot options not a list"
  			       <Internal-Error> 'options ops))
    (let ((name *nothing*)
	  (slot-class def-slot-class)
	  (slot-initargs *nothing*)
	  (initform *nothing*)
	  (initarg nil)
	  (readers nil)
	  (writers nil)
	  (accessors nil))
      (labels
       ((inner (l)
	       (unless (null l) 
		       (let ((key (car l)) 
			     (val (cadr l)))
			 (cond ((eq key 'initarg)
				(setq initarg (list val)))
			       ((eq key 'initform)
				(if (eq initform *nothing*)
				    (setq initform `(lambda () ,val))
				  (error "bad initform"
					 invalid-slot-options 'options ops)))
			       ((eq key 'initfunction)
				(if (eq initform *nothing*)
				    (setq initform val)
				  (error "Initform multiply defined"
					 invalid-slot-options 
					 'options ops)))
			       ((eq key 'slot-class) 
				(if (eq slot-class def-slot-class)
				    (setq slot-class   val);;do find-dbclass of val
				  (error "slot-class multiply defined"
					 invalid-slot-options 'options ops)))
			       ((eq key 'slot-initargs)
				(if (eq slot-initargs *nothing*)
				    (setq slot-initargs val);; was class-initargs
				  (error "slot initargs multiply defined"
					 invalid-slot-options 'options ops)))
			       ((eq key 'reader)
				(setq readers (cons (cons val name) readers)))
			       ((eq key 'writer)
				(setq writers (cons (cons val name) writers)))
			       ((eq key 'accessor)
				(setq accessors (cons (cons val name) accessors)))
			       (t (error "unknown slot option"
					 invalid-slot-options 'options ops))))
		       (inner (cddr l)))))
       (setq name (car ops))
       (inner (cdr ops))
       (setq *readers* (nconc readers *readers*))
       (setq *writers* (nconc writers *writers*))
       (setq *accessors* (nconc accessors *accessors*))
       (setq *initargs* (nconc initarg *initargs*))
       (when (eq slot-class *nothing*) 
	     (setq slot-class '<local-slot-description>))
       (when (eq slot-initargs *nothing*)
	     (setq slot-initargs nil))
       (nconc `(list 'name          ',name 
		     ,@(if slot-class `('slot-class    ,slot-class ) nil)
		     ,@slot-initargs
		     ,@(if initarg `('initarg ',(car initarg)) ()))
	      (if (eq initform *nothing*)
		  `('initfunction ',unbound-slot-value) 
		`('initfunction ,initform))))))
  
  (defun reader-defs (o) 
    (mapcar 
      (lambda (pair) 
	`(defconstant ,(car pair) (find-slot-reader ,*name* ',(cdr pair))))
      *readers*))

  (defun writer-defs (o) 
    (mapcar 
      (lambda (pair) 
	`(defconstant ,(car pair) (find-slot-writer ,*name* ',(cdr pair))))
      *writers*))

  (defun accessor-defs (o) 
    (mapcar 
      (lambda (pair) 
	`(progn
	   (defconstant ,(car pair) (find-slot-reader ,*name* ',(cdr pair)))
	   ((setter setter) ,(car pair) (find-slot-writer ,*name* ',(cdr pair)))))
      *accessors*))

  (defun make-constructor-initarg-list (ll)
    (if (not (consp ll)) ()
      (cons (list 'quote (car ll))
	    (cons (car ll) (make-constructor-initarg-list (cdr ll))))))

  (defun improper-list-p (l)
    (if (null (consp l)) l (improper-list-p (cdr l))))

  (defun make-positional-constructor-def (spec)
    (let* ((name (car spec))
	   (ll (cdr spec))
	   (tail (improper-list-p ll)))
      (if (null tail)
	`(defun ,name ,ll
	   (make ,*name*
		 ,@(make-constructor-initarg-list ll)))
	`(defun ,name ,ll
	   (apply
	     make
	     ,*name*
	     (nconc (list ,@(make-constructor-initarg-list ll)) ,tail))))))
    
  (defun constructor-defs (o)
    (cond ((null o) nil)
	  ((null (cdr o)) (error "unbalance class ops" 
				 invalid-slot-options 'options o))
	  ((eqcar o 'constructor)
	    (let ((spec (car (cdr o))))
	      (if (atom spec)
		(cons (make-positional-constructor-def (cons spec 'args))
		      (constructor-defs (cddr o)))
		(cons (make-positional-constructor-def spec)
		      (constructor-defs (cddr o))))))
	  ((eqcar o 'predicate)
	    (cons `(progn
		     (defgeneric ,(car (cdr o)) (obj))
		     (defmethod ,(car (cdr o)) ((obj <object>)) ())
		     (defmethod ,(car (cdr o)) ((obj ,*name*)) obj))
		  (constructor-defs (cddr o))))
	  (t (constructor-defs (cddr o)))))

  (defun quotify-alternate (l)
    (if (null l) ()
      (cons (list 'quote (car l)) 
	    (cons (car (cdr l)) 
		  (quotify-alternate (cdr (cdr l)))))))

  (defun metaclass-initargs (ops)
    (let ((args (search-key-list ops 'metaclass-initargs)))
      (unless (eq args *key-list-fail*)
	(quotify-alternate args))))
      
  (defmacro defstruct (name super slot-ops . class-ops)
    (reset)
    (setq *name* name)
    `(progn
       (defconstant ,name
	 (make <structure-class>
	   'name ',name
	   'direct-superclasses ,(if super `(list ,super) '(list <structure>)) 
	   'direct-slot-descriptions
	     (list ,@(mapcar (lambda (x)
			       (canonicalise x '<local-slot-description>))
			     slot-ops))
	   'metaclass-hypotheses nil))
       ,@(reader-defs slot-ops)
       ,@(writer-defs slot-ops)
       ,@(accessor-defs slot-ops)
       ,@(constructor-defs class-ops)
       ',name))

  (export defstruct)

  (defmacro defclass (name supers slot-ops . class-ops)
    (reset)
    (setq *name* name)
    (let ((metaclass
	   (or (search-key-list class-ops 'class)
	       (search-key-list class-ops 'metaclass)
	       '<class>))
	  (initargs 
	   (or (search-key-list class-ops 'metaclass-initargs) nil))
	  (additional-initargs
	   (or (search-key-list class-ops 'direct-initargs)
	       (search-key-list class-ops 'initargs)
	       nil))
	  (slot-class (search-key-list class-ops 'default-slot-class) ))
      `(progn
	 (defconstant ,name
	   (make ,metaclass
	     'name ',name
	     'direct-superclasses
	     ,(if supers `(list ,@supers) '(list <object>))
	     'direct-slot-descriptions
	     (list ,@(mapcar (lambda (x) (canonicalise x slot-class))
			     slot-ops))
	     'metaclass-hypotheses ()
	     'direct-initargs ',(append additional-initargs *initargs*)
	     ,@(metaclass-initargs class-ops)))
	 ,@(reader-defs slot-ops)
	 ,@(writer-defs slot-ops)
	 ,@(accessor-defs slot-ops)
	 ,@(constructor-defs class-ops)
	 ',name)))

  (export defclass)

  (defmacro defreader (name class slot)
    `(defconstant ,name (find-slot-reader ,class ',slot)))

  (defmacro defwriter (name class slot)
    `(defconstant ,name (find-slot-writer ,class ',slot)))

  (defmacro defaccessor (name class slot)
    `(progn
       (defconstant ,name (find-slot-reader ,class ',slot))
       ((setter setter) ,name (find-slot-writer ,class ',slot))))

  (defmacro defpredicate (name class)
    `(progn
       (defgeneric ,name (x))
       (defmethod ,name ((x <object>)) ())
       (defmethod ,name ((x ,class)) x)))

  (export defreader defwriter defaccessor defpredicate)

  (defun method-extra-args ()
    (if (compile-time-p)
	()
      (list '***method-status-handle*** '***method-args-handle***)))

  (defun sll-signature (ll)
    (cond ((atom ll) nil)
	  ((consp (car ll)) (cons (cadar ll) (sll-signature (cdr ll))))
	  (t (cons '<object> (sll-signature (cdr ll))))))

  (defun sll-formals (ll)
    (cond ((null ll) nil)
	  ((atom ll) ll)
	  ((consp (car ll)) (cons (caar ll) (sll-formals (cdr ll))))
	  (t (cons (car ll) (sll-formals (cdr ll))))))

  (defun gf-class (ops)
    (let ((val (search-key-list ops 'class)))
      (if (eq val *key-list-fail*) '<generic-function> val)))

  (defun gf-method-class (ops)
    (let ((val (search-key-list ops 'method-class)))
      (if (eq val *key-list-fail*) '<method> val)))
  
  (defun gl-name (ops)
    (let ((val (search-key-list ops 'name)))
      (if (eq val *key-list-fail*) '*unnamed-lambda* val)))

  (defun find-method-list (ops)
    (labels ((grab-methods (ops so-far)
			   (cond ((null ops) (nreverse so-far))
				 ((eq (car ops) 'method)
				  (grab-methods (cddr ops)
						(cons (cadr ops) so-far)))
				 (t (grab-methods (cddr ops) so-far)))))
	    (let ((meths (search-key-list ops 'methods)))
	      (if (eq meths *key-list-fail*) 
		  (grab-methods ops nil)
		(nconc (grab-methods ops nil) meths)))))

  (defun gf-methods (ops mc)
    (let ((val (find-method-list ops)))
      `(list
	,@(mapcar
	   (lambda (form)
	     `(make ,mc
		    'signature (list ,@(sll-signature (car form)))
		    'function
		    (lambda (,@(method-extra-args)
				,@(sll-formals (car form)))
		      ,@(cdr form)))) 
	   val))))

  (defmacro defgeneric (name ll . ops)
    `(,@(if (symbolp name) (list 'defconstant name)
	  (list `(setter setter) (car (cdr name))))
       (make ,(gf-class ops)
	  'name ',name
          'lambda-list ',ll
	  'method-class ,(gf-method-class ops)
	  'argtype ,(list-length ll)
	  'methods ,(gf-methods ops (gf-method-class ops))
	  'domain (list ,@(sll-signature ll))
	  )))
       

  (export defgeneric)

  (defmacro defmethod (name sll . body)
    (defmethod-aux `(generic-method-class ,name) nil name sll body))
  
  (defmacro defextmethod (opts name sll . body)
      (let ((class (scan-args 'class opts (default-argument
					    `(generic-method-class ,name) )))
	    (opts (quotify-alternate opts)))
	(defmethod-aux class opts name sll body)))

  (export defextmethod)
  

  (defun defmethod-aux (class opts name sll body)
    `(progn
       (add-method 
	,name
	(make ,class
	      'signature (list ,@(sll-signature sll))
	      'argtype ,(length sll)
	      'function
	      (lambda ,(append (method-extra-args)
			       (sll-formals sll))
		,@body)
	      ,@opts))))

  (export defmethod)

  (defun defcondition-slot-descriptions (l)
    (if (null l) nil
      (cons `(list 'name ',(car l) 
	           'slot-class <local-slot-description>
                   'initargs ',(list (car l))
                   'initform (lambda () ,(cadr l)))
	    (defcondition-slot-descriptions (cddr l)))))

  (defmacro defcondition (name super . pairs)
    `(defconstant ,name
       (make <condition-class>
          'name ',name
          'direct-superclasses (list ,(if super super '<condition>))
	  'direct-slot-descriptions
	    (list ,@(defcondition-slot-descriptions pairs)))))

  (export defcondition)

   (defmacro call-next-method ()
     (if (compile-time-p)
	 '(call-method-by-list (method-method-list) 
			     (method-arg-list))
       '(if  ***method-status-handle***
	    (progn ;;(format t "Call next: ~a ~a\n"
	      ;;***method-status-handle***
	      ;;	 ***method-args-handle***)
	      (apply call-method-by-list
		     (list ***method-status-handle***
			   ***method-args-handle***)))
	  (error "No Next Method" <Internal-Error> nil))))

   (defmacro next-method-p ()
     (if (compile-time-p)
	 (progn (error "Next-method-p: not implemented" <Internal-Error>)
		nil)
       '***method-status-handle***))

   (export next-method-p)

  (defmacro generic-lambda (args . ops)
    `(make ,(gf-class ops)
	  'name ',(gl-name ops)
          'lambda-list ',args
	  'method-class ,(gf-method-class ops)
	  'methods ,(gf-methods ops (gf-method-class ops))))

  
  (export call-next-method generic-lambda)

)

