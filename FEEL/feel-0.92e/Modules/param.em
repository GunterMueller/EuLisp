;; Eulisp Module
;; Author: pab
;; File: test.em
;; Date: Mon Apr 19 11:03:33 1993
;;
;; Project:
;; Description: 
;;   Sample parameterised classes.
;;   Really telos abuse, plus example use
;;   of generic-apply.

(defmodule param
  (eulisp0
   scanners
   mixins
   )
  ()
  
  (defun detect (fn lst)
    (if (null lst) nil
      (or (fn (car lst)) 
	  (detect fn (cdr lst)))))

  ;; Parameterisation. This is mixin abuse. Idea is that 
  ;; (<class> args) gives new parameterised classes. These
  ;; cannot be subclassed.

  (defclass <parameterised-class> (<class>)
    ((sigs initform nil accessor parameterised-sigs)
     (superclass accessor parameterised-superclass)
     (meta initform <parameterised-meta> initarg instance-meta reader parameterised-meta)
     (options initform nil initarg instance-metaclass-initargs reader parameterised-initargs))
    metaclass <class>)

  (defclass <non-subclassable> ()
    ()
    metaclass <mixin-class>)

 (defclass <parameterised> ()
    ((parameters initarg parameters reader parameterised-args))
    metaclass <mixin-class>)
 
 ;; standard parameterised instance 
 (defclass <parameterised-meta> (<non-subclassable> <class>)
   ()
   metaclass <mixin-base-class>)

  (defmethod compatible-superclass-p ((cl <class>) (cl <non-subclassable>))
    nil)

  (defmethod add-subclass ((cl <non-subclassable>) sub)
    (error "Attempt to subclass non-subclassable class" clock-tick 'error-value cl))

  (defmethod initialize ((cl <parameterised-class>) lst)
    (let ((instance-supers (scan-args 'instance-superclasses lst 
				      (default-argument (list <object>))))
	  (this-meta (scan-args 'super-meta lst (default-argument <mixin-class>)))
	  (cl (call-next-method)))
      ((setter parameterised-superclass) cl 
       (let ((initargs 
	      (nconc (list 'direct-superclasses instance-supers
			   'direct-slot-descriptions (scan-args 'instance-slot-descriptions lst null-argument)
			   'name (make-symbol (format nil "[~a]" (symbol-unbraced-name (class-name cl)))))
		     (parameterised-initargs cl))))
	 (initialize (allocate this-meta initargs) initargs)))
      cl))
  
  (defun find-parameterised (cl sig)
    (if (null sig) 
	(parameterised-superclass cl)
      (or (lookup-parameterised cl sig)
	  (let ((new (make-parameterised cl sig)))
	    (add-parameterised cl sig new)
	    new))))

  (defun lookup-parameterised (cl sig)
    (let ((xx 
	   (assoc sig 
		  (parameterised-sigs cl)
		  equal)))
      (if (null xx) nil (cdr xx))))

  (defun make-parameterised (cl sig)
    (let ((initargs (nconc 
		     (list 
		      'direct-superclasses (list (parameterised-superclass cl))
		      'direct-slot-descriptions nil
		      'parameters sig
		      'name (make-symbol (format nil "~a~a" 
						 (symbol-unbraced-name (class-name cl))
						 (mapcar object-name sig))))
		     (parameterised-initargs cl))))
      (initialize (allocate (parameterised-meta cl) initargs) initargs)))

  (defgeneric object-name (x)
    method (((x <class>)) (symbol-unbraced-name (class-name x)))
    method (((o <object>)) o)
    method (((fn <generic-function>)) (generic-function-name x)))

  (defun add-parameterised (cl sig new)
    ((setter parameterised-sigs) cl 
     (cons (cons sig new) (parameterised-sigs cl))))

  ;; Just for entertainment value
  ;; apply creates new parameterised classes

  (defmethod generic-apply ((x <parameterised-class>) args)
    (find-parameterised x args))


    )

  
  ;; end module
  )

(defclass <H-List> ()
  ()
  metaclass <parameterised-class>
  metaclass-initargs ())




 (defclass Polynomial ()
    ()
    metaclass parameterised-class
    metaclass-initargs (instance-meta parameterised-domain-class
			instance-superclasses (list Ring)
			super-meta domain-class
			instance-metaclass-initargs nil)
