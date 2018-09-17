;; Eulisp Module
;; Author: pab
;; File: mixins.em
;; Date: Fri Apr 16 16:20:24 1993
;;
;; Project:
;; Description: 
;;

(defmodule mixins
  (eulisp0
   )
  ()
  
  (defun detect (fn lst)
    (if (null lst) nil
      (or (fn (car lst)) 
	  (detect fn (cdr lst)))))
  
  (export <mixin-class> <mixin-base-class>)

  (defclass <mixin-class> (<mi-class>) 
    ()
    predicate mixin-class-p
    metaclass <class>)

  (defclass <mixin-base-class> (<class>)
    ()
    predicate mixin-base-p
    metaclass <class>)

  ;; compatability
  (defmethod compatible-superclasses-p ((cl <mixin-class>) lst)
    (and (call-next-method)
	 (not (detect (lambda (x)
			(not (or (mixin-class-p x)
				 (eq x <object>))))
		      lst))))
  
  (defun check-reps (lst reps)
    (cond ((null lst) t)
	  ((eq (car lst) object)
	   (check-reps (cdr lst) reps))
	  ((memq (car lst) reps) nil)
	  (t (check-reps (cdr lst) (cons (car lst) reps)))))

  ;; don't call next method as we are specialising single inheritance
  (defmethod compatible-superclasses-p ((cl <mixin-base-class>) lst)
    (let ((last (last-pair lst))
	  (not-last (cdr (reverse lst))))
      (and (not (detect (lambda (super)
			  (not (or (mixin-class-p super)
				   (eq <object> super))))
			not-last))
	   (not (mixin-class-p last)))))

  ;; class precedence lists

  ;; only duplicate should be object...
  (defmethod compute-precedence-list ((cl <mixin-base-class>)  (direct-superclasses <cons>))
    (cons cl (remove-duplicates-from-end (depth-first-preorder direct-superclasses))))

  (defun remove-duplicates-from-end (elements)
    (labels ((fold (elements result)
		   (cond
		    ((null elements) result)
		    ((member (car elements) result eq) (fold (cdr elements) result))
		    (t (fold (cdr elements) (cons (car elements) result))))))
	    (fold (reverse elements) '())))

 
 (defun depth-first-preorder (lst)
   (if (null lst) nil
     (cons (car lst)
	   (append (depth-first-preorder (cdr (class-precedence-list (car lst))))
		   (depth-first-preorder (cdr lst))))))
	     
 ;; slot description creation
 ;; Plan is that mixin-sds do not have accessors,
 ;; except when instantiated into a base class. 

 (defclass <mixin-slot-description> (<local-slot-description>)
   ((home-class accessor mixin-sd-home))
   metaclass <slot-description-class>)

  (defmethod metaclass-default-slot-description-class ((cl <mixin-class>))
    <mixin-slot-description>)
  
  (defmethod compute-defined-slot-description ((cl <mixin-class>) spec)
    (let ((sd (call-next-method)))
      ((setter mixin-sd-home) sd cl)
      sd))

  (defun check-names (lst)
    (labels ((aux (lst seen)
		  (cond ((null lst) t)
			((memq (car lst) seen)
			 nil)
			(t (aux (cdr lst) (cons (car lst) seen))))))
	    (aux lst nil)))

  ;; slot accessors
  ;; refuse to add methods on mixins..
  ;; hope we don't get caught by method lookups later...
  
  (defmethod ensure-slot-reader ((cl <mixin-class>) sd sds fn)
    nil)

  (defmethod ensure-slot-writer ((cl <mixin-class>) sd sds fn)
    nil)

  ;; have to change ensure-slot-reader s.t. when a 
  ;; new mixin-slot is inherited, we add a method
  
  (defmethod ensure-slot-reader ((cl <mixin-base-class>) (sd <mixin-slot-description>) sds fn)
    (if ((generic-method-lookup-function fn) (list cl)) nil
      (let ((reader (compute-primitive-reader-using-slot-description sd cl sds)))
	(add-method fn
		    (make <method> 
			  'signature (list cl)
			  'function (method-lambda (o) (reader o))))))
    fn)

  (defmethod ensure-slot-writer 
    ((cl <mixin-base-class>) (sd <mixin-slot-description>) sds fn)
    (if ((generic-method-lookup-function fn) (list cl <object>)) nil
      (let ((writer (compute-primitive-writer-using-slot-description sd cl sds)))
	(add-method fn
		    (make <method> 
			  'signature (list cl <object>)
			  'function (method-lambda (o v) (writer o v))))))
    fn)

  (defmethod allocate ((cl <mixin-class>) lst)
    (error "Can't allocate a mixin class" <Internal-Error> 'error-value cl))
				 
  ;; end module
  )


;; Next trick: Mixin-metaclasses.

;;; Define a base-class Point:

(defclass <point> ()
  ((x initform 0 accessor point-x initarg x)
   (y initform 0 accessor point-y initarg y))
  )

(defclass <coloured> ()
  ((colour initform 'black initarg colour 
	  reader colour))
  metaclass <mixin-class>)

(defgeneric colour-of (obj)
  method (((obj <object>)) 'gray)
  method (((obj <coloured>))
	  (colour obj)))

(defclass <coloured-point> (<coloured> <point>) 
  ()
  metaclass <mixin-base-class>)

(setq p1 (make <point>))
(colour-of p1)

(setq p2 (make <coloured-point> 'x 1 'y 1 'colour 'red))
(colour-of p2)

