;; Eulisp Module
;; Author: pab
;; File: eql.em
;; Date: Thu Jul  8 11:20:01 1993
;;
;; Project:
;; Description: 
;;

(defmodule eql
  (eulisp0         
   )
  ()
  
  ;; any value at all --- o/wise we can't do eql on nil.
  (defconstant *any* '*_any_*)

  ;; new sort of generic function --- we stash the eql-methods separately

  (defclass <eql-generic-function> (<generic-function>)
    ((eql-method-list initform nil accessor eql-methods))
    metaclass <generic-class>
    )
  
  ;; new type of method---we recycle the signature slot.
  ;; This is possibly an error
  (defclass <eql-method> (<method>)
    ()
    metaclass <method-class>)

  ;; have to do this, o/wise method-prin fails
  (defmethod generic-prin ((x <eql-method>) stream)
    (format stream "#<eql-method: ~a (~a)>" 
	    (if (null (method-generic-function x)) 
		"{unattached}"
	      (generic-name (method-generic-function x)))
	    (method-signature x)))

  (defmethod add-method ((gf <eql-generic-function>) (meth <eql-method>))
    ((setter method-generic-function) meth gf)
    ((setter eql-methods) gf
     (cons (cons (method-signature meth) meth)
	   (eql-methods gf))))

  ;; Call std-discriminator if there are no eql methods. Doing this
  ;; means that we may get the optimised std. method lookup. 
  (defmethod compute-discriminating-function ((gf <eql-generic-function>) domain lookup-fn methods)
    (let ((std-discriminator (call-next-method)))
      (lambda (args)
	(let ((odd-methds (find-eql-methods gf args)))
	  (if (null odd-methds)
	      (std-discriminator args)
	    (let ((chain (append odd-methds
				 (lookup-fn args))))
	      (if (null chain)
		  (error "eql: no-applicable-method" <no-applicable-method>)
		(call-method-by-list chain args))))))))
  
  ;; we dont change compute-method-lookup-function to do eql-lookup,
  ;; but rely on discrimination to check that it should be invoked.
  ;; A small asymetry in this is that find-eql-methods should be
  ;; replaced by a compute-eql-method-lookup-function, but that is 
  ;; a) Too long a name
  ;; b) Not really needed for this version

  (defgeneric find-eql-methods (gf args))
  
  ;; by default, no caching...
  (defmethod find-eql-methods ((gf <eql-generic-function>) args)
    (match-sigs args (eql-methods gf)))

  (defun match-sigs (args lst)
    (accumulate (lambda (so-far sig-meth)
		  (if (match-sig args (car sig-meth))
		      (cons (cdr sig-meth) so-far)
		    so-far))
		nil
		lst))

  (defun match-sig (args sig)
    (not (anyp (lambda (arg sig-val)
		 (not (or (eq sig-val *any*)
			  (eql arg sig-val))))
	       args
	       sig)))

		    
  ;; end module
  )


(defgeneric foo (a)
  class <eql-generic-function>)

(defextmethod (class <eql-method>) foo ((x 1))
  (print "found a one")
  (call-next-method))

(defmethod foo ((x <number>))
  (print "number"))

(defmethod foo ((x <object>))
  (print "object"))

(defgeneric bar (a b)
  class <eql-generic-function>)

(defmethod bar ((x <number>) y)
  (print "number"))

(defextmethod (class <eql-method>) bar ((x *any*) (y nil))
  (print "nil")
  (call-next-method))
