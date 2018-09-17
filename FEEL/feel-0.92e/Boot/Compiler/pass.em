;; Eulisp Module
;; Author: pete broadbery
;; File: pass.em
;; Date: 28/jul/1991
;;
;; Project:
;; Description: 
;;  initialises a compiler pass 
;;  --- creates a generic with appropriate methods
;;

(defmodule pass 
  (standard0
   list-fns
   abs-syntx
   
   )
  ()
  
  (defclass compiler-pass-function (<generic-function>)
    ((pre-functions initform () 
		    accessor compiler-pass-pre-functions)
     (post-functions initform () 
		     accessor compiler-pass-post-functions))
    constructor (compiler-pass-maker name lambda-list method-class)
    metaclass <generic-class>)

  (defun make-compiler-pass (name)
    (compiler-pass-maker name (list 'syntax 'thang) <method>))

  (defun add-pre-function (pass function)
    ((setter compiler-pass-pre-functions) pass 
     (cons function (compiler-pass-pre-functions pass))))

  (defun add-post-function (pass function)
    ((setter compiler-pass-post-functions) pass 
     (cons function (compiler-pass-post-functions pass))))


  (defmethod initialize-instance ((x compiler-pass-function) lst)
    (let ((fn (call-next-method)))
      (add-method fn
		  (make-instance (generic-function-method-class fn)
				 'signature (list syntax-obj <object>)
				 'function 
				 (or (interpret-time 
				      (lambda (h1 h2 x thang)
					(pre-pass fn x thang)
					(let ((xx (mapcar (lambda (y)
							    (fn y thang))
							  (subcomponents x))))
					  (post-pass fn x thang)
					  xx)))
				     (compile-time 
				      (lambda (x thang)
					(pre-pass fn x thang)
					(let ((xx (mapcar (lambda (y)
							    (fn y thang))
							  (subcomponents x))))
					  (post-pass fn x thang)
					  xx))))))
      fn))

  (defun pre-pass (pass val x)
    (mapc (lambda (f) (f val x))
	  (compiler-pass-pre-functions pass)))

  (defun post-pass (pass val x)
    (mapc (lambda (f) (f val x))
	  (compiler-pass-pre-functions pass)))

  (defgeneric subcomponents (syntax))

  (defmethod subcomponents ((x leaf-term))
    nil)

  (defmethod subcomponents ((x condition-term))
    (list (cond-test x) (cond-t-part x) (cond-f-part x)))

  (defmethod subcomponents ((x lambda-term))
    (list (lambda-body x)))

  (defmethod subcomponents ((x applic-term))
    (cons (applic-fun x)
	  (applic-args x)))

  (defmethod subcomponents ((x block-term))
    (list (block-decl x)
	  (block-body x)))

  (defmethod subcomponents ((x sequence))
    (sequence-content x))

  (defmethod subcomponents ((x assignment-term))
    (list (assign-var x)
	  (assign-body x)))

  (defmethod subcomponents ((x local-definition))
    (list (defn-body x)))

  (defmethod subcomponents ((x and-decl))
    (and-decl-decls x))

  (defmethod subcomponents ((x rec-decl))
    (list (rec-decl-decl x)))

  (defmethod subcomponents ((x module-block))
    (list (module-body x)))

  (defmethod subcomponents ((x syntax-obj))
    (format t "** No explicit method: ~a~%" (class-of x))
    nil)

  (defmethod subcomponents ((x special-term))
    (special-term-objects x))

  ;; export subcomponents in case someone adds syntax...
  (export make-compiler-pass subcomponents
	  add-post-function add-pre-function
	  pre-pass post-pass)
  
  ;; end module
  )
