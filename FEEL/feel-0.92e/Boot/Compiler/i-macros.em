;; Eulisp Module
;; Author: pab
;; File: byte-macros.em
;; Date: Sun Jan 19 21:17:38 1992
;;
;; Project:
;; Description: 
;;    Macro to make defining instructions a breeze

(defmodule i-macros
  (standard0
   list-fns
   scan-args
   )
  ()
  
  (defmacro definstruction (name number . props)
    (let ((xx (scan-args 'nargs props 0))
	  (iname (make-symbol (format nil "~a-info" name))))
      `(progn (defconstant ,iname (make-instruction 'name ',name 'nargs ,xx
						    'bytecode ,number 
						    ,@(mapcar (lambda (x) (list 'quote x))
							      props)))
	      (defconstant ,name 
		(lambda (x)
		  (make-instance instruction 'info ,iname 
				 'args (convert x <vector>))))
	      (add-instruction ',name ,name)
	      (export ,iname ,name))))


  (export definstruction)
  ;; end module
  )
