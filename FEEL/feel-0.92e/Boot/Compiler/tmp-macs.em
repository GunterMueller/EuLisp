;; Eulisp Module
;; Author: pab
;; File: tmp-macs.em
;; Date: Tue Dec  8 15:35:25 1992
;;
;; Project:
;; Description: 
;;

(defmodule tmp-macs
  ((except (define-simple-generic method-lambda) macros0)
;   list-operators
   lists
   others
   )
  ()
  (defmacro method-lambda (args . junk)
    `(lambda ,(append (method-extra-args) args) ,@junk))

   (defun method-extra-args ()
     (if (compile-time-p)
       ()
       (list '***method-status-handle*** '***method-args-handle***)))


   (export method-lambda)

  (defmacro define-simple-generic (name sig fn)
    `(progn (defconstant ,name (make-instance generic-function
					      'lambda-list ',sig
					      'argtype ,(list-length sig)
					      'name ',name
					      'method-class method))
	    (add-method ,name (make-instance method
                                           'signature (list ,@sig)
                                           'function ,fn))
	    (export ,name)))
  (export define-simple-generic)

      ;; end module
      )
