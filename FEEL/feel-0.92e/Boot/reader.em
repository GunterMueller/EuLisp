;; Eulisp Module
;; Author: pab
;; File: reader.em
;; Date: Tue Jul 13 21:49:01 1993
;;
;; Project:
;; Description: 
;;

(defmodule reader
  (eulisp0         
   (rename ((make-obj-reader internal-make-reader)) lreader)
   )
  ()
  
  (export add-reader add-writer make-obj-reader read-next write-next)

  (defun make-obj-reader ()
    (let ((writer (make <generic-function> 
			'lambda-list '(a)
			'argtype 1
			'name 'simple-writer
			'method-class <method>)))
      (internal-make-reader writer)))

  (defun add-writer (reader class id fn)
    (add-method (vector-ref reader 1)
		(make <method> 
		      'signature (list class)
		      'function (method-lambda (obj)
					       (cons id fn)))))
     

  ;; end module
  )
