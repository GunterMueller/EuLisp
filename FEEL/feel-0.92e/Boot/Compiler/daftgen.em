;; Eulisp Module
;; Author: pab
;; File: daftgen.em
;; Date: Mon May 18 09:57:02 1992
;;
;; Project:
;; Description: 
;;   Eql methods

(defmodule daftgen
  (standard0
   list-fns
   
   initcode
   )
  ()
  
  (defclass daft-gf (generic-function)
    ()
    )
  
  (defmethod compute-discriminating-function ((gf generic-function))
    (lambda (sig)
      (let ((meths (find-applicable-methods 
		    ))))))

  ;; end module
  )
