;; Eulisp Module
;; Author: pab
;; File: proggy.em
;; Date: Wed May  6 18:04:13 1992
;;
;; Project:
;; Description: 
;;

(defmodule proggy
  (standard0
   list-fns
         
   )
  ()
  
  (tagbody 
   top
   (print 'hello)
   (let ((v (lambda (x) 
	      (if (null x) (go base)))))
     (v nil))
   (print 'eek)
   base
   (print 'done))
  

  (defmacro go (x) (*cont* ',x))

  ;; end module
  )
