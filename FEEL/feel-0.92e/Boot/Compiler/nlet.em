;; Eulisp Module
;; Author: pab
;; File: nlet.em
;; Date: Fri Jan 17 00:59:37 1992
;;
;; Project:
;; Description: 
;;

(defmodule nlet
  (standard0
   list-fns
         
   )
  ()

  (defun Y (ob)
    (lambda (f)
      (lambda (x)
	((f (x x))
	 (lambda (f) (f (x x)))))))

  ;; end module
  )
