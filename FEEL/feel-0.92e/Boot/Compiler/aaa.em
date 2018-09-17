;; Eulisp Module
;; Author: pab
;; File: xxx.em
;; Date: Mon Feb 10 13:15:56 1992
;;
;; Project:
;; Description: 
;;

(defmodule aaa
  (standard0
   list-fns
   aux-macros

   )
  ()

  (defconstant xxx cadr)
  
  (defun f () (xxx (cons 1 2)))
  ;; end module
  )
