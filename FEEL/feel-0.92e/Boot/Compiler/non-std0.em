;; Eulisp Module
;; Author: pab
;; File: non-std0.em
;; Date: Tue Feb 25 15:15:21 1992
;;
;; Project:
;; Description: 
;;

(defmodule non-std0
  (standard0
         
   )
  ()
  
  (definline cons (a b) (i-cons))

  (definline vector-ref (a b)
    (vref))

  (definline null (x) (not))
  (definline not (x) (not))

  ;; end module
  )
