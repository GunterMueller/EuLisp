;; Eulisp Module
;; Author: pab
;; File: test-in.em
;; Date: Wed Jun  3 17:14:01 1992
;;
;; Project:
;; Description: 
;;   Test of inline junk

(defmodule test-in
  (standard0
   list-fns
   
   )
  ()
  
  (defconstant acar (compile-inline  1 (slot-ref 0)))
  (defconstant set-acar (compile-inline 2 (set-slot 0)))

  (export acar set-acar)
  
  ((setter setter) acar set-acar)
  ;; end module
  )
