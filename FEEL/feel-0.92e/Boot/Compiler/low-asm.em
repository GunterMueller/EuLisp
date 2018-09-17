;; Eulisp Module
;; Author: pab
;; File: low-asm.em
;; Date: Wed Jan 29 21:51:30 1992
;;
;; Project:
;; Description: 
;;

(defmodule low-asm
  (standard0
   list-fns
         
   )
  ()
  
  ;; Target for compilation

  (defstruct compile-unit ()
    ;; a-list of values
    ((statics initarg statics
	      reader compile-unit-statics)
     (name initarg name 
	   reader compile-unit-name)
     (local-count initarg local-count
		  reader compile-unit-local-count)
     (byte-codes initarg byte-codes
		 reader compile-unit-byte-codes)
     ;; Import format is (module-name name)
     (imports initarg imports 
	      reader compile-unit-imports)
     ;; list of ids
     (exports initarg exports
	      reader compile-unit-exports))
    constructor make-compile-unit)

  ;; Link tables.
  ;; The idea is that all the 
  ;; references (internal and external) get shoved into here
  ;; and one pass resolves the lot at load time (call me lazy 
  ;; if you must).
  
  (defstruct link-table ()
    ((tab initform (make-table eq)
	  reader link-table-tab)
     (local-link-tab initform (make-table eq)
		     reader link-tab-internal-links))
    constructor (make-link-table))
     
  
  ;; end module
  )
