;; Eulisp Module
;; Author: pab
;; File: copy.em
;; Date: Tue Jun 29 22:43:51 1993
;;
;; Project:
;; Description: 
;;   Copy module.
;;   Defines default behaviour for copy.
;;   Sequences handled separately


(defmodule copy
  (extras0
   init
   macros0
   defs
   gens
   )
  ()
  
;  (export copy deep-copy shallow-copy)
  (export deep-copy shallow-copy)

  (defmethod deep-copy ((x <object>))
    (shallow-copy x))

  (defmethod shallow-copy ((x <object>))
    x)
  
  (defmethod deep-copy ((s <structure>))
    (let ((r (make (class-of s))))
      (do
	  (lambda (sd)
	    ((slot-description-slot-writer sd)
	     r
	     (deep-copy ((slot-description-slot-reader sd) s))))
	  (class-slot-descriptions (class-of s)))
      r))

  ;; end module
  )
