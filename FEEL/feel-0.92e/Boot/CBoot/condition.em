;; Eulisp Module
;; Author: pab
;; File: condition.em
;; Date: Sat Jul  3 17:55:35 1993
;;
;; Project:
;; Description: 
;;

(defmodule condition
  (init
   gens
   macros0
   extras0
   thread
   )
  ()
  
  (export <Internal-Error> 
	  ;;<wrong-cond-class> 
	  signal
	  with-handler
	  conditionp
	  condition-message
	  error
	  cerror
	  ;;defcondition
	  )

  (defun signal (cond cont . thread)
    (if (null thread)
	(internal-signal cond cont)
      (thread-signal cond cont (car thread))))
  
  ;; end module
  )
