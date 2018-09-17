;; Eulisp Module
;; Author: pab
;; File: eulisp0.em
;; Date: Wed Jun 30 14:47:26 1993
;;
;; Project:
;; Description: 
;;

(defmodule eulisp0
  (gens
   (rename ((car unchecked-car) (cdr unchecked-cdr)) init)
   extras0
   macros0
   error0
   stream
   telos1
   defs
   lock
   thread
   wait
   table
   character
   vector
   string
   list
   copy
   collect
   condition
   numbers
   )
  ()
       
  (expose
   gens
   (except (car cdr) init)
   extras0
   error0
   macros0
   telos1
   stream
   defs
   lock
   thread
   wait
   table
   character
   vector
   string
   list
   copy
   collect
   condition
   numbers
   )

  (defun car (x) 
    (if (consp x) (unchecked-car x)
      (error "car: Not a list" <Internal-Error> 'error-value x)))

  (defun cdr (x) 
    (if (consp x) (unchecked-cdr x)
      (error "cdr: Not a list" <Internal-Error> 'error-value x)))

  (export car cdr)
  
  ((setter setter) car
   (lambda (x v)
     (if (consp x) ((setter unchecked-car) x v)
	 (error "(setter car): Not a list" <Internal-Error> 'error-value x))))

  ((setter setter) cdr
   (lambda (x v)
    (if (consp x) ((setter unchecked-cdr) x v)
	(error "(setter cdr): Not a list" <Internal-Error> 'error-value x))))

  ;; end module
  )
