;; Eulisp Module
;; Author: pab
;; File: fib.em
;; Date: Sat Dec  5 19:25:37 1992
;;
;; Project:
;; Description: 
;;

(defmodule fib
        (standard0
         list-fns
         
         )
        ()

  (defun ffib (x) (if (< x 2) 1 (+ (ffib (- x 1)) (ffib (- x 2)))))

  (defun fib (x) 
    (if (< x 2) 
	1
      (binary+ (fib (binary- x 1)) 
		     (fib (binary- x 2)))))
  (defun rfib (x) 
    (if (binary<_Integer x 2) 
	1
      (binary+_Integer (rfib (binary-_Integer x 1)) 
		       (rfib (binary-_Integer x 2)))))
  
  (defgeneric gfib (x)
    methods ((((x <integer>))
	      (if (< x 2) 1 
		(+ (gfib (- x 1))
		   (gfib (- x 2)))))))
    
			
      ;; end module
      )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;  EuLisp Module                     Copyright (C) University of Bath 1991  ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule fib 

  (standard0) ()

  ()

  (defun ffib (x) (if (< x 2) 1 (+ (ffib (- x 1)) (ffib (- x 2)))))

  (defun fib (x) 
    (if (< x 2) 
	1
        (binary-plus (fib (binary-difference x 1)) 
		     (fib (binary-difference x 2)))))
  
  (defgeneric gfib (x)
    methods ((((x <integer>))
	      (if (< x 2) 1 
		(+ (gfib (- x 1))
		   (gfib (- x 2)))))))
    
			
  ;; Employ the methods directly (horrible n'est pas)
  (defun rfib (x) 
    (if (generic_binary_lt\,Integer\,Integer x 2)
	1
      (generic_binary_plus\,Integer\,Integer 
       (rfib (generic_binary_difference\,Integer\,Integer x 1))
       (rfib (generic_binary_difference\,Integer\,Integer x 2)))))
       

)
