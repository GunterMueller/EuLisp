;; Eulisp Module
;; Author: pab
;; File: bench.em
;; Date: Thu Jul  2 19:03:18 1992
;;
;; Project:
;; Description: 
;;

(defmodule bench
  (standard0
   list-fns
         
   )
  ()

  (defun i (compile-inline 0)ff

  (defun repeat (f n)
    (if (= n 0) nil
      (progn (f) (repeat f (- n 1)))))

  (defun time (f)
    (let ((xx (cpu-time)))
      (f)
      (- (cpu-time) xx)))

  (defstruct a ()  ((a accessor a-a initarg a)) constructor make-a)

  (defun base (n)
    (time (lambda () (repeat i n))))
  
  (defun other (n)
    (time (lambda () (repeat (lambda () (exp 1.1)) n))))


  ;;end module
  )

