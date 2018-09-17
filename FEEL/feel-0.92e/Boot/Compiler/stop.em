;; Eulisp Module
;; Author: pete broadbery
;; File: stop.em
;; Date: 28/jun/1991
;;
;; Project:
;; Description: 
;; function to stop processing andf then allows continuation
;;

(defmodule stop 
  (standard0
   list-fns
   )
  ()

  (defclass Stop (<condition>)
    ())

  (deflocal *cont* ())
  (deflocal *vals* ())
  (deflocal *ignore* ())

  (defun set-ignore (x)
    (setq *ignore* x))

  (defun stop (vals)
    (if (not *ignore*)
	(let/cc continue
		(setq *cont* continue)
		(setq *vals* vals)
		(cerror "Stopping..." Stop 'error-value vals))
      vals))
       
  (defun cont ()
    (*cont* *vals*))
  
  (defun vals ()
    *vals*)
  
  ((setter setter) vals 
   (lambda (x) (setq *vals* x)))

  (export stop cont vals set-ignore)
  ;; end module
  )
