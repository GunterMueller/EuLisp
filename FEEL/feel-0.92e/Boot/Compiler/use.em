;; Eulisp Module
;; Author: pete broadbery
;; File: use.em
;; Date: 15/sep/1991
;;
;; Project:
;; Description: 
;; calculates the use-set of a form
;;

(defmodule use 
  (standard0
   list-fns
   
   abs-syntx
   syntax-utils
   props
   pass
   )
  ()
  
  (defun use-set (x)
    (if (cached-use-set x)
	(car (cached-use-set x))
      (let ((cs (calc-use-set x)))
	(format t "use: ~a is: ~a~%" x cs)
	((setter cached-use-set) x (cons cs t))
	cs)))
  
  (defgeneric calc-use-set (obj))

  (defmethod calc-use-set ((x syntax-obj))
    (fold append
	  (mapcar use-set (subcomponents x))
	  nil))

  (defmethod calc-use-set ((x ident-term))
    (print x)
    (setq y x)
    (list (car (ident-defblock x))))

  (defmethod calc-use-set ((x lambda-term))
    (if (inline-lambda x)
	(set-difference (call-next-method)
			(lambda-ids x))
      nil))

  (defmethod calc-use-set ((x block-term))
    (set-difference (call-next-method)
		    (find-decls x)))

  ;; should be destructive delete.

  (defun set-difference (x y)
    (cond ((null y) x)
	  (t (set-difference 
	      (deleteq x (car y))
	      (cdr y)))))
		    
  (export use-set)
  ;; end module
  )
