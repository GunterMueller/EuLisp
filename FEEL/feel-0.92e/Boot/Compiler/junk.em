;; Eulisp Module
;; Author: pab
;; File: junk.em
;; Date: Sat Feb 22 19:58:46 1992
;;
;; Project:
;; Description: 
;;

(defmodule junk
  (standard0
   list-fns
   
   ;;   instruct
   streams
   )
  ()
  
  (defstruct state ()
    ((stream initarg stream
	       reader state-stream)
     (vars initarg vars 
	   reader state-vars))
    constructor (new-state loc vars))

  (defconstant peep-rules
    '(one-of (instruct 
	      slide (d1 keep1)
	      (next 
	       (one-of (instruct 
			slide (d2 k2)
			(test ((not (> keep1 k2)))
			      (output (slide (+ d1 d2 (- keep1)) k2)))))))
	     (instruct 
	      nth (n)
	      (next
	       (one-of (instruct slide (d1 k1)
				 (one-of (test ((> n d1))
					       (output (slide (- d1 1) (- k1 1))
						       (nth (+ n (- d1) k1))))
					 (test ((< n k1))
					       (output (slide (- d1 1) (- k1 1))
						       (nth n)))
					 (test ((= k1 2) (= n 1))
					       (output (slide (- d1 1) 2) (swap))))))))
	     (instruct swap ()
		       (next 
			(instruct swap ()
				  (output))))
	     ;; rats... can't do anything.
	     ))


  (defun match-instruct (name i)
    (eq name (car i)))

  (defun preprocess-instruct (args)
    `(if (not (match-instruct ',(car args) i))
	 nil
       (let ,(make-bindings (cadr args))
	 ,(preprocess-rules (caddr args)))))

  (defun make-bindings (lst)
    (labels ((binder (names n)
		     (if (null names) nil
		       (cons (list (car names) `(nth ,n (cdr i)))
			     (binder (cdr names) (+ n 1))))))
	    (binder lst 0)))

  (defun preprocess-test (args)
    `(if ,(preprocess-test-conditions (car args))
	 ,(preprocess-rules (cadr args))
       nil))
  
  (defun preprocess-test-conditions (tests)
    (cons 'and tests))
			  
  (defun preprocess-next (args)
    `(list 'incomplete 
	   (lambda (i) 
	     ,(preprocess-rules (car args)))))

  (defun preprocess-one-of (args)
    (fold (lambda (preproc lst)
	    `(append ,preproc ,lst))
	  (mapcar (lambda (x) 
		    (preprocess-rules x))
		  args)
	  nil))

  (defun preprocess-output (args)
    `(cons 'complete ,(mapcar 
		       (lambda (text) 
			 `(cons ,(car text) 
				(list ,(cdr text))))
		       args)))

  (defconstant find-preproc (mk-finder))
  (progn ((setter find-preproc) 'output  preprocess-output)
	 ((setter find-preproc) 'next  preprocess-next)
	 ((setter find-preproc) 'instruct  preprocess-instruct)  
	 ((setter find-preproc) 'test  preprocess-test)
	 ((setter find-preproc) 'one-of  preprocess-one-of))

  (defun preprocess-rules (rule)
    ((find-preproc (car rule)) (cdr rule)))
  
  (defmacro peephole-matcher (x)
    `(lambda (i)
       ,(preprocess-rules  x)))
  ;; end module
  )
nth
static
pop
set



