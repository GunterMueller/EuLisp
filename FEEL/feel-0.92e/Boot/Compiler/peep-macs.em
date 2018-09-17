;; Eulisp Module
;; Author: pab
;; File: comp-rules.em
;; Date: Wed Apr  1 00:50:29 1992
;;
;; Project:
;; Description: 
;;   Rule compiler for peephole optimizer

(defmodule peep-macs
  (standard0
   list-fns
   
   )
  ()
       
  (defun preprocess-instruct (args)
    (if (= (length args) 3)
	`(if (not (match-instruct ',(car args) i))
	     (cons nil nil)
	   (let ,(make-bindings (cadr args))
	     ,(preprocess-rules (caddr args))))
      (error "Instruct error.." <clock-tick> 'error-value args)))

  (defun make-bindings (lst)
    (labels ((binder (names n)
		     (if (null names) nil
		       (cons (list (car names) `(i-arg-ref i ,n))
			     (binder (cdr names) (+ n 1))))))
	    (binder lst 0)))

  (defun preprocess-attributes (args)
    `(if ,(mk-attrib-test (car args))
	 (let ((,(cadr args) i))
	   ,(preprocess-rules (caddr args)))
       (cons nil nil)))

  (defun mk-attrib-test (args)
    `(let ((@-info-@ (i-info i)))
       (and ,@(mapcar (lambda (test) 
			`(equal (slot-value @-info-@ ',(car test))
				,(cadr test)))
		      args))))
    

  (defun preprocess-test (args)
    `(if ,(preprocess-test-conditions (car args))
	 ,(preprocess-rules (cadr args))
       (cons nil nil)))
  

  (defun preprocess-test-conditions (tests)
    (cons 'and tests))
			  
  (defun preprocess-next (args)
    `(cons nil 
	   (lambda (i) 
	     ,(preprocess-rules (car args)))))

  (defun preprocess-one-of (args)
    (fold (lambda (preproc lst)
	    `(combine-results ,preproc ,lst))
	  (mapcar (lambda (x) 
		    (preprocess-rules x))
		  args)
	  '(cons nil nil)))

  (defun preprocess-do-rules (args)
    `(,(car args) i))

  (defun preprocess-output (args)
    `(cons (cons (list ,@(mapcar 
			  (lambda (text) 
			    (if (atom text)
				text
			      `(,(car text) 
				(list ,@(cdr text)))))
			  args))
		 nil)
	   nil))

  (defconstant find-preproc (mk-finder))
  (progn ((setter find-preproc) 'output  preprocess-output)
	 ((setter find-preproc) 'next  preprocess-next)
	 ((setter find-preproc) 'instruct  preprocess-instruct)  
	 ((setter find-preproc) 'test  preprocess-test)
	 ((setter find-preproc) 'attributes  preprocess-attributes)
	 ((setter find-preproc) 'one-of  preprocess-one-of)
	 ((setter find-preproc) 'do-rules  preprocess-do-rules))

  (defun preprocess-rules (rule)
    ((find-preproc (car rule)) (cdr rule)))
  
  (defmacro peephole-matcher (x)
    `(lambda (i)
       ,(preprocess-rules  x)))

  (defun combine-results (new-thing rest)
    (cons (append (car new-thing)
		  (car rest))
	  (if (null (cdr rest))
	      (if (null (cdr new-thing))
		  nil
		(cdr new-thing))
	    (if (null (cdr new-thing))
		(cdr rest)
	      (lambda (i)
		(combine-results ((cdr new-thing) i)
				 ((cdr rest) i)))))))


  (export peephole-matcher  preprocess-rules combine-results)

  ;; end module
)
