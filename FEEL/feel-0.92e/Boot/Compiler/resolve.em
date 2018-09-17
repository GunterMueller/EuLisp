;; Eulisp Module
;; Author: pab
;; File: resolve.em
;; Date: Tue Feb  4 11:25:29 1992
;;
;; Project:
;; Description: 
;;   Takes a bytecode stream and (destructively) resolve the labels into
;;   relative distances, also turns static references into 
;;   Note: all instructions have a single 

(defmodule resolve
  (standard0
   list-fns
   
   instruct
   )
  ()
    
  ;; should use some ADT as input.
  ;; All to simple --- should use a couple of tables (hashed?)
  (defun resolve-code-list (lst)
    (resolve-aux lst 0 () ()))
  
  (defun resolve-aux (lst i-count branches labels)
    (cond ((null lst) nil)
	  ((is-branch (car lst))
	   (let ((xx (find-label (instruction-label (car lst)) labels)))
	     (if (null xx)
		 (resolve-aux (cdr lst) (+ i-count 1) 
			      (cons (cons (car lst) i-count)
				    branches)
			      labels)
	       (progn ((setter instruction-label) (car lst)
		       (- (cdr xx) i-count))
		      (resolve-aux (cdr lst) (+ i-count 1)
				   branches labels)))))
	  ((is-label (car lst))
	   (let ((lab (car lst)))
	     ((setter car) lst (car (cdr lst)))
	     ((setter cdr) lst (cdr (cdr lst)))
	     (resolve-aux lst i-count
			(fold (resolve-branch i-count labels)
			      branches
			      nil)
			(cons (cons (instruction-label lab)
				    i-count)
			      labels))))
	  (t (resolve-aux (cdr lst) (+ i-count 1) branches labels))))
  
  ;; This _should_ be destructive !
  (defun resolve-branch (i-num labels)
    (lambda (brancher left)
      (let ((xx (assoc (instruction-label (car brancher)) labels equal)))
	(if (null xx) (cons brancher left)
	  (progn ((setter instruction-label) (car brancher)
		  (- (cdr xx) (cdr brancher)))
		 left)))))

  (defun find-label (lab lst)
    (assoc lab lst equal))

  (export resolve-code-list)
  ;; end module
  )
