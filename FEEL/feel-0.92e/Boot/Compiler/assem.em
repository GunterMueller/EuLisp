;; Eulisp Module
;; Author: pab
;; File: assem.em
;; Date: Sat May  2 14:25:06 1992
;;
;; Project:
;; Description: 
;;

(defmodule assem
  ((except (fold) standard)
   (only (pair) standard0)
   list-fns
  
   instruct
   byte-stream
   comp-defn

   stop
   )
  ()
         ;; Assembly....
  ;;

  (defun reify-code-list (lst)
    (reify-bytecode-stream lst (make-simple-stream) 0 () ()))
  
  (export reify-code-list)

  ;; I assume that input is a list. Really should abstract more 
  ;; so that I can spot, and hack basic blocks, and do micro-optimisation

  (defun reify-bytecode-stream (input stream i-count branches lablist)
    (if (null input)
	(progn (format t "Done: ~a instructions~%" i-count)
	       (list (convert stream pair) i-count))
      (let ((xx (car input)))
	(labels ((reify-std-instruct (args types branches lablist so-far)
		  (cond ((null args)
			 (reify-bytecode-stream (cdr input)
						(write-stream stream 
							      (cons (i-inumber xx)
								    (reverse so-far)))
						(+ i-count (instruction-size xx))
						branches lablist))
			((is-branch-arg (car types))
			 (let ((i-and-branch (make-branch (car args) branches lablist i-count)))
			   (reify-std-instruct (cdr args)
					       (cdr types)
					       (cdr i-and-branch)
					       lablist
					       (cons (car i-and-branch) so-far))))
			(t (let ((ins (reify-arg (car types) (car args) i-count branches lablist)))
			     (reify-std-instruct (cdr args)
						 (cdr types)
						 branches
						 lablist
						 (cons ins so-far))))))
		 (add-label (lab)
			    (reify-bytecode-stream (cdr input)
						   stream
						   i-count 
						   (fold (tie-branch i-count lab)
							 branches
							 nil)
						   (cons (cons lab
							       i-count)
							 lablist))))
		(cond ((is-inline-code xx)
		       (reify-bytecode-stream (cdr input) 
					      (write-stream-list stream 
								 (inline-code xx))
					      (+ (inline-code-count xx) i-count)
					      branches 
					      lablist))
		      ((is-null-op xx)
		       (reify-bytecode-stream (cdr input) stream i-count branches lablist))
		      ((is-label xx)
		       (add-label (instruction-label xx)))
		      (t 
		       (reify-std-instruct (i-arg-list xx)
					   (instruction-argtypes (i-info xx))
					   branches
					   lablist
					   nil)))))))

  (defun reify-arg (argtype arg i-count branches labels)
    ;; returns list  converted arg + new branch info
    (cond
     ((is-link-arg argtype)
      (make-link arg))
     ((is-static-arg argtype) 
      (if (consp arg)
	  arg
	(make-static arg)))
     ((= argtype 4);; XXX
      (if (consp arg)
	  arg
	(list (the-long-handle) arg)))
     (t arg)))

  ;; convert a branch into something sane.
  ;; returns cons of instruction, plus branches

  (defun make-branch (label branches labels i-count)
    (let ((xx (find-label label labels)))
      (if (null xx)
	  (let ((bytes (list (the-long-handle) label)))
	    (cons bytes 
		  (cons (cons (cdr bytes) i-count)
			branches)))
	;; found the label
	(cons (list (the-long-handle)
		    (- (cdr xx) i-count))
	      branches))))

  (defun find-label (lab lst)
    ;; Labels ought to be unique.
    (assoc lab lst eq))

  (defun make-link (arg)
    (cons (the-link-handle) arg))

  (defun make-static (arg)
    (cons (the-static-handle) arg))

  ;; enclosing fold ought to be destructive...
  (defun tie-branch (i-count lab)
    (lambda (branch left)
      (if (eq (caar branch) lab)
	  (progn ;;(format t "setting cadr:~a~%" branch)
		 ((setter car) (car branch)
		  (- i-count (cdr branch)))
		 left)
	(cons branch left))))

  (defun instruction-size (x)
    ;; may be a prob with null instructions...
    (fold + (instruction-argwidth (i-info x)) 1))


  ;; end module
  )
