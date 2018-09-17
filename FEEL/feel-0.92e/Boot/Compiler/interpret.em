;; Eulisp Module
;; Author: pab
;; File: interpret.em
;; Date: Sun Jan  5 15:10:54 1992
;;
;; Project:
;; Description: 
;;  Simple pseudocode for bytecode interpreter
;;

(defmodule interpret
  (standard0
   list-fns
         
   )
  ()

  (defcondition Interpreter-Error ())

  ;; A bytevector function 
  (defstruct bv-function ()
    ((bytevector initarg bv reader bv-function-code)
     (env initarg env reader bv-function-env))
    constructor (make-bv-function bv env)
    predicate bv-fn-p)

 (defun push (x y) (cons y x))
 
 (defstruct state ()
   ((bytestream initarg bs
		reader state-bs)
    (stack initarg stack 
	   reader state-stack))
   constructor make-state)

 (defstruct address ()
   ((bv initarg bv 
	reader addr-bv)
    (loc initarg loc 
	 accessor addr-loc))
   constructor (make-address bv))

 (defun update-state (oldstate fn)
   (make-state 'bs (cdr (state-bs oldstate))
	       'stack (fn (state-stack oldstate))))

  (defun call-function (state)
    (let ((fn (car (state-stack state))))
      (cond ((bv-fn-p fn)
	     (make-state 'bs (bv-function-code fn)
			 'stack (if (not (null (bv-function-env fn)))
				    (push (cdr (state-stack state) (bv-function-env fn)))
				  (state-stack state))))
	    (t (format t "Unkown function type: ~a~%" 
		       Interpreter-Error 
		       'error-value fn)))))

 (defun do-return (state)
   (let ((stack (state-stack state)))
     (let ((rval (car stack))
	   (lab (cadr stack))
	   (addr (caddr stack)))
       (if (eq lab 'label)
	   (make-state 'bs (addr-bv addr)
		       'stack (push (cdddr stack) rval))
	 (error "Unbalanced return")))))
					     

 (defun interpret (state)
   (format t "Interpret: ~a ~a~%" (car (state-bs state)) (state-stack state))
   (let ((bytestream (state-bs state))
	 (update-state (lambda (x) (update-state state x))))
     (cond ((eq (caar bytestream) 'push-static)
	    (interpret (update-state
			(lambda (stack)
			  (push stack (cadar bytestream))))))
	   ((eq (caar bytestream) 'pop-value)
	    (interpret (update-state cdr)))
	   ((eq (caar bytestream) 'call-function)
	    (interpret (call-function bytestream stack)))
	   ((eq (caar bytestream) 'stack-ref)
	    (interpret (update-state (lambda (stack)
				       (push (nth (cdr stack)
						  (intval (car stack))))))))
	   ((eq (caar bytestream) 'env-ref)
	    (interpret (update-state (lambda (stack)
				       (push (cdddr stack)
					     (do-env-ref (caddr stack)
							 (cadr stack)
							 (car stack)))))))
	   ;; Hacks
	   ((eq (caar bytestream) 'push-ret-addr)
	    (interpret (update-state (lambda (stack)
				       (push (push stack (cdar bytestream)) 'label)))))
	    ((eq (caar bytestream) 'return)
	    (interpret (do-return state)))
	   ((eq (caar bytestream) 'jump)
	    (interpret (make-state 'bs (addr-bv (cdar bytestream))
				   'stack (state-stack state))))
	   ((eq (caar bytestream) 'jump-eq)
	    (let ((stack (state-stack state))
		  (bs (state-bs state)))
	      (interpret (make-state 'bs (if (eq (car stack) (cadr stack))
					     (addr-bv (cdar bytestream))
					   (cdr bytestream))
				     'stack (cddr stack)))))
	   ((eq (caar bytestream) 'exit)
	    (state-stack state))
	   ;; primitive functions
	   ((eq (caar bytestream) 'alloc-vect)
	    (interpret (cdr bytestream)
		       (update-state 
			(lambda (stack)
			  (push (cdr stack)
				(make-vector (car stack)))))))
	   ((eq (caar bytestream) 'slot-ref)
	    (interpret (update-state 
			(lambda (stack)
			  (push (caddr stack)
				(vector-ref (car stack) (cadr stack)))))))
	   ((eq (caar bytestream) 'cons)
	    (format t "Cons...~%")
	    (interpret (update-state
			(lambda (stack)
			  (push (cddr stack)
				(cons (cadr stack) (car stack)))))))
	   ((eq (caar bytestream) 'car)
	    (interpret (update-state 
			(lambda (stack)
			  (push (cdr stack)
				(car (car stack)))))))
	   ((eq (caar bytestream) 'cdr)
	    (interpret (update-state
			(lambda (stack)
			  (push (cdr stack)
				(cdr (car stack)))))))
	   ((eq (caar bytestream) 'mk-bv-func)
	    (interpret (update-state 
			(lambda (stack)
			  (push (cddr stack)
				(make-bv-function (car stack);; env
						  (cadr stack)))))))
	   (t (format t "Could not find: ~a~%" (caar bytestream)))
	   )))

 ;; Worlds simplest linker...

 (defun link-bv (bv)
   (link-aux bv nil nil))

 (defun link-aux (bv labs jumps)
   (format t "Link: ~a ~a ~a~%" bv labs jumps)
   (cond ((null bv)
	  nil)
	 ((eq (caar bv) 'label)
	  (let ((label (car bv)))
	    ((setter car) bv (car (cdr bv)))
	    ((setter cdr) bv (cdr (cdr bv)))
	    (link-aux bv
		      (cons (cons (cadr label) (make-address bv))
			    labs)
		      (resolve-lab (cadr label) jumps (make-address bv)))))
	 ((or (eq (caar bv) 'jump)
	      (eq (caar bv) 'push-ret-addr)
	      (eq (caar bv) 'jump-eq))
	  (let ((xx (find-label labs (cadar bv))))
	    (if (null xx)
		(link-aux (cdr bv) labs (cons (car bv) jumps))
	      (progn ((setter cdr) (car bv)  (cdr xx))
		     (link-aux (cdr bv) labs jumps)))))
	 (t (link-aux (cdr bv) labs jumps))))

 (defun resolve-lab (label jumps to)
   (if (null jumps) nil
     (let ((jump (car jumps)))
       (cond ((eq label (cadr jump))
	      ((setter cdr) jump to)
	      (resolve-lab label (cdr jumps) to))
	     (t (cons jump 
		      (resolve-lab label (cdr jumps) to)))))))
 
 (defun find-label (labs name)
   (assoc name labs eq))
 
 ;; starter
 (defun ib (bv)
   (interpret (make-state 'bs bv 'stack ())))

 ;; Test data
 (deflocal test '((push-static 1) (push-static 2) (cons) (exit)))
 
 (deflocal test-fun '((push-ret-addr lab1)
		      (push-static 2)
		      (push-static 1)
		      (jump lab2)
		      (label lab1)
		      (exit)
		      (label lab2)  ;; Define cons function
		      (cons)
		      (return)
		      ))
  ;; end module
 )
