;; Eulisp Module
;; Author: pab
;; File: generate.em
;; Date: Wed Jan  8 16:59:29 1992
;;
;; Project:
;; Description: 
;;    Functions that modify the state by adding instructions 
;;
(defmodule generate
  (standard0
   list-fns
         
   compstate
   instruct
   byte-stream
   
   ;;rshow
   )
  ()
  (expose compstate)
  (export make-label)

  ;; normal bytecodes
  (defun do-nop (obj state)
    (modify-compiler-state state
			   'state-stream (add-instruction (nop nil) (state-stream state))))
  
  (defun do-push-global (name state)
    (update-comp-state state
     (lambda (stack)
       (stack-push stack name))
     (lambda (stream)
       (add-instruction (push-global (list name))
			stream))))

  (defun do-global-set (name state)
    (update-comp-state 
     state
     (lambda (stack)
       (stack-pop stack 1))
     (lambda (stream)
       (add-instruction (set-global (list name)) stream))))

  (defun do-push-label (label state)
    (update-comp-state state
     (lambda (stack)
       (stack-push (stack-push stack label) label))
     (lambda (stream)
       (let ((i (push-label (list label))))
	 (add-lab-ref label i)
	 (add-instruction i stream)))))

  (defun do-apply-bvf (nargs state)
    (update-comp-state state
     (lambda (stack) stack)
     (lambda (stream) 
       (add-instruction (apply-bvf (list nargs))
			stream))))

  (defun do-apply-any (nargs state)
    (update-comp-state state
     (lambda (stack) stack)
     (lambda (stream)
       (add-instruction (apply-any (list nargs))
			stream))))

  (defun do-apply-cfn (nargs state)
    (update-comp-state state
     (lambda (stack) stack)
     (lambda (stream)
       (add-instruction (apply-cfn (list nargs))
			stream))))

  (defun do-apply-methods (nargs state)
    (update-comp-state state
     (lambda (stack) stack)
     (lambda (stream)
       (add-instruction (apply-methods (list nargs))
			stream))))

  (defun do-return (state)
    (update-comp-state 
     state
     (lambda (stack) 
       ;; right?
       stack)
     (lambda (stream)
       (add-instruction (return nil) stream))))

  (defun do-pop (count state)
    (update-comp-state state
     (lambda (stack)
       (stack-pop stack count))
     (lambda (stream)
       (add-instruction (drop (list count)) stream))))
    
  (defun do-slide (low keep state)
    (if (= low keep)
	state
      (update-comp-state 
       state
       (lambda (stack)
	 (stack-slide stack low keep))
       (lambda (stream)
	 (add-instruction (i-slide-stack (list low keep)) stream)))))

  (defun do-stack-ref (count state)
    (update-comp-state state
     (lambda (stack)
       (stack-push stack
		   (stack-ref (state-stack state)
			      count)))
     (lambda (stream)
       (add-instruction (nth-ref (list count)) stream))))

  (defun do-set-stack-ref (count state)
    (update-comp-state 
     state
     (lambda (stack) (stack-pop stack 1))
     (lambda (stream)
       (add-instruction (set-nth (list count)) stream))))

  (defun do-swap (state)
    (update-comp-state
     state
     stack-swap
     (lambda (stream)
       (add-instruction (swap nil) stream))))

  ;; Primitive functions

  (defun do-cons (state)
    (update-comp-state state
     (lambda (stack)
       (stack-push (stack-pop stack 2) (make-stack-val)))
     (lambda (stream)
       (add-instruction (i-cons nil) stream))))

  (defun do-alloc (state)
    (update-comp-state state
     (lambda (stack)
       (stack-pop stack 2))
     (lambda (stream)
       (add-instruction (alloc-thing nil) stream))))

  (defun do-car (state)
    (update-comp-state state
     (lambda (stack)
       (stack-push (stack-pop stack 1) (make-stack-val)))
     (lambda (stream)
       (add-instruction (slot-ref (list 0)) stream))))


  (defun do-cdr (state)
    (update-comp-state state
     (lambda (stack)
       (stack-push (stack-pop stack 1) (make-stack-val)))
     (lambda (stream)
       (add-instruction (slot-ref (list 1))
			stream))))
  
  

  (defun do-slot-ref (n state)
    (update-comp-state state
     (lambda (stack)
       (stack-push (stack-pop stack 1) (make-stack-val)))
     (lambda (stream)
       (add-instruction (slot-ref (list n))
			stream))))
  
  ;; leaves its arg on the stack
  (defun do-setter-slot-ref (n state)
    (update-comp-state state
     (lambda (stack)
       (stack-push (stack-pop stack 2) (stack-top stack)))
     (lambda (stream)
       (add-instruction (set-slot (list n))
			stream))))

  (defun do-setter-cdr (state)
    (do-setter-slot-ref 1 state))

  ;; Special constants
  (defun do-push-special (state const)
    nil)

  (defun do-dead-code (state)
    (update-comp-state state
     (lambda (stack)
       stack)
     (lambda (stream)
       (add-instruction (dead-code ()) stream))))
    
  ;; Random things
  (defun special-id (x)
    (if (eq x ()) 0
      1))

  (defun do-push-static (static state)
    (if (or (eq static nil)
	    (eq static t))
	(update-comp-state state
			   (lambda (stack)
			     (stack-push (state-stack state) static))
			   (lambda (stream) 
			     (add-instruction (push-special (list (special-id static))) stream)))
      (let ((new-statics (add-static static
				     (state-statics state))))
	(modify-compiler-state
	 state
	 'state-stack 
	 (stack-push (state-stack state) static)
	 'state-stream
	 (add-instruction (push-static (list (static-val-id (car new-statics))))
			  (state-stream state))
	 'state-statics (cadr new-statics)))))
       
  (defun do-push-fixnum (n state)
    (update-comp-state state
		       (lambda (stack) 
			 (stack-push stack n))
		       (lambda (stream)
			 (add-instruction (push-fixnum (list n)) stream))))

		       ;;  (defun do-load-tmp (state)
;;    (modify-compiler-state 
;;     state 'state-stack 
;;     (stack-pop (state-stack state)
;;		count)))
;;    
;;  (defun do-unload-tmp (state)
;;    (modify-compiler-state 
;;     state
;;     'state-stack (stack-push (state-stack state)
;;			      (make-stack-val))))
    

  (defun do-vector-ref (static state)
    nil)
  
  (defun do-add-comment (comment state)
    nil)

  ;; allocating closures

  (defun do-allocate-closure (argcode ext state)
    (update-comp-state 
     state
     (lambda (stack)
       (stack-push (stack-pop stack (if ext 4 3)) ;; pop label(2)+env+info?
		   (make-stack-val)))
     (lambda (stream)
       (add-instruction (if ext
			    (alloc-extended-closure (list (argcode2other argcode)))
			  (alloc-closure (list (argcode2other argcode))))
			stream))))

  (defun argcode2other (x)
    (if (null (car x)) 
	(cdr x) 
      (- (cdr x))))

	      
  ;; env functions
  (defun do-alloc-env (env state)
    (let ((size (env-object-size env)))
      (format t "{Making Env: ~a " size)
      (prog1 (update-comp-state 
	      state
	      (lambda (stack)
		(let ((prev (stack-top stack)))
		  (stack-push (stack-pop stack 1) env)))
	      (lambda (stream)
		(print stream)
		(add-instruction (make-env (list size)) stream)))
	nil)))

  (defun do-env-ref (x y state)
    (update-comp-state state
     (lambda (stack)
       (stack-push (stack-pop stack 1)
		   (compile-env-ref x y (stack-top stack))))
     (lambda (stream)
       (add-instruction (env-ref (list x y))
			stream))))
  
  (defun do-setter-env-ref (x y state)
    ;; stack is ... env value
    (update-comp-state state
     (lambda (stack)
       (compile-env-ref-set x y (stack-ref stack 1) (stack-top stack))
       (stack-pop stack 1))
     (lambda (stream)
       (add-instruction (set-env (list x y))
			stream))))


  (defun do-pop-env (n state)
    (update-comp-state
     state
     (lambda (stack)
       (stack-push (stack-pop stack 1)
		   (compile-nth-env n (stack-top stack))))
     (lambda (stream)
       (add-instruction (pop-env (list n)) stream))))

  (defun compile-nth-env (n env)
    (if (zerop (env-object-size env))
	(compile-nth-env n (env-object-prev env))
      (if (= n 0)  env
	(compile-nth-env (- n 1) 
			 (env-object-prev env)))))

  (defun compile-env-ref-set (x y env val)
    ((setter vector-ref) (env-object-content (compile-nth-env x env)) y val))

  (defun compile-env-ref (x y env)
    (vector-ref (env-object-content (compile-nth-env x env)) y))

  ;; labels and such

  (defun do-add-label (lab state)
    (modify-compiler-state 
     state
     'state-stream (add-instruction (i-label (list lab) )
				    (state-stream state))))

  (defun do-branch (lab state)  
    (update-comp-state state
     (lambda (stack)
       stack)
     (lambda (stream)
       (let ((i (branch (list lab))))
	 (add-lab-ref lab i)
	 (add-instruction i
			  (state-stream state))))))


  (defun do-branch-on-nil (lab state)
    (update-comp-state state
     (lambda (stack)
       (stack-pop stack 1))
     (lambda (stream)
       (let ((i (branch-nil (list lab))))
	 (add-lab-ref lab i)
	 (add-instruction i
			  (state-stream state))))))
  
  (defun do-inline-code (lst nargs state)
    (let ((xx (state-stream state))
	  (yy (state-stack state)))
      (modify-compiler-state 
       state
       'state-stream (fold (lambda (desc stream)
			     (add-instruction (make-instruction desc) stream))
			   (if (and (consp lst)
				    (eq (car lst) 'returning))
			       (cdr lst)
			     lst)
			   xx)
       'state-stack
       (stack-push (stack-pop yy nargs) (make-stack-val)))))

  (defun make-instruction (desc)
    ((find-instruction (car desc)) (cdr desc)))

  ;; utility functions...
  (defun do-code-sequence (lst state)
    (fold (lambda (fn state)
	    (fn state))
	  lst 
	  state))

  ;; Operations of the code-stream
  ;; Could use tconc, but what the hell
  (defun make-new-code-stream ()
    (make-simple-stream))
  
  (export make-new-code-stream)
  

  (export do-code-sequence)
  
  (export do-allocate-closure)
  (export do-push-global)
  (export do-push-static)
  (export do-push-fixnum)
  (export do-global-set)
  (export do-nop)
  (export do-pop)
  (export do-apply-any)
  (export do-apply-cfn)
  (export do-apply-bvf)
  (export do-apply-methods)
  (export do-stack-ref)
  (export do-set-stack-ref)
  (export do-alloc)
  (export do-cons)
  (export do-car)
  (export do-cdr)
  (export do-slot-ref)
  (export do-setter-slot-ref)
  (export do-setter-cdr)
  (export do-vector-ref)
  (export do-add-comment)
  (export do-alloc-env)
  (export do-env-ref)
  (export do-setter-env-ref)
  (export do-pop-env)
  (export do-add-label)
  (export do-branch)
  (export do-branch-on-nil)
  (export do-code-sequence)
  (export do-push-label)
  (export do-return)
  (export do-slide)
  (export do-swap)
  (export do-inline-code)
  (export do-dead-code)

  (export  bc-macro-type)
  ;; end module
  )
