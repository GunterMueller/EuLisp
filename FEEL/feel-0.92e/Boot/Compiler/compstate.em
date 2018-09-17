;; Eulisp Module
;; Author: pete broadbery
;; File: comp-state.em
;; Date: 3/sep/1991
;;
;; Project:
;; Description: 
;; structure describing the current state of the compiler
;;

(defmodule compstate
  (standard0
   list-fns
   scan-args
   
   byte-stream
   abs-syntx
   props

;;   stop
   )
  ()
  
  (defcondition Compiler-State-Error ())

  ;; stack model
  (defstruct stack ()
    ((state initarg initstate
	    initform ()
	    reader stack-state))
    constructor (make-stack)
    constructor (m-s-internal initstate)
    constructor (copy-stack stack))

  (defmethod generic-prin ((x stack) stream)
    (format stream "#S<~a>" (stack-state x)))

  (defstruct stack-val ()
    ((names accessor stack-val-names))
    constructor make-stack-val)
  
  (defmethod generic-prin ((x stack-val) stream)
    (prin "#<?>" stream))

  (export make-stack-val)

    (defun stack-top (stack)
    (if (null (stack-state stack))
	(error "Empty stack in Stack-top" Compiler-State-Error)
      (car (stack-state stack))))

  (defun stack-pop (stack count)
    (if (null (stack-state stack))
	(error "Empty stack in Stack-pop" Compiler-State-Error)
      (m-s-internal (nthcdr count (stack-state stack)))))
  
  (defun stack-slide (stack low keep)
    (if (< (stack-depth stack) low)
	(m-s-internal nil)
      (m-s-internal (append (copy-n keep (stack-state stack))
			    (nthcdr low (stack-state stack))))))

  (defun copy-n (n lst)
    (if (= n 0) ()
      (cons (car lst) (copy-n (- n 1) (cdr lst)))))

  (defun stack-push (stack val)
    (m-s-internal (cons val (stack-state stack))))

  (defun stack-swap (stack)
    (m-s-internal (cons (cadr (stack-state stack))
			(cons (car (stack-state stack))
			      (cddr (stack-state stack))))))

  (defun scanq-stack (stack val)
    (scan-aux (stack-state stack) val 0 eq))
  
  (defun scan-aux (lst val n fn)
    (cond ((null lst)
	   (error "Value not on stack" Compiler-State-Error 
		  'error-value val))
	  ((fn (car lst) val)
	   n)
	  (t (scan-aux (cdr lst) val (+ n 1) fn))))


  (defun stack-ref (stack n)
    ;; if the value is over the edge, don't worry.
    (let ((xx (st-ref-aux (stack-state stack) n)))
      (if xx (car xx) nil)))
  
  ((setter setter) stack-ref
   (lambda (stack n val)
     (let ((xx (st-ref-aux (stack-state stack) n)))
       (if (null xx) nil
	 ((setter car) xx val)))))
 
  (defun st-ref-aux (lst n)
    (if (null lst) nil
      (if (= n 0) 
	  lst
	(st-ref-aux (cdr lst)
		    (- n 1)))))
  
  (defun stack-depth (stack)
    (list-length (stack-state stack)))

  (export stack make-stack copy-stack stack-top stack-pop
	  stack-push scanq-stack stack-ref stack-depth
	  stack-slide stack-swap)

  ;; Objects stored in the state

  ;; note that it may be tricky to calculate the env size
  (defstruct env-object () 
    ((size initarg size accessor env-object-size)
     (content initarg content
	      accessor env-object-content)
     (prev initarg prev
	   accessor env-object-prev))
    constructor (make-env-object size content prev))
  
  (export make-env-object env-object-size env-object-content env-object-prev)

  (defstruct static ()
    ((id initarg id 
	 accessor static-id)
     (type initarg type 
	   accessor static-id-type)
     (content initarg content
	      accessor static-content))
    constructor (make-static id type content))

  
  (defconstant mk-new-static-id (mk-counter 0))

  ;; We cheat 
  (defun make-static-store ()
    (list (mk-counter 0) ()))
  
  (defun append-statics (s1 s2)
    (fold insert-static (cadr s1) s2))

  (defun add-static (val statics)
    (let ((xx (assq val (cadr statics))))
      (if (not (null xx))
	  (list xx statics)
	(let ((new (cons val ((car statics)))))
	  (list new (list (car statics) 
			  (cons new (cadr statics))))))))


  (defun insert-static (val statics)
    (list (car statics) (cons val statics)))


  (defun statics-2-list (x)
    (reverse (mapcar car (cadr x))))

  (defun static-val-id (x) 
    (cdr x))

  (export make-static-store add-static append-statics 
	  statics-2-list static-val-id)

  ;; Code lists
  
  (defun add-code-vectors (values state)
    (append values (state-code state)))

  (defun make-code-list ()
    nil)

  (export add-code-vectors make-code-list)
  
  ;; Posh name for assemble...
  ;; 
  
  (defun reify-code-stream (state)
    (convert (state-stream state) pair))

  (defun instructions2link-table (stream)
    (instruct-2-links-aux stream ()))
  
  (defun instruct-2-links-aux (stream external-refs )
    nil)
  
  (export reify-code-stream)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; The complete compiler state
  
  (defstruct compiler-state ()
    ((stack initform 'no-value
	    initarg state-stack
	    accessor state-stack)
     (output-stream initform 'no-value
		    initarg state-stream
		    accessor state-stream)
     (statics initarg state-statics 
	      initform 'no-value
	      accessor state-statics)
     (code initarg state-code
	   initform 'no-value
	   accessor state-code))
    constructor (make-compiler-state state-stream state-stack state-statics state-code)
    constructor (modify-compiler-state state . junk))


  (defmethod generic-prin ((state compiler-state) stream)
    (format stream "#<state: ~a>" (state-stack state)))

  (export state-stack state-stream state-statics state-code)

  ;; Real hack...
  ;;(defun make-compiler-state (from initargs)
  ;;(initialize-instance (copy from) initargs))
  
  (export make-compiler-state modify-compiler-state)

  (defmethod initialize-instance ((x compiler-state) args)
    (let ((new-state (call-next-method)))
      (if (not (eq (car args) 'state))
	  new-state
	;; if we want a copy...
	(let ((old-state (cadr args)))
	  (if (eq (state-stream new-state) 'no-value)
	      ((setter state-stream) new-state 
	       (state-stream old-state))
	    nil)
	  (if (eq (state-statics new-state) 'no-value)
	      ((setter state-statics) new-state
	       (state-statics old-state))
	    nil)
	  (if (eq (state-stack new-state) 'no-value)
	      ((setter state-stack) new-state
	       (state-stack old-state))
	    nil)
	  (if (eq (state-code new-state) 'no-value)
	      ((setter state-code) new-state
	       (state-code old-state))
	    nil)
	  new-state))))

  ;; And before I forget,
  ;; Generating code..
  
  (defun add-instruction (i stream)
    (write-stream stream i))
  
  (defun update-comp-state (state stacker streamer)
    (modify-compiler-state 
     state
     'state-stack (stacker (state-stack state))
     'state-stream (streamer (state-stream state))))
  
  (export add-instruction update-comp-state)
  ;; end module
  )
