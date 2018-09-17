;; Eulisp Module
;; Author: pete broadbery
;; File: gen-code.em
;; Date: 30/jul/1991
;;
;; Project:
;; Description:
;;  code generator pass of compiler.
;;  assume names pass is complete.
;;

(defmodule gen-code
  ((except (fold) standard)
   list-fns

   syntx-utl
   props;; Should try to avoid this --- maybe an analysis module?
   syntx-env;; Should try to avoid this --- maybe an analysis module?
   generate
   pass
   ;; use Should try to avoid this --- maybe an analysis module?

   rshow
   stop
   gen-docs
   )
  ()

  (defcondition Compile-Time-Error ()
    msg "" values ())
  (export Compile-Time-Error)

  (export generate-code)

  (deflocal *last* ())
  (deflocal lastobj ())
  (defun code-gen (thing state)
    (let ((prev *last*))
      (setq *last* state)
      (setq lastobj thing)
      ;;(format t "(Generating code for: ~a~%state: ~a" thing state)
      ;;(print-props thing)
      (let ((xx (generic-code-gen thing state)))
	(setq *last* prev)
	;;(format t ")~%" xx)
	xx)))

  (defun laststate ()
    *last*)
  (export laststate)

  (defun generate-code (module)
    (initialize-docs (module-name module))
    (let ((final-state
	   (code-gen module
		     (make-initial-compiler-state module))))
      (complete-docs)
      (modify-compiler-state
       final-state
       'state-code
       (add-code-vectors (list (reify-code-stream
				(complete-installation module final-state)))
			 final-state)
       ;; blast the old stream
       'state-stream (make-new-code-stream))))

  (defconstant generic-code-gen (make-compiler-pass 'code-gen))

  (defmethod generic-code-gen ((seq sequence) state)
    (let ((newstate (fold (lambda (obj state)
			    (do-pop 1 (code-gen obj state)))
			  (sequence-body seq)
			  state)))
      (code-gen (sequence-end seq) newstate)))


  (defun make-initial-compiler-state (module)
    (make-compiler-state (make-new-code-stream)
			 (make-stack)
			 (make-static-store)
			 (make-code-list)))

  (defun complete-installation (mod state)
    state)

  ;; Modules. Unfortunately they have a closure...
  ;; Makes them easy to init though
  (defmethod generic-code-gen ((mod module-block) state)
    (let ((env (make-lambda-environment mod)))
      ;; Idea is to make a nice, clean easy to call
      ;; function...
      ((setter real-lambda-env) mod env)
      (do-code-sequence
       (list (lambda (state)
	       (modify-compiler-state
		state
		'state-stack (stack-push (state-stack state) nil)))
	     (lambda (state)
	       (do-alloc-env env state))
	     (lambda (state)
	       (code-gen (module-body mod) state))
	     ;; the return from progn, and throw the env away
	     (lambda (state)
	       (do-pop 2 state))
	     (lambda (state)
	       (do-push-static t state)) ;; hack
	       (lambda (state)
		 (do-return state)))
       state)))

  (defmethod generic-code-gen ((cd condition-term) state)
    (let ((lab1 (make-label state))
	  (lab2 (make-label state)))
      (let ((stack (state-stack state)))
	(cond
	 ;; can we get away with losing a 'not'
	 ;;((negation-p (cond-test cd))
	 ;;(gen-code (wind-negation cd) state))
	 ;; test for spectacularly dumb conditions...
	 ((literal-p (cond-test cd))
	  (if (null (literal-content (cond-test cd)))
	      (code-gen (cond-f-part cd) state)
	    (code-gen (cond-t-part cd) state)))
	 ((term-tail-call cd)
	  (do-code-sequence
	   (list (lambda (state) (code-gen (cond-test cd) state))
		 (lambda (state) (do-branch-on-nil lab1 state))
		 (lambda (state) (code-gen (cond-t-part cd) state))
		 (lambda (state) (modify-compiler-state
				  state
				  'state-stack stack))
		 (lambda (state) (do-add-label lab1 state))
		 (lambda (state) (code-gen (cond-f-part cd) state)))
	   state))
	 (t (do-code-sequence
	     (list (lambda (state) (code-gen (cond-test cd) state))
		   (lambda (state) (do-branch-on-nil lab1 state))
		   (lambda (state) (code-gen (cond-t-part cd) state))
		   (lambda (state) (do-branch lab2 state))
		   ;; Should unscrew state here...
		   ;; Hopefully only stack in wrong
		   (lambda (state)
		     (modify-compiler-state
		      state
		      'state-stack stack))
		   (lambda (state)
		     (do-add-label lab1 state))
		   (lambda (state)
		     (code-gen (cond-f-part cd) state))
		   (lambda (state)
		     (do-add-label lab2 state)))
	     state))))))


  (defmethod generic-code-gen ((id ident-term) state)
    ;; maybe ought to take a quick look at the stack here...
    (let ((state (value-ref (ident-decl id) (ident-block id) id state)))
      (if (term-tail-call id)
	  (add-tidy-code (enclosing-lambda id) state)
	state)))

  (defgeneric value-ref (id loc orig state))

  (defmethod value-ref ((x <object>) loc id state)
    (do-push-global 'some-value state))

  (defmethod value-ref ((x module-definition) loc id state)
    (local-module-ref x state))

  (defmethod value-ref ((x definition) loc id state)
    (if (binding-closed x)
	;; Generate env. ref
	(closed-value-ref x loc id state)
      (open-value-ref x loc state)))

  (defmethod value-ref ((x imported-definition) loc id state)
    (do-push-global (external-name x)
		    state))

  (defmethod value-ref ((x lambda-id) loc id state)
    (if (binding-closed x)
	;; Generate env. ref
	(closed-value-ref x loc id state)
      (open-value-ref x loc state)))

  ;; Ahrrhgg. Assignments
  (defmethod generic-code-gen ((x assignment-term) state)
    (let ((id (assign-var x)))
      (let* ((state (code-gen (assign-body x) state))
	     (state2 (set-value-ref (ident-decl id) (ident-block id) id state)))
	(if (term-tail-call x)
	    (add-tidy-code (enclosing-lambda x) state2)
	  state2))))


  (defgeneric set-value-ref (id loc orig state))

  (defmethod set-value-ref ((x <object>) loc id state)
    (do-global-set 'some-value state))

  (defmethod set-value-ref ((x module-definition) loc id state)
    (set-local-module-ref x state))

  (defmethod set-value-ref ((x definition) loc id state)
    (if (binding-closed x)
	;; Generate env. ref
	(set-closed-value-ref x loc id state)
      (set-open-value-ref x loc state)))

  (defmethod set-value-ref ((x lambda-id) loc id state)
    (prog1 (if (binding-closed x)
	       ;; Generate env. ref
	       (set-closed-value-ref x loc id state)
	     (set-open-value-ref x loc state))
      nil))

  ;; here?
  (defun find-env-depth (env target)
    (if (null env)
	(format t "Could not env ~a in ~a~%" env target)
      (cond ((eq env target)
	     0)
	    ((= (env-object-size env) 0)
	     (find-env-depth (env-object-prev env) target))
	    (t
	     (+ (find-env-depth (env-object-prev env) target) 1)))))

  (defun open-value-ref (x loc state)
    (do-stack-ref (scanq-stack (state-stack state) x)
		  state))

  (defun local-module-ref (x state)
    (let ((xx (do-push-global (external-name x) state)))
      xx))


  (defun closed-value-ref (binding loc id state)
    (let ((env (stacked-lambda-env (enclosing-lambda id)))
	  (posn (binding-posn binding))
	  (def-env (lambda-env (enclosing-lambda binding))))
      ;;(stop (list env posn def-env))
      (let ((depth (find-env-depth env def-env)))
	(do-code-sequence
	 (list (lambda (state) (do-stack-ref (scanq-stack (state-stack state) env) state))
	       (lambda (state) (do-env-ref depth posn state)))
	 state))))

  (defun set-open-value-ref (x loc state)
    (let ((state (do-stack-ref 0 state)))
      (do-set-stack-ref (scanq-stack (state-stack state) x)
			state)))

  (defun set-local-module-ref (x state)
    (do-code-sequence
     (list (lambda (state)
	     (do-stack-ref 0 state))
	   (lambda (state)
	     (do-global-set (external-name x) state)))
     state))

  (defun set-closed-value-ref (binding loc id state)
    (let ((env (stacked-lambda-env (enclosing-lambda id)))
	  (posn (binding-posn binding))
	  (def-env (lambda-env (enclosing-lambda binding))))
      ;;(format t "set-closed-ref: ~a ~a ~a ~a~%" env def-env posn)
      ;;(stop (list env posn def-env))
      (let ((depth (find-env-depth env def-env)))
	(do-code-sequence
	 (list (lambda (state) (do-stack-ref (scanq-stack (state-stack state) env) state))
	       (lambda (state)
		 (do-stack-ref 1 state))
	       (lambda (state)
		 (do-setter-env-ref depth posn state))
	       (lambda (state)
		 (do-pop 1 state)))
	 state))))

  ;;
  ;;; this-context: grab this function from the stack
  ;;

  (defmethod generic-code-gen ((ob special-term) state)
    (cond ((eq (special-term-name ob) 'call-next-method-internal)
	   (do-code-sequence
	    (list (lambda (state)
		    (do-stack-ref (+ (stack-depth (state-stack state)) 0) state))
		  ;; check here not needed --- apply methods ought to check though
		  (lambda (state)
		    (do-cdr state)))
	    state))
	  ((eq (special-term-name ob) 'inline-fn)
	   ;;(format t "In special ~a~%" ob)
	   (make-inline-lambda (special-term-data ob) state))
	  ((eq (special-term-name ob) 'add-property)
	   (let ((val (ident-decl (car (special-term-objects ob)))))
	     (add-defn-prop val (car (special-term-data ob)) (cadr (special-term-data ob))))
	   (do-push-static nil state))
	  ((eq (special-term-name ob) 'add-callback)
	   (let ((val (ident-decl (car (special-term-objects ob)))))
	     (add-defn-prop val 'callbacks (list (car (special-term-data ob)))))
	   (do-push-static nil state))
	  (t (format t "Unknown special")
	     state)))

  (defun make-inline-lambda (args state)
    (let ((ns (modify-compiler-state state
				     'state-stack (stack-push
						   (stack-push
						    (stack-push
						     (stack-push 
						      (stack-push
						       (make-stack) 'a) 'b) 'c) 'd) 'e)
				     'state-stream (make-new-code-stream)
				     'state-statics (state-statics state)
				     'state-code (make-code-list)))
	  (init-label (make-label state)))
      (let ((inlined-state (let* ((state (do-add-label init-label ns))
				  (state (do-pop 1 state))
				  (state (do-inline-code (cdr args) (abs (car args)) state))
				  (state (do-return state)))
			     state)))
	(let ((new-state (modify-compiler-state
                         state
                         'state-code
                         (add-code-vectors (cons (reify-code-stream inlined-state)
						 nil)
                                           state))))
	  (let* ((state (do-push-label init-label new-state))
		 (state (do-push-static nil state))
		 (state (do-allocate-closure (if (< (car args) 0)
						 (cons t (- (car args)))
					       (cons nil (car args)))
					     nil ;; nobody documents these things -- right..
					     state)))
	    state)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Function Applications
  ;;
  ;;

  (deflocal this-applic)

  (defmethod generic-code-gen ((applic applic-term) state)
    (let ((obj (find-fn (applic-fun applic)))
	  (tail-flag (term-tail-call applic)))
      (setq this-applic (cons obj applic))
      (check-arguments applic obj)
      ;; do any strange side effect type stuff specified by obj
      (do-callbacks (function-fn obj) applic)
      ((find-app-fn (function-type obj)) obj applic tail-flag state)))


  (defun check-arguments (applic obj)
    (let ((nargs (list-length (applic-args applic)))
	  (reqd-nargs (function-nargs obj)))
      (if (or (eq (function-type obj) 'unknown)
	      (= (cdr reqd-nargs) 9999)
	      (= nargs (cdr reqd-nargs))
	      (and (car reqd-nargs)
		   (>= (+ nargs 1) (cdr reqd-nargs))))
	  t
	(error "Function called with wrong number of args"
		Compile-Time-Error
		'values (list reqd-nargs applic)
		'msg "Function called with wrong number of args (should be ~a): ~%~a~%"))))

  (defconstant find-callback (mk-finder))

  (defgeneric do-callbacks (obj applic)
    methods ((((x abs-definition) applic)
	      (labels ((do-callback (l)
				    (cond ((null l) nil)
					  (t ((find-callback (car l)) applic)
					     (do-callback (cdr l))))))
		      (do-callback (defn-prop-ref x 'callbacks))))
	     (((x syntax-obj) y)
	      nil)))

  ((setter find-callback) 'setter-setter-function
   (lambda (applic)
     ;;(format t "set-setter: ~a~%" applic)
     (if (not (ident-p (cadr (applic-args applic))))
	 nil
       ((setter decl-setter)
	(ident-decl (car (applic-args applic)))
	(ident-decl (cadr (applic-args applic)))))))

  (defconstant find-app-fn (mk-finder))

  ;; local function. Called with branch
  (defun apply-local-function (obj applic tail state)
    (let ((branch-lab (read-init-label (defn-body (function-fn obj))) state))
      (if tail
	  (do-branch branch-lab
		     (push-local-env (defn-body (function-fn obj))
				     (tail-prepare-args obj applic state)))
	(let ((ret-lab (make-label nil)))
	  (do-code-sequence
	   (list (lambda (state)
		  (std-prepare-args obj applic state ret-lab))
		 (lambda (state)
		   (push-local-env (defn-body (function-fn obj)) state))
		 (lambda (state)
		   (do-branch branch-lab state))
		 (lambda (state)
		   (do-add-label ret-lab state))
		 (lambda (state)
		   (correct-stack applic obj state)))
	   state)))))

  (defun apply-nonlocal-function (obj applic tail state)
    (if tail
	(do-apply-bvf (mk-calltype applic obj)
		      (tail-prepare-args obj applic state))
      (let ((ret-lab (make-label nil)))
	(do-code-sequence
	 (list (lambda (state)
		 (std-prepare-args obj applic state ret-lab))
	       (lambda (state)
		 (do-apply-bvf (mk-calltype applic obj) state))
	       (lambda (state)
		 (do-add-label ret-lab state))
	       (lambda (state)
		 (correct-stack applic obj state)))
	 state))))

  (defun apply-unknown-function (obj applic tail state)
    (if tail
	(do-apply-any (mk-calltype applic obj)
		      (tail-prepare-args obj applic state))
      (let ((ret-lab (make-label nil)))
	(do-code-sequence
	 (list (lambda (state)
		 (std-prepare-args obj applic state ret-lab))
	       (lambda (state)
		 (do-apply-any (mk-calltype applic obj) state))
	       (lambda (state)
		 (do-add-label ret-lab state))
	       (lambda (state)
		 (correct-stack applic obj state)))
	 state))))

  ;;(defun apply-c-function (obj applic tail state)
  ;; (if tail
  ;;    (do-apply-cfn (mk-calltype applic obj)
  ;;    	      (tail-prepare-args obj applic state))
  ;;  (let ((ret-lab (make-label nil)))
  ;;    (do-code-sequence
  ;;     (list (lambda (state)
  ;;    	 (std-prepare-args obj applic state ret-lab))
  ;;           (lambda (state)
  ;;    	 (do-apply-cfn (mk-calltype applic obj) state))
  ;;           (lambda (state)
  ;;    	 (do-add-label ret-lab state))
  ;;           (lambda (state)
  ;;    	 (correct-stack applic obj state)))
  ;;     state))))
  
  (defun apply-c-function (obj applic tail state)
    ;;(format t "[In C-fn: ~a~%~a~%" state applic)
    (if tail 
	(let* ((state (code-gen (applic-fun applic) state))
	       (state (push-args (applic-args applic) obj state))
	       (state (do-slide (stack-depth (state-stack state))
				(+ (actual-args applic obj) 1)
				state))
	       (state (do-apply-cfn (mk-calltype applic obj) state))
	       ;; should tidy the stack..
	       (state (do-return state)))
	  ;;(format t "Out: ~a]~%" state)
	  state)
      (let* ((state (code-gen (applic-fun applic) state))
	     (state (push-args (applic-args applic) obj state))
	     (state (do-apply-cfn (mk-calltype applic obj) state))
	     (state (correct-stack-cfn applic obj state)))
	;;(format t "out: ~a~%]" state )
	state)))

  ;; self call or labels call.

  (defun apply-lexical-function (obj applic tail state)
    (let ((branch-lab (read-init-label (defn-body (function-fn obj))) state))
      (if tail
	  (do-code-sequence
	   ;; Should blast the function call position...
	   (list (lambda (state)
		   (push-args (applic-args applic) obj state))
		 (lambda (state)
		   (grab-environment applic
				     (defn-body (function-fn obj))
				     state))
		 (lambda (state)
		   (do-slide (stack-depth (state-stack state))
			     (+ (actual-args applic obj) 1)
			     state))
		 (lambda (state)
		   (do-branch branch-lab state)))
	   state)
	(let ((ret-lab (make-label nil)))
	  (let* ((state (do-push-label ret-lab state))
		 (state (do-push-static nil state)) ;; XXX should be an env description{or something}
		 (state (push-args (applic-args applic) obj state))
		 (state (grab-environment applic
					  (defn-body (function-fn obj))
					  state))
		 (state (do-branch branch-lab state))
		 (state (do-add-label ret-lab state))
		 (state (correct-stack applic obj state)))
	    state)))))

  (defun apply-inline-function (obj applic tail state)
    (if tail
	(cond ((and (consp (function-prop obj 'code))
		    (eq (car (function-prop obj 'code)) 'returning))
	       (let* ((state (push-args (applic-args applic) obj state))
		      (state (do-slide (stack-depth (state-stack state))
				       (actual-args applic obj)
				       state))
		      (state (do-inline-code (function-prop obj 'code)
					     (actual-args applic obj)
					     state)))
		 state))
	      (t (do-code-sequence
		  (list (lambda (state)
			  (push-args (applic-args applic) obj state))
			(lambda (state)
			  (do-slide (stack-depth (state-stack state))
				    (actual-args applic obj)
				    state))
			(lambda (state)
			  (do-inline-code (function-prop obj 'code)
					  (actual-args applic obj)
					  state))
			(lambda (state)
			  (do-return state)))
		  state)))
      (let* ((state (push-args (applic-args applic) obj state))
	     (state (do-inline-code (function-prop obj 'code)
				    (actual-args applic obj)
				    state)))
	state)))

  (defun apply-special-function (obj applic tail state)
    ;;(format t "In special: [~a] ~a~%" obj applic)
    (cond ((eq (special-term-name (function-fn obj)) 'call-next-method-internal)
	   (if tail
	       (let* ((state  (tail-prepare-args obj applic state))
		      (state (do-apply-methods (mk-calltype applic obj)
					       state)))
		 state)
	     (let* ((ret-lab (make-label nil))
		    (state (std-prepare-args obj applic state ret-lab))
		    (state (do-apply-methods (mk-calltype applic obj) state))
		    (state (do-add-label ret-lab state))
		    (state (correct-stack applic obj state)))
	       state)))
	  ((eq (special-term-name (function-fn obj)) 'inline-fn)
	   (apply-inline-function obj applic tail state))
	  (t (error "Unknown special" Compile-Time-Error 'msg "Bad special: ~a~%"
		    'values (list obj)))))



  (progn
    ((setter find-app-fn) 'lexical apply-lexical-function)
    ((setter find-app-fn) 'local apply-local-function)
    ((setter find-app-fn) 'bytefunction apply-nonlocal-function)
    ((setter find-app-fn) 'inline apply-inline-function)
    ((setter find-app-fn) 'unknown apply-unknown-function)
    ((setter find-app-fn) 'function apply-c-function)
    ((setter find-app-fn) 'special apply-special-function)
    )
  ;; move the arguments onto the stack, together with a
  ;; label in the right (TM) place, and move the function
  ;; to the top
  (defun std-prepare-args (obj applic state ret-lab)
    (do-code-sequence
     (list (lambda (state)
	     (do-push-label ret-lab state))
	   (lambda (state)
	     (code-gen (applic-fun applic) state))
	   (lambda (state)
	     (push-args (applic-args applic) obj state))
	   (lambda (state)
	     (do-stack-ref (actual-args applic obj) state)))
     state))


  ;; Push the arguments on to the stack, and slide down to a position where
  ;; the tail call can be done

  (defun tail-prepare-args (obj applic state)
    (do-code-sequence
     (list (lambda (state)
	     (code-gen (applic-fun applic) state))
	   (lambda (state)
	     (push-args (applic-args applic) obj state))
	   (lambda (state)
	     (do-stack-ref (actual-args applic obj) state))
	   (lambda (state)
	     (do-slide (stack-depth (state-stack state))
		       (+ (actual-args applic obj) 1)
		       state))
	   (lambda (state)
	     (blast-current-fn state)))
     state))

  ;; this way 'cos I want to see what the code looks like...
  (defun blast-current-fn (state)
    (let* ((state (do-stack-ref 0 state)))
      (do-set-stack-ref (stack-depth (state-stack state)) state)))

  (defun correct-stack (applic obj state)
	(modify-compiler-state
	 state
	 'state-stack
	 (let ((stack (state-stack state)))
	   (stack-push (stack-pop stack
				  (+ (actual-args applic obj) 4))
		       (make-stack-val)))))

  (defun correct-stack-cfn (applic obj state)
	(modify-compiler-state
	 state
	 'state-stack
	 (let ((stack (state-stack state)))
	   (stack-push (stack-pop stack
				  (+ (actual-args applic obj) 1))
		       (make-stack-val)))))

  (defun push-args (args obj state)
    ;; should do nary-check here
    (if (car (function-nargs obj))
	(push-nary-args (cdr (function-nargs obj)) args state)
      (fold (lambda (arg state)
	      (let ((xx (code-gen arg state)))
		xx))
	    args
	    state)))

  (defun push-nary-args (nargs args state)
    ;; keep pushing till we get to optionals
    (if (= nargs 1)
	(push-optional-args args state)
      (push-nary-args (- nargs 1) (cdr args)
		      (code-gen (car args) state))))

  (defun push-optional-args (args state)
    (if (null args)
	(do-push-static nil state)
      (do-code-sequence
       (list (lambda (state)
	       (code-gen (car args) state))
	     (lambda (state)
	       (do-push-static nil state))
	     (lambda (state)
	       (do-cons state))
	     (lambda (state)
	       (do-stack-ref 0 state))
	     (lambda (state)
	       (push-remaining-args (cdr args) state)))
       state)))

  (defun push-remaining-args (args state)
    (if (null args)
	(do-pop 1 state)
      (push-remaining-args (cdr args)
			   (do-code-sequence
			    (list (lambda (state)
				    (code-gen (car args) state))
				  (lambda (state)
				    (do-push-static nil state))
				  (lambda (state)
				    (do-cons state))
				  (lambda (state)
				    (do-setter-cdr state)))
			    state))))

  ;; actual number of args pushed:
  (defun actual-args (applic obj)
    (if (car (function-nargs obj))
	(cdr (function-nargs obj))
      (list-length (applic-args applic))))

  (defun mk-calltype (applic obj)
    (if (car (function-nargs obj))
	(- (cdr (function-nargs obj)))
      (list-length (applic-args applic))))

  (defun grab-environment (applic fn state)
    (if (not (lambda-closed-p fn))
	(do-push-static nil state)
      (let ((env (stacked-lambda-env (enclosing-lambda fn)))
	    (enc-lambda (enclosing-lambda applic)))
	;;(format t "(Grab env: ~a ~a ~a~%" applic env state)
	(let ((local-env (stacked-lambda-env enc-lambda)))
	  (if (= (env-object-size env) 0)
	      (do-push-static nil state)
	    (let ((state (fetch-environment enc-lambda state)))
	      (if (eq env local-env)
		  state
		(do-pop-env (find-env-depth local-env env) state))))))))


  (defun push-local-env (fn state)
    (let ((env (stacked-lambda-env fn)))
      (if (= (env-object-size env) 0)
	  (let* ((state (do-pop 1 state))
		 (state (do-push-static nil state)))
	    state)
	(do-slot-ref 0 state))))

  ;; Calling sequence...
  ;; args are in the order
  ;; [fn]/[mds] [return address] a0 a1 a2 a3 <env> [fn]

  ;; code gen for lambda should assume args on stack,
  ;; and that, if nec. an env will be placed on the stack
  ;; by its own calling routine.

  ;; When a fn completes, its stack should be contain it's return value
  ;; Compiling tail calls:
  ;; should be just a jump to the relavant routine,
  ;; preparing the arguments as low on the stack as possible

  ;; if inline,
  ;; args should (hem hem) be OK
  ;; just insert the relavant code
  ;; o/w create new code-vector and compile into that.
  ;;   add code to initialise the function
  ;; exit: restore the stack to the initial state

  (defmethod generic-code-gen ((lam lambda-term) state)
    ;;(format t "~%(In Lambda: ")
    ;;(format t "State: ~a~%" state)
    (let ((new-state (code-gen (lambda-body lam)
			       ;; does env-construction, etc
			       (add-entry-code lam
					       (new-code-state lam state)))))
      (let ((next-state (modify-compiler-state
			 state
			 'state-statics (state-statics new-state)
			 'state-code
			 (add-code-vectors (cons (reify-code-stream new-state)
						 (state-code new-state))
					   state))))
	;; bung lambda onto stack
	(let ((state (do-allocate-function lam
					   next-state
					   )))
	  (let ((aa (if (term-tail-call lam)
			(add-tidy-code (enclosing-lambda lam) state)
		      state)))
	    ;;(format t "Final state: ~a)~%" aa)
	    aa)))))


  (defun new-code-state (lam state)
    ;;(format t "(New state: ")
    (let ((stack (fold (lambda (arg stack)
			 (stack-push stack arg))
		       (lambda-ids lam)
		       (make-stack)))
	  (out-stream (make-new-code-stream)))
      (modify-compiler-state state
			     ;; should only push env if we have one
			     'state-stack (stack-push stack (stacked-lambda-env (enclosing-lambda lam)))
			     'state-stream out-stream
			     'state-statics (state-statics state)
			     'state-code (make-code-list))))

  ;; add the stuff the program will have to do on entry
  (defun add-entry-code (lam state)
    (let* ((env (lambda-env lam))
	   (init-label (read-init-label lam))
	   (new-state (do-code-sequence
		       (cons (lambda (state)
			       (do-add-label init-label state))
			     (if (> (env-object-size env) 0)
				 (list (lambda (state)
					 (do-alloc-env env state)))
			       ()))
		       state)))
      ;;(format t "Env: ~a~%" env)
      (if t ;;(lambda-closed-p lam)
	  ;; copy things into the closure
	  (fold (lambda (bind state)
		  (if (binding-closed bind)
		      (add-to-env bind state)
		    state))
		(lambda-ids lam)
		new-state)
	;; throw away parent environment
	(do-pop 1 state))))

  ;; Get the entry point right !
  (defun read-init-label (lam)
    (or (lambda-init-label lam)
	(let ((newlab (make-label nil)))
	  ((setter lambda-init-label) lam newlab)
	  newlab)))

  (defun add-to-env (bind state)
    (let ((posn (scanq-stack (state-stack state)
			     bind)))
      (do-code-sequence
       (list (lambda (state)
	       (do-stack-ref posn state))
	     (lambda (state)
	       (do-setter-env-ref 0 (binding-posn bind) state)))
       state)))

  ;; Lazily calculate environments
  (defun lambda-env (lam)
    (let ((e (real-lambda-env lam)))
      (or e
	  (let ((xx (make-lambda-environment lam)))
	    ((setter real-lambda-env) lam xx)
	    xx))))

  ;; make an environment...
  (defun make-lambda-environment (lam)
    (let ((closed-bindings (collect allocable-defn-p (find-closure lam))))
      ;; enumerate them
      (fold (lambda (bind n)
	      ((setter binding-posn) bind n)
	      (+ n 1))
	    closed-bindings
	    0)
      (make-env-object (list-length closed-bindings)
		       (convert closed-bindings <vector>)
		       (enclosing-env lam))))

  (defgeneric allocable-defn-p (defn))
  (defmethod allocable-defn-p ((x lambda-id))
    t)

  (defmethod allocable-defn-p ((defn definition))
    (if (binding-as-arg defn)
	t
      (not (inhibit-alloc (defn-body defn)))))

  (defun binding-needed-p (defn)
    (and (binding-used defn)
	 (not (lambda-inhibit-alloc (defn-body defn)))))

  ;; discover what needs to be placed in the environment

  (defgeneric find-closure (obj)
    methods ((((lam lambda-term))
	      (append (collect (lambda (x) (if (binding-closed x) x nil))
			       (lambda-ids lam))
		      (get-internal-closed-bindings (lambda-body lam))))
	     (((mod module-block))
	      (get-internal-closed-bindings (module-body mod)))))

  (defgeneric enclosing-env (obj)
    methods ((((lam lambda-term))
	      (lambda-env (enclosing-lambda lam)))
	     (((mod module-block))
	      nil)))

  ;; finally [This is called in many places]
  (defun add-tidy-code (lam state)
    (do-code-sequence
     (list (lambda (state)
	     (do-slide (stack-depth (state-stack state)) 1 state))
	   (lambda (state)
	     (do-dead-code (do-return state))))
     state))

  ;; other side of the fence
  (defgeneric allocate-closure-code (lam argcode state))

  (defun do-allocate-function (lam state)
    ;;(format t "Alloc: ~a ~a~%" (lambda-inhibit-alloc lam) lam)
    (if (lambda-inhibit-alloc lam)
	(progn ;;(format t "Inhibit: ~a~%" lam)
	       (do-push-static nil state))
      (let ((s1 (do-push-label (read-init-label lam)
			       state))
	    (init-ilist (if t;; do we need to shove an env on the stack?
			    (list (lambda (state)
				    (fetch-environment (enclosing-lambda lam) state))
				  )
			  (list (lambda (state)
				  (do-push-static nil state))))))
	(allocate-closure-code lam 
			       (lambda-nargs lam)
			       (do-code-sequence init-ilist s1))
	)))

  (defun fetch-environment (lam state)
    ;; Should find the env of this function or block...
    ;; and place it on the top of the stack (maybe registerise if we're
    ;; feeling keen)
    ;;(format t "(Fetch env: ~a ~a" lam state)
    ;;(print-props lam)
    (let ((posn (scanq-stack (state-stack state) (stacked-lambda-env lam))))
      ;;(format t "at ~a" posn)
      (do-stack-ref posn state)
      ))

  (defun stacked-lambda-env (lam)
    (cond ((module-p lam) (lambda-env lam))
	  ((> (env-object-size (lambda-env lam)) 0)
	   (lambda-env lam))
	  (t (stacked-lambda-env (enclosing-lambda lam)))))

  (defmethod allocate-closure-code ((x lambda-term) argcode state)
    (do-allocate-closure argcode nil state))

  ;; Macro lambdas...
  ;; do what we normally do, then turn it into a macro
  ;; the last bit is really just for the benifit of
  ;; the interpreter.

  (defmethod generic-code-gen ((x macro-lambda-term) state)
    (let ((state (call-next-method)))
      (let ((s1 (do-push-static bc-macro-type state)))
	(do-inline-code '((i-set-type)) 2 s1))))

  ;; extended (ie. name/comment-possesing) functions
  
  (defmethod generic-code-gen ((x extended-lambda-term) state)
    (call-next-method))
  
  (defmethod allocate-closure-code ((x extended-lambda-term) argcode state)
    (let ((ref (add-documented-entry (extended-lambda-name x)
				     (extended-lambda-comment x)))
	  (state (do-push-static (docs-name) state)))
      (let* ((state (do-push-fixnum ref state))
	     (state (do-cons state)))
	(do-allocate-closure argcode t state))))
      
  ;; Blocks...
  ;; rely on code-gen-for-decl.
  ;;
  (defgeneric generic-code-gen-for-decl (decl state))

  (defun code-gen-for-decl (decl state)
    ;;(format t "Generating code for decl: ~a~%" decl)
    (generic-code-gen-for-decl decl state))

  (defmethod generic-code-gen ((blk block-term) state)
    (let ((state-locs (code-gen-for-decl (block-decl blk) state)))
      (let ((state (cdr state-locs))
	    (posns (car state-locs)))
	(let ((state (code-gen (block-body blk) state)))
	  (if (term-tail-call blk)
	      state
	    (fold delete-decl posns state))))))

  (defun delete-decl (posn state)
    (let ((offset (- (stack-depth (state-stack state)) posn)))
      (do-slide (+ offset 1) offset state)))

  ;;   for normal lets, stuff each arg onto the stack in turn
  ;;   recursive lets: allocate space for the objects all at once

  (defmethod generic-code-gen-for-decl ((decl and-decl) state)
    ;; over simple. I could be real cunning here.
    (fold (lambda (decl state)
	    (let ((aa (code-gen-for-decl decl (cdr state))))
	      (cons (append (car aa) (car state))
		    (cdr aa))))
	  (and-decl-decls decl)
	  (cons nil state)))

  (defmethod generic-code-gen-for-decl ((rec rec-decl) state)
    (code-gen-for-decl (rec-decl-decl rec)
		       state))

  (defmethod generic-code-gen-for-decl ((defn definition) state)
    ;; XX should map this down as a post-pass to annotate
    (let ((state (code-gen (defn-body defn) state)))
      (if (not (binding-needed-p defn))
	  (let ((state (do-pop 1 state)))
	    ;;(format t "defn: Thrown away: ~a~%" defn)
	    (cons nil state))
	(if (binding-closed defn)
	    (cons nil (put-defn defn state))
	  (let ((state (name-stack-top defn state)))
	    (cons (list (stack-depth (state-stack state)))
		  state))))))

  (defgeneric inhibit-alloc (x)
    methods ((((x lambda-term))
	      ((setter lambda-inhibit-alloc) x t)
	      t
	      )
	     (((x syntax-obj))
	      nil)))

  (defun put-defn (defn state)
    (let ((posn (binding-posn defn)))
      (do-code-sequence
       (list (lambda (state)
	       (fetch-environment (enclosing-lambda defn) state))
	     do-swap
	     (lambda (state)
	       (do-setter-env-ref 0 posn state))
	     (lambda (state)
	       (do-pop 1 state)))
       state)))

  (defmethod generic-code-gen-for-decl ((defn module-definition) state)
    (format t "~a " (defn-ide defn))
    (cons nil (do-global-set (external-name defn) (code-gen (defn-body defn) state))))


  (defun name-stack-top (name state)
    ;;(format t "Name tos: ~a ~a~%" name state)
    (modify-compiler-state
     state
     'state-stack
     (stack-push (stack-pop (state-stack state) 1)
		 name)))


  ;; Statics.

  (defmethod generic-code-gen ((x literal-term) state)
    (let ((state (if (eq (class-of (literal-content x)) <integer>)
		     (do-push-fixnum (literal-content x) state)
		   (do-push-static (literal-content x) state))))
      (if (term-tail-call x)
	  (add-tidy-code (enclosing-lambda x) state)
	state)))


  ;; Exports (we just ignore them)
  (defmethod generic-code-gen ((x export-spec) state)
    (do-push-static nil state))


   ;; end module
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OLD function call methods....
;;
;; Nasty

;;XXX  (defmethod generic-code-gen ((applic applic-term) state)
;;XXX    (let ((obj (find-fn (applic-fun applic)))
;;XXX	  (lab (make-label state))
;;XXX	  (tail-flag (term-tail-call applic)))
;;XXX      (check-arguments applic obj)
;;XXX      (do-callbacks (function-fn obj) applic)
;;XXX      ;;(format t "*Found function: ~a-->~a~%" (applic-fun applic) obj)
;;XXX      ;; make the code for the call
;;XXX      (if tail-flag
;;XXX	  (add-tail-call-code applic lab obj state)
;;XXX	(add-std-call-code applic lab obj state))))
;;XXX
;;XXX  ;; paranoia
;;XXX  (defun check-arguments (applic obj)
;;XXX    (let ((nargs (list-length (applic-args applic)))
;;XXX	  (reqd-nargs (function-nargs obj)))
;;XXX      (if (or (eq (function-type obj) 'unknown)
;;XXX	      (= (cdr reqd-nargs) 9999)
;;XXX	      (= nargs (cdr reqd-nargs))
;;XXX	      (and (car reqd-nargs)
;;XXX		   (>= (+ nargs 1) (cdr reqd-nargs))))
;;XXX	  t
;;XXX	(error "Function called with wrong number of args"
;;XXX		Compile-Time-Error
;;XXX		'values (list reqd-nargs applic)
;;XXX		'msg "Function called with wrong number of args (should be ~a): ~%~a~%"))))
;;XXX
;;XXX  (defconstant find-callback (mk-finder))
;;XXX
;;XXX  (defgeneric do-callbacks (obj applic)
;;XXX    methods ((((x imported-definition) applic)
;;XXX	      (labels ((do-callback (l)
;;XXX				    (cond ((null l) nil)
;;XXX					  (t ((find-callback (car l)) applic)
;;XXX					     (do-callback (cdr l))))))
;;XXX		      (do-callback (import-prop-ref x 'callbacks))))
;;XXX	     (((x syntax-obj) y)
;;XXX	      nil)))
;;XXX
;;XXX  ((setter find-callback) 'set-setter
;;XXX   (lambda (applic)
;;XXX     ;;(format t "set-setter: ~a~%" applic)
;;XXX     (if (not (ident-p (cadr (applic-args applic))))
;;XXX	 nil
;;XXX       ((setter obj-setter-decl)
;;XXX	(ident-decl (car (applic-args applic)))
;;XXX	(ident-decl (cadr (applic-args applic)))))))
;;XXX
;;XXX  (defun add-std-call-code (applic label obj state)
;;XXX    (do-code-sequence
;;XXX     (list
;;XXX      ;; entry code
;;XXX      ;; ho hum
;;XXX      (if (eq (function-type obj) 'inline)
;;XXX	  (lambda (state) state)
;;XXX	(lambda (state)
;;XXX	    (do-push-label label state)))
;;XXX      ;; calc fn.
;;XXX      (lambda (state)
;;XXX	(do-compute-fn applic obj state))
;;XXX      ;; args
;;XXX      (lambda (state)
;;XXX	(push-fn-args (applic-args applic) obj
;;XXX		      state))
;;XXX      (lambda (state)
;;XXX	(if (eq (function-type obj) 'inline)
;;XXX	    state
;;XXX	  (do-stack-ref (+ (actual-args applic obj) 1) state)))
;;XXX      (lambda (state)
;;XXX	(do-apply-function applic obj state))
;;XXX      (lambda (state)
;;XXX	(do-add-label label state))
;;XXX      (lambda (state)
;;XXX	(modify-compiler-state
;;XXX	 state
;;XXX	 'state-stack
;;XXX	 (compute-final-stack applic obj state))))
;;XXX     state))
;;XXX
;;XXX  (defun add-tail-call-code (applic label obj state)
;;XXX    (do-code-sequence
;;XXX     (list (lambda (state)
;;XXX	     (do-compute-fn applic obj state))
;;XXX	   (lambda (state)
;;XXX	     (push-fn-args (applic-args applic)
;;XXX			   obj
;;XXX			   state))
;;XXX	   (lambda (state)
;;XXX	     (do-tidy-tail-call applic obj state))
;;XXX	   (lambda (state)
;;XXX	     (do-apply-function applic obj state))
;;XXX	   )
;;XXX     state))
;;XXX
;;XXX  (defun std-compute-fn (applic obj state)
;;XXX    (code-gen (applic-fun applic) state))
;;XXX
;;XXX  (defun push-args (args obj state)
;;XXX    ;; should do nary-check here
;;XXX    (if (car (function-nargs obj))
;;XXX	(push-nary-args (cdr (function-nargs obj)) args state)
;;XXX      (fold (lambda (arg state)
;;XXX	      (let ((xx (code-gen arg state)))
;;XXX		xx))
;;XXX	    args
;;XXX	    state)))
;;XXX
;;XXX  (defun push-nary-args (nargs args state)
;;XXX    ;; keep pushing till we get to optionals
;;XXX    (if (= nargs 1)
;;XXX	(push-optional-args args state)
;;XXX      (push-nary-args (- nargs 1) (cdr args)
;;XXX		      (code-gen (car args) state))))
;;XXX
;;XXX  (defun push-optional-args (args state)
;;XXX    (if (null args)
;;XXX	(do-push-static nil state)
;;XXX      (do-code-sequence
;;XXX       (list (lambda (state)
;;XXX	       (code-gen (car args) state))
;;XXX	     (lambda (state)
;;XXX	       (do-push-static nil state))
;;XXX	     (lambda (state)
;;XXX	       (do-cons state))
;;XXX	     (lambda (state)
;;XXX	       (do-stack-ref 0 state))
;;XXX	     (lambda (state)
;;XXX	       (push-remaining-args (cdr args) state)))
;;XXX       state)))
;;XXX
;;XXX  (defun push-remaining-args (args state)
;;XXX    (if (null args)
;;XXX	(do-pop 1 state)
;;XXX      (push-remaining-args (cdr args)
;;XXX			   (do-code-sequence
;;XXX			    (list (lambda (state)
;;XXX				    (code-gen (car args) state))
;;XXX				  (lambda (state)
;;XXX				    (do-push-static nil state))
;;XXX				  (lambda (state)
;;XXX				    (do-cons state))
;;XXX				  (lambda (state)
;;XXX				    (do-setter-cdr state)))
;;XXX			    state))))
;;XXX
;;XXX  ;; actual args pushed:
;;XXX  (defun actual-args (applic obj)
;;XXX    (if (car (function-nargs obj))
;;XXX	(cdr (function-nargs obj))
;;XXX      (list-length (applic-args applic))))
;;XXX
;;XXX  (defun mk-calltype (applic obj)
;;XXX    (if (car (function-nargs obj))
;;XXX	(- (cdr (function-nargs obj)))
;;XXX      (list-length (applic-args applic))))
;;XXX
;;XXX  ;; state is: fn args...
;;XXX
;;XXX  (defun tidy-std-tail-call (applic obj state)
;;XXX    (do-code-sequence
;;XXX     (list
;;XXX      ;; function at the top...
;;XXX      (lambda (state)
;;XXX	(do-stack-ref (actual-args applic obj) state))
;;XXX      (lambda (state)
;;XXX	(do-stack-ref 0 state))
;;XXX      ;; trash the fn. frame of self
;;XXX      (lambda (state)
;;XXX	(do-set-stack-ref (+ (stack-depth (state-stack state)) 1) state))
;;XXX      ;; slide down
;;XXX      (lambda (state)
;;XXX	(do-slide (stack-depth (state-stack state))
;;XXX		  (+ (actual-args applic obj) 1)
;;XXX		  state)))
;;XXX      state))
;;XXX
;;XXX  (defun apply-bytefunction (applic obj state)
;;XXX    (do-apply-bvf (mk-calltype applic obj) state))
;;XXX
;;XXX  (defun apply-any (applic obj state)
;;XXX    (do-apply-any (mk-calltype applic obj) state))
;;XXX
;;XXX  (defun apply-methods (applic obj state)
;;XXX    ;;(format t "Apply methods: ~a~%" state)
;;XXX    (do-apply-methods (mk-calltype applic obj)
;;XXX		      state))
;;XXX
;;XXX  (defun compute-std-final-stack (applic obj state)
;;XXX    (let ((stack (state-stack state)))
;;XXX      (stack-push (stack-pop stack
;;XXX			     (+ (actual-args applic obj) 4))
;;XXX		  (make-stack-val))))
;;XXX
;;XXX  (defun apply-inline-call (applic obj state)
;;XXX    (let ((state (do-inline-code (import-prop-ref (function-fn obj) 'code)
;;XXX				 (actual-args applic obj)
;;XXX				 state)))
;;XXX      (if (term-tail-call applic)
;;XXX	  (add-tidy-code (enclosing-lambda applic) state)
;;XXX	state)))
;;XXX
;;XXX  ;; slide down+ return...
;;XXX  (defun tidy-inline-call (applic obj state)
;;XXX    (do-slide (stack-depth (state-stack state))
;;XXX	      (actual-args applic obj)
;;XXX	      state))
;;XXX
;;XXX  ;; for the time being...
;;XXX  (defun compute-no-function (applic obj state)
;;XXX    (code-gen (applic-fun applic) state))
;;XXX
;;XXX  (defun apply-local-fn (applic obj state)
;;XXX    (do-code-sequence
;;XXX     ;; XXX 0 is the posn of the environment of a function
;;XXX     ;; unfortunately, we ain't calculated its env yet, so just push it.
;;XXX     (list (lambda (state)
;;XXX	     (do-slot-ref 0 state))
;;XXX	   (lambda (state)
;;XXX	     (do-branch (read-init-label (car obj)) state)))
;;XXX     state))
;;XXX
;;XXX  ;; Apply-self: call by shoving env
;;XXX  (defun apply-self (applic obj state)
;;XXX    (let ((s1 (stack-enclosing-env applic state)))
;;XXX      (do-branch (read-init-label (car obj)) s1)))
;;XXX
;;XXX  (defun stack-enclosing-env (applic state)
;;XXX    ;;(print (list applic state))
;;XXX    (let ((enc (enclosing-lambda applic)))
;;XXX      (let ((env (stacked-lambda-env enc))
;;XXX	    ;; for other locals, could use (enclosing-lambda (car obj)).
;;XXX	    (e2 (stacked-lambda-env (enclosing-lambda enc))))
;;XXX	;; XXX really a check for non-existent module-environment
;;XXX	(if (= (env-object-size e2) 0)
;;XXX	    (do-push-static nil (do-pop 1 state))
;;XXX	  (do-code-sequence
;;XXX	   (if (term-tail-call applic)
;;XXX	       (list (lambda (state)
;;XXX		       (name-stack-top env (do-slot-ref 0 state))))
;;XXX	     (cons (lambda (state)
;;XXX		     (fetch-environment enc (do-pop 1 state)))
;;XXX		   (if (eq env e2) nil
;;XXX		     (list
;;XXX		      (lambda (state)
;;XXX			(do-pop-env (find-env-depth env e2) state))))))
;;XXX	   state)))))
;;XXX
;;XXX
;;XXX
;;XXX  (defconstant find-fns (mk-finder))
;;XXX
;;XXX  (defun find-fn-computer (obj)
;;XXX    (car (find-fns (function-type obj))))
;;XXX
;;XXX  (defun find-apply-fn (obj)
;;XXX    (cadr (find-fns (function-type obj))))
;;XXX
;;XXX  (defun find-tidy-fn (obj)
;;XXX    (caddr (find-fns (function-type obj))))
;;XXX
;;XXX  (defun find-arg-pusher (obj)
;;XXX    (nth 3 (find-fns (function-type obj))))
;;XXX
;;XXX  (defun find-stack-computer (obj)
;;XXX    (nth 4 (find-fns (function-type obj))))
;;XXX
;;XXX  (defun do-compute-fn (applic obj state)
;;XXX    ((find-fn-computer obj) applic obj state))
;;XXX
;;XXX  (defun push-fn-args (applic obj state)
;;XXX    ((find-arg-pusher obj) applic obj state))
;;XXX
;;XXX  ;; fns called by applic generators
;;XXX  (defun compute-final-stack (applic obj state)
;;XXX    ((find-stack-computer obj) applic obj state))
;;XXX
;;XXX  (defun do-tidy-tail-call (applic obj state)
;;XXX    ((find-tidy-fn obj) applic obj state))
;;XXX
;;XXX  (defun do-apply-function (applic obj state)
;;XXX    ((find-apply-fn obj) applic obj state))
;;XXX
;;XXX  ;; description of the calling procedures
;;XXX  ((setter find-fns) 'bytefunction
;;XXX   (list std-compute-fn
;;XXX	 apply-bytefunction tidy-std-tail-call
;;XXX	 push-args  compute-std-final-stack))
;;XXX
;;XXX  ((setter find-fns) 'unknown
;;XXX   (list std-compute-fn
;;XXX	 apply-any tidy-std-tail-call
;;XXX	 push-args  compute-std-final-stack))
;;XXX
;;XXX  ((setter find-fns) 'function
;;XXX   (list std-compute-fn
;;XXX	 apply-any tidy-std-tail-call
;;XXX         push-args compute-std-final-stack))
;;XXX
;;XXX  ((setter find-fns) 'inline
;;XXX   (list (lambda (applic obj state) state)
;;XXX	 apply-inline-call
;;XXX	 tidy-inline-call
;;XXX	 push-args
;;XXX	 (lambda (applic obj x) (state-stack x))))
;;XXX
;;XXX  ((setter find-fns) 'cnm
;;XXX   (list std-compute-fn
;;XXX	 apply-methods
;;XXX	 tidy-std-tail-call
;;XXX	 push-args
;;XXX	 compute-std-final-stack))
;;XXX
;;XXX  ((setter find-fns) 'local-defun
;;XXX   (list compute-no-function
;;XXX	 apply-local-fn
;;XXX	 tidy-std-tail-call
;;XXX	 push-args
;;XXX	 compute-std-final-stack))
;;XXX
;;XXX  ((setter find-fns) 'self-call
;;XXX   (list compute-no-function
;;XXX	 apply-self
;;XXX	 tidy-std-tail-call
;;XXX	 push-args
;;XXX	 compute-std-final-stack))
;;XXX
;;XXX
