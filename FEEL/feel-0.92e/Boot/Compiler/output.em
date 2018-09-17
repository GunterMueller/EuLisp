;; Eulisp Module
;; Author: pab
;; File: output.em
;; Date: Tue Feb  4 22:40:42 1992
;;
;; Project:
;; Description: 
;;   Takes a compiler state and shoves it somewhere
;;   In this case it just constructs the right object
;;   and puts it into a file.

(defmodule output
  ((except (cadr) standard0)
   list-fns
  
   comp-defn
   compstate
   mod-info
   syntx-env
   props
   instruct
   assem
   peep-drv

   byte-stream
   )

  ()
  
  (defun cadr (x) (car (cdr x)))

  (defun output-compile-state (block state)
    ;; Does this get the prize for least efficient hack?
    (let ((code (reify-code-list (fold (lambda (x l) (append l x))
				       (state-code state)
				       nil))))
      (make-compile-unit 'name (module-name block)
			 'statics (statics-2-list (state-statics state))
			 'local-count (list-length (module-declarations block))
			 'byte-codes (car code)
			 'imports (module-import-desc (module-imports block))
			 'exports (module-exported-names (module-exports block))
			 'length (nth 1 code))))

  (export output-compile-state)

  ;; writing these things...
  
  (defun write-compile-unit (x)
    (write-object x (unit-name x)))
  
  (export write-compile-unit)

  ;; should be destructive, and faster
  (defun make-code-from-state (state)
    (let ((stream (make-simple-stream)))
      (mapc (lambda (code) 
	      (write-stream-list stream code))
	    (state-code state))
      (convert stream pair)))

  ;;(fold (lambda (x l) (append l x))
  ;;(state-code state)
  ;;nil))

  (deflocal xxx ())
  (defun output-sc-state (module state initflag)
    (let ((statics (statics-2-list (state-statics state)))
	  (nslots (list-length (module-declarations module))))
      (let* ((real-code (make-code-4-output state))
	     (new-c-s (if initflag 
			  (add-self-install-code module real-code statics)
			(add-micro-install-code module real-code statics)))
	     (bytes (reify-code-list (car new-c-s))))
	(setq xxx real-code)
	(make-instance sc-compile-unit 
		       'statics (cdr new-c-s)
		       'code (car bytes)
		       'length (cadr bytes)
		       'nslots nslots
		       'names (list (module-name module))
		       'dependencies (module-flat-imports module)))))

  (export output-sc-state)
  (deflocal zz ())
  (defun make-code-4-output (state)
    (if (optimize-code)
	(optimize-lst (setq zz (make-code-from-state state)) (micro-optimize-code))
      (make-code-from-state state)))

  ;; output flags

  (defconstant peephole-optimize-code (local-var t))
  (defconstant micro-optimize-code (local-var t))
  (defconstant boot-module (local-var 'boot))
  (defconstant strip-module (local-var nil))

  (defun optimize-code ()
    (peephole-optimize-code))
  
  ((setter setter) optimize-code
   (lambda (x) 
     ((setter peephole-optimize-code) x)
     ((setter micro-optimize-code) x)))

  (export optimize-code boot-module micro-optimize-code strip-module)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Interface to module system.
  ;;
  ;; return new code + statics pair
  ;; 
  (defun module-defined-names (module)
    (if (not (strip-module))
	(module-declarations module)
      (let ((locals (module-declarations module)))
	(mapcan (lambda (bind)
		  (if (memq bind locals) 
		      (list bind)
		    nil))
		(module-exports module)))))


  (defun add-self-install-code (module code static-list)
    (let ((name-list (mapcar defn-ide
			     (module-defined-names module)))
	  (code-start-label (make-refed-label))
	  (len (list-length static-list)))
      (let ((name-list-id len)
	    (interface-list-id (+ 1 len))
	    (mod-name-id (+ 2 len)))
	(labels ((make-module-creation-code-start 
		  ()
		  (let ((rlab1 (make-refed-label))
			(rlab2 (make-refed-label))
			(rlab3 (make-refed-label)))
		    ;; First we call the initialisation
		    (list (push-label (list rlab1)) ;; return
			  (push-fixnum '(0))        ;; 'Fn' for backtrace
			  (push-special '(0))       ;; 'env'
			  ;; call and throw away the rest
			  (branch (list code-start-label))
			  (i-label (list rlab1))
			  (drop '(1))              ;;return
			  ;; make a module
			  (push-label (list rlab2)) 
			  (push-fixnum '(0))
			  (push-static (list mod-name-id))
			  (current-context nil)
			  (push-global (list (cons (boot-module) 'make-installed-module)))
			  (apply-bvf (list 2));; stack is: module context
			  (i-label (list rlab2))
			  )))
		 ;;(push-label (list rlab3))
		 ;;(push-fixnum '(1))
		 ;;(nth-ref '(3))
		 ;;(push-static (list interface-list-id))
		 ;;(push-global (list (cons (boot-module) 'make-interface)))
		 ;;(apply-bvf (list 2))
		 ;;(i-label (list rlab3))
		 ;;(drop (list 1))
		 (make-module-code-loc-list 
		  ()
		  (let ((stream (make-simple-stream))
			(rlab (make-refed-label))
			(rlab3 (make-refed-label)))
		    (write-stream-list stream
				       (list (push-label (list rlab))
					     (push-fixnum '(0))
					     (nth-ref (list 3))
					     (push-static (list name-list-id))))
		    (if (null name-list)
			(write-stream-list stream 
					   (list (push-special '(0))))
		      (let ((new-stream (fold (lambda (name stream)
						(write-stream-list
						 stream
						 (list (push-fixnum (list (cons (the-local-handle)
										name)))
						       (push-special '(0))
						       (i-cons nil)
						       (set-slot (list 1)))))
					      (cdr name-list)
					      (make-simple-stream))))
			(write-stream-list stream
					   (list (push-fixnum (list (cons (the-local-handle)
									  (car name-list))))
						 (push-special '(0))
						 (i-cons nil)
						 (nth-ref (list 0))))
			(write-stream-list stream (convert new-stream pair))
			(write-stream-list stream (list (drop '(1))))))
		    (write-stream-list stream 
				       (list	
					;; make an interface...
					(push-global (list (cons (boot-module) 'install-local-bindings)))
					(apply-bvf (list 3))
					(i-label (list rlab))
					(drop (list 1))
					(push-label (list rlab3))
					(push-fixnum '(1))
					(nth-ref '(3))
					(push-static (list interface-list-id))
					(push-global (list (cons (boot-module) 'make-interface)))
					(apply-bvf (list 2))
					(i-label (list rlab3))
					(drop (list 1))
					(i-slide-stack (list 2 1))
					(return nil))))))
		;; make up the list...
		(let ((lst (fold append
				 (list (list (i-label (list code-start-label)))
				       (convert (make-module-code-loc-list) pair)
				       (make-module-creation-code-start))
				 code))
		      (new-statics 
		       (append static-list 
			       (list name-list
				     (cons (if (strip-module) 
					       (list 'strip (module-import-desc (module-imports module)))
					     (module-import-desc (module-imports module)))
					   (mapcar defn-ide (module-exports module)))
				     (module-name module)))))
		  (cons lst new-statics))))))

  ;; adding trivial installation code (for initialisation...)
  (defun add-micro-install-code (module code static-list)
    (cons code static-list))
			    
  ;; end module
  )
