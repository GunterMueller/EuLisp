;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;  EuLisp Module                     Copyright (C) University of Bath 1991  ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;; futures.em
;;
;;   General purpose-ish future package allowing constant limit and
;;   usage controlled creation.
;

(defmodule futures

  (eulisp0) ()

  ;
  ;; Book Keeping.
  ;;
  ;;   We keep track of useful stats like the number of futures
  ;;   created together with the number of currently active futures.
  ;;   That kind of junk anyhow.
  ;

  ;; Global accounting...

  (deflocal future-count-value 0)
  (defconstant *fcv* (make <lock>))

  (defun future-count () future-count-value)
  (defun set-future-count (n) (setq future-count-value n))

  ((setter setter) future-count set-future-count)

  (defun increment-future-count () 
    (lock *fcv*)
    (setq future-count-value (+ future-count-value 1))
    (unlock *fcv*))

  (defun zero-future-count () (setq future-count-value 0))

  (export future-count increment-future-count zero-future-count)

  ;; Active acounting...

  (deflocal active-future-count-value 0)
  (defconstant *afcv* (make <lock>))

  (defun active-future-count () active-future-count-value)
  (defun set-active-future-count (n) (setq active-future-count-value n))
  ((setter setter) active-future-count set-active-future-count)

  (defun increment-active-future-count () 
    (lock *afcv*)
    (setq active-future-count-value (+ active-future-count-value 1))
    (unlock *afcv*))

  (defun decrement-active-future-count () 
    (lock *afcv*)
    (setq active-future-count-value (- active-future-count-value 1))
    (unlock *afcv*))

  (defun zero-active-future-count () (setq active-future-count-value 0))

  (export active-future-count zero-future-count)

  ;
  ;; Future Structure.
  ;;
  ;;   In this hacked implementation really just a mailbox by another
  ;;   name - hangs on to lots of useful information though.
  ;

  (defstruct future-object ()

    ((function 
        initarg function
        accessor future-object-function)
     (thread 
        initarg thread
        accessor future-object-thread)
     (value 
        accessor future-object-value)
     (done  
        initform nil
	accessor future-object-done-p))

    constructor make-future-object
    predicate futurep)

  (defmethod generic-prin ((f future-object) str)
    (format str "#<future-object: ~a>" 
	    (if (future-object-done-p f) (future-object-value f)
	      'unresolved))
    f)

  (defmethod generic-write ((f future-object) str)
    (format str "#<future-object: ~s>" 
	    (if (future-object-done-p f) (future-object-value f)
	      'unresolved))
    f)

  (export future-object future-object-value future-object-function
	  future-object-done-p make-future-object future-object-thread
	  futurep)

  ;
  ;; Future Macro.
  ;;
  ;;   Just the usual syntax interface calling the function version.
  ;

  (defmacro future exp
    `(futurize (lambda () ,@exp)))

  (export future)

  ;
  ;; Futurization.
  ;;
  ;;   Make a given closure into a future object depending on the
  ;;   current creation heuristic.
  ;

  (defun futurize (fn)
    (if (not (really-create-future-p)) (fn)
      (let*
	((task (make-thread
		 (lambda (future fun)
		   ((setter future-object-value) future (fun))
		   ((setter future-object-done-p) future t)
		   (decrement-active-future-count)
		   t)))
	 (future (make-future-object 'function fn 'thread task)))
	;; Enable the thread...
	(increment-future-count)
	(increment-active-future-count)
	(thread-start task future fn)
	future)))
       
  (export futurize)

  ;
  ;; Future Evaluation.
  ;;
  ;;   Touch a future object for its value. Block until completed.
  ;

  (defun future-value (fut)
    (if (futurep fut)
      (if (future-object-done-p fut) 
	(future-value (future-object-value fut))
	(progn
	  (thread-value (future-object-thread fut))
	  (future-value fut)))
      fut))
	
  (export future-value)

  ;
  ;; Creation Heuristic.
  ;;
  ;;   Should I really create or not.
  ;

  (deflocal future-creation-heuristic-fn ())

  (defun future-creation-heuristic () 
    future-creation-heuristic-fn)
  (defun set-future-creation-heuristic (val)
    (setq future-creation-heuristic-fn val))
  ((setter setter) future-creation-heuristic set-future-creation-heuristic)

  (export future-creation-heuristic)

  (defun really-create-future-p ()
    (future-creation-heuristic-fn))

  ;
  ;; Creation Modes.
  ;;
  ;;  Modes of creation throttling.
  ;

  (defmacro define-future-mode (name . body)
    `(register-future-mode ',name (lambda () ,@body)))

  (defconstant *mode-table* (make-table eq))

  (defun register-future-mode (name fn)
    ((setter table-ref) *mode-table* name fn))

  (deflocal current-mode ())

  (defun future-mode () current-mode)
  (defun set-future-mode (name)
    (let ((fn (table-ref *mode-table* name)))
      (if (null fn)
	(error (format () "future-mode: unknown mode - ~a" name) clock-tick)
	(progn
	  (setq current-mode name)
	  ((setter future-creation-heuristic) fn)
	  name))))
  ((setter setter) future-mode set-future-mode)

  (export future-mode)

  ;; Pre-defined modes.

;;  (define-future-mode always t) ;; Always create
;;  (define-future-mode never ()) ;; Never create
  
;;  ((setter future-mode) 'always)

  ((setter future-creation-heuristic) 
    (lambda () (< (active-future-count) 3)))
)

