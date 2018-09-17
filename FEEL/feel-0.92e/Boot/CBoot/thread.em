;; Eulisp Module
;; Author: pab
;; File: thread.em
;; Date: Mon Jun 28 17:18:22 1993
;;
;; Project:
;; Description: 
;;  Higher level thread operations.
;;  Mostly deal with signals, initialization and printing

(defmodule thread
  (defs
   extras0
   macros0
   telos1
   init
   stream
   (rename ((open-primitive-semaphore lock)
	    (close-primitive-semaphore unlock))
	   sems)
   threads
   )
  ()
  
  (expose threads)

  (export <thread> threadp thread-reschedule current-thread thread-start
	  thread-value <thread-condition> <wrong-thread-continue>
	  lock-with-signals make-thread thread-signal
	  <interrupt>)
  
  (defclass <thread-condition> (<condition>)
    ()
    class <condition-class>)

  (defclass <wrong-thread-continue> (<thread-condition>)
    ()
    class <condition-class>)
  
  (defclass <interrupt> (<thread-condition>)
    ((flags initarg flags reader interrupt-flags))
    class <condition-class>)

  (defun lock-with-signals (isem)
    (or (lock isem)
	(progn (handle-pending-signals)
	       (lock-with-signals isem))))

  (defun thread-reschedule ()
    (internal-thread-reschedule)
    (handle-pending-signals))
  
  (defun thread-value (thread)
    (let ((res (internal-thread-value thread)))
;      (format t "in thread value: ~a~%" res)
      (if (car res) (cdr res)
	(progn (handle-pending-signals)
	       (thread-value thread)))))
  
  ;; Use of this function is deprecated. Use it and hope.
  ;; don't, and wonder.

  (defun thread-suspend ()
    (or (internal-thread-suspend)
	(progn (handle-pending-signals)
	       (thread-suspend))))

  ;; NB: it is impossible to raise a non-continuable error on a thread...
  ;; Should wake up the thread if it is asleep.
  ;; luckily, threads are always waiting or running, mod thread-suspend.
  ;; Thread suspend ain't part of the system, so all is cool.

  (defun thread-signal (cond fn thread)
    (let ((sem (car (thread-signals thread))))
      (lock sem)
      ((setter thread-signals) thread 
       (nconc (thread-signals thread) (cons cond fn)))
      (thread-set-signalled thread t)
      (unlock sem))
    (if (eq (current-thread) thread)
	(handle-pending-signals)
      (thread-reschedule)))

  (defun handle-pending-signals ()
    (let* ((thread (current-thread))
	   (thread-signals (thread-signals thread)))
      (lock (car thread-signals))
      (let ((lst (copy-list (cdr thread-signals))))
	((setter cdr) thread-signals nil)
	(thread-set-signalled thread nil)
	(unlock (car thread-signals))
	(mapcar (lambda (cond)
		  (format (standard-error-stream) 
			  "dealing with: ~a~%" cond)
		  (let/cc next 
			  (internal-signal cond next)))
		lst)
	nil)))
  
  (defun internal-thread-signal (thread flags)
    (print (list thread flags) (standard-error-stream))
    (thread-signal (make <interrupt> 'flags flags) 
		   nil
		   thread))
	

  (set-sig-handler internal-thread-signal)
  (set-bc-global 5 internal-thread-signal)

  (defmethod allocate ((x <thread-class>) lst)
    (if *threads-available*
	(generic_allocate_instance\,Thread_Class x lst)
	(progn (format (standard-error-stream) "*** threads not supported~%")
	       ())))

  (defmethod initialize ((x <thread>) lst)
    (let ((new (call-next-method)))
      (initialize-thread new lst)
      ((setter thread-signals) new 
       (cons (make-primitive-semaphore) nil))
      new))
  
  (defun make-thread (fun . junk)
    (apply make <thread> 'function fun junk))

  (when *threads-available*
    ((setter thread-signals) (current-thread)
     (cons (make-primitive-semaphore) nil)))
  
  (add-method generic-prin 
	      (make <method>
		    'signature (list <thread> <object>)
		    'function (method-lambda (thread s)
					     (let ((state (thread-state thread)))
					       (format s "#<~a: ~u ~a ~a>"
						       (symbol-unbraced-name (class-name (class-of thread)))
						       thread state
						       (if (eq state 'returned)
							   (thread-value thread) 
							 "{undetermined}"))))))
  
  

  ;; end module
  )
