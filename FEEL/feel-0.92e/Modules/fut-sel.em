;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;  EuLisp Module                     Copyright (C) University of Bath 1991  ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;   EuLisp Module  -   Copyright (C) Codemist and University of Bath 1989   ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;

;; Change Log:
;;   Version 1.0 (20/5/90)

;; added Future-select

(defmodule fut-sel

  (eulisp0 futures)()

  (defstruct Future-Selector ()
    ((comm-sem initform (make-semaphore)
	       accessor Future-Selector-comm-sem)
     (lock-sem initform (make-semaphore)
	       accessor Future-Selector-lock-sem)
     (result-future initform () 
		    accessor Future-Selector-result-future))
    constructor make-Future-Selector)
    
  (defmethod initialize-instance ((proto Future-Selector) lst)
    (let ((new-obj (call-next-method)))
      (open-semaphore (Future-Selector-comm-sem new-obj))
      new-obj))

  (defun make-future-selector (futs)
    (let ((fs (make-Future-Selector)))
      (mapc (lambda (fut) (thread-start (make-thread future-select-aux)
					fut fs))
	    futs)
      fs))

  ;; returns next future + reinitialises the sem.
  (defun select-future (fs)
    (open-semaphore (Future-Selector-comm-sem fs))
    (let ((result (Future-Selector-result-future fs)))
      ((setter Future-Selector-result-future) fs nil)
      result))

  (defun select-one-future (futs)
    (let ((fs (make-Future-Selector)))
      (mapc (lambda (fut)
	      (thread-start (make-thread future-select-aux)
			    fut fs))
	    futs)
      (open-semaphore (Future-Selector-comm-sem fs))
      (Future-Selector-result-future fs)))
  
  (defun future-select-aux (fut fs)
    (let ((value (future-value fut)))
      (open-semaphore (Future-Selector-lock-sem fs))
      (cond ((Future-Selector-result-future fs)
	     (close-semaphore (Future-Selector-lock-sem fs))
	     (thread-reschedule)
	     (future-select-aux fut fs)
	     nil)
	    (t ((setter Future-Selector-result-future) fs fut)
	       (close-semaphore (Future-Selector-comm-sem fs))	       
	       (close-semaphore (Future-Selector-lock-sem fs))
	       fut))))
  ;;
  ;; Test...
  ;;

  (defun fibbing (x y)
    (thread-reschedule)
    (if (< x 2) y
      (and (fibbing (- x 1) y)
	   (progn (thread-reschedule) t)
	   (fibbing (- x 2) y))))

  
  (defun mk-tasks (n) 
    (cond ((= n 0) 
	   ())
	  (t (let ((x  (remainder (c-rand) 16)))
	       (format t "Task: ~a%" x)
	       (cons (future (fibbing x x))
		     (mk-tasks (- n 1)))))))

  (defun get-results (sel n)
    (if (= n 0)
	()
      (progn (format t "Result: ~a~%" (future-value (select-future sel)))
	     (get-results sel (- n 1)))))
  

  (defun test (n) 
    (get-results (make-future-selector (mk-tasks n)) n))

  

     
  (defun future-done-p (fut) (future-object-done fut))

  (export make-future-selector future-select)

)
