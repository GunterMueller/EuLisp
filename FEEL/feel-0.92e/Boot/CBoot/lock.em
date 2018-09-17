;; Eulisp Module
;; Author: pab
;; File: lock.em
;; Date: Tue May  5 23:23:43 1992
;;
;; Project:
;; Description: 
;;

(defmodule lock
  (extras0
   macros0
   defs
   init
   sems
   thread
   telos1
   )
  ()
  (expose thread)

  (defstruct <lock> ()
    ((real-sem initform (make-primitive-semaphore)
	       reader semaphore-real-sem))
    constructor make-lock
    predicate semaphore-p)
  
	       
  (defgeneric lock (sem))
  (defgeneric unlock (sem))
  (defgeneric initialize-lock (sem))
    
  (defmethod lock ((x <object>))
    (error "Wrong class for lock" <Internal-Error>))
  
  (defmethod lock ((x <lock>))
    (lock-with-signals (semaphore-real-sem x))
    x)

  (defmethod unlock ((x <object>))
    (error "Wrong class for unlock" <Internal-Error>))
  
  (defmethod unlock ((x <lock>))
    (close-primitive-semaphore (semaphore-real-sem x))
    x)

  (defmethod initialize-lock ((x <object>))
    (error "Wrong class for initialize-semaphore" <Internal-Error>))
  
  (defmethod initialize-lock ((x <lock>))
    (initialize-primitive-semaphore (semaphore-real-sem x))
    x)

  (export make-lock lock unlock <lock> semaphore-p initialize-lock)

  ;; end module
  )



