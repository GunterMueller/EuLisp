;; Eulisp Module
;; Author: pab
;; File: purge.em
;; Date: Thu Feb 18 16:05:26 1993
;;
;; Project:
;; Description: 
;;

(defmodule purge
  (eulisp0
   scanners
   describe
   )
  ()
  

  (defclass <finalisable-class> (<class>)
    ((count accessor finalisable-slot-count)
     (handle-posn accessor finalisable-handle-posn)
     (proxy accessor proxy-class))
    )

  (defclass <finalisable-sd> (<slot-description>)
    ()
    predicate finalisable-sd-p
    )
  
  ;; initializing classes. 
  ;; need to add methods on obj-reader so that we can 
  ;; access the handle safely.
  
  (defmethod initialize ((cl <finalisable-class>) lst)
    (let ((cl (call-next-method)))
      (let ((slot-posn (class-instance-size cl)))
	((setter class-instance-size) cl (+ slot-posn 1))
	((setter finalisable-handle-posn) cl slot-posn)
	((setter proxy-class) cl 
	 (scan-args 'proxy-class lst (default-argument cl))))
      cl))

  (defmethod compute-initargs ((cl <finalisable-class>) direct inherited)
    (let ((lst (call-next-method)))
      (if (memq 'proxy lst) lst
	(cons 'proxy lst))))
       
  ;; allocating instances -- we make a vector to store the values
  (defmethod allocate ((cl <finalisable-class>) lst)
    (let ((handle (make-vector (finalisable-slot-count cl)))
	  (obj (call-next-method)))
      (primitive-set-slot-ref obj (finalisable-handle-posn cl) handle)
      (if (not (scan-args 'proxy lst null-argument))
	  (setq *final-lst* 
		(cons (list (make-weak-wrapper obj) 
			    cl
			    handle)
		      *final-lst*))
	nil)
      obj))

  ;; superclass of all objects (not of itself finalisable)
  (defclass <finalisable> ()
    ()
    )

  ;; accessing the handle

  (defgeneric obj-handle (obj))
  (defmethod obj-handle ((x <finalisable>))
    (primitive-slot-ref x (finalisable-handle-posn (class-of x))))
  
  (defgeneric (setter obj-handle) (x v)
    method (((x <finalisable>) v)
	    (primitive-set-slot-ref x 
				    (finalisable-handle-posn (class-of x))
				    v)))
  ;; Slot access
  (defmethod compute-and-ensure-slot-accessors 
    ((cl <finalisable-class>) effective-sds inherited-sds)
    (labels ((register-sds (lst n)
			   (cond ((null lst) 
				  ((setter finalisable-slot-count) cl n))
				 ((finalisable-sd-p (car lst))
				  ((setter slot-description-position) (car lst) n)
				  (register-sds (cdr lst) (+ n 1)))
				 (t (register-sds (cdr lst) n)))))
	    (register-sds effective-sds 0))
    (call-next-method))
  

  (defmethod compute-primitive-reader-using-slot-description
    ((sd <finalisable-sd>) (cl <finalisable-class>) sds)
    (let ((posn (slot-description-position sd))
	  (handle-posn nil))
      (lambda (o)
	(vector-ref (primitive-slot-ref o (finalisable-handle-posn (class-of o))) posn))))

  (defmethod compute-primitive-writer-using-slot-description
    ((sd <finalisable-sd>) (cl <finalisable-class>) sds)
    (let ((posn (slot-description-position sd))
	  (handle-posn nil))
      (lambda (o v)
	((setter vector-ref) (primitive-slot-ref o (finalisable-handle-posn (class-of o)))
	 posn v))))


  ;; initializing objects

  (defmethod initialize ((x <finalisable>) lst)
    (let ((new (call-next-method)))
      new))

  ;; Finalising objects
  (defgeneric finalise (x)
    method (((x <object>))
	    (format t "Finalise: ~a~%" x)))

  (defun finalise-objects ()
    (format t "Killing objects...~%")
    (mapc (lambda (obj)
	    (if (weak-wrapper-ref (car obj))
		nil
	      (progn (finalise (make-proxy-object (cadr obj) (caddr obj)))
		     ((setter weak-wrapper-ref) (car obj) 'x))))
	  *final-lst*))
  
  (defun make-proxy-object (class values)
    (let ((new-cl (proxy-class class)))
      (let ((obj (allocate new-cl
			   (list 'proxy t))))
	(mapc (lambda (sd)
		(if (finalisable-sd-p sd)
		    ((slot-description-slot-writer 
		      (find-slot-description new-cl
					     (slot-description-name sd)))
		     obj
		     (vector-ref values (slot-description-position sd)))
		  nil))
	      (class-slot-descriptions class))
	obj)))
  
  (set-post-gc-callback finalise-objects)

  ;; List of objects

  (deflocal *final-lst* nil)
  )
  ;; poxy example
  
  (defclass <f1> (<finalisable>)
    ;;((s1 slot-class <finalisable-sd> accessor f1s))
    ()
    metaclass <finalisable-class>
    predicate f1p)

  ;; less useless example
  ;; probably won't work, but you get the idea

  (defclass file (<finalisable>)
    ((h initarg handle accessor file-handle))
    metaclass <finalisable-class>)

  (defmethod finalise ((x <file>))
    (close (file-handle file)))

  (defmethod initialize ((x <file>) lst)
    (let ((handle (open lst))
	  (new (call-next-method)))
      ((setter file-handle) new handle)
      new))

  ;; end module
  )
