;; Eulisp Module
;; Author: pab
;; File: class-macs.em
;; Date: Mon Dec 14 11:39:08 1992
;;
;; Project:
;; Description: 
;;

(defmodule class-macs
  ((except (scan-args) standard0)
   list-fns
   )
  ()

  (export find-class find-slot the-classlist find-slot 
	  accessor-location
	  define-prim-class reset-classes)

  (defun scan-args (arg lst default)
    (cond ((null lst) default)
	  ((eq (car lst) arg) (car (cdr lst)))
	  (t (scan-args arg
			(cdr (cdr lst))
			default))))

  
  (deflocal *defs* nil)

  (deflocal find-slot (mk-finder))
  (deflocal find-class (mk-finder))

  (defun reset-classes () 
    (progn (setq *defs* nil)
	   nil))

  (defun the-classlist () *defs*)

  (defmacro define-prim-class (name supers slots . options)
    (let ((classd (make-class-init-list name supers slots options)))
      ((setter find-class) (scan-args 'name classd 'anonymous) classd)
      (setq *defs* 
	    (nconc *defs*
		   (list classd)))
      nil))

  (defun make-class-init-list (name supers slots options)
    (append (list 'name name
		  'direct-superclasses supers
		  'direct-slot-descriptions 
		  (mapcar (lambda (d) 
			    (let ((s (append (list 'class-name name)
					     (cons 'name d))))
			      (make-slot-record s)
			      s))
			  slots))
	    options))

  (defun make-slot-record (slotd)
    (let ((accessor (scan-args 'accessor slotd nil)))
      (if (null accessor) nil
	  ((setter find-slot) accessor (cons (scan-args 'position slotd nil)
					     slotd)))))
  
  
  ;; Should be slot-desc-position
  (defun accessor-location (x) 
    (let ((aa (find-slot x)))
      (if (null aa) nil
	(car aa))))

  (deflocal *socks* t)
  (defun set-sockets (x) (setq *socks* x))
  
  (defmacro with-sockets x
    (if *socks* `(progn ,@x)
      nil))
  
  (export with-sockets set-sockets)
  ;; end module
  )
