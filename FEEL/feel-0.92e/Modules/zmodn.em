;; Eulisp Module
;; Author: pab
;; File: zmodn.em
;; Date: Thu Feb 11 15:06:30 1993
;;
;; Project:
;; Description: 
;;   Numbers modulo n in feel

(defmodule zmodn
  (         
   eulisp0   )
  ()
  

  (defclass <zmodn-class> (<class>)
    ((n initarg n reader zmodn-class-n))
    metaclass <class>)

  (defclass <zmodn-object> (<number>)
    ((z accessor zmodn-z))
    metaclass <zmodn-class>)
  
  (defun make-zmodn-class (n)
    (make <zmodn-class>
	  'direct-superclasses (list <zmodn-object>)
	  'name (make-symbol (format nil "Zmod-~a" n))
	  'n n))

  (defconstant *zmodn-table* (make <table> 'comparator = 'hash-function generic-hash))

  (defun find-zmodn-class (n)
    (or (table-ref *zmodn-table* n)
	(let ((cl (make-zmodn-class n)))
	  ((setter table-ref) *zmodn-table* n cl)
	  cl)))

  ;; i mod n
  (defun make-modular-number (z n)
    (make (find-zmodn-class n) 'z z))
  
  (defmethod initialize ((proto <zmodn-object>) lst)
    (let ((i (call-next-method)))
      ((setter zmodn-z) i 
       (remainder (scan-args 'z lst required-argument) 
		  (zmodn-n i)))
      i))
  
   (defgeneric zmodn-n (obj))

   (defmethod zmodn-n ((z <zmodn-object>))
     (zmodn-class-n (class-of z)))

  ;; printing (on prin only, as this magically handles write too)
  (defmethod generic-prin ((i <zmodn-object>) stream)
    (format stream "~a<mod ~a>" (zmodn-z i) (zmodn-n i)))

  (defmethod binary+ ((i <zmodn-object>) (j <zmodn-object>))
    (when (compatible-moduli i j)
      (make-modular-number (+ (zmodn-z i) (zmodn-z j)) 
			   (zmodn-n i))))
  
  (defun compatible-moduli (n m) 
    (if (= (zmodn-n n) (zmodn-n m)) t
      (error "Incompatible-moduli" <number-error>)))

  ;; end module
  )
