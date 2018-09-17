(defmodule rshow
  (standard0) ()

  (defun show (object)
    (mapcar (lambda (slot-name)
	      (format t "~a: ~a\n" slot-name
		      (slot-value object slot-name)))
	    (mapcar slot-description-name
		    (class-slot-descriptions (class-of object)))))

  (defun rshow (x)
    (rshow-aux x ""))

  ;; same, but generic + recursive
  (defun rshow-aux (x st)
    (cond ((> (string-length st) 30)
	   (format t "..."))
	  (t (generic-rshow x st))))


  (defgeneric generic-rshow (ob st))

  (defmethod generic-rshow (ob string)
    (print ob)
    (mapc (lambda (slot-name)
	      (format t "~a ~a:" string slot-name)
	      (rshow-aux (slot-value ob slot-name)
			 (string-append string "  ")))
	  (mapcar slot-description-name 
		  (class-slot-descriptions (class-of ob))))
    nil)

;;  (defmethod generic-rshow ((l pair) st)
;;     (format t "~a List: ~a\n" st (car l))
;;     (rshow-aux (car l) (string-append st "      "))
;;     (rshow-aux (cdr l) st))

  (defconstant Null (class-of nil))

;;  (defmethod generic-rshow ((a Null) st)
;;    nil)
		  

  (export show)
  (export rshow)
  

;;end module
  )
