;; Eulisp Module
;; Author: pab
;; File: standard0.em
;; Date: Thu Dec 17 16:03:19 1992
;;
;; Project:
;; Description: 
;;

(defmodule standard0
  (standard
   )
  ()
  (expose (except (scan-args fold) standard))

  (export slot-description-initargs slot-value
	  class-direct-slot-descriptions
	  allocate-instance 
	  make-instance initialize-instance
	  generic-function-method-class
	  slot-value-using-slot-description
	  class-direct-slot-descriptions)

  (defconstant make-instance make)
  (defconstant allocate-instance allocate)
  (defconstant initialize-instance initialize)
  (defconstant generic-function-method-class generic-method-class)

  (defun slot-description-initargs (x)
    (if (slot-description-initarg x)
	(list (slot-description-initarg x))
      nil))
  
  (defun slot-value (obj name)
    ((slot-description-slot-reader (find-slot-description (class-of obj) name)) obj))

  ((setter setter) slot-value
   (lambda (obj name value)
     ((slot-description-slot-writer (find-slot-description (class-of obj) name)) obj value)))

  (defun slot-value-using-slot-description (o s)
    ((slot-description-slot-reader s) o))

  ((setter setter) slot-value-using-slot-description 
   (lambda (o s v)
     ((slot-description-slot-writer s) o v)))

  (defconstant class-direct-slot-descriptions class-slot-descriptions)
  (defconstant pair <cons>)

  (export pair)
;; end module
  )
