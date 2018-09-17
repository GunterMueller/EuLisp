;; Eulisp Module
;; Author: pab
;; File: csd.em
;; Date: Fri Jul  9 00:15:44 1993
;;
;; Project:
;; Description: 
;;   classed-local-slot-descriptions

(defmodule csd
  (standard0
   list-fns
   
   )
  ()
  
  (defclass <classed-local-slot-description> (<local-slot-description>)
    ((contents-class initform <object> initarg contents-class
		     reader classed-local-slot-description-contents-class))
    metaclass <slot-description-class>)

  (defmethod compute-primitive-writer-using-slot-description 
    ((csd <classed-local-slot-description>) cl lst)
    (let ((std-writer (call-next-method))
	  (contents-cl (classed-local-slot-description-contents-class csd)))
      (lambda (obj val)
	(format t "In write: ~a, ~a~%" obj val)
	(if (subclassp (class-of val) contents-cl)
	    (std-writer obj val)
	  (error "invalid class of value for slot"
		 some-error 'object obj 'sd csd 'val val)))))

  (defclass <person> ()
    ((age slot-class <classed-local-slot-description>
	  initarg age
	  slot-initargs ('contents-class <integer>) accessor age)
     (name slot-class <classed-local-slot-description>
	   slot-initargs ('contents-class <string>)
	   accessor name) 
     (ordinary-slot initform 'bleagh))
    )
  

  ;; (make <person> 'age 24 'name "Pete Broadbery")
  ;; (make <person> 'age 'wibble 'name 22)

  ;; end module
  )
