;; Eulisp Module
;; Author: pab
;; File: poly.em
;; Date: Thu Jul  8 15:14:13 1993
;;
;; Project:
;; Description: 
;;

(defmodule poly
  (eulisp0         
   )
  ()

  (defclass <polynomial> (<number>) 
    ((ldeg accessor ldeg initarg ldeg initform 1)
     (lc accessor lc initarg lc initform 1) 
     (red accessor red initarg red initform 0))
    constructor make-polynomial)
  
  ;; Zero should be a class of its own, but I want
  ;; to be brief.

  (defconstant *zero-polynomial* 
    (make-polynomial 'ldeg 0 'lc 0 'red 0))

  (defmethod = ((x <polynomial>) (y <polynomial>))
    (and (= (ldeg x) (ldeg y))
	 (= (lc x) (lc y))
	 (= (red x) (red y))))

  (defmethod binary+ ((p <polynomial>) (q <polynomial>))
    (cond ((= (ldeg p) (ldeg q))
	   (let ((sum (binary+ (lc p) (lc q))))
	     (if (zerop sum)
		 (if (> (ldeg p) 0)
		     (binary+ (red p) (red q))
		   *zero-polynomial*)
	       (make-polynomial 'ldeg (ldeg p) 'lc sum 'red 
				(binary+ (red p) (red q))))))
	  ((< (ldeg p) (ldeg q))
	   (make-polynomial 'ldeg (ldeg q) 'lc (lc q) 
			    'red (binary+ p (red q))))
	  (t (make-polynomial 'ldeg (ldeg p) 'lc (lc p)
			      'red (binary+ (red p) q)))))
  
  (defmethod lift-numbers ((i <integer>) (p <polynomial>))
    <polynomial>)

  (defmethod lift-numbers ((p <polynomial>) (i <integer>))
    <polynomial>)

  (defmethod (converter <polynomial>) ((x <integer>))
    (make-polynomial 'lc x 'ldeg 0))
  
  (defmethod zerop ((x <polynomial>))
    (and (= (ldeg x) 0)
	 (= (lc x) 0))

  (defmethod generic-prin ((x <polynomial>) stream)
    (if (zerop x)
	(prin 0 stream)
      (format stream "~ax^~a+~a" (lc x) (ldeg x) (red x))))


  ;; end module
  )
