;; Eulisp Module
;; Author: pab
;; File: complex.em
;; Date: Fri Dec  4 12:22:01 1992
;;
;; Project:
;; Description: 
;;

(defmodule complex
  (standard0
   list-fns
   numbers
   )
  ()
  

  (defclass <complex> (number)
    ((real initarg real reader real-part)
     (imag initarg imag reader imag-part))
    )

  (defclass <gaussian> (<complex>)
    ()
    constructor (make-gaussian real imag))
  
  (defclass real-complex (<complex>)
    ()
    constructor (make-real-<complex> real imag))
  
  (defgeneric make-complex (x y)
    methods ((((x <float>) (y <float>))
	      (make-real-complex x y))
	     (((x <integer>) (y <integer>))
	      (make-gaussian x y))
	     (((x <complex>) (y <complex>))
	      (+ x y))
	     (((x <number>) (y <number>))
	      (lift make-<complex> x y))))

  (defmethod generic-prin ((z <complex>) stream)
    (format stream "#C(~a+~ai)" (real-part z) (imag-part z)))

  (defmethod generic-write ((z <complex>) stream)
    (format stream "#C(~a+~ai)" (real-part z) (imag-part z)))
  

  (defmethod binary+ ((z1 <complex>) (z2 <complex>))
    (make-<complex> (binary+ (real-part z1) (real-part z2))
		  (binary+ (imag-part z1) (imag-part z2))))

  (defmethod binary- ((z1 <complex>) (z2 <complex>))
    (make-<complex> (binary- (real-part z1) (real-part z2))
		  (binary- (imag-part z1) (imag-part z2))))

  (defmethod negate ((z1 <complex>))
    (make-<complex> (negate (real-part z1))
		  (negate (imag-part z1))))

  (defmethod binary* ((z1 <complex>) (z2 <complex>))
    (make-<complex> (binary- (binary* (real-part z1) (real-part z2))
			   (binary* (imag-part z1) (imag-part z2)))
		  (binary+ (binary* (real-part z1) (imag-part z2))
			   (binary* (imag-part z1) (real-part z2)))))

  (defmethod binary/ ((z1 <complex>) (z2 <complex>))
    (let ((mod2 (binary+ (binary* (real-part z2) (real-part z2))
			 (binary* (imag-part z2) (imag-part z2)))))
      (make-<complex> (binary/ (binary+ (binary* (real-part z1) (real-part z2))
				      (binary* (imag-part z1) (imag-part z2)))
			     mod2)
		    (binary/ (binary- (binary* (imag-part z1) (real-part z2))
				      (binary* (real-part z1) (imag-part z2)))
			     mod2))))
  
  (defmethod = ((z1 <complex>) (z2 <complex>))
    (and (= (real-part z1) (real-part z2))
	 (= (imag-part z1) (imag-part z2))))


  (defmethod quotient ((x <gaussian>) (y <gaussian>))
    (binary/ x y))
  
  (defmethod remainder ((x <gaussian>) (y <gaussian>))
    (binary- x (binary* (quotient x y) y)))

  ;; I'll leave this to someone who knows the answer....
  '(defmethod binary-gcd ((x <gaussian>) (y <gaussian>))
     (labels ((g-aux (a b)
		     (print (list a b))
		     (let ((r (remainder a b)))
		       (if (= r 0) b
			 (g-aux b r)))))
	     (g-aux x y)))
	       

  (defmethod lift-numbers ((x <complex>) (y <float>))
    <complex>)

  (defmethod lift-numbers ((x <complex>) (y <integer>))
    <complex>)

  (defmethod (converter <complex>) ((x <integer>))
    (make-complex x 0))

  (defmethod (converter <complex>) ((x <float>))
    (make-complex x 0))

  (defconstant i (make-complex 0 1.0))

  (defconstant I (make-complex 0 1))

  ;; end module
  )

;; Number Implementations
;; 1.
(defmethod binary+ ((x number) (y number))
  (let ((new-class (lift-numbers x y)))
    (binary+ (convert x new-class)
	     (convert y new-class))))


;; 2.
(defmethod binary+ ((x number) (y number))
  (let ((new-y (coerce x y)))
    (if (null new-y)
	(let ((new-x (coerce y x)))
	  (if (null new-x)
	      (error "Can't do it" number-error 'error-value (cons x y))
	    (binary+ new-x y)))
      (binary+ x new-y))))


(defmethod coerce ((x number) (y number))
  nil)

  ;; <Complex> numbers:
  ;; Method 1.
  ;; use lifting...


  (defclass <complex> number
    ((real initarg real accessor real-part)
     (imag initarg imag accessor imag-part))
    constructor (make-<complex> real imag))
  

  (defconstant i (make-<complex> 0 i))

  (defmethod binary+ ((x <complex>) (y <complex>))
    (make-<complex> (+ (real-part x) (real-part y))
		  (+ (imag-part x) (imag-part y))))

  (defmethod lift-numbers ((x <complex>) (y <integer>))
    <complex>)

  (defmethod lift-numbers ((x <complex>) (y float))
    <complex>)

  (defmethod lift-numbers ((y <integer>) (x <complex>))
    <complex>)

  (defmethod lift-numbers ((y float) (x <complex>))
    <complex>)

  (defmethod (converter <complex>) ((x float))
    (make-<complex> x))

  (defmethod (converter <integer>) ((x <integer>))
    (make-<complex> x))

  ;; Method 2. Coersion
  ;;
  
  (defclass <complex> number
    ((real initarg real accessor real-part)
     (imag initarg imag accessor imag-part))
    constructor (make-<complex> real imag))
  

  (defconstant i (make-<complex> 0 i))

  (defmethod binary+ ((x <complex>) (y <complex>))
    (make-<complex> (+ (real-part x) (real-part y))
		  (+ (imag-part x) (imag-part y))))


  ;; Coerce forces the second arg to be of the 1st's class 
  ;; or compatible.

  (defmethod coerce ((x <complex>) (y <integer>))
    (make- (convert y <float>) 0))

  (defmethod coerce ((x <complex>) (y <float>))
    (make-<complex> y 0))
