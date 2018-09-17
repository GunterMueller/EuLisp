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
  

  (defclass <complex> (<number>)
    ((real initarg real reader real-part)
     (imag initarg imag reader imag-part))
    )

  (defclass <gaussian> (<complex>)
    ()
    constructor (make-gaussian real imag))
  
  (defclass <real-complex> (<complex>)
    ()
    constructor (make-real-complex real imag))
  
  (defgeneric make-complex (x y)
    methods ((((x <float>) (y <float>))
	      (make-real-complex x y))
	     (((x <integer>) (y <integer>))
	      (make-gaussian x y))
	     (((x <number>) (y <number>))
	      (lift make-complex x y))))
  
  (defmethod generic-prin ((z <complex>) stream)
    (format stream "#C(~a+~ai)" (real-part z) (imag-part z)))

  (defmethod generic-write ((z <complex>) stream)
    (format stream "#C(~a+~ai)" (real-part z) (imag-part z)))
  

  (defmethod binary+ ((z1 <complex>) (z2 <complex>))
    (make-complex (binary+ (real-part z1) (real-part z2))
		  (binary+ (imag-part z1) (imag-part z2))))

  (defmethod binary- ((z1 <complex>) (z2 <complex>))
    (make-complex (binary- (real-part z1) (real-part z2))
		  (binary- (imag-part z1) (imag-part z2))))

  (defmethod negate ((z1 <complex>))
    (make-complex (negate (real-part z1))
		  (negate (imag-part z1))))

  (defmethod binary* ((z1 <complex>) (z2 <complex>))
    (make-complex (binary- (binary* (real-part z1) (real-part z2))
			   (binary* (imag-part z1) (imag-part z2)))
		  (binary+ (binary* (real-part z1) (imag-part z2))
			   (binary* (imag-part z1) (real-part z2)))))

  (defmethod binary/ ((z1 <complex>) (z2 <complex>))
    (let ((mod2 (binary+ (binary* (real-part z2) (real-part z2))
			 (binary* (imag-part z2) (imag-part z2)))))
      (make-complex (binary/ (binary+ (binary* (real-part z1) (real-part z2))
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
  
  (defmethod (converter <real-complex>) ((x <gaussian>))
    (+ (convert 0.0 <complex>) x))

  (defconstant i (make-complex 0 1.0))

  (defconstant I (make-complex 0 1))


  ;; Polar representation for complex numbers
  (defclass <angle> (<number>) 
    ((v initarg v accessor angle-value))
    constructor (make-angle v))
  
  (defmethod initialize-instance ((x <angle>) lst)
    (let ((x (call-next-method)))
      ((setter angle-value) x
       (- (angle-value x) (* (floor (/ (angle-value x) (* 2 pi)))
			     (* 2 pi))))
      x))

  (defmethod binary+ ((x <angle>) (y <angle>))
    (make-angle (+ (angle-value x) (angle-value y))))

  (defmethod binary- ((x <angle>) (y <angle>))
    (make-angle (- (angle-value x) (angle-value y))))
  
  (defmethod binary* ((x <angle>) (y <float>))
    (make-angle (* (angle-value x) y)))

  (defmethod binary/ ((x <angle>) (y <float>))
    (make-angle (/ (angle-value x) y)))

  (defmethod binary* ((y <float>) (x <angle>))
    (make-angle (* (angle-value x) y)))

  (defmethod binary/ ((y <float>) (x <angle>))
    (make-angle (/ (angle-value x) y)))

  (defmethod = ((x <angle>) (y <angle>))
    (= (angle-value x) (angle-value y)))
  
  (defmethod negate ((x <angle>))
    (make-angle (- (angle-value x))))

  (defmethod sin ((x <angle>))
    (sin (angle-value x)))
  (defmethod cos ((x <angle>))
    (cos (angle-value x)))
  (defmethod tan ((x <angle>))
    (tan (angle-value x)))

  (defmethod generic-prin ((x <angle>) s)
    (format s "#<~a rads>" (angle-value x)))
  (defmethod generic-write ((x <angle>) s)
    (format s "#<~a rads>" (angle-value x)))

  (defclass <polar> (<number>)
    ((r initarg r accessor polar-r)
     (theta initarg theta accessor polar-theta))
    constructor (make-polar r theta)
    )

  (defmethod (converter <complex>) ((x <polar>))
    (make-complex (* (cos (polar-theta x))
		     (polar-r x))
		  (* (sin (polar-theta x))
		     (polar-r x))))
  
  (defmethod (converter <polar>) ((x <complex>))
    (let ((x (convert x <real-complex>)))
      (let ((real (real-part x))
	    (imag (imag-part x)))
	(make-polar (sqrt (+ (* real real) (* imag imag)))
		    (cond ((positivep real) 
			   (make-angle (atan (/ imag real))))
			  ((negativep real)
			   (make-angle (+ (atan (/ imag real)) pi)))
			  ((positivep imag) 
			   (make-angle (/ pi 2)))
			  (t (make-angle (/ pi -2))))))))

  (defmethod binary+ ((x <polar>) (y <polar>))
    (convert (binary+ (convert x complex) (convert y complex))
	     polar))

  (defmethod binary- ((x <polar>) (y <polar>))
    (convert (binary- (convert x complex) (convert y complex))
	     polar))

  (defmethod binary* ((x <polar>) (y <polar>))
    (make-polar (binary* (polar-r x) (polar-r y))
		(binary+ (polar-theta x) (polar-theta y))))

  (defmethod binary/ ((x <polar>) (y <polar>))
    (make-polar (binary/ (polar-r x) (polar-r y))
		(binary- (polar-theta x) (polar-theta y))))
  
  (defmethod = ((x <polar>) (y <polar>))
    (and (= (polar-r x) (polar-r y))
	 (= (polar-theta x) (polar-theta y))))
  
  (defmethod lift-numbers ((y <polar>) (x <complex>))
    polar)

  (defmethod lift-numbers ((y <polar>) (x <float>))
    polar)

  (defmethod lift-numbers ((y <polar>) (x <integer>))
    polar)

  (defmethod (converter <polar>) ((x <integer>))
    (make-polar (convert x <double-float>) (make-angle 0)))

  (defmethod (converter <polar>) ((x <float>))
    (make-polar x (make-angle 0)))

  (defmethod generic-prin ((x <polar>) y)
    (format y "#<polar: ~a (~a)>" (polar-r x) (polar-theta x)))
  (defmethod generic-write ((x <polar>) y)
    (format y "#<polar: ~a (~a)>" (polar-r x) (polar-theta x)))

  (defmethod exp ((x <complex>))
    (convert (make-polar (exp (real-part x))
			 (make-angle (imag-part x)))
	     <complex>))
  
  (defmethod exp ((x <polar>))
    (exp (convert x <complex>)))

  (defmethod log ((x <polar>))
    (convert (make-complex (log (polar-r x))
			   (angle-value (polar-theta x)))
	     polar))

  (defmethod log ((x <complex>))
    (convert (log (convert x polar))
	     <complex>))

  (defmethod sin ((x <complex>))
    (/ (- (exp (* i x))
	  (exp (* (- i) x)))
       2))

  (export make-complex i I <complex> <gaussian>)

  ;; end module
  )

