;; Eulisp Module
;; Author: pab
;; File: numbers.em
;; Date: Fri Dec  4 17:12:39 1992
;;
;; Project:
;; Description: 
;;

(defmodule numbers
  (extras0
   macros0
   init
   )
  ()
  
  (export lift-numbers min max lift pi = abs zerop
	  positivep negativep evenp oddp
	  quotient remainder modulo
	  most-positive-double-float least-positive-double-float
	  least-negative-double-float most-negative-double-float
	  most-positive-fixed-precision-integer
	  most-negative-fixed-precision-integer)
  ;;
  ;; Simple functions
  ;;

  (defun max (n1 . rest)
    (labels ((check (max lst)
		    (if (null lst) max
		      (if (< max (car lst))
			  (check (car lst) (cdr lst))
			(check max (cdr lst))))))
	    (check n1 rest)))

  (defun min (n1 . rest)
    (labels ((check (min lst)
		    (if (null lst) min
		      (if (< (car lst) min)
			  (check (car lst) (cdr lst))
			(check min (cdr lst))))))
	    (check n1 rest)))
  
  ;; 
  ;; additional generics
  ;;

  ;; abs
  (defconstant abs (make <generic-function> 
				'lambda-list '(a)
				'argtype 1
				'name 'abs
				'method-class <method>))

  ;; zerop
  
  (defconstant zerop (make <generic-function> 
				    'lambda-list '(a)
				    'argtype 1
				    'name 'zerop
				    'method-class <method>))

  ;; Lift numbers
  
  (defconstant lift-numbers (make <generic-function>
					   'lambda-list '(a b)
					   'argtype 2
					   'name 'lift-numbers
					   'method-class <method>))
  ;; elt functions...


  ;; Float methods
  (add-method binary+ (make <method> 
				     'signature (list <double-float> <double-float>)
				     'function binary+_Float))
  (add-method binary- (make <method> 
			    'signature (list <double-float> <double-float>)
				     'function binary-_Float))
  (add-method binary* (make <method> 
				     'signature (list <double-float> <double-float>)
				     'function binary*_Float))
  (add-method binary/ (make <method> 
				     'signature (list <double-float> <double-float>)
				     'function binary/_Float))
  (add-method binary< (make <method> 
				     'signature (list <double-float> <double-float>)
				     'function binary<_Float))

  (add-method = (make <method>
		      'signature (list <double-float> <double-float>)
		      'function binary=_Float))
  
  (add-method negate (make <method>
			   'signature (list <double-float>)
			   'function negate-float))

  ;; Number methods
  
  
  (add-method binary+
	      (make <method> 'signature (list <number> <number>)
			     'function 
			     (method-lambda (x y)
					    (lift binary+ x y))))
  (add-method binary-
	      (make <method> 'signature (list <number> <number>)
			     'function 
			     (method-lambda ( x y)
					    (lift binary- x y))))
  (add-method binary*
	      (make <method> 'signature (list <number> <number>)
			     'function 
			     (method-lambda ( x y)
					    (lift binary* x y))))
  (add-method binary/
	      (make <method> 'signature (list <number> <number>)
			     'function 
			     (method-lambda ( x y)
					    (lift binary/ x y))))
  (add-method binary<
	      (make <method> 'signature (list <number> <number>)
			     'function 
			     (method-lambda ( x y)
					    (lift binary< x y))))
  (add-method =
	      (make <method> 'signature (list <number> <number>)
			     'function 
			     (method-lambda ( x y)
					    (lift = x y))))
  
  (add-method lift-numbers
	      (make <method> 
			     'signature (list <number> <number>)
			     'function (method-lambda ( x y) nil)))

  (add-method lift-numbers (make <method> 
				 'signature (list <fixint> <double-float>)
				 'function (method-lambda ( x y) 
							  <double-float>)))

  (defun lift (fn x y)
    (let ((class (or (lift-numbers x y)
		     (lift-numbers y x)
		     (error "Can't lift numbers" <Internal-Error>
			    'error-value (cons x y)))))
      (add-method fn 
		  (make <method> 
			'signature (list (class-of x) (class-of y))
			'function (cond ((eq x class)
					 (method-lambda (x y)
							(fn  x (convert y class))))
					((eq y class)
					 (method-lambda (x y)
							(fn (convert x class) y)))
					(t (method-lambda (x y)
							  (fn (convert x class)
							      (convert y class)))))))
      (fn (convert x class)
	  (convert y class))))


  ;; Elt. Functions
  
  (define-simple-generic sin (<double-float>) sin-float)
  (define-simple-generic cos (<double-float>) cos-float)
  (define-simple-generic tan (<double-float>) tan-float)
  (define-simple-generic asin (<double-float>) asin-float)
  (define-simple-generic acos (<double-float>) acos-float)
  (define-simple-generic atan (<double-float>) atan-float)
  (define-simple-generic log (<double-float>) log-float)
  (define-simple-generic log10 (<double-float>) log10-float)
  (define-simple-generic sqrt (<double-float>) sqrt-float)
  (define-simple-generic exp (<double-float>) exp-float)
  (define-simple-generic sinh (<double-float>) sinh-float)
  (define-simple-generic cosh (<double-float>) cosh-float)
  (define-simple-generic tanh (<double-float>) tanh-float)
  (define-simple-generic asinh (<double-float>) asinh-float)
  (define-simple-generic acosh (<double-float>) acosh-float)
  (define-simple-generic round (<double-float>) round-float)
  (define-simple-generic floor (<double-float>) floor-float)
  (define-simple-generic ceiling (<double-float>) ceiling-float)

  (define-simple-generic pow (<double-float> <double-float>) pow-float-float)

  (add-method pow (make
		   <method>
		   'signature (list <fixint> <double-float>)
		   'function pow-integer-float))
  (add-method pow (make
		   <method>
		   'signature (list <double-float> <fixint>)
		   'function pow-float-integer))
  (add-method pow (make
		   <method>
		   'signature (list <fixint> <fixint>)
		   'function pow-integer-integer))

  ;; constants
  (defconstant zero 0)

  (defconstant true-pred
    (method-lambda (x) t))

  (defconstant false-pred
    (method-lambda (x) nil))

  ;; floatp
  (define-simple-generic floatp (<double-float>) true-pred)
  (add-method floatp (make <method> 
			   'signature (list <object>) 
			   'function false-pred))
  ;; numberp
  (define-simple-generic numberp (<number>) true-pred)
  (add-method numberp (make <method>
			    'signature (list <object>) 
			    'function false-pred))

  ;; integerp
  (define-simple-generic integerp (<integer>) true-pred)
  (add-method integerp (make <method>
			     'signature (list <object>) 
			     'function false-pred))
  
  ;; equal

  (add-method equal (make <method> 
				   'signature (list <number> <number>)
				   'function (method-lambda ( a b) (= a b))))

  
  ;; positivep
  ;; Should be generic...
  (defun positivep (x)
    (< 0 x))
  
  ;; negativep
  (defun negativep (x)
    (< x 0))
  
  ;; zerop 
  (add-method zerop
	      (make <method> 
		    'signature (list <number>)
		    'function (method-lambda (c)
					     (= c zero))))
  
  (add-method zerop
	      (make <method> 'signature (list <fixint>)
		    'function (method-lambda (c)
					     (eq c zero))))

  ;; evenp
  (defun evenp (x)
    (zerop (remainder x 2)))

  ;; oddp
  (defun oddp (x)
    (not (evenp x)))

  ;; quotient
  (define-simple-generic quotient (<fixint> <fixint>) quotient-integer)
  
  ;; XX: There is a difference (sign) between mod and remainder.
  ;;     Would someone fill the details in...

  ;;remainder 
  (define-simple-generic remainder (<fixint> <fixint>) remainder-integer)

  ;;modulo
  (define-simple-generic modulo (<fixint> <fixint>) modulo-integer)

  (add-method quotient (make <method>
			     'signature (list <number> <number>)
			     'function (method-lambda ( x y)
						      (lift quotient x y))))
  (add-method remainder (make <method>
				       'signature (list <number> <number>)
				       'function (method-lambda (x y)
								(lift remainder x y))))

  ;;convert float->int.

  ;; convert int-> float
  (add-method (converter <double-float>)
	      (make <method> 
		    'signature (list <integer>)
		    'function convert-integer-float))


  ;; abs 
  
  (add-method abs 
	      (make <method> 
			     'signature (list <number>)
			     'function
			     (method-lambda (c)
			       (if (positivep c) c
				 (negate c)))))

  
  ;; truncate

  ;; round

  ;; end module
  )
