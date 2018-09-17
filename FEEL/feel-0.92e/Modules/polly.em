;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;  EuLisp Module                     Copyright (C) University of Bath 1991  ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;									    ;
; 	C78 Computer Algebra Project I : Polynomial Algebra System	    ;
;									    ;
;	Author : Keith Playford ( ma6kjp )				    ;
;									    ;
;	For : Russell Bradford						    ;
;									    ;
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;									    ;
;	Polynomial Representation					    ;
;									    ;
;	A sparse recursive form ( analogous to that used within REDUCE )    ;
;	is used which is canonical with lexicographic ordering or the       ;
;	variables.							    ;
;									    ;
;	( See the accompanying notes for a more detailed description )	    ;
;									    ;
;        Included in the EuLisp distribution for the sheer heck of it       ;
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

(defmodule polly

  (lists
   list-operators
   streams
   extras
   symbols
   strings
   arith
   ccc
   (except (null) class-names)
   generics
   
   threads
   futures) ()

  ;; Future arith...

  (defmethod binary-plus ((a future-object) b) (binary-plus (futureeval a) b))
  (defmethod binary-plus (b (a future-object)) (binary-plus b (futureeval a)))

  (defun mkterm(var exp coef)
    (cond ((null coef) nil)	
	  ((zerop exp) coef)	
	  (t (cons (cons var exp) coef))))

  (defun cons-pol(term f)
    (cond ((null term) f)
	  ((numberp term) term)
	  (t (cons term f))))

  (defun lead-t(f) (car f))

  (defun exp-t(term) (cdar term))

  (defun exp-l(f) (cdaar f))

  (defun var-t(term) (caar term))

  (defun var-l (f) (caaar f))

  (defun coef-t (term) (cdr term))

  (defun coef-l (f) (cdar f))

  (defun vex-l(f) (caar f))

  (defun remains(f) (cdr f))

  ;; Term ordering...

  (defun orderp (s1 s2) 
    (or (eq s1 s2) (string-lt (symbol-name s1) (symbol-name s2))))

  (defun ordervar(t1 t2)
    (cond ((numberp t1) nil)
	  ((numberp t2) t)
	  ((not (orderp (var-t t2) (var-t t1))))))

  (defun greater-lead-p (vex1 vex2)
    (cond ((eq (car vex1) (car vex2)) (< (cdr vex2) (cdr vex1)))
	  (t (not (orderp (car vex2) (car vex1))))))

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;									    ;
;	Polynomial Addition						    ;
;									    ;
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

  (defun standadd(a b)
    ((lambda(n) (cond ((zerop n) nil)
		      (t n)))
     (+ a b)))

  (defun add-pol(f g)
    (cond ((null f) g)
	  ((null g) f)
	  ((numberp f) (add-con-pol f g))  
	  ((numberp g) (add-con-pol g f))  
	  ((greater-lead-p (vex-l f) (vex-l g)) 
	     (cons (lead-t f) (add-pol (remains f) g)))
	  ((greater-lead-p (vex-l g) (vex-l f)) 
	     (cons (lead-t g) (add-pol f (remains g))))
	  (t (cons-pol (add-term (lead-t f) (lead-t g)) 
		       (add-pol (remains f) (remains g))))))

  (defun add-con-pol(c pol)
    (cond ((null pol) c)
	  ((numberp pol) (standadd c pol))
	  (t (cons (lead-t pol) (add-con-pol c (remains pol))))))

  (defun add-term(t1 t2)
    (cond ((numberp t1) (standadd t1 t2))
	  (t (mkterm (var-t t1) (exp-t t1) 
		     (add-pol (coef-t t1) (coef-t t2))))))

  (defmethod binary-plus ((a pair) (b pair)) (add-pol a b))
  (defmethod binary-plus ((a pair) (b number)) (add-pol a b))
  (defmethod binary-plus ((a number) (b pair)) (add-pol a b))

  (export add-pol)

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;									    ;
;	Polynomial Multiplication					    ;
;									    ;
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

  (defun multi-pol(f g)
    (cond ((or (null f) (null g)) nil)
	  ((numberp f) (multi-term-pol f g)) 
	  ((numberp g) (multi-term-pol g f))
	  (t (add-pol (list (multi-term (lead-t f) (lead-t g)))
		      (add-pol (multi-term-pol (lead-t f) (remains g))
			       (add-pol (multi-term-pol (lead-t g) (remains f))
					(multi-pol (remains f) 
						   (remains g))))))))

  (defun multi-term-pol(term pol)
    (cond ((null pol) nil)
	  ((and (numberp term) (numberp pol)) (* term pol))
	  ((numberp pol) (cons-pol (multi-term term pol) nil))
	  (t (cons-pol (multi-term term (lead-t pol)) 
		       (multi-term-pol term (remains pol))))))

  (defun multi-term(t1 t2)
    (cond ((and (numberp t1) (numberp t2)) (* t1 t2))
	  ((ordervar t1 t2) (mkterm (var-t t1) (exp-t t1)
				    (multi-term-pol t2 (coef-t t1))))
	  ((ordervar t2 t1) (mkterm (var-t t2) (exp-t t2)
				    (multi-term-pol t1 (coef-t t2))))
	  (t (mkterm (var-t t1) (+ (exp-t t1) (exp-t t2))
		     (multi-pol (coef-t t1) (coef-t t2))))))

  (defmethod binary-times ((a pair) (b pair)) (multi-pol a b))
  (defmethod binary-times ((a pair) (b number)) (multi-pol a b))
  (defmethod binary-times ((a number) (b pair)) (multi-pol a b))

  (export multi-pol)

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;									    ;
;	Polynomial Subtraction						    ;
;									    ;
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

  (defun sub-pol(f g) (add-pol f (multi-pol -1 g)))

  (defmethod binary-difference ((a pair) (b pair)) (sub-pol a b))
  (defmethod binary-difference ((a pair) (b number)) (sub-pol a b))
  (defmethod binary-difference ((a number) (b pair)) (sub-pol a b))

  (export sub-pol)

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;									    ;
;	Polynomial Exponentiation					    ;
;									    ;
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

; Raises f to the power n ( n a natural number ) using multi-pol by the     ;
; method of repeated squaring.						    ;

  (defun raise-pol(f n) 
    (cond ((< n 0) (error "Attempt to raise poly to a negative power"))
	  (t (raise-main f n))))

  (defun raise-main(f n)
    (cond ((zerop n) 1)
	  ((zerop (remainder n 2)) (raise-main (multi-pol f f) (quotient n 2)))
	  (t (multi-pol f (raise-main (multi-pol f f) (quotient n 2))))))

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;									    ;
;	Polynomial Pseudo-division					    ;
;									    ;
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

; Tests for unacceptable cases and call the main routine.                   ;

  (defun psdiv-pol(u v)
    (cond ((null v) (error "Attempt to pseudo dividefun by zero"))
	  ((or (null u) (not (consp v)) (not (consp u))) nil)
	  ((neq (var-l u) (var-l v)) nil)
	  ((greater-lead-p (vex-l v) (vex-l u)) nil)
	  (t (divmain u v (exp-l u)))))
 
; Recursively constructs the quotient.					    ;
; Keeps track of the order u should be in case of cancellation.		    ;

  (defun divmain(u v m)
    (let ((qterm (get-qterm u v m))
	  (new-u (get-newu u v m)))
      (cond ((= m (exp-l v)) qterm)
	    (t (cons-pol qterm (divmain new-u v (- m 1)))))))

; Returns the pseudo remainder of u v (checking for unacceptable cases)	    ;

  (defun psrem-pol(u v)
    (cond ((null v) (cerror "Attempt to pseudodivide by zero"))
	  ((or (null u) (numberp u) (numberp v)) u)
	  ((neq (var-l u) (var-l v)) u)
	  ((greater-lead-p (vex-l v) (vex-l u)) u)
	  (t (remmain u v (exp-l u)))))

; Works as for pseudo division but does not keep track of the quotient	    ;

  (defun remmain(u v m)
    (cond ((< m (exp-l v)) u)
	  (t (remmain (get-newu u v m) v (- m 1)))))

; Constructs the current quotient term via the given algorithm              ;

  (defun get-qterm(u v m)
    (let ((k (- m (exp-l v))))
      (mkterm (var-l v) k (multi-pol (coefn u m (var-l v))
				     (raise-pol (coef-l v) k)))))

; Constructs the new remainder polynomial via the given algorithm           ;

  (defun get-newu(u v m)
    (let ((k (- m (exp-l v))))
      (sub-pol (multi-pol (coef-l v) u)
	       (multi-pol (multi-term-pol (mkterm (var-l v) k 1) v)
			  (coefn u m (var-l v))))))

; Returns the var to the power n th coefficient of f                        ;

  (defun coefn(f n var)
    (cond ((null f) nil)
	  ((eq var (var-l f)) (cond ((= (exp-l f) n) (coef-l f))
				    (t nil)))
	  ((zerop n) f)
	  (t nil)))

; The given example polynomials.					    ;

(setq r1 '(((x . 5) . 1) ((x . 4) ((y . 1) . 2)) ((x . 2) . 1) ((y . 2) . 1)))
(setq r2 '(((x . 3) ((y . 2) . 1)) ((x . 1) ((y . 1) . 4)) . 1))

; Power product ordering.

  (defun power-product-degree (pp)
    (cond ((or (numberp pp) (null pp)) 0)
	  (t (+ (exp-l pp) (power-product-degree (coef-l pp))))))

  (defun power-product-coef (pp)
    (cond ((null pp) 0)
	  ((numberp pp) pp)
	  (t (power-product-coef (coef-l pp)))))

  (defun power-product-without-coef (pp)
    (cond ((null pp) 0)
	  ((numberp pp) 1)
	  (t (list (mkterm (var-l pp) (exp-l pp)
			   (power-product-without-coef (coef-l pp)))))))

; Need variable dicrimination

  (defun power-product-lt (pp1 pp2)
    (< (power-product-degree pp1) (power-product-degree pp2)))

  (defun leading-power-product-with-coef (p)
    (cond 
      ((null p) ())
      ((numberp p) 1)
      (t
        (let ((max ()))
	  (mapc
	   (lambda (term)
	     (let ((sub (* (list term)
				   (leading-power-product-with-coef
				     (coef-t term)))))
	       (when (power-product-lt max sub) (setq max sub))))
	   p)
	  max))))

  (defun leading-power-product-coef (p)
    (power-product-coef (leading-power-product-with-coef p)))

  (defun leading-power-product (p)
    (power-product-without-coef (leading-power-product-with-coef p)))

  (defun unity-check (v e c)
    (if (zerop e) c (list (mkterm v e c))))

  (defun simple-power-product-quotient (pp1 pp2)
    (cond ((numberp pp1) 1)
	  ((numberp pp2) pp1)
	  ((eq (var-l pp1) (var-l pp2))
	    (unity-check
	      (var-l pp1) (- (exp-l pp1) (exp-l pp2))
	      (simple-power-product-quotient (coef-l pp1) 
					     (coef-l pp2))))
	  (t
	    (list (mkterm (var-l pp1) (exp-l pp1)
			  (simple-power-product-quotient (coef-l pp1) pp2))))))

  (defun power-product-very-simply-divisible-p (pp1 pp2)
    (cond ((and (numberp pp1) (numberp pp2)) (cons pp1 pp2))
	  ((or (numberp pp1) (numberp pp2)) ())
	  ((and (eq (var-l pp1) (var-l pp2)) (= (exp-l pp1) (exp-l pp2)))
	    (power-product-very-simply-divisible-p (coef-l pp1) (coef-l pp2)))
	  (t ())))

  (defun power-product-lcm (pp1 pp2)
    (cond ((null pp1) pp2)
	  ((null pp2) pp1)
	  ((and (numberp pp1) (numberp pp2)) (lcm pp1 pp2))
	  ((numberp pp2)
	    (list (mkterm (var-l pp1) (exp-l pp1)
			  (power-product-lcm (coef-l pp1) pp2))))
	  ((numberp pp1)
	    (list (mkterm (var-l pp2) (exp-l pp2)
			  (power-product-lcm (coef-l pp2) pp1))))
	  ((eq (var-l pp1) (var-l pp2))
	    (list (mkterm (var-l pp1) (max (exp-l pp1) (exp-l pp2))
			  (power-product-lcm (coef-l pp1) (coef-l pp2)))))   
	  ((greater-lead-p (vex-l pp1) (vex-l pp2))
	    (list (mkterm (var-l pp1) (exp-l pp1)
			  (power-product-lcm (coef-l pp1) pp2))))
	  ((greater-lead-p (vex-l pp2) (vex-l pp1))
	    (list (mkterm (var-l pp2) (exp-l pp2)
			  (power-product-lcm (coef-l pp2) pp1))))))

  (defun s-pol (p1 p2)
    (let* ((lpp1 (leading-power-product p1))
	   (lpp2 (leading-power-product p2))
	   (lpc1 (leading-power-product-coef p1))
	   (lpc2 (leading-power-product-coef p2))
	   (lcpp (power-product-lcm lpp1 lpp2)))
      (sub-pol
        (multi-pol
	  lpc1
	  (multi-pol
	    (simple-power-product-quotient lcpp lpp1) p1))
	(multi-pol
	  lpc2
	  (multi-pol
	    (simple-power-product-quotient lcpp lpp2) p2)))))
	    
  (setq p1 (leading-power-product r1))
  (setq p2 (leading-power-product r2))

  (export s-pol leading-power-product power-product-very-simply-divisible-p
	  power-product-lcm simple-power-product-quotient
	  leading-power-product-with-coef)
	  
)

