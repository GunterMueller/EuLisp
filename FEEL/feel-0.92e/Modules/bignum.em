;; Eulisp Module
;; Author: JPB
;; File: bignum.em
;; Date: 20 May 93
;;
;; Project:  Feel interpreted module
;; Description: A Bignum package
;;

;; defmodule, then the name of the module (must be the same as the file)

(defmodule bignum

  ;; what modules to import
  ;; eulisp0 is most useful things
  ;; describe allows us to find out about objects

  (eulisp0 describe)

  ;; The syntax section. This can be nil for us

  () 
	
  ;; Now the code. First define a bignum class, with a single slot which will
  ;; be the list of digits for this number. It is a sub-class of number.
  ;; The slot is called bignum-value, with accessor bignum-ref and predicate
  ;; bignump.

  (defclass bignum
    (<number>)

    ((bignum-value
      initarg  bignum-initvalue
      accessor bignum-ref
    ))

    constructor (internal-make-bignum bignum-initvalue)
    predicate   bignump
  )
    

  ;; Now write our own make-bignum, which takes an integer and turns it into a
  ;; list of digits for use with internal-make-bignum

  (defun make-bignum (n)
    (internal-make-bignum (integer-to-list n)))


  ;; Routine to take a number and return it as a list of digits. Note that the
  ;; LS digit is at the front.

  (defun integer-to-list (n)
    (if (zerop n)
      nil
      (cons (remainder n 10) (integer-to-list (/ n 10)))) )

  ;; Now a method to add to binary+, so that it works with two bignums. This
  ;; calls add-list, which takes two lists and a carry to add.

  (defmethod binary+ ((a bignum) (b bignum))
    (internal-make-bignum (add-list (bignum-ref a) (bignum-ref b) 0)))


  ;; A function to add two numbers represented as a list of digits (LS digit
  ;; first) and construct a list of the result. The third argument is any
  ;; carry to be added in.

  (defun add-list (a b c) (cond
    ((null a)
      (if (zerop c)
        b
        (add-list (list c) b 0)))
    ((null b)
      (if (zerop c)
        a
        (add-list a (list c) 0)))
    (t (let ((tmp (+ (car a) (car b) c)))
      (cons
        (remainder tmp 10)
        (add-list (cdr a) (cdr b) (/ tmp 10)))) )))


  ;; A method to add to generic-write, so that it prints bignums

  (defmethod generic-write ((x bignum) s)
    (write-list-number (bignum-ref x) s))


  ;; A routine to write a number represented as a list, x, on a stream, s.

  (defun write-list-number (x s)
    (cond
      (x (progn
        (write-list-number (cdr x) s)
        (write (car x) s)))) )


  ;; A method to add to generic-prin, so that it prints bignums
      
  (defmethod generic-prin ((x bignum) s)
    (write-list-number (bignum-ref x) s))


  ;; Now a fast version of fibonacci, which will be suitable for trying out
  ;; the bignum package.

  (setq fib-tab (make <table> 'comparator = 
		      'hash-function (standard-hash-function)))

  ((setter table-ref) fib-tab 1 (make-bignum 1))
  ((setter table-ref) fib-tab 2 (make-bignum 1))

  (defun fib (n)
    (let ((val (table-ref fib-tab n)))
    (cond
      (val val)
      (t (setq val (+ (fib (- n 1)) (fib (- n 2))))
         ((setter table-ref) fib-tab n val)
         val))))

  '(defmethod generic-hash ((n <integer>))
    n)

  ;; Anything we wish to export

  ;; end module

)
