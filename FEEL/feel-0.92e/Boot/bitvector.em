;; Eulisp Module
;; Author: pab
;; File: bitvectors.em
;; Date: Tue Jun 29 22:57:10 1993
;;
;; Project:
;; Description: 
;;

(defmodule bitvector
  (bit-vectors
   telos1
   macros0
   extras0
   defs
   init
   gens
   )
  ()
  
  (defclass <bit-vector> (<sequence>)
    ((self accessor bits))
    direct-initargs (size fill-value)
    )

  (defmethod initialize ((x <bit-vector>) args)
    (let ((new (call-next-method))
	  (size (scan-args 'size args required-argument))
	  (fill-val (scan-args 'fill-value args null-argument)))
      ((setter bits) new (primitive-make-bit-vector size))
      (when (eql fill-val 1)
	(fill new 1 0 (- size 1)))
      new))
  
  (defmethod element ((bv <bit-vector>) s)
    (primitive-bit-vector-ref (bits bv) s))

  (defmethod (setter element) ((bv <bit-vector>) s v)
    ((setter primitive-bit-vector-ref) (bits bv) s v))
  
  (defmethod size ((bv <bit-vector>))
    (bit-vector-length (bits bv)))

  (defmethod current-key ((c <bit-vector>) (s <fixint>)) s)
  
  (defmethod clone ((x <bit-vector>))
    (make <bit-vector> 'size x))

  ; hmmm...not the expected behaviour
  (defmethod (converter <bit-vector>) ((x <fixint>))
    (let ((tmp (make <bit-vector> 'size 1)))
      ((setter bits) tmp
       (integer-to-bitvector x))
      tmp))
  
  (defmethod generic-prin ((bv <bit-vector>) s)
    (format s "#<~a (~a) "
	    (symbol-unbraced-name (class-name (class-of bv)))
	    (size bv))
    (do (lambda (x) (generic-prin x s)) bv)
    (format s ">")
    bv)

  ;; end module
  )
