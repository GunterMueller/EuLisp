;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;  EuLisp Module                     Copyright (C) University of Bath 1991  ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; -*- Mode: LISP; Package: random; Syntax: Common-lisp; Base: 10.;  -*- 

;;;%Header
;;;----------------------------------------------------------------------------
;;;
;;; Pseudo-random number generator
;;;
;;; Author: Chris McConnell, ccm@cs.cmu.edu
;;;
;;; This file implements a portable pseudo-random number generator for
;;; Common LISP.  It has been converted from a C program that was
;;; converted from a FORTRAN program.  I did not pick the variable
;;; names or pretend to have figured out how it works.  The
;;; correctness of the generator can be verified by the TEST function
;;; at the end of the file.
;;;
;;; Original C header:
;;;
;;;    This is the random number generator proposed by George Marsaglia in
;;;    Florida State University Report: FSU-SCRI-87-50
;;;
;;;    This Random Number Generator is based on the algorithm in a FORTRAN
;;;    version published by George Marsaglia and Arif Zaman, Florida State
;;;    University; ref.: see original comments below.
;;; 
;;;    At the fhw (Fachhochschule Wiesbaden, W.Germany), Dept. of Computer
;;;    Science, we have written sources in further languages (C, Modula-2
;;;    Turbo-Pascal(3.0, 5.0), Basic and Ada) to get exactly the same test
;;;    results compared with the original FORTRAN version.
;;;                                                          April 1989
;;; 
;;;                                         Karl-L. Noell <NOELL@DWIFH1.BITNET>
;;;                                    and  Helmut  Weber <WEBER@DWIFH1.BITNET>
;;;
;;; Eulisp'ed: PAB

(defmodule random 
  (eulisp0 loops) ()
  ()
;; Extra define
(defconstant mod remainder)
(defun trunc (a . b)
  (cond ((null b) (floor a))
	(t (floor (/ a (car b))))))
(defmethod floor ((x <integer>)) x)
(defun minusp (a)
  (< a 0))

;;;%globals
(deflocal *state* nil) ;; "the default state structure."

;;;%%state 
(defstruct state ()
  ;;  "this contains random state for a state.  the names of slots are the
  ;; same as the variable names in the orginal program."
  ((ij initform 1802 initarg ij accessor state-ij)
   (kl initform 9373 initarg kl accessor state-kl)
   (u initform (make-vector 97 0) initarg u accessor state-u)
   (c initform (/ 362436.0 16777216.0) initarg c accessor state-c)
   (cd initform (/ 7654321.0 16777216.0) initarg cd accessor state-cd)
   (cm initform (/ 16777213.0 16777216.0) accessor state-cm)
   (i97 initform 96 accessor state-i97)
   (j97 initform 32 accessor state-j97))
  constructor make-state)

;;;
(defun print-state (state stream level)
  (declare (ignore level))
  (format stream "#<state ~a ~a>" (state-ij state) (state-kl state)))

;;; %interface
;;; this is the initialization routine for the random number generator  
;;; note: the seed variables can have values between:    0 <= ij <= 31328
;;;                                                      0 <= kl <= 30081
;;; the random number sequences created by these two seeds are of sufficient
;;; length to complete an entire calculation with. for example, if sveral
;;; different groups are working on different parts of the same calculation,
;;; each group could be assigned its own ij seed. this would leave each group
;;; with 30000 choices for the second seed. that is to say, this random
;;; number generator can create 900 million different subsequences -- with
;;; each subsequence having a length of approximately 10^30.
;;; 
;; (defun seed-state (&optional (ij (mod (get-internal-real-time) 31329))
;;			     (kl (mod (get-internal-real-time) 30082))))
(defun seed-state (ij kl)
  ;;  "given the seed values 0 <= ij <= 31328 and 0 <= kl <= 30081,
  ;; generate a state structure, set it as the default state and return it."
  ;; (declare (optimize (speed 3) (safety 0)))
  (let* ((state (make-state 'ij ij 'kl kl))
	 (vector (state-u state))
	 (i (+ (mod (trunc ij 177) 177) 2))
	 (j (+ (mod ij 177) 2))
	 (k (+ (mod (trunc kl 169) 178) 1))
	 (l (mod kl 169))
	 (ii 0))
    ;;(declare (type fixnum i j k l)
    ;; (type (simple-vector 97) vector))
    (while (< ii 97)
      (let ((s 0.0)
	    (t1 0.5)
	    (jj 0))
	;;(declare (type single-float s t1))
	(while (< jj 24)
	     (let ((m (mod (* (mod (* i j) 179) k) 179)))
	       ;; (declare (type fixnum m))
	       (setq i j)
	       (setq j k)
	       (setq k m)
	       (setq l (mod (+ (* 53 l) 1) 169))
	       (if  (not (< (mod (* l m) 64) 32))
		   (setq s (+ s t1))
		 nil)
	       (setq t1 (* 0.5 t1)))
	     (setq jj (+ jj 1)))
	((setter vector-ref) vector ii s))
      (setq ii (+ ii 1)))
    (setq *state* state)))

;;;
(defun random-one (state)
  ;; "return a random value between 0.0 and 1.0 from state."
  ;; (declare (optimize (speed 3) (safety 0)))
  (let* ((u (state-u state))
	 (uni (- (vector-ref u (state-i97 state))
		 (vector-ref u (state-j97 state)))))
    ;;(declare (type simple-vector u)
    ;;         (type single-float uni))
    (when (minusp uni)
	  (setq uni (+ uni 1)))
    ((setter vector-ref) u (state-i97 state) uni)
    ((setter state-i97) state (- (state-i97 state) 1))
    (when (minusp (state-i97 state))
	  ((setter state-i97) state 96))
    ((setter state-j97) state (- (state-j97 state) 1))
    (when (minusp (state-j97 state))
	  ((setter state-j97) state 96))
    ((setter state-c) state
     (- (state-c state) (state-cd state)))
    (when (minusp (state-c state))
	  ((setter state-c) state
	   (+ (state-c state) (state-cm state))))
    (setq uni (- uni (state-c state)))
    (if (minusp uni)
	(setq uni (+ uni 1))
      nil)
    uni))

;;;
(defun random-float (n state)
  ;;"return a random float between 0 and n.
  ;; use seed-state to initialize a pseudo-random sequence."
  ;; (declare (optimize (speed 3) (safety 0)))
  (* n (random-one state)))

;;;
(defun random-integer (n &optional (state *state*))
  ;; "return a random integer between 0 and n.
  ;; use seed-state to initialize a pseudo-random sequence."
  (declare (optimize (speed 3) (safety 0)))
  (trunc (* n (random-one state))))

;;;
(defun random-number (n &optional (state *state*))
  ;; "return a random number between 0 and n with the same type as n.
  ;; use seed-state to initialize a pseudo-random sequence."
  ;; (declare (optimize (speed 3) (safety 0)))
  (if (floatp n)
      (random-float n state)
      (random-integer n state)))
		      
;;;
(defun random-range (min max &optional (state *state*))
  ;; "return a number between min and max with the same type.
  ;; use seed-state to initialize a pseudo-random sequence."
  (declare (optimize (speed 3) (safety 0)))
  (let ((number (+ min (* (- max min) (random-one state)))))
    (if (floatp min)
	number
	(trunc number))))
   
;;;%test code
(defun test ()
  ;; "test the random number generator.  it should print out n = n t six
  ;; times if it works correctly."
  (let ((state (seed-state 1802 9373))
	(x 0))
    (while (< x 20000)
      (format t "~a: ~a~% " x (random-one state))
      (setq x (+ 1 x)))
    (setq x 0)
    (while (< x 6)
      (let ((value (round (* 4096 4096 (random-one state))))
	    (should-be (vector-ref #(6533892 14220222 7275067
					     6172232 8354498 10633180) x)))
	(format t "~&~a = ~a ~a"
		value should-be (= value should-be)))
      (setq x (+ 1 x)))))

;;;%initial seed -- numbers chosen by pab.
;;(setq *state* (seed-state 23465 987458))

;; export stuff
(export *state*
	seed-state random-one random-float random-integer random-range)

;; close module
)
