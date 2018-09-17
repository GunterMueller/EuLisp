;; Eulisp Module
;; Author: pab
;; File: out-mod.em
;; Date: Mon Nov 23 17:11:52 1992
;;
;; Project:
;; Description: 
;;   Fast reading/linking and writing
;;   or interface files

(defmodule out-fast
  (standard0
   list-fns

   comp-utl
   comp-defn
   )
  ()

  (defclass Compiler-Error (<condition>)
    ()
    metaclass <condition-class>)

  ;; Format is:
  ;; code-length
  ;; nslots
  ;; statics
  ;; bytecodes
  ;; a bytecode is: 
  ;;   byte
  ;;   (nonlocal . value)
  (export write-fastbytes)

  (defun write-fastbytes (unit)
    (let ((file (open (fast-file-name (car (sc-names unit))) 'output t)))
      (write (sc-dependencies unit) file)
      (newline file)
      (write (sc-nslots unit) file)
      (newline file)
      (write (sc-length unit) file)
      (newline file)
      (write (sc-statics unit) file)
      (newline file)
      (write-bytecode-list unit file)
      (close file)))

  (defconstant big-arg-symbol (the-long-handle))
  (defconstant link-symbol (the-link-handle))
  (defconstant local-symbol (the-local-handle))
  (defconstant static-symbol (the-static-handle))

  (defun write-bytecode-list (unit file)
    (let ((local-alloc (mk-local-alloc (first-posn unit))))
      (mapc (lambda (i)
	      (write-instruction i local-alloc file))
	    (sc-code unit))))
  
  (defun first-posn (unit)
    (+ 1 (list-length (sc-statics unit))))

  (defun write-instruction (lst local-alloc file)
    (cond ((null lst) nil)
	  ((atom (car lst))
	   (write (car lst) file)
	   (newline file)
	   (write-instruction (cdr lst) local-alloc file))
	  ((eq (caar lst) local-symbol)
	   (write-bignum (local-alloc (cdr (car lst))) file)
	   (newline file)
	   (write-instruction (cdr lst) local-alloc file))
	  ((eq (caar lst) big-arg-symbol)
	   (write-bignum (cadar lst) file)
	   (newline file)
	   (write-instruction (cdr lst) local-alloc file))
	  ((eq (caar lst) static-symbol)
	   (write-bignum (+ (cdr (car lst)) 1) file)
	   (newline file)
	   (write-instruction (cdr lst) local-alloc file))
	  ((not (eq (caar lst) link-symbol))
	   (error "unknown object type" Compiler-Error 'error-value (car lst)))
	  ((eq (cadar lst) local-symbol)
	   (error "Can't deal with non-optimised code" Compiler-Error 'error-value (car lst)))
	  (t (write (cdar lst) file)
	     (newline file)
	     (write-instruction (cdr lst) local-alloc file))))

  (defun mk-local-alloc (start)
    (let ((count start)
	  (finder (mk-finder)))
      (lambda (name)
	(let ((cached-name (finder name)))
	  (if (null cached-name)
	      (progn (let ((this-val count))
		       (setq count (+ count 1))
		       ((setter finder) name this-val)
		       this-val))
	    cached-name)))))
  
  (defun space (file)
    (prin " " file))

  (defun write-bignum (n file)
    (mapc (lambda (x)
	    (write x file) (space file))
	  (int2bytes n)))

  ;; making 4 bytes from integers.

  (defun int2bytes (x)
    (let ((sign (< x 0))
	  (val (abs x)))
      (let* ((v1 (/ val 256))
	     (v2 (/ v1 256))
	     (v3 (/ v2 256)))
	(list (modulo v2 256)
	      (modulo v1 256)
	      (modulo val 256)
	      (if sign 1 0)))))
  
  ;; end module
  )
