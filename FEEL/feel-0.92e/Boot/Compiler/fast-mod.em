;; Eulisp Module
;; Author: pab
;; File: fast-mod.em
;; Date: Mon Nov 23 17:11:52 1992
;;
;; Project:
;; Description: 
;;   Fast reading/linking and writing
;;   or interface files

(defmodule fast-mod
  (standard0
   list-fns
         
   )
  ()

  ;; Format is:
  ;; code-length
  ;; nslots
  ;; statics
  ;; bytecodes
  ;; a bytecode is: 
  ;;   byte
  ;;   (nonlocal . value)

  (defun write-file-unit (unit)
    (let ((file (open (get-fast-name unit) 'input t)))
      (write (sc-nslots unit) file)
      (newline file)
      (write (sc-length unit) file)
      (write (sc-statics unit) file)
      (write-bytecode-list (sc-code unit) file)
      (close file)))

  (defun write-bytecode-list (sc-code file)
    (let ((local-alloc (mk-local-alloc)))
      (mapc (lambda (i) (write-instructions i local-alloc))
	    sc-code)))
  
  (defun write-instruction (lst local-alloc)
    (cond ((null lst) nil)
	  ((atom (car lst))
	   (write (car lst) file)
	   (newline file))
	  ((eq (car lst) local-symbol)
	   (write-bignum (get-local-name find-local (cdr (car lst)))))
	  ((eq (car lst) big-arg-symbol)
	   (write-bignum (cadar lst)))
	  ((not (eq (car lst) link-symbol))
	   (error "unknown object type" Compiler-Error 'error-value (car lst)))
	  ((eq (cadar lst) local-symbol)
	   (error "Can't deal with non-optimised code" Compiler-Error 'error-value (car lst)))
	  (t (write (cdar lst)))))

  (defun mk-local-alloc (unit)
    (let ((count (list-length (sc-statics unit)))
	  (finder (mk-finder)))
      (lambda (name)
	(let ((cached-name (finder name)))
	  (if (null (cached-name))
	      (progn (let ((this-val count))
		       (setq count (+ count 1))
		       ((setter finder) name this-val)
		       this-val))
	    cached-name)))))
  
  
  ;; end module
  )
