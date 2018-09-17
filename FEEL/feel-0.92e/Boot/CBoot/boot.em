;; Eulisp Module
;; Author: pab
;; File: boot.em
;; Date: Sat Apr 11 19:05:09 1992
;;
;; Project:
;; Description:
;;   Module that contains functions necessary to boot
;;   the bytecode system successfully.
;;
;; Defines some useful functions, then
;; adds code suffient to create and install bindings into
;; modules.

(defmodule boot
  (module-operators
   bci
   streams
   macros0
   (only (symbol-name) symbols)
   )
  ()

  (defconstant cons (compile-inline 2 (i-cons)))
  (defconstant car (compile-inline 1 (slot-ref 0)))
  (defconstant cdr (compile-inline 1 (slot-ref 1)))

  (defconstant set-car (compile-inline 2 (set-slot 0)))
  (defconstant set-cdr (compile-inline 2 (set-slot 1)))

  (defconstant cadr (compile-inline 1 (slot-ref 1) (slot-ref 0)))
  (defconstant cddr (compile-inline 1 (slot-ref 1) (slot-ref 1)))

  (defconstant vector-ref (compile-inline 2 (vref)))
  (defconstant set-vector-ref (compile-inline 3 (set-vref)))

  (defconstant consp (compile-inline 1 (i-consp)))
  (defconstant null (compile-inline 1 (nullp)))
  (defconstant atom (compile-inline 1 (i-consp) (nullp)))
  (defconstant eq (compile-inline 2 (eqp)))

  (defconstant list (compile-inline -1))
  (defconstant assq (compile-inline 2 (i-assq)))
  (defconstant memq (compile-inline 2 (i-memq)))
  (defconstant scanq (compile-inline 2 (i-scanq)))
  (defconstant identity (compile-inline 1))

  (defconstant bf-setter (compile-inline 1 (slot-ref 4)))
  (defconstant bf-setter-setter (compile-inline 2 (set-slot 4)))

  (defconstant call-method-by-list (compile-inline 2 (swap) (apply-method-list)))
  
  (defconstant %do-apply (compile-inline 2 returning (apply-args)))

  (export cons car cdr set-car set-cdr cadr cddr vector-ref
	  set-vector-ref list null consp atom assq list
	  memq identity eq bf-setter call-method-by-list
	  %do-apply)

  (defconstant primitive-slot-ref-0 (compile-inline 1 (slot-ref 0)))
  (defconstant primitive-slot-ref-1 (compile-inline 1 (slot-ref 1)))
  (defconstant primitive-slot-ref-2 (compile-inline 1 (slot-ref 2)))
  (defconstant primitive-slot-ref-3 (compile-inline 1 (slot-ref 3)))
  (defconstant primitive-slot-ref-4 (compile-inline 1 (slot-ref 4)))
  (defconstant primitive-slot-ref-5 (compile-inline 1 (slot-ref 5)))
  (defconstant primitive-slot-ref-6 (compile-inline 1 (slot-ref 6)))
  (defconstant primitive-slot-ref-7 (compile-inline 1 (slot-ref 7)))
  (defconstant primitive-slot-ref-8 (compile-inline 1 (slot-ref 8)))
  (defconstant primitive-slot-ref-9 (compile-inline 1 (slot-ref 9)))


  (defconstant primitive-set-slot-ref-0 (compile-inline 2 (set-slot 0)))
  (defconstant primitive-set-slot-ref-1 (compile-inline 2 (set-slot 1)))
  (defconstant primitive-set-slot-ref-2 (compile-inline 2 (set-slot 2)))
  (defconstant primitive-set-slot-ref-3 (compile-inline 2 (set-slot 3)))
  (defconstant primitive-set-slot-ref-4 (compile-inline 2 (set-slot 4)))
  (defconstant primitive-set-slot-ref-5 (compile-inline 2 (set-slot 5)))
  (defconstant primitive-set-slot-ref-6 (compile-inline 2 (set-slot 6)))
  (defconstant primitive-set-slot-ref-7 (compile-inline 2 (set-slot 7)))
  (defconstant primitive-set-slot-ref-8 (compile-inline 2 (set-slot 8)))
  (defconstant primitive-set-slot-ref-9 (compile-inline 2 (set-slot 9)))

  (export
   primitive-slot-ref-0 
   primitive-slot-ref-1
   primitive-slot-ref-2 
   primitive-slot-ref-3 
   primitive-slot-ref-4 
   primitive-slot-ref-5 
   primitive-slot-ref-6 
   primitive-slot-ref-7 
   primitive-slot-ref-8 
   primitive-slot-ref-9) 

  (export
   primitive-set-slot-ref-0 
   primitive-set-slot-ref-1
   primitive-set-slot-ref-2 
   primitive-set-slot-ref-3 
   primitive-set-slot-ref-4 
   primitive-set-slot-ref-5 
   primitive-set-slot-ref-6 
   primitive-set-slot-ref-7 
   primitive-set-slot-ref-8 
   primitive-set-slot-ref-9) 

  (set-bc-global 3 nil)

  ;; setup setter so we can use it nicely
  (compile-declare bf-setter setter-function t)
  (compile-add-callback bf-setter-setter setter-setter-function xx)
  (export bf-setter-setter)
  ;; fixup setter...
  (bf-setter-setter bf-setter bf-setter-setter)
  
  ((bf-setter bf-setter) vector-ref set-vector-ref)
  ((bf-setter bf-setter) car set-car)
  ((bf-setter bf-setter) cdr set-cdr)

  ;; useful functions --- fold mapcar, reverse, mapc, member, append, mapcan, not
  (defun fold (fn lst val)
    (if (null lst) val
      (fold fn (cdr lst)
	    (fn (car lst) val))))

  (defun mapcar1 (fn lst)
    (if (null lst) nil
      (let ((new-lst (list (fn (car lst)))))
	(labels ((map-aux (l end)
			  (if (null l) nil
			    (let ((newpair (list (fn (car l)))))
			      ((bf-setter cdr) end newpair)
			      (map-aux (cdr l) (cdr end))))))
		(map-aux (cdr lst) new-lst))
	new-lst)))

  (defun mapc1 (f l)
    (if (null l) nil
      (progn (f (car l))
	     (mapc1 f (cdr l)))))

  (defun member-list (x l f)
    (cond ((null l) nil)
	  ((f x (car l)))
	  (t (member-list x (cdr l) f))))

  (defun append (a b)
    (if (null a) b
      (let ((lst (cons (car a) nil)))
	(labels ((app-aux (l end)
			  (if (null l)
			      end
			    (let ((newpair (cons (car l) nil)))
			      ((bf-setter cdr) end newpair)
			      (app-aux (cdr l) newpair)))))
		((bf-setter cdr) (app-aux (cdr a) lst) b)
		lst))))

  (defun nconc (a b)
    (if (null a) b
      (labels ((nconc-aux (l)
			  (if (cdr l)
			      (nconc-aux (cdr l))
			    (progn ((bf-setter cdr) l b) a))))
	      (nconc-aux a))))
			      
  '(defun mapcan (f l)
     (if (consp l)
	 (nconc (f (car l))
		(mapcan f (cdr l)))
       nil))

  (defun mapcan (f l)
    (labels ((mapcan-aux (eol lst)
			 (if (null lst) nil
			   (let ((new-lst (f (car lst))))
			     (if (null new-lst)
				 (mapcan-aux eol (cdr lst))
			       (progn ((bf-setter cdr) eol new-lst)
				      (mapcan-aux (last-pair new-lst) 
						  (cdr lst)))))))
	     (last-pair (lst)
			(if (atom (cdr lst))
			    lst
			  (last-pair (cdr lst))))
	     (mapcan-aux-0 (lst)
			   (if (null lst) nil
			     (let ((new-lst (f (car lst))))
			       (if (null new-lst)
				   (mapcan-aux-0 (cdr lst))
				 (progn (mapcan-aux (last-pair new-lst)
						    (cdr lst))
					new-lst))))))
	    (mapcan-aux-0 l)))

  ;;(defun mapcan (f l)
  ;;(fold (lambda (x lst)
  ;;(nconc lst (f x)))
  ;;l
  ;;nil))

  (defun not (x)
    (null x))

  ;; Copied from  internals --- do not change!
  (defconstant unbound-slot-value '%_*unbound*_%)

  ;; XXX: Should be inline
  (defun scan-args (arg init-lst panic)
    (let ((val (scanq arg init-lst)))
      (if (eq val unbound-slot-value)
	  (panic arg init-lst)
	val)))


  (export fold mapcar1 mapc1 member-list append not scan-args nconc)

  ;; NB: No generic calls in this module

   ;; globals
   (deflocal *mod-loc-list* nil)

   ;; include making TELOS here?

   ;; install this module....


   (defun make-installed-module (name context)
     (let ((mod (make-module name 0)))
       (prin-object "{" t)
       (setq *mod-loc-list* (cons (cons mod context) *mod-loc-list*))
       (set-module-statics mod context)
       mod))

   (defun all-registered-modules ()
     *mod-loc-list*)

   (defun make-interface (mod if-desc)
     (let ((import-desc (car if-desc))
	   (exports (cdr if-desc))
	   (strip-imports (eq (car (car if-desc)) 'strip)))
       (mapc1
	(if strip-imports
	    (lambda (x)
	      ;;(prin-object (module-name mod) t)
	      ;;(prin-object (car x) t)
	      ;;(prin-object "\n" t)
	      (if (memq (car x) exports)
		  (add-module-import mod
				     (car x)
				     (car (cdr x))
				     (car (cdr (cdr x))))
		nil))
	  (lambda (x)
	    (add-module-import mod
			       (car x)
			       (car (cdr x))
			       (car (cdr (cdr x))))
	    nil))
	(find-imports import-desc))
       ;;(prin "{")
       ;; Note that we forget where the hell it came from,
       ;; but make sure it exists!
       (mapc1 (lambda (x)
	       (if (dynamic-accessible-p mod x)
		   (add-module-export mod x)
		 nil))
	     exports)
       (prin-object (symbol-name (module-name mod)) t)
       (prin-object "}" t)
       mod))

   (defun find-imports (ispec)
     (cond ((eq (car ispec) 'import)
	    (find-module-exports (car (cdr ispec))))
	   ((eq (car ispec) 'union)
	    (mapcan (lambda (spec)
		      (find-imports spec))
		    (cdr ispec)))
	   ((eq (car ispec) 'except)
	    (let ((lst (car (cdr ispec))))
	      (fold (lambda (x l)
		      (if (memq (car x) lst)
			  l
			(cons x l)))
		    (find-imports (car (cdr (cdr ispec))))
		    nil)))
	   ;; rename
	   ((eq (car ispec) 'rename)
	    (let ((rename-lst (car (cdr ispec)))
		  (imports (find-imports (car (cdr (cdr ispec))))))
	      (mapc1 (lambda (import)
		      (let ((xx (assq (car import) rename-lst)))
			(if xx
			    ((bf-setter car) import (car (cdr xx)))
			  nil)))
		    imports)
	      imports))
	   ;; only
	   ((eq (car ispec) 'only)
	    (fold (lambda (imp l)
		    (if (memq (car imp)
			      (car (cdr ispec)))
			(cons imp l)
		      l))
		  (find-imports (car (cdr (cdr ispec))))
		  nil))
	   ((eq (car ispec) 'strip)
	    (find-imports (car (cdr ispec))))
	   (t
	    ;;(print "Unknown import type")
	    ;;(print ispec)
	    nil)))


   (defun find-module-exports (mod-name)
     (let ((mod (get-module mod-name)))
       (if (null mod)
	   (progn;;(prin "no module: ")
	     ;;(print mod-name)
	     nil)
	 (let ((lst (module-exports mod)))
	   (mapcar1 (lambda (name)
		     (list name mod name))
		   lst)))))

   (defun install-local-bindings (mod name-list loc-list)
     (if (null name-list)
	 nil
       (progn (add-module-binding mod (car name-list) (car loc-list))
	      (install-local-bindings mod (cdr name-list) (cdr loc-list)))))

   ;; used by initcode
   (export make-interface make-installed-module install-local-bindings)

   ;; used by linker...
   (export all-registered-modules)

   ;;(print "Boot Initialised.")
   ;; end module
   )

  (defun $boot ()
    (let ((my-mod (make-module 'a)))
      (make-interface 'b 'c)
      ($boot-aux mod d)
      ))

  (defun $boot-aux (mod names)
    (if (null names)
	mod
      (set
