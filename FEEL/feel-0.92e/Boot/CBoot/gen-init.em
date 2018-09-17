;; Eulisp Module
;; Author: pab
;; File: gen-init.em
;; Date: Fri Dec 11 19:36:46 1992
;;
;; Project:
;; Description:
;;  Creates the initialisation program from
;;  Defclasses, etc

(defmodule gen-init
  ((except (scan-args required-argument unbound-argument null-argument default-argument unbound-slot-value) standard0)
   list-fns
   class-macs
   class-defs
   )
  ()

  (defconstant unbound-slot-value '%_*unbound*_%)

  (defun scan-args (arg init-lst panic)
    (labels ((scan-aux (arg lst)
		       (if (null lst)
			   (panic arg init-lst)
			 (if (eq (car lst) arg)
			     (car (cdr lst))
			   (scan-aux arg (cdr (cdr lst)))))))
	    (scan-aux arg init-lst)))

  (defun required-argument (arg args)
    (error "Missing init-argument" Internal-Error 'error-value (cons arg args)))

  (defun unbound-argument (arg args)
    unbound-slot-value)

  (defun null-argument (arg args)
    nil)

  (defun default-argument (x)
    (lambda (arg args) x))


  (defun make-boot-code (flag)
    (setq *init-type* flag)
    (let ((classlist (the-classlist)))
      `(defmodule init
	 ,(init-modules)
	 ()
	 (expose ,@(init-modules))
	 ,@(un-progn
	    `(progn (progn ,@(preprocess-forms (make-initial-constants classlist)))

		   (progn ,@(preprocess-forms (make-class-hierarchy classlist)))

		   (progn ,@(preprocess-forms (make-slot-descriptions classlist)))

		   (progn ,(preprocess-forms (make-slot-accessors classlist)))

		   (progn ,@(preprocess-forms-2 (method-initialisation-code classlist)))
		   ))
	 )))

  (defun write-std-code ()
    ...)

  (defun write-nl (x y)
    (write x y)
    (newline y))

  (defun write-i-code ()
    (let ((code (make-boot-code 'interpreted))
	  (file (open "init.em" 'output )))
      (print "(" file)
      (mapc (lambda (x) (write-nl x file))
	    code)
      (print ")" file)
      (close file)))

  (defun write-c-code ()
    (let ((code (make-boot-code 'compiled))
	  (file (open "init.em" 'output t)))
      (print "(" file)
      (mapc (lambda (x) (write-nl x file))
	    code)
      (print ")" file)
      (close file)))

  (defconstant slot-reffers
    #(primitive-slot-ref-0
      primitive-slot-ref-1
      primitive-slot-ref-2
      primitive-slot-ref-3
      primitive-slot-ref-4
      primitive-slot-ref-5
      primitive-slot-ref-6
      primitive-slot-ref-7
      primitive-slot-ref-8
      primitive-slot-ref-9) )

  (defconstant slot-setters
    #(primitive-set-slot-ref-0
      primitive-set-slot-ref-1
      primitive-set-slot-ref-2
      primitive-set-slot-ref-3
      primitive-set-slot-ref-4
      primitive-set-slot-ref-5
      primitive-set-slot-ref-6
      primitive-set-slot-ref-7
      primitive-set-slot-ref-8
      primitive-set-slot-ref-9) )

  (deflocal *init-type* 'interpreted)

  (defun init-for-compile-p ()
    (eq *init-type* 'compiled))

  (defun init-for-interpret-p ()
    (eq *init-type* 'interpreted))

  '(defun mapcan (f l)
    (if (atom l) nil
      (nconc (f (car l))
	     (mapcan f (cdr l)))))
  (print mapcan)

  (defun un-progn (form)
    (cond ((null form) nil)
	  ((eq (car form) 'progn)
	   (mapcan un-progn (cdr form)))
	  (t (list form))))

  (defun my-mapcar (fn lst)
    (if (atom lst) (fn lst)
      (cons (fn (car lst))
	    (my-mapcar fn (cdr lst)))))

  (defun preprocess-forms (form)
    (cond ((atom form) form)
	  ((eq (car form) 'quote)
	   form)
	  ((and (eq (car form) 'setter)
		(eq (cadr form) 'car))
	   'primitive-set-slot-ref-0 )
	  ((and (eq (car form) 'setter)
		(eq (cadr form) 'cdr))
	   'primitive-set-slot-ref-1)
	  ((and (eq (car form) 'setter)
		(accessor-location (cadr form)))
	   (vector-ref slot-setters (accessor-location (cadr form))))
	  ((accessor-location (car form))
	   (cons (vector-ref slot-reffers (accessor-location (car form)))
		 (preprocess-forms (cdr form))))
	  ((eq (car form) 'lambda)
	   (cons 'lambda (cons (cadr form)
			       (preprocess-forms (cddr form)))))
	  ;; interpreter only macroexpansions
	  ((and (eq (car form) 'let)
		(init-for-interpret-p))
	   (preprocess-forms
	    `((lambda ,(mapcar car (cadr form))
		,@(cddr form))
	      ,@(mapcar cadr (cadr form)))))
	  ((and (eq (car form) 'labels)
		(init-for-interpret-p))
	   (preprocess-forms
	    `(let ,(mapcar (lambda (fn) (list (car fn) nil)) (cadr form))
	       ,@(mapcar (lambda (fn)
			  `(setq ,(car fn) (lambda ,@(cdr fn))))
			(cadr form))
	       ,@(cddr form))))
	  ((eq (car form) 'method-lambda)
	   (preprocess-forms
	    `(lambda (,@(method-extra-args) ,@(cadr form))
	       ,@(cddr form))))
	  ((eq (car form) 'compile-time)
	   (cond ((eq *init-type* 'interpreted)
		  nil)
		 ((eq *init-type* 'compiled)
		  (cons 'progn (preprocess-forms (cdr form))))))
	  ((eq (car form) 'interpret-time)
	   (cond ((init-for-compile-p)
		  nil)
		 ((init-for-interpret-p)
		  (cons 'progn (preprocess-forms (cdr form))))))
	  (t (my-mapcar preprocess-forms form))))

  (defun preprocess-forms-2 (form)
    (cond ((atom form) form)
	  ((eq (car form) 'quote)
	   form)
	  ((and (eq (car form) 'setter)
		(eq (cadr form) 'car))
	   'primitive-set-slot-ref-0 )
	  ((and (eq (car form) 'setter)
		(eq (cadr form) 'cdr))
	   'primitive-set-slot-ref-1)
	  ;; interpreter only macroexpansions
	  ((eq (car form) 'lambda)
	   (cons 'lambda (cons (cadr form)
			       (preprocess-forms-2 (cddr form)))))
	  ((and (eq (car form) 'let)
		(init-for-interpret-p))
	   (preprocess-forms-2
	    `((lambda ,(mapcar car (cadr form))
		,@(cddr form))
	      ,@(mapcar cadr (cadr form)))))
	  ((and (eq (car form) 'labels)
		(init-for-interpret-p))
	   (preprocess-forms-2
	    `(let ,(mapcar (lambda (fn) (list (car fn) nil)) (cadr form))
	       ,@(mapcar (lambda (fn)
			  `(setq ,(car fn) (lambda ,@(cdr fn))))
			(cadr form))
	       ,@(cddr form))))
	  ((eq (car form) 'method-lambda)
	   (preprocess-forms-2
	    `(lambda (,@(method-extra-args) ,@(cadr form))
	       ,@(cddr form))))
	  ((and (eq (car form) 'call-next-method)
		(init-for-interpret-p))
	   '(if  ***method-status-handle***
		 (progn ;;(format t "Call next: ~a ~a\n"
		   ;;***method-status-handle***
		   ;;	 ***method-args-handle***)
		   (call-method-by-list
		     ***method-status-handle***
		     ***method-args-handle***))
	      (error "No Next Method" <Internal-Error> nil)))
	  ((eq (car form) 'compile-time)
	   (cond ((init-for-interpret-p)
		  nil)
		 ((init-for-compile-p)
		  (cons 'progn (preprocess-forms-2 (cdr form))))))
	  ((eq (car form) 'interpret-time)
	   (cond ((init-for-compile-p)
		  nil)
		 ((init-for-interpret-p)
		  (cons 'progn (preprocess-forms-2 (cdr form))))))
	  (t (my-mapcar preprocess-forms-2 form))))

  (defun method-extra-args ()
    (if (init-for-interpret-p)
	(list '***method-status-handle*** '***method-args-handle***)
      nil))

  (defun make-initial-constants (classlist)
    (mapcar (lambda (classd)
	      (let ((name (scan-args 'name classd (default-argument 'anonymous))))
		(print (list name (scan-args 'allocate classd null-argument)))
		`(progn ,@(if (scan-args 'allocate classd null-argument)
			      `((defconstant ,name (allocate-object <class>)))
			    ())
			(export ,name)
			(set-class-of ,name ,(scan-args 'metaclass classd (default-argument '<class>))))))
	    classlist))

  (defun make-class-fill-list (classlist)
    (cons 'list (mapcar (lambda (classd)
			  (let ((cpl (find-class-precedence-list classd)))
			    `(list ,(scan-args 'name classd required-argument)
				   ',(scan-args 'name classd required-argument)
				   ,(find-class-size cpl)
				   ',(find-class-initargs cpl)
				   (list ,@(scan-args 'direct-superclasses classd null-argument))
				   (list ,@(mapcar (lambda (classd)
						     (scan-args 'name classd required-argument))
						   cpl))
				   )))
			classlist)))

  (defun find-class-size (class-prec-list)
    (fold (lambda (classd n)
	    (+ n
	       (length (scan-args 'direct-slot-descriptions classd
				  required-argument))))
	  class-prec-list
	  0))

  (defun find-class-initargs (cpl)
    (fold (lambda (classd inits)
	    (append (scan-args 'direct-initargs classd null-argument)
		    inits))
	  cpl
	  nil))

  ;; assume single inheritance
  (defun find-class-precedence-list (classd)
    (cons classd
	  (let ((supers (scan-args 'direct-superclasses classd required-argument)))
	    (if (null supers) nil
	      (find-class-precedence-list (find-class (car supers)))))))


  ;; lst -> (progn (defconstant accessor (make-prim-accessor ...)) ...)
  (defun make-slot-accessors (classlist)
    (cons 'progn
	  (mapcar (lambda (classd)
		    (let ((name (scan-args 'name classd required-argument)))
		      `(progn ,@(mapcar (lambda (slotd)
					 (let ((accessor (scan-args 'accessor slotd null-argument)))
					   (if (null accessor) nil
					     `(progn (defconstant ,accessor
						       (simple-find-accessor ,name
										  ',(scan-args 'name slotd
											       required-argument)))
						     (export ,accessor)))))
				       (scan-args 'direct-slot-descriptions classd null-argument)))))
		  classlist)))


  ;;
  ;; Code starts here
  ;;
  (defconstant *interpret-init-modules*
    '(arith bci
      lists  classes
      sockets streams ccc symbols strings calls others
      tables  vectors
      (except (error cerror)
	      errors)
      (only (set-compute-and-apply-fn generic-function-p methodp call-method-by-list) generics)
      class-names
      ))

  (defconstant *compile-init-modules*
    '(arith bci
       ;;list
      (except (car cdr cons list append nconc memq null consp mapcar mapc atom) lists)
      (only (class-of set-type set-class-of allocate-object subclassp
		      make-structure-reader
		      make-structure-writer
		      initialize-local-slots
		      allocate initialize primitive-slot-ref primitive-set-slot-ref) 
	    classes)
      sockets streams
      (except (eq) ccc )
      symbols strings
      (except (atom) others)
      (except (apply) calls)
      tables
      (except (vector-ref) vectors)
      (except (error cerror)
	      errors)
      (only (set-compute-and-apply-fn generic-function-p methodp ) generics)
      class-names
      boot
      ))

  (defun init-modules ()
    (if (init-for-interpret-p)
	*interpret-init-modules*
      *compile-init-modules*))


  ;; Fill-list is:
  ;; name size initargs supers

  (defconstant class-hierarchy-literals
    '(

      (interpret-time
        (defconstant mapcar1 mapcar)
	(defconstant mapc1 mapc))

      ;; Copied from  internals --- do not change!
      (defconstant unbound-slot-value '%_*unbound*_%)
      (export unbound-slot-value class-type)
      (defconstant generic-type #xa4)
      (defconstant method-type  #x25)
      (defconstant class-type  #xd)

      (defun fill-class (class desc)
	;;(generic_generic_prin\,Object (car desc) nil)
	;;(newline nil)
	((setter class-name) class (car desc))
	(setq desc (cdr desc))
	((setter class-instance-size) class (car desc))
	(setq desc (cdr desc))
	((setter class-initargs) class (car desc))
	(setq desc (cdr desc))
	((setter class-direct-superclasses) class (car desc))
	(setq desc (cdr desc))
	((setter class-precedence-list) class (car desc))
	(setq desc (cdr desc))
	((setter class-direct-subclasses) class nil)
	(set-type class class-type)
	(mapc1 (lambda (cl)
		((setter class-direct-subclasses) cl (cons class (class-direct-subclasses cl))))
	      (class-direct-superclasses class)))

      (defun initialise-hierarchy (lst)
	(if (null lst) nil
	  (progn
	    (fill-class (car (car lst)) (cdr (car lst)))
	    (initialise-hierarchy (cdr lst)))))))


  (defun make-class-hierarchy (classlist)
    `(,@class-hierarchy-literals
      (initialise-hierarchy ,(make-class-fill-list classlist))))

(defconstant make-slot-descriptions-literals
    '(
      ;;(generic_generic_prin\,Object "Done Class hierarchy\n" nil)
      ;; add-method bootstrap...
      (defun i-add1 (x)
	(binary+_Integer x 1))

      (defun i-sub1 (x)
	(binary-_Integer x 1))

      (defun i-zerop (x)
	(binary=_Integer 0 x))

      (defun i-greaterp (x y)
	(binary<_Integer y x))

      (defun fold (fn lst val)
	(if (null lst) val
	  (fold fn (cdr lst)
		(fn (car lst) val))))
      (export fold )

      ;; should define cons as compile-inline...
      (defun reverse (x)
	(fold cons x nil))

      (defun assq (x lst)
	(if (null lst) nil
	  (if (eq (car (car lst)) x)
	      (car lst)
	    (assq x (cdr lst)))))

      (defun identity (x) x)

      (defconstant slot-readers
	(make-initialized-vector
	 primitive-slot-ref-0
	 primitive-slot-ref-1
	 primitive-slot-ref-2
	 primitive-slot-ref-3
	 primitive-slot-ref-4
	 primitive-slot-ref-5
	 primitive-slot-ref-6
	 primitive-slot-ref-7
	 primitive-slot-ref-8
	 primitive-slot-ref-9) )

      (defconstant slot-writers
	(make-initialized-vector
	 primitive-set-slot-ref-0
	 primitive-set-slot-ref-1
	 primitive-set-slot-ref-2
	 primitive-set-slot-ref-3
	 primitive-set-slot-ref-4
	 primitive-set-slot-ref-5
	 primitive-set-slot-ref-6
	 primitive-set-slot-ref-7
	 primitive-set-slot-ref-8
	 primitive-set-slot-ref-9))

      
      (defun %compute-reader (n)
	(if (i-greaterp 10 n)
	    (vector-ref slot-readers n)
	  (method-lambda (x)
			 (primitive-slot-ref x n))))

      (defun %compute-writer (n)
	(if (i-greaterp 10 n)
	    (vector-ref slot-writers n)
	  (method-lambda (x v)
			 (primitive-set-slot-ref x n v))))

      ;; Major data-structure --- method table implemented as a tree
      (defun make-initial-table (key entry)
	(mk-tab-aux key entry))

      (defun mk-tab-aux (key entry)
	(labels ((add-part (lst tab)
			   (if (null lst) tab
			     (add-part (cdr lst)
				       (cons (cons (car lst) tab) nil)))))
		(add-part (reverse key) entry)))

      (defun add-table-entry (table key value)
	(if (null table)
	    (error "Can't happen" <Internal-Error>)
	  (let ((xx (assq (car key) table)))
	    (if (null xx)
		(progn (nconc table
			      (make-initial-table key value))
		       table)
	      (if (null (cdr key))
		  ;; replacement method
		  ((setter cdr) xx value)
		(add-table-entry (cdr xx) (cdr key) value))))))
      
      ;; Make accessors look pretty...
      (defun symbol-unbraced-name (sym)
	(let ((x (symbol-name sym)))
	  (if (eq (string-ref x 0) #\<)
	      (substring x 1 (i-sub1 (i-sub1 (string-length x))))
	    x)))

      (export symbol-unbraced-name)
      ;; General stuff for scan-args

      ;; XXX: Should be inline
      (interpret-time
       (defun scan-args (arg init-lst panic)
	 ;;(generic_generic_prin\,Object arg nil)
	 ;;(newline nil)
	 (labels ((scan-aux (arg lst)
			    (if (null lst)
				(panic arg init-lst)
			      (if (eq (car lst) arg)
				  (car (cdr lst))
				(scan-aux arg (cdr (cdr lst)))))))
		 (scan-aux arg init-lst)))
       )
      (defun required-argument (arg args)
	(error "Missing init-argument" <Internal-Error> 'error-value arg))

      (defun unbound-argument (arg args)
	unbound-slot-value)

      (defun null-argument (arg args)
	nil)

      (defun default-argument (x)
	(lambda (arg args) x))

      (export required-argument unbound-argument null-argument default-argument scan-args)

      ;; find the right sort of lookup
      (defun simple-compute-method-lookup-function (gf domain)
	(lambda (args)
	  (find-applicable-methods gf args)))


      (defun %generic-domain (gf)
	(let ((dom (cdr (generic-method-description gf))))
	  (if dom dom
	      (let ((obj (list <object>)))
		((setter cdr) obj obj)
		obj))))

      (defun method-signature-depth (gf meth)
	(let ((sig (method-signature meth))
	      (domain (%generic-domain gf )))
	  (labels ((calc-depth (lst domain depth n)
			       (if (null lst) depth
				 (if (eq (car lst) (car domain))
				     (calc-depth (cdr lst) (cdr domain) depth (i-add1 n))
				   (calc-depth (cdr lst) (cdr domain) (i-add1 n) (i-add1 n))))))
		  (calc-depth sig domain 0 0))))

      ;; adds a method w/out any type-checking, or protocol.

      (defun simple-add-method (gf meth)
	;;(generic_generic_prin\,Object (list 'add-method (generic-name gf))
	;;  nil)
	(let ((sig (method-signature meth))
	      (table (generic-method-table gf)))
	  (if (null table)
	      ((setter generic-method-table) gf (make-initial-table sig (list meth)))
	    (add-table-entry table sig (list meth)))
	  ((setter generic-fast-cache) gf nil)
	  ((setter generic-slow-cache) gf nil)
	  ((setter method-generic-function) meth gf)
	  (let ((true-depth (method-signature-depth gf meth)))
	    (if (i-greaterp true-depth (generic-discrimination-depth gf))
		((setter generic-discrimination-depth) gf true-depth)
	      nil))
	  gf))

      (defun std-generic-discriminator (gf lookup)
	(lambda (args)
	  (let ((meths (lookup args)))
	    (if (null meths)
		(error "No applicable method" no-applicable-method
		       'sig (mapcar1 class-of args))
	      (call-method-by-list meths args)))))

      (defun simple-make-generic args
	;;(generic_generic_prin\,Object "Make generic" nil)
	;;(generic_generic_prin\,Object args nil)
	(let ((obj (allocate-object <generic-function>)))
	  ;;(generic_generic_prin\,Object obj nil)
	  ;;(newline nil)
	  ((setter generic-name) obj (scan-args 'name args required-argument))
	  ((setter generic-argtype) obj (scan-args 'argtype args required-argument))
	  ((setter generic-fast-cache) obj nil)
	  ((setter generic-slow-cache) obj nil)
	  ((setter generic-method-table) obj nil)
	  ((setter generic-method-description) obj 
	   (cons <method>
		 (scan-args 'domain args null-argument)))
	  (let ((lookup (simple-compute-method-lookup-function obj nil)))
	    ((setter generic-method-lookup-function) obj lookup)
	    ((setter generic-discriminator) obj (std-generic-discriminator obj lookup)))
	  ((setter generic-discrimination-depth) obj 0)
	  (set-type obj generic-type)
	  ;;(if (symbolp (%generic-name obj)) nil
	  ;;  (character-to-integer 'a))
	  obj))

      (defun simple-make-method args
	;;(generic_generic_prin\,Object "Make method" nil)
	;;(generic_generic_prin\,Object args nil)
	(let ((meth (allocate-object <method>)))
	  ((setter method-qualifier) meth nil)
	  ((setter method-generic-function) meth nil)
	  ((setter method-signature) meth (scan-args 'signature args required-argument))
	  ((setter method-function) meth (scan-args 'function args required-argument))
	  ((setter method-fixed) meth (scan-args 'fixed args null-argument))
	  (set-type meth method-type)
	  meth))


      (defun simple-compute-reader (cl args)
	;;(generic_generic_prin\,Object "Compute Reader" nil)
	;;(generic_generic_prin\,Object args nil)
	(let ((pos (scan-args 'position args required-argument))
	      (gf (simple-make-generic
		   'argtype 1
		   'name (make-symbol
			  (string-append (symbol-unbraced-name (scan-args 'owner-class args (default-argument 'anonymous)))
					 (string-append "-"
							(symbol-name (scan-args 'name args required-argument)))))))
	      )
	  (if (eq (scan-args 'class args null-argument) <unreadable-slot-description>)
	      (simple-add-method gf
				 (simple-make-method
				  'signature (list cl)
				  'function (method-lambda (o)
							   (error "Can't read slot" <Internal-Error>))))
	    ;; XXX: Should lookup lambda in a table --- there are only 13.
	    (simple-add-method gf
			       (simple-make-method 'signature (list cl)
						   'function (%compute-reader pos))))
	  gf))


      (defun simple-compute-writer (cl args)
	;;(generic_generic_prin\,Object "Compute Writer" nil)
	;;(generic_generic_prin\,Object args nil)
	(let ((pos (scan-args 'position args null-argument))
	      (gf (simple-make-generic
		   'argtype 2
		   'domain (list cl <object>)
		   'name (make-symbol
			  (string-append (string-append
					  (symbol-name (scan-args 'owner-class args
								  (default-argument 'anonymous)))
					  (string-append "-"
							 (symbol-name (scan-args 'name args
										 required-argument))))
					 "-setter"))))
	      )
	  (if (eq (scan-args 'class args null-argument) <unreadable-slot-description>)
	      (simple-add-method gf
				 (simple-make-method
				  'signature (list cl <object>)
				  'function (method-lambda (o v)
							   (error "Can't set slot" <Internal-Error> ))))
	    ;; XXX: Should lookup lambda in a table --- there are only 13.
	    (simple-add-method gf
			       (simple-make-method
				'signature (list cl <object>)
				     'function (%compute-writer pos))))
	  gf))

      ;; Okay, now make all those slots..

      (defun fill-slot-description (obj class args)
	(let ((access-args (list 'class (car args)
				 'owner-class (car (cdr args))
				 'name  (car (cdr (cdr args)))
				 'position (car (cdr (cdr (cdr args))))
				 )))
	  (setq args (cdr (cdr  args)))
	  ((setter slot-description-name) obj (car args))
	  (setq args (cdr args))
	  ((setter slot-description-position) obj (car args))
	  (setq args (cdr args))
	  (let ((initform (car args)))
	    ((setter slot-description-initfunction) obj
	     (if (eq initform unbound-slot-value)
		 unbound-slot-value
	       (lambda () initform))))
	  (setq args (cdr args))
	  ((setter slot-description-initarg) obj (car args))
	  ((setter slot-description-slot-reader) obj
	   (simple-compute-reader class access-args))
	  ((setter slot-description-slot-writer) obj
	   (simple-compute-writer class access-args))
	  ;;(generic_generic_prin\,Object "Done slot" nil)
	  ;;(newline nil)
	  obj))


      (defun simple-find-slot-description (class name)
	(let ((xx (class-slot-descriptions class)))
	  (labels ((l1 (slots)
		       (if (null slots)
			   (error "Could not find slot" <Internal-Error> 'error-value name)
			 (if (eq (slot-description-name (car slots)) name)
			     (car slots)
			   (l1 (cdr slots))))))
		  (l1 xx))))

      (defun simple-find-slot-reader (class slot-name)
	(slot-description-slot-reader (simple-find-slot-description class slot-name)))

      (defun simple-find-slot-writer (class slot-name)
	(slot-description-slot-writer (simple-find-slot-description class slot-name)))

      (defun simple-find-accessor (class slot-name)
	(let ((reader (simple-find-slot-reader class slot-name))
	      (writer (simple-find-slot-writer class slot-name)))
	  ((setter generic-setter) reader writer)
	  reader))


      (defun initialize-slots (lst)
	(if (null lst) nil
	  (let ((class (car (car lst)))
		(slots (cdr (car lst))))
	    ;;(generic_generic_prin\,Object (car lst) nil)
	    ((setter class-slot-descriptions) class
	     (append (if (null (class-direct-superclasses class)) nil
		       (class-slot-descriptions (car (class-direct-superclasses class))))
		     (make-slot-list class slots)))
	    ((setter class-local-slot-descriptions) class 
	     (mapcar1 (lambda (sd)
			;;(cons (slot-description-initarg sd)
			;; (slot-description-initfunction sd))
			nil)
		     (class-slot-descriptions class)))
	    ((setter class-non-local-slot-descriptions) class (class-slot-descriptions class))
	    (initialize-slots (cdr lst)))))

      (defun make-slot-list (class slotds)
	(if (null slotds) nil
	  (let ((slotd (car slotds))
		(slot (allocate-object (car (car slotds)))))
	    ;;(generic_generic_prin\,Object slotd nil)
	    (fill-slot-description slot class (cons (car slotd) (cons (class-name class) (cdr slotd))))
	    (cons slot (make-slot-list class (cdr slotds))))))

      (defconstant internal-gf-setter-setter (setter generic-setter))
      (defconstant internal-gf-setter primitive-slot-ref-9)
      (defconstant internal-gf-method-table (lambda (x) (generic-method-table x)))
      (defconstant internal-gf-name (lambda (x) (generic-name x)))
      (defconstant internal-gf-discrimination-depth (lambda (x) (generic-discrimination-depth x)))
      (defconstant internal-gf-method-lookup-function (lambda (x) (generic-method-lookup-function x)))
      (defconstant internal-class-precedence-list (lambda (x) (class-precedence-list x)))

      (defun init-generic (gf)
	(let ((lookup (simple-compute-method-lookup-function gf nil)))
	  ((setter generic-method-lookup-function) gf lookup)
	  ((setter generic-discriminator) gf
	   (std-generic-discriminator gf lookup))
	  ((setter generic-method-description) gf
	   (cons <method> nil))))

      (defun add-method-to-caches (gf sig meths)
	((setter generic-fast-cache) gf (cons sig meths))
	(let ((table (generic-slow-cache gf)))
	  (if (null table)
	      ((setter generic-slow-cache)
	       gf
	       (make-initial-table sig (cons sig meths)))
	    (add-table-entry table sig (cons sig meths)))))


      ))

  ;; lst -> (class (name position initarg ) ...)
(defun make-fill-slot-list (classlist)
    (cons 'list
	  (mapcar (lambda (classd)
		    `(list ,(scan-args 'name classd required-argument)
			   ,@(mapcar (lambda (slotd)
				       `(list ,(scan-args 'class slotd (default-argument '<local-slot-description>))
					      ',(scan-args 'name slotd null-argument)
					      ,(scan-args 'position slotd null-argument)
					      ',(scan-args 'initform slotd unbound-argument)
					      ',(scan-args 'initarg slotd unbound-argument)))
				     (scan-args 'direct-slot-descriptions classd required-argument))))
		  classlist)))

(defun make-slot-descriptions (classlist)
    `(
       ,@make-slot-descriptions-literals
       (initialize-slots ,(make-fill-slot-list classlist))))


  ;; NOTE: This form is NOT Pre-processed.

  (defconstant finalise-bootstrap-literals
    '(

      ;; Define functions to avoid circularity in the bootstrap process

      (defun stable-generic-method-table (gf)
	(if (eq (class-of gf) <generic-function>)
	    (internal-gf-method-table gf)
	  (generic-method-table gf)))

      (defun stable-generic-lookup-function (gf)
	(if (eq (class-of gf) <generic-function>)
	    (internal-gf-method-lookup-function gf)
	  (generic-method-lookup-function gf)))

      (defun stable-generic-name (gf)
	(if (eq (class-of gf) <generic-function>)
	    (internal-gf-name gf)
	  (generic-name gf)))

      (defun stable-generic-discrimination-depth (gf)
	(if (eq (class-of gf) <generic-function>)
	    (internal-gf-discrimination-depth gf)
	  (generic-discrimination-depth gf)))

      (defun stable-class-precedence-list (cl)
	;;(generic_generic_prin\,Object (list 'stable-cpl cl) nil)
	(if (eq (class-of cl) <class>)
	    (internal-class-precedence-list cl)
	  (class-precedence-list cl)))

      ;; Create other necessary generic functions

      (defconstant setter (simple-make-generic 'argtype 1
							 'name 'setter))
      (export setter)


      (defconstant setter-setter (simple-make-generic 'argtype 2
								'name 'setter-setter))
      (export setter-setter)

      ;;(generic_generic_prin\,Object "Adding methiods to setter\n" nil)

      (simple-add-method setter-setter
			 (simple-make-method
			  'signature (list <generic-function> <object>)
			  'function internal-gf-setter-setter))

      (compile-time
       (%Compiler-special-object add-property
				 (setter-function t) setter)

       (%Compiler-special-object add-callback
				 (setter-setter-function xx) setter-setter)

       (simple-add-method setter-setter
			  (simple-make-method
			   'signature (list <bytefunction> <object>)
			   'function (bf-setter bf-setter)))
       )

      (simple-add-method setter
			 (simple-make-method
			  'signature (list <generic-function>)
			  'function internal-gf-setter))

      (compile-time
       (simple-add-method setter
			  (simple-make-method
			   'signature (list <bytefunction>)
			   'function bf-setter))
       )

      ;; generic-method restrictions
      (defun generic-method-class (gf)
	(car (generic-method-description gf)))

      (defun generic-method-domain (gf)
	(cdr (generic-method-description gf)))

      (export generic-method-domain generic-method-class)

      (defun set-generic-method-description (gf class domain)
	((setter generic-method-description) gf (cons class domain)))

      (defconstant add-method-method
	;; XX:
	(method-lambda (gf meth)
		       ;;(generic_generic_prin\,Object "add-method-method:" nil)
		       ;;(generic_generic_prin\,Object (generic-name gf) nil)
		       ;;(newline nil)
		       (if (= (generic-argtype gf) (list-length (method-signature meth)))
			   (let ((sig (restrict-method gf (method-signature meth)))
				 (table (generic-method-table gf)))
			     (if (null table)
				 ((generic-setter generic-method-table)
				  gf (make-initial-table sig (list meth)))
			       (add-table-entry table sig (list meth)))
			     (let ((true-depth (method-signature-depth gf meth)))
			       (if (i-greaterp true-depth (generic-discrimination-depth gf))
				   ((generic-setter generic-discrimination-depth) gf true-depth)
				 nil))
			     ((generic-setter generic-fast-cache) gf nil)
			     ((generic-setter generic-slow-cache) gf nil)
			     ((setter method-generic-function) meth gf)
			     gf)
			 (error "add-method: argument mismatch" <Internal-Error> 'error-value (cons gf meth)))))
      (export add-method-method)



      (defun trim-signature (gf sig)
	(if (i-zerop (stable-generic-discrimination-depth gf))
	    nil
	  (labels ((add-obj (last lst n)
			    (if (i-zerop n) nil
			      (progn (let ((new  (cons (car lst) nil)))
				       ((setter cdr) last new)
				       (add-obj new (cdr lst) (i-sub1 n)))))))
		  (let ((first (cons (car sig) nil)))
		    (add-obj first (cdr sig) (i-sub1 (stable-generic-discrimination-depth gf)))
		    first))))

      ;; XXX: Boot problem
      (defun find-applicable-methods (gf args)
	(find-applic-methods-aux (stable-generic-method-table gf)
				 (mapcar1 (lambda (x)
					   (stable-class-precedence-list (class-of x)))
					 args)))

      ;; wasteful...
      (defun find-applic-methods-aux (table cpl-lst)
	(if (null cpl-lst)
	    nil
	  (if (null (car cpl-lst))
	      nil
	    (let ((xx (assq (car (car cpl-lst)) table)))
	      (if (null xx)
		  (find-applic-methods-aux table
					   (cons (cdr (car cpl-lst))
						 (cdr cpl-lst)))
		(if (null (cdr cpl-lst))
		    ;; found summat
		    (if (methodp (car (cdr xx)))
			(cons (car (cdr xx))
			      (find-applic-methods-aux table
						       (cons (cdr (car cpl-lst))
							     (cdr cpl-lst))))
		      (progn
			;;(print "error-1")
			;;(print (list xx cpl-lst))
			(cerror "yowzer" <Internal-Error> 'error-value xx)))
		  (append (find-applic-methods-aux (cdr xx) (cdr cpl-lst))
			  (find-applic-methods-aux table
						   (cons (cdr (car cpl-lst))
							 (cdr cpl-lst))))))))))


      (deflocal debug nil)
      ;;(defun set-debug (x) (setq debug x))
      ;;(export set-debug)
      ;; This is the TYPE_Generic only routine => cache handling is _bound_ to be ok.

      ;;(defun my-prin (x)
      ;;(if (null (consp x))
      ;;(prin-object x nil)
      ;;(progn  (prin-object "(" nil) 
      ;;(mapc (lambda (y) 
      ;;(my-prin y))
      ;;x)
      ;;(prin-object ")" nil))))

      (defun find-and-call-generic (gf args)
	;;(prin-object "call " nil)
	;;(prin-object (symbol-name (primitive-slot-ref-0 gf)) nil)
	;;(my-prin args)
	;;(prin-object "\n" nil)
	;;(newline nil)
	(let ((meths ((stable-generic-lookup-function gf) args))
	      (sig (mapcar1 class-of args)))
	  ;;(generic_generic_prin\,Object (list 'got-info meths sig) nil)
	  (if (null meths)
	      (error "No applicable methods" no-applicable-method
		     'error-value gf
		     'sig sig
		     'args args)
	    (let ((trimmed-sig (trim-signature gf sig)))
	      ;;(generic_generic_prin\,Object trimmed-sig nil)
	      ;;(set-bc-global 3 t)
	      (add-method-to-caches gf trimmed-sig meths)
	      (call-method-by-list meths args)))))

      ;; called by add-method-method.
      ;; Forces method signatures to be a subset of the domain of the function.
      ;; It deliberately allows methods on superclasses of elts of the domain---
      ;; makes defmethod easier.
      (defun restrict-method (gf sig)
	(let ((domain (generic-method-domain gf)))
	  ;;(generic_generic_prin\,Object domain nil)
	  (if (null domain) sig
	    (labels ((restrict-lsts (sig dom)
				    (if (null sig) nil
				      (if (subclassp (car sig) (car dom))
					  (cons (car sig)
						(restrict-lsts (cdr sig) (cdr dom)))
					(if (subclassp (car dom) (car sig))
					    (cons (car dom)
						  (restrict-lsts (cdr sig) (cdr dom)))
					  (progn (error "Add-method: outside domain" <Internal-Error>)
						 2))))))
		    (restrict-lsts sig domain)))))

      ;; now set it up...

      ;; shove new version of gf into place...
      (set-compute-and-apply-fn find-and-call-generic)
      (compile-time
      (set-bc-global 0 find-and-call-generic)
      )

      (compile-time
       (defun make (x . l)
	 (initialize (allocate x l) l))
       (export make)
       )
      (defconstant add-method (simple-make-generic 'argtype 2 'lambda-list '(object lst) 'name 'add-method))
      (export add-method)

      (defconstant compute-method-lookup-function
	(simple-make-generic 'argtype 2 'lambda-list '(object lst) 'name 'compute-method-lookup-function))
      (export compute-method-lookup-function)

      (defconstant compute-discriminating-function
	(simple-make-generic 'argtype 4 'lambda-list '(object lst object object) 'name 'compute-discriminating-function))
      (export compute-discriminating-function)

      (defconstant =
	(simple-make-generic 'argtype 2 'lambda-list '(x y) 'name '=))
      (export =)

      ;; Bootstrap methods
      (defconstant std-allocate-object
	(method-lambda (a b)
		       (allocate-object a)))

      ;; the standard method on init-instance
      (defconstant std-initialize-object
	(method-lambda (obj initlist)
		       ;;(generic_generic_prin\,Object (cons (class-name (class-of obj))
		       ;;  initlist)
		       ;;			     nil)
		       (initialize-local-slots obj initlist)
		       (mapc1 (lambda (slot)
			       (let ((initarg (slot-description-initarg slot))
				     (initfunction (slot-description-initfunction slot)))
				 (if (eq initarg unbound-slot-value)
				     (if (eq initfunction unbound-slot-value)
					 nil
				       ((slot-description-slot-writer slot) obj (initfunction)))
				   (let ((value (scan-args initarg initlist unbound-argument)))
				     (if (eq value unbound-slot-value)
					 (if (eq initfunction unbound-slot-value)
					     nil
					   ((slot-description-slot-writer slot) obj (initfunction)))
				       ((slot-description-slot-writer slot) obj value)))))
			       )
			     (class-non-local-slot-descriptions (class-of obj)))
		       obj))

      ;;(generic_generic_prin\,Object "** Initing generics\n" nil)
      ;; for previously allocated generics...


      (init-generic allocate)
      (init-generic initialize)
      (init-generic generic-write)
      (init-generic generic-prin)
      (init-generic output)
      (init-generic generic-read)
      (init-generic flush)

      (init-generic binary+)
      (init-generic binary-)
      (init-generic binary*)
      (init-generic binary/)
      (init-generic binary-gcd)
      (init-generic binary-lcm)
      (init-generic binary<)
      (init-generic negate)
      (init-generic equal)

      ;;(prin-object "** Initted generics\n" nil)

      (simple-add-method add-method (simple-make-method 'signature (list <generic-function> <method>)
							'function add-method-method))


      (simple-add-method =
			 (simple-make-method
			  'signature (list <fixint> <fixint>)
			  'function binary=_Integer))

      ;;(prin-object "** Initted add-method-method\n" nil)
      ;; First generic call

      (add-method allocate
		  (simple-make-method 'signature (list <class> <object>)
				      'function std-allocate-object))

      ;;(prin-object "** Done Gcall\n" nil)
      (add-method initialize
		  (simple-make-method 'signature (list <object> <object>)
				      'function std-initialize-object))

      ;;(prin-object "** Done init\n" nil)
      (add-method initialize
		  (simple-make-method 'signature (list <method> <object>)
				      'function (method-lambda (a b)
							       (let ((new (call-next-method)))
								 ((setter method-signature) a
								  (scan-args 'signature b
									     required-argument))
								 (set-type new method-type)
								 new))))

      ;;(generic_generic_prin\,Object "** Done Gcall\n" nil)
      (add-method initialize
		  (simple-make-method
		   'signature (list <generic-function> <object>)
		   'function (method-lambda (a initargs)
					    (let ((new (call-next-method)))
					      ((generic-setter generic-slow-cache) new nil)
					      ((generic-setter generic-fast-cache) new nil)
					      ((generic-setter generic-method-table) new nil)
					      (if (eq (generic-argtype new) unbound-slot-value)
						  ((setter generic-argtype) new
						   (list-length (scan-args 'lambda-list initargs
									   required-argument)))
						nil)
					      (let ((domain (scan-args 'domain initargs null-argument)))
						(let ((lookup-fn
						       (compute-method-lookup-function new domain))
						      (methods (scan-args 'methods initargs null-argument))
						      (method-class (scan-args 'method-class initargs
									       (default-argument <method>))))
						  ((setter generic-method-lookup-function) new lookup-fn)
						  (let ((disc-fun
							 (compute-discriminating-function new domain lookup-fn
											  methods))
							(disc-methods (find-applicable-methods
								       compute-discriminating-function
								       (list new domain lookup-fn methods))))
						    ((setter generic-discriminator) new disc-fun)
						    (if (eq (car disc-methods) std-discrimination-method)
							(set-type new generic-type)
						      nil))
						  (set-generic-method-description new method-class domain)
						  ((generic-setter generic-discrimination-depth) new 0)
						  (mapc1 (lambda (meth) (add-method new meth)) methods)))
					      new))))

      ;;(generic_generic_prin\,Object "** Done init (generic)\n" nil)
      (add-method compute-method-lookup-function
		  (simple-make-method 'signature (list <generic-function> <object>)
				      'function (method-lambda (gf domain)
							       (lambda (args)
								 (find-applicable-methods gf args)))))

      ;; Notes:
      ;; This method can assume that it is working on class discrimination,
      ;; rather than instances. Thus the internal caching techniques are
      ;; entirely cool, and perhaps should be used.
      (defconstant std-discrimination-method
	(simple-make-method 'signature (list <generic-function> <object> <object> <object>)
			    'function (method-lambda (gf dom lookup meths)
						     (lambda (args)
						       (let ((meths (lookup args)))
							 (if (null meths)
							     (error "No applicable method" no-applicable-method
								    'error-value gf
								    'sig (mapcar1 class-of args))
							   (call-method-by-list meths args)))))))

      (add-method compute-discriminating-function
		  std-discrimination-method)

      ;; This is the internal algorithm.
      ;;       Unless you want caching, there is no point using it
      ;;(defconstant std-discrimination-method
      ;;(simple-make-method 'signature (list <generic-function> <object> <object> <object>)
      ;;                    'function (method-lambda (gf dom lookup-fn meths)
      ;; (lambda (args)
      ;;   (let ((fast-cache (generic-fast-cache gf)))
      ;;     (labels ((fast-lookup (cache lst)
      ;;			   ;;(format t "fast: ~a~%" cache)
      ;;			   (if (null cache)
      ;;			       (progn ;; (format t "fast success: ~a~%" (cdr fast-cache))
      ;;				      (call-method-by-list (cdr fast-cache) args))
      ;;			     (if (eq (car cache) (class-of (car lst)))
      ;;				 (fast-lookup (cdr cache) (cdr lst))
      ;;			       (slow-lookup (generic-slow-cache gf)
      ;;					    (generic-discrimination-depth gf)
      ;;					    args))))
      ;;	      (slow-lookup (cache n lst)
      ;;			   ;;(format t "slow: ~a~%" cache)
      ;;			   (if (> n 0)
      ;;			       (if (null cache) (lookup)
      ;;				 (if (eq (car (car cache))
      ;;					 (class-of (car lst)))
      ;;				     (slow-lookup (cdr (car cache))
      ;;						  (- n 1)
      ;;						  (cdr lst))
      ;;				   (slow-lookup (cdr cache) n lst)))
      ;;			     (progn ((setter generic-fast-cache) gf cache)
      ;;				    ;;(format t "Slow success: ~a~%" (cdr cache))
      ;;				    (call-method-by-list (cdr cache) args))))
      ;;	      (lookup ()
      ;;		      (let ((meths (lookup-fn args)))
      ;;			(if (null meths)
      ;;			    (error "No applicable method" no-applicable-method
      ;;				   'error-value gf
      ;;				   'sig (mapcar class-of args))
      ;;			  (progn
      ;;			    (add-method-to-caches gf (trim-signature gf (mapcar class-of args)) meths)
      ;;			    (call-method-by-list meths args))))))
      ;;	     (if fast-cache
      ;;		 (fast-lookup (car fast-cache) args)
      ;;	       (lookup))))))))

      ;; 1st generic call to allocate+initialize
      (add-method generic-prin
		  (make <method>
			'signature (list <object> <object>)
			'function prin-object))

      ;;(add-method generic-prin
      ;;  (make <method> 'signature (list <pair> <object>)
      ;;       'function generic_generic_prin\,Cons))

      (add-method generic-write
		  (make <method>
			'signature (list <object> <object>)
			'function  (method-lambda (x y)
						  (generic-prin x y))))

      (add-method flush
		  (make <method>
			'signature (list <object>)
			'function  (method-lambda (y) nil)))
						  

      ;; classes to fix up later

      (deflocal no-applicable-method ())

      (defun set-no-applicable-method (x)
	(setq no-applicable-method x))

      (export set-no-applicable-method)

      (add-method allocate
		  (make <method>
			'signature (list <primitive-class> <object>)
			'function (method-lambda (c l)
						 (error "Cannot allocate primitive class"
							<Internal-Error>
							'error-value c))))
      ;; Copy
      (defconstant copy
	(make <generic-function>
	      'lambda-list '(x)
	      'argtype 1
	      'name 'copy
	      'method-class <method>))

      (add-method copy
		  (make <method>
;			'signature (list <pair>)
			'signature (list <cons>)
			'function (method-lambda (x)
						 (cons (car x)
						       (cdr x)))))


      (add-method copy
		  (make <method>
			'signature (list (class-of nil))
			'function (method-lambda (x) nil)))

      ;; I don't want a method on Object!
      (add-method copy
		  (make <method>
			'signature (list <structure>)
			'function 
			(method-lambda (x)
				       (labels ((copy-slots (old new slot-list)
							    (if (null slot-list) nil
							      (progn ((slot-description-slot-writer (car slot-list))
								      new ((slot-description-slot-reader (car slot-list)) new))
								     (copy-slots old new (cdr slot-list))))))
					       (copy-slots x (allocate (class-of x) nil)
							   (class-slot-descriptions x))))))

      (add-method copy
		  (make <method>
			'signature (list <symbol>)
			'function identity))

      (add-method copy (make <method>
			     'signature (list <vector>)
			     'function generic_copy\,Vector))
      (export copy)
      ;; Hashing

      (defconstant generic-hash
	(make <generic-function>
	      'lambda-list '(x)
	      'argtype 1
	      'name 'generic-hash
	      'method-class <method>))

      (add-method generic-hash
		  (make <method>
			'signature (list <i-function>)
			'function (method-lambda (x)
				    99)))

      (add-method generic-hash
		  (make <method>
			'signature (list <object>)
			'function (method-lambda (x)
				    0)))

      (add-method generic-hash
		  (make <method>
			'signature (list <integer>)
			'function (method-lambda (x)
				    (abs x))))

      (add-method generic-hash
		  (make <method>
			'signature (list <symbol>)
			'function (method-lambda (x)
				    (symbol-hash x))))

      (add-method generic-hash
		  (make <method>
			'signature (list <string>)
			'function (method-lambda (x)
				    (generic-hash (make-symbol x)))))

      (set-standard-tab-functions generic-hash eq)

      (export generic-hash)

      ;; Setters and the like...


      (defconstant i-function-setters (make-table ()))

      (defconstant i-setter
	(method-lambda (x)
		       (let ((xx (sys-table-ref i-function-setters x)))
			 (if (functionp xx)
			     xx
			   (error "Setter: no setter for function" <Internal-Error> 'error-value x)))))

      (defconstant i-setter-setter
	(method-lambda (x y)
		       (if (if (functionp x)
			       (if (functionp y) t nil) nil)
			   ((setter sys-table-ref) i-function-setters x y)
			 (error "Bad setter" <Internal-Error> 'error-value (cons x y)))))

      (add-method setter
		  (make <method>
			'signature (list <i-function>)
			'function i-setter))

      (add-method setter
		  (make <method>
			'signature (list <c-function>)
			'function c-setter))

      (add-method setter-setter
		  (make <method>
			'signature (list <i-function> <object>)
			'function i-setter-setter))

      (add-method setter-setter
		  (make <method>
			'signature (list <c-function> <object>)
			'function c-setter-setter))

      ;; I know it looks nasty....
      (setter-setter setter setter-setter)

      ;; errors
      (defconstant error
	(lambda (message type . junk)
	  (let ((lst (cons 'message (cons message junk))))
	    (internal-signal (initialize (allocate type lst) lst)
			     nil))))

	(defconstant cerror
	  (lambda (message type . junk)
	    (let ((lst (cons 'message (cons message junk))))
	      (simple-call/cc
	       (lambda (cont)
		 (internal-signal (initialize (allocate type lst) lst)
				  cont))))))

      (export error cerror)  
      
      (compile-time 
       (defun apply (fn a1 . rest)
	 (labels ((aux (last rest)
		       (if (cdr rest)
			   (let ((next (cons (car rest) nil)))
			     ((setter cdr) last next)
			     (aux next (cdr rest)))
			 (if (if (car rest) (consp (car rest)) t)
			     ((setter cdr) last (car rest))
			   (error "apply: last arg must be a list" <Internal-Error> 'error-value (car rest))))))
		 (if rest
		     (let ((first (cons a1 nil)))
		       (aux first rest)
		       (%do-apply fn first))
		   (if (consp a1) 
		       (%do-apply fn a1)
		     (if (null a1)
			 (%do-apply fn nil)
		       (error "apply: arg must be a list" <Internal-Error> 'error-value a1))))))
       (export apply)
       )


      ;; Integer Methods. Needed by telos
      (add-method binary+ (make <method>
				'signature (list <fixint> <fixint>)
				'function binary+_Integer))

      (add-method binary- (make <method>
					 'signature (list <fixint> <fixint>)
					 'function binary-_Integer))

      (add-method binary* (make <method>
					 'signature (list <fixint> <fixint>)
				     'function binary*_Integer))

      (add-method binary/ (make <method>
					 'signature (list <fixint> <fixint>)
				     'function binary/_Integer))

      (add-method binary-lcm (make <method>
					    'signature (list <fixint> <fixint>)
					    'function binary-lcm-integer))

      (add-method binary-gcd (make <method>
					    'signature (list <fixint> <fixint>)
					    'function binary-gcd-integer))

      (add-method binary< (make <method>
					 'signature (list <fixint> <fixint>)
					 'function binary<_Integer))

      (add-method negate (make <method>
					'signature (list <fixint>)
					'function negate-integer))

(compile-time
  (defun mapcar  (fn l1 . others)
    (if (null others)
	(mapcar1 fn l1)
      (let ((allargs (cons l1 (copy-list others))))
	(labels ((step-lists (result last)
			     (let ((next (get-new-lsts allargs)))
			       (if (null next)
				   result
				 (let ((new (list (apply fn next))))
				   (if (null result)
				       (step-lists new new)
				     (progn ((setter cdr) last new)
					    (step-lists result new)))))))
		 (get-new-lsts1 (arglst lastarg)
			       (if (null arglst) t
				 (if (null (car arglst)) nil
				   (let ((new (cons (car (car arglst)) nil)))
				     ((setter cdr) lastarg new)
				     ((setter car) arglst (cdr (car arglst)))
				     (get-new-lsts1 (cdr arglst) new)))))
		 (get-new-lsts (args)
			       (if (null (car args)) nil
				 (let ((newargs (cons (car (car args)) nil)))
				   ((setter car) args (cdr (car args)))
				   (if (get-new-lsts1 (cdr args) newargs)
				       newargs
				     nil)))))
		(step-lists nil nil)))))
  
  (defun mapc  (fn l1 . others)
    (if (null others)
	(mapc1 fn l1)
      (let ((allargs (cons l1 (copy-list others))))
	(labels ((step-lists ()
			     (let ((next (get-new-lsts allargs)))
			       (if (null next)
				   nil
				 (progn (apply fn next)
					(step-lists)))))
		 (get-new-lsts1 (arglst lastarg)
			       (if (null arglst) t
				 (if (null (car arglst)) nil
				   (let ((new (cons (car (car arglst)) nil)))
				     ((setter cdr) lastarg new)
				     ((setter car) arglst (cdr (car arglst)))
				     (get-new-lsts1 (cdr arglst) new)))))
		 (get-new-lsts (args)
			       (if (null (car args)) nil
				 (let ((newargs (cons (car (car args)) nil)))
				   ((setter car) args (cdr (car args)))
				   (if (get-new-lsts1 (cdr args) newargs)
				       newargs
				     nil)))))
		(step-lists)))))

  (export mapc mapcar)


  )
      ))

  (defun method-initialisation-code (classlist)
    finalise-bootstrap-literals)



  ;; end module
)

