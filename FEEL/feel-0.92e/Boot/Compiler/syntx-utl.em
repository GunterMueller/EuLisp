;; Eulisp Module
;; Author: pete broadbery
;; File: syntax-utils.em
;; Date: 15/sep/1991
;;
;; Project: Compiler
;; Description: 
;;   General utils related to ast's
;;  Guesses type of fn. calls.

(defmodule syntx-utl 
  ((except (fold) standard)
   list-fns
   
   syntx-env
   pass
   props

   stop
   )
  ()
  
  (expose syntx-env)

  (defun find-decls (defn)
    (cond ((module-definition-p defn)
	   nil)
	  ((definition-p defn) 
	   (list defn))
	  ((and-decl-p defn)
	   (fold append
		 (mapcar find-decls
			 (and-decl-decls defn))
		 nil))
	  ((rec-decl-p defn)
	   (find-decls (rec-decl-decl defn)))))
  
  (export find-decls)

  (defgeneric get-internal-closed-bindings (obj))

  (defmethod get-internal-closed-bindings ((x syntax-obj))
    (fold append
	  (mapcar get-internal-closed-bindings (subcomponents x))
	  nil))

  (defmethod get-internal-closed-bindings ((x definition))
    (if (binding-closed x) 
	(cons x (call-next-method))
      (call-next-method)))

  (defmethod get-internal-closed-bindings ((x lambda-term))
    ;; XXX Not if its inline...
    nil)

  ;; finds the innermost non-tail posn lambda
  
  (defun get-enclosing-object (fn start)
    (get-enclose-aux fn (enclosing-block start)))

  (defun get-enclose-aux (fn obj)
    (if (fn obj)
	obj
      (get-enclose-aux fn (enclosing-block obj))))

  (defun enclosing-lambda (obj)
    (get-enclosing-object is-real-lambda obj))

  (defgeneric is-real-lambda (obj)
    methods ((((x lambda-term))
	      t)
	     (((x module-block))
	      t)
	     (((x <object>))
	      nil)))

  (defun enclosing-module (x)
    (get-enclosing-object module-p x))
  
  (export get-internal-closed-bindings enclosing-lambda)

  (defun function-fn (lst)
    (cdr (assq 'object lst)))
  
  (defun function-type (lst)
    (cdr (assq 'class lst)))

  (defun function-prop (lst x)
    (cdr (assq x lst)))

  ;; if we dont know, pretend we do+that it will be sorted out later.
  (defun function-nargs (lst)
    (let ((type (assq 'argtype lst)))
      (if (null type) (cons () 9999)
	(cond ((consp (cdr type))
	       (car (cdr type)))
	      (t (cons (< (cdr type) 0) 
		       (if (< (cdr type) 0) (- (cdr type)) (cdr type))))))))

  (defun function-nary-p (obj) 
    nil)

  ;; If at all possible, find the function object referenced by obj. 
  ;; Guessing what a function is.
  
  (defgeneric find-fn (x)
    methods ((((x syntax-obj))
	      (cons (cons 'object x) (unknown-object-properties)))
	     (((x ident-term))
	      (let ((props
		     (cons (cons 'object (ident-decl x)) (read-defn-properties (ident-decl x)))))
		(if (eq (function-type props) 'bytefunction)
		    (cond ((module-definition-p (ident-decl x))
			   (cons (cons 'class 'local) props))
			  ((definition-p (ident-decl x))
			   (cons (cons 'class 'lexical) props))
			  (t props))
		  props)))
	     (((x lambda-id))
	      (cons (cons 'object x) (unknown-object-properties)))
	     (((x special-term))
	      (compute-special-proplist x))
	     (((x abs-definition))
	      (read-defn-properties x))
	     (((x applic-term))
	      (let ((xx (compute-compile-time-proplist x)))
		(if (null xx)
		    (cons (cons 'object x) (unknown-object-properties))
		  (progn (format t "Inlining ~a~%" (cdar xx))
			 xx))))
	     (((x <object>))
	      (format t "Unknown object: ~a~%" x)
	      (error "dunno" <clock-tick>))
	     ))
  
  (export function-fn function-type function-nargs function-nary-p find-fn function-prop)

  ;; accessing an objects property list...

  (defun unknown-object-properties ()
    '((class . unknown) (mutable nil)))

  (defgeneric read-defn-properties (defn)
    methods ((((defn imported-definition))
	      (defn-properties defn))
	     (((defn local-definition))
	      (if (decl-done-properties defn)
		  (defn-properties defn)
		(let ((props (compute-properties defn)))
		  ((setter decl-done-properties) defn t)
		  ((setter defn-properties) defn 
		   (append props (defn-properties defn)))
		  props)))
	     (((defn lambda-id))
	      (unknown-object-properties))
	     (((defn <object>))
	      (error "no way" <clock-tick>))))

  ;; Calculate the property list for a binding
  (defgeneric compute-properties (defn))
  
  (defmethod compute-properties ((defn local-definition))
    (if (binding-mutable defn) 
	(list (list 'mutable t) 
	      (list 'class 'unknown))
      (cons (list 'mutable nil)
	    (classify (defn-body defn)))))
  
  (defmethod compute-properties ((defn module-definition))
    (let ((lst (call-next-method)))
      (append (list (list 'address (module-name (enclosing-module defn)) (defn-ide defn)))
	      (append (list (cons 'name (defn-ide defn)))
		      lst))))

  (defun compute-special-proplist (special)
    (generic-classify special))
  
  
  ;; Real analysis: Find the type of a declaration.
  ;; Returns a-list --- keys: mutable, class (bytefunction, bytemacro, internal, 
  ;;                  function --- 'C', object), argtype, 
  
  (defun classify (obj)
    (generic-classify obj))

  (defgeneric generic-classify (body))
  
  (defmethod generic-classify ((lam lambda-term))
    (list (cons 'class 'bytefunction)
	  (list 'argtype (lambda-nargs lam))))

  (defmethod generic-classify ((mlam macro-lambda-term))
    (list (cons 'class 'macro)
	  (list 'argtype (lambda-nargs mlam))))

  (defmethod generic-classify ((x term))
    (list (cons 'class 'unknown)))

  (defmethod generic-classify ((x special-term))
    (cond ((eq (special-term-name x) 'inline-fn)
	   (list (cons 'class 'inline)
		 (cons 'argtype (car (special-term-data x)))
		 (cons 'code (cdr (special-term-data x)))))
	  ((eq (special-term-name x) 'call-next-method-internal)
	   (list (cons 'object x) (cons 'class 'special)))
	  (t (cons 'class 'unknown))))
    
  (defun decl-class (x)
    (let ((props (read-defn-properties x))
	  (setter (decl-setter x)))
      (if (null setter)
	  props
	(append props 
		(list (cons 'setter (read-defn-properties setter)))))))

;;    (let ((xx (decl-class-uncached x)))
;;      (if (null xx)
;;	  (let ((aa (classify-decl x)))
;;	    ((setter decl-class-uncached) x aa)
;;	    aa)
;;	xx))

  (defun std-class-list (x)
    ;; just the address at the moment...
    (cons (cons 'name (defn-ide x))
	  (cons (list 'address (module-name (enclosing-module x)) (defn-ide x))
		(let ((xx nil)) ;;; (obj-setter x)
		  (if (null xx)
		      nil
		    (list (cons 'setter (decl-class xx))))))))
		
  (export decl-class)

  ;; dependencies...
  
  (defun add-dependency (mod defn)
    (if (memq (import-home defn) (module-dependencies mod))
	nil
      ((setter module-dependencies) mod
       (cons (import-home defn) (module-dependencies mod)))))
  
  (export add-dependency)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (defun compute-compile-time-proplist (x)
    (let ((val (compute-compile-time-value x)))
      (if (abs-definition-p val)
	  (cons (cons 'object val) (read-defn-properties val))
	nil)))

  (defgeneric compute-compile-time-value (x))

  (defmethod compute-compile-time-value ((app applic-term))
    (let ((fn (compile-time-value (applic-fun app)))
	  (args (applic-args app)))
      (cond ((null fn) nil)
	    ((setter-function-p fn)
	     (find-setter (compile-time-value (car args))))
	    (t nil))))

  (defmethod compute-compile-time-value ((x ident-term))
    (compute-compile-time-value (car (ident-defblock x))))

  (defmethod compute-compile-time-value ((x module-definition))
    (if (binding-mutable x) nil x))

  (defmethod compute-compile-time-value ((x imported-definition))
    x)

  (defmethod compute-compile-time-value ((x syntax-obj))
    nil)

  ;; Generic --- could look at the property thang...
  ;;
  ;; nasty local-defn case cos of
  ;; (let ((x (if foo a b)))
  ;;   ((setter x) y))
  (defgeneric find-setter (value)
    methods ((((defn imported-definition))
	      (import-defn-setter defn))
	     (((defn local-definition))
	      (decl-setter defn))
	     (((o <object>))
	      nil)))

  (defun compile-time-value (x)
    (let ((xx (cached-compile-time-value x)))
      (cond ((null xx)
	     (let ((val (compute-compile-time-value x)))
	       ((setter cached-compile-time-value) x 
		(if (null val) 'no-way val))
	       val))
	    ((eq xx 'no-way) nil)
	    (t xx))))
  
  ;; setting setter functions
  
  (defgeneric set-setter-function (fn new-setter)
    methods ((((ident ident-term) new-setter)
	      (set-setter-function (ident-decl ident) new-setter))
	     ((thing (ide ident-term))
	       (set-setter-function thing (ident-decl ide)))
	     (((decl local-definition) (setter-decl module-definition))
	      (if (or (binding-mutable decl)
		      (binding-mutable setter-decl))
		  nil
		((setter decl-setter) decl setter-decl)))
	     (((decl imported-definition) (setter-decl module-definition))
	      (if (or (binding-mutable decl)
		      (binding-mutable setter-decl))
		  nil
		(add-defn-prop decl 'setter (read-defn-properties setter-decl))))
	     (((o1 <object>) (o2 <object>))
	      nil)))
  
  (defun setter-function-p (x) 
    (let ((aa (defn-prop-ref x 'setter-function)))
      aa))
	
  ;; end module
  )
  (defgeneric find-fn (obj)
    methods ((((x syntax-obj))
	      (list x 'unknown 0))
	     (((x applic-term))
	      (let ((xx (compile-time-value x)))
		(if (null xx)
		    (list x 'unknown 0)
		  (find-fn xx))))
	     (((x lambda-term))
	      ;;(format t "Lambda: nargs: ~a~%" (lambda-nargs x))
	      (list x 'bytefunction (lambda-nargs x)))
	     (((x ident-term))
	      (let ((props (read-defn-properties (ident-decl x))))
		(list (ident-decl x) 'unknown 0)))
	      ;;(let ((xx (find-fn (car (ident-defblock x)))))
	      ;;(if (and (eq (function-type xx) 'local-defun)
	      ;;(eq (car xx) (enclosing-lambda x)))
	      ;;(cons (car xx)
	      ;;(cons 'lexical
	      ;;(cddr xx)))
	      ;;xx))
	     (((x definition))
	      (if (defn-mutable-p x)
		  (list x 'unknown 0)
		(let ((actual (find-fn (defn-body x))))
		  ;; try to env+call via a jump
		  (cond ((eq (function-type actual) 'bytefunction)
			 (cons (car actual) 
			       (cons 'lexical ;; was local-defun
				     (cddr actual))))
			((eq (function-type actual) 'special)
			 actual)
			(t (list x 'unknown 0))))))
	     (((x module-definition))
	      (if (defn-mutable-p x) 
		  (list x 'unknown 0)
		(let ((actual (find-fn (defn-body x))))
		  (cond ((eq (function-type actual) 'bytefunction)
			 (cons (car actual) 
			       (cons 'local
				     (cddr actual))))
			((eq (function-type actual) 'special)
			 actual)
			(t (list x 'unknown 0))))))
	     (((x imported-definition))
	      ;; defined in syntax-utils
	      (list x (import-object-type x) 
		    (import-function-nargs x)))
	     (((x special-term))
	      (list x 'special 0))
	     (((x object))
	      ;;(format t "Find Fn Got Strange: ~a~%" x)
	      (stop x))))
