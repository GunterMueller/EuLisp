;; Eulisp Module
;; Author: pete broadbery
;; File: syntax-env.em
;; Date: 31/aug/1991
;;
;; Project:
;; Description: 
;;   "optimised" syntatic env reference
;;

(defmodule syntx-env 
  ((except (fold) standard)
   list-fns
   abs-syntx
   comp-utl
   mod-cache

   )
  ()
  
  (expose mod-cache)
  (expose abs-syntx)

  (defstruct module-imports ()
    ((imports initarg imports 
	      reader module-imports-imports)
     (macros initarg macros
	     accessor module-import-macros)
     (text initarg text 
	   reader module-import-desc))
    constructor (make-module-imports text imports macros))
  
  (defun module-imports-list (mi)
    (mapcar cdr (module-imports-imports mi)))

  (export module-imports-list module-import-desc)

  ;; Define a new syntax object with convienient properties

  (defstruct imported-definition abs-definition
    ((home initarg home
	   reader import-home-mod))
    ;; ide for name in abs-syntx.
    constructor (make-imported-definition ide props home)
    predicate imported-defn-p
    )

  (export imported-definition imported-defn-p)

  ;; copied in comp-defn
  (defconstant *local-module-name* '%%-me-local--%%)

  (defgeneric external-name (x)
    methods ((((import imported-definition))
	      (let ((xx (defn-prop-ref import 'address)))
		(if (numberp (cdr xx))
		    (cons (car xx) (defn-ide import))
		  (cons (car xx) (cadr xx)))))
	     (((x local-definition))
	      (cons *local-module-name* (defn-ide x)))))

  (export external-name)

  (defun import-home (x)
    (car (defn-prop-ref x 'address)))
  
  (defun import-name (x)
    (let ((xx (defn-prop-ref x 'address)))
      (if (consp (cdr xx)) (cadr xx)
	(defn-prop-ref x 'name))))

  (defun import-real-name (x) 
    (defn-prop-ref x 'name))
  
  (defun import-object-type (x) 
    (let ((xx (defn-prop-ref x 'class)))
      (if (null xx)
	  'internal
	xx)))

  (defun import-defn-setter (x)
    (let ((setter (defn-prop-ref x 'setter)))
      (if setter
	  (make-imported-definition 
	   'some-setter setter (import-home-mod x))
	nil)))

  (defun import-function-nargs (x)
    (let ((xx (defn-prop-ref x 'argtype)))
      (cond ((consp xx) (car xx))
	    ;; xxx should be something strange...
	    ((null xx) (cons nil 0))
	    (t (cons (< xx 0)
		     (if (< xx 0) (- xx) xx))))))
  
  
  (defun import-module-prop-ref (x name)
    (assq (imported-module-props (import-home x)) name))

  (export defn-prop-ref import-home import-real-name 
	  import-object-type import-function-nargs
	  import-defn-setter)

  ;; 
  (defun is-macro (obj)
    (eq (defn-prop-ref obj 'class) 'macro))

  (defun expander (obj)
    (get-expander (import-home obj)
		  (import-name obj)))
  
  (defun compile-macro-expand (expander forms)
    ((setter compile-time-p) t)
    (prog1 (apply expander forms)
      ((setter compile-time-p) nil)))

  (export compile-macro-expand)

  (defun find-macro (env name)
    (assq name (module-import-macros env)))

  (defun simple-find-name (env name)
    (assq name (module-imports-imports env)))

  (deflocal lastenv (cons nil nil))

  (defun find-name (env name)
    (cond ((eq (car lastenv) env)
	   (or (table-ref (cdr lastenv) name)
	       (let ((xx (simple-find-name env name)))
		 ((setter table-ref) (cdr lastenv) name xx)
		 xx)))
	  (t (setq lastenv (cons env (make-table eq)))
	     (find-name env name))))

  (export find-name find-macro expander)
  
  ;; returns a module-imports structure

  (defconstant mod-imports-cache (mk-finder))

  (deflocal xx nil)

  (defun read-imports (ispec)
    (or (mod-imports-cache ispec)
	(let* ((imports (generic-read-imports ispec))
	       (macros (collect (lambda (x) (is-macro (cdr x)))
				imports))
	       (mi (make-module-imports (import-text ispec) imports macros)))
	  ((setter mod-imports-cache) ispec mi)
	  mi)))
	       
  ;; returns a list of import objects
  (defgeneric generic-read-imports (import-specifier))
  
  (defmethod generic-read-imports ((import import-directive))
    (format t "Env: generic-read-imports: ~a~%" (import-directive-name import))
    (let ((xx (mapcar construct-import-defn (read-exportations (import-directive-name import)))))
      (format t "Env: Done ~a~%" (import-directive-name import))
      xx))
  
  ;; import is (name props module)

  (defun construct-import-defn (import)
    (cons (car import)
	  (make-imported-definition (car import)
				    (cadr import)
				    (caddr import))))
  ;; this *should* check for name-clashes....
  (defun xx-mapcan (f l)
    (fold (lambda (x lst)
	    (nconc lst (f x)))
	  l
	  nil))

  (defmethod generic-read-imports ((union union-directive))
    (xx-mapcan generic-read-imports (union-content union)))


  (defmethod generic-read-imports ((only only-directive))
    (collect (lambda (x) 
	       (format t "Only: ~a ~a~%" x  (only-name-list only))
	       (let ((xx (memq (car x) (only-name-list only))))
		 (if xx x nil)))
	     (generic-read-imports (only-imports only))))
  
  (defmethod generic-read-imports ((rename rename-directive))
    (format t "Rename: ~a~%" (rename-name-list rename))
    (mapcar (lambda (import)
	      (let ((new (assq (car import)
			       (rename-name-list rename))))
		(if new (format t "Rename: ~a->~a~%" (car import) new) nil)
		(if (null new) import
		  (cons (cadr new)
			(make-imported-definition (cadr new)
						  ;; hack to get name+c right.
						  (cons (cons 'name (cadr new))
							(cons (list 'address 
								    (car (defn-prop-ref (cdr import)
											  'address))
								    (car new))
							      (defn-properties (cdr import))))
						  (import-home-mod (cdr import)))))))
	    (generic-read-imports (rename-imports rename))))

  (defmethod generic-read-imports ((except except-directive))
    (collect (lambda (x) (not (memq (car x) (except-name-list except))))
	     (generic-read-imports (except-imports except))))
  

  (export read-imports)

  ;; Name lists

  (defun module-exported-names (lst)
    (mapcar external-name lst))

  (defun module-imported-names (obj)
    (mapcar (lambda (x) (external-name (cdr x)))
	    (module-imports-imports obj)))
  
  (defgeneric import-text (ispec)
    methods ((((x import-directive))
	      (list 'import (import-directive-name x)))
	     (((x union-directive))
	      (cons 'union 
		    (mapcar import-text
			    (union-content x))))
	     (((x only-directive))
	      (list 'only (only-name-list x)
		    (import-text (only-imports x))))
	     (((x rename-directive))
	      (list 'rename (rename-name-list x)
		    (import-text (rename-imports x))))
	     (((x except-directive))
	      (list 'except (except-name-list x) 
		    (import-text (except-imports x))))
	     (((x <object>))
	      (error "Import-text: strange-directive" <clock-tick>
		       'error-value x))))

  (defgeneric flatten-imports (ispec)
    methods ((((x import-directive))
	      (list (import-directive-name x)))
	     (((x union-directive))
	      (xx-mapcan (lambda (import)
			(flatten-imports import))
		      (union-content x)))
	     (((x rename-directive))
	      (flatten-imports (rename-imports x)))
	     (((x except-directive))
	      (flatten-imports (except-imports x)))
	     (((x only-directive))
	      (flatten-imports (only-imports x)))
	     (((x <object>))
	      (error "Import-text: strange-directive" <clock-tick>
		     'error-value x))))
 
    
  (export module-exported-names module-imported-names flatten-imports)
  ;; end module
)
