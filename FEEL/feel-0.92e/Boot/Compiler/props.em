;; Eulisp Module
;; Author: pete broadbery
;; File: props.em
;; Date: 3/sep/1991
;;
;; Project:
;; Description: 
;; syntactic properties go here -- should be a web of structures...
;;

(defmodule props 
  (standard0
   list-fns
   low-seman
   )
  ()
  
  ;; From low-seman...
  (export print-props)

  (defconstant enclosing-block (make-semantic-ref 'enclosing-block))

  (defconstant binding-used (make-semantic-ref 'binding-used))
  (defconstant binding-closed (make-semantic-ref 'binding-closed))
  (defconstant binding-mutable (make-semantic-ref 'binding-mutable))
  (defconstant binding-as-arg (make-semantic-ref 'binding-as-arg))

  (defconstant cached-use-set (make-semantic-ref 'use-set))

  (defconstant inline-lambda (make-semantic-ref 'inline-lambda))
  
  ;; a pair: (nary? . args)
  (defconstant lambda-nargs (make-semantic-ref 'lambda-nargs))
  
  (defconstant lambda-closed-p (make-semantic-ref 'lambda-closed-p))
  (defconstant lambda-inhibit-alloc (make-semantic-ref 'lambda-inhibit-alloc))
  (defconstant term-tail-call (make-semantic-ref 'tail-call))
  (defconstant term-fn-call-loc (make-semantic-ref 'term-fn-call-loc))
  (defconstant ident-defblock (make-semantic-ref 'ident-defblock))
  
  (defun ident-decl (x)
    (car (ident-defblock x)))

  (defun ident-block (x)
    (cdr (ident-defblock x)))

  ;; Just for modules
  (defconstant module-declarations (make-semantic-ref 'module-declarations))
  (defconstant module-imports (make-semantic-ref 'module-imports))
  (defconstant module-exports (make-semantic-ref 'module-exports))
  (defconstant module-flat-imports (make-semantic-ref 'module-flat-import))
  (defconstant module-name-cache (make-semantic-ref 'module-name-cache))
  
  ;; actual, not what the imports claim
  (defconstant module-dependencies (make-semantic-ref 'module-dependencies))
  
  (export enclosing-block inline-lambda term-tail-call term-fn-call-loc
	  ident-defblock ident-decl ident-block
	  lambda-nargs lambda-closed-p lambda-inhibit-alloc binding-used 
	  binding-closed binding-as-arg binding-mutable cached-use-set 
	  module-declarations module-name-cache module-imports 
	  module-exports module-dependencies module-flat-imports)

  ;; Interface properties
  
  (defconstant decl-class-uncached (make-semantic-ref 'decl-class))
  (defconstant decl-done-properties (make-semantic-ref 'decl-done-properties))
  (export decl-class-uncached decl-done-properties)
  (defconstant decl-setter (make-semantic-ref 'decl-setter))

  ;; Compiler properties
  ;; For recording the state of the compiler at an object
  (defconstant cached-compile-time-value (make-semantic-ref 'compile-time-value))

  (defconstant compile-state (make-semantic-ref 'compile-state))

  (defconstant condition-label (make-semantic-ref 'condition-label))

  (defconstant lambda-tail-call-label (make-semantic-ref 'lambda-tail-label))
  (defconstant real-lambda-env (make-semantic-ref 'real-lambda-env))
  (defconstant lambda-init-label (make-semantic-ref 'lambda-init-label))
  
  (defconstant binding-posn (make-semantic-ref 'binding-posn))
  (defconstant lambda-tidyup (make-semantic-ref 'lambda-tidyup))

  (export compile-state condition-label cached-compile-time-value)
  
  (export lambda-tail-call-label real-lambda-env lambda-init-label binding-posn
	  lambda-tidyup decl-setter)
  
  
  ;; end module
  )
