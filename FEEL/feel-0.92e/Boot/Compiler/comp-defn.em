;; Eulisp Module
;; Author: pab
;; File: comp-defn
;; Date: Wed Jan 29 21:51:30 1992
;;
;; Project:
;; Description: 
;;   What is  produced by a compilation
;;   and is read by the load/link phase

(defmodule comp-defn
  (standard0
   list-fns
   
   comp-utl
   )
  ()
  (expose comp-utl)
  ;; Target for compilation

  (defstruct compile-unit ()
    ;; a-list of values
    ((statics initarg statics
	      reader compile-unit-statics)
     (name initarg name 
	   reader compile-unit-name)
     (local-count initarg local-count
		  reader compile-unit-local-count)
     (byte-codes initarg byte-codes
		 reader compile-unit-byte-codes)
     ;; Import format is (module-name name)
     (imports initarg imports 
	      reader compile-unit-imports)
     ;; list of ids
     (exports initarg exports
	      reader compile-unit-exports)
     (length initarg length
	     reader compile-unit-length)
     (local-names initarg local-names
		  reader compile-unit-local-names))
    constructor make-compile-unit)

  ;; Target for self-contained-code
  (defstruct sc-compile-unit ()
    ((statics initarg statics 
	      reader sc-statics)
     (code initarg code 
	   reader sc-code)
     (slots initarg nslots
		  reader sc-nslots)
     (length initarg length 
	     reader sc-length)
     (name-list initarg names
		reader sc-names)
     (dependencies initarg dependencies
		   reader sc-dependencies))
    constructor make-sc-unit)
  
  (defgeneric unit-name (x))
  (defmethod unit-name ((x compile-unit))
    (bytecode-file-name (compile-unit-name x)))

  (defmethod unit-name ((x sc-compile-unit))
    (sc-file-name (car (sc-names x))))

  (export compile-unit make-compile-unit compile-unit-statics
	  compile-unit-name
	  compile-unit-local-count compile-unit-byte-codes 
	  compile-unit-imports compile-unit-exports
	  compile-unit-length
	  sc-compile-unit 
	  sc-code sc-length
	  sc-statics sc-names
	  sc-nslots
	  sc-dependencies
	  unit-name)
  
  (defconstant *unresolved-label* '%%-unresolved-%%)
  (defconstant *link-handle* '%%-link-me-%%)
  (defconstant *long-label* '%%-big-arg-%%)
  (defconstant *local-module-name* '%%-me-local--%%)
  (defconstant *static-handle* '%%-static-%%)

  (defun the-unresolved-handle () 
    *unresolved-label*)

  (defun the-long-handle ()
    *long-label*)

  (defun the-link-handle ()
    *link-handle*)

  (defun the-local-handle ()
    *local-module-name*)

  (defun the-static-handle ()
    *static-handle*)

  (export the-long-handle the-link-handle the-unresolved-handle
	  the-local-handle the-static-handle)
  
  ;; end module
  )
