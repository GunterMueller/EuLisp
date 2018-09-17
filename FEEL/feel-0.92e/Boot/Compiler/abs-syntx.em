;; Eulisp Module
;; Author: pete broadbery
;; File: abs-syntx.em
;; Date: 30/jun/1991
;;
;; Project: Compiler
;; Description: 
;;  abstract sysntax model
;; will be used nearly everywhere
;;

(defmodule abs-syntx 
  (standard0
   list-fns
   scan-args
   )
  ()
  
  ;; 
  ;;  Syntax
  ;;  structures

  (defstruct syntax-obj () 
    ((properties accessor syntactic-properties))
    predicate syntactic-object-p)

  (export syntactic-properties)
  ;; given a prototype object plus initargs, make a property structure 
  ;; really a forward reference for the analysis phases.
  (defgeneric make-syntactic-properties (syntax lst)
    ;; for now...
    methods ((((x <object>) lst)
	      nil)))
  
  (defmethod initialize-instance ((proto syntax-obj) lst)
    (let ((new-obj (call-next-method)))
      (make-syntactic-properties new-obj lst)
      new-obj))
  
  (export make-syntactic-properties)

  (defstruct term syntax-obj
    ()
    predicate term-p)
  
  (defstruct leaf-term term
    ()
    predicate leaf-term-p)

  (defstruct ident-term leaf-term
    ((ide initarg ide reader term-id))
    constructor (ident ide)
    predicate ident-p)

  (defstruct literal-term leaf-term
    ((content initarg content 
	      reader literal-content))
    constructor (literal content)
    predicate literal-p)

  (defstruct error-term leaf-term
    ()
    predicate error-term-p
    constructor (make-error-term))

  ;; class: special-term
  ;;  Internal object used where we effectively have an 
  ;;  application, but the normal channels ain't up to it.

  (defstruct special-term leaf-term
    ((name initarg name reader special-term-name)
     (data initarg data reader special-term-data)
     (obj-data initform () initarg obj-data
	       reader special-term-objects))
    constructor (mk-special-term name data)
    constructor (mk-special-term2 name data obj-data))

  (defstruct condition-term term
    ((test initarg test
	   accessor cond-test)
     (t-part initarg t-part 
	     accessor cond-t-part)
     (f-part initarg f-part 
	     accessor cond-f-part))
    constructor (make-cond test t-part f-part))


  (defstruct lambda-id syntax-obj 
    ((name initarg name 
	   reader lambda-id-name))
    constructor (make-lambda-id name))

  (defstruct applic-term term
    ((fun initarg fun accessor applic-fun)
     (args initarg args accessor applic-args))
    constructor (make-applic fun args))
  
  ;; bit of a hack
  (defstruct call-next-method-term applic-term
    ()
    constructor (mk-call-next-method-term))

  (defstruct block-obj term 
    ((body initarg body accessor block-body))
    )

  (defstruct block-term block-obj
    ((decl initarg decl accessor block-decl))
    constructor (make-block decl body))
  
  (defstruct lambda-term block-obj
    ((id-list initarg id-list 
	      accessor lambda-ids))
    constructor (make-lambda id-list body))
  
  (defconstant lambda-body block-body)
  
  (defstruct macro-lambda-term lambda-term
    ()
    constructor (make-macro-lambda id-list body))
  
  (defstruct extended-lambda-term lambda-term
    ((name initarg name reader extended-lambda-name)
     (comment initarg comment reader extended-lambda-comment))
    constructor (make-extended-lambda name comment id-list body))

  (defstruct sequence term
    ((content initarg content
	      accessor sequence-content)
     (end accessor sequence-end)
     (rest initarg rest
	   accessor sequence-body))
    constructor (make-sequence content))
  
  (defmethod initialize-instance ((proto sequence) initargs)
    (let ((new (call-next-method))
	  (lst (reverse (scan-args 'content initargs ()))))
      ((setter sequence-end) new (car lst))
      ((setter sequence-body) new (nreverse (cdr lst)))
      new))

  (defstruct assignment-term term ;; albeit an expletive
    ((var initarg var accessor assign-var)
     (body initarg body accessor assign-body))
    constructor (assignment var body))
     
  (export syntax-obj term ident-term  condition-term lambda-term
	  applic-term block-obj block-term sequence literal-term assignment-term
	  error-term make-error-term)
  (export extended-lambda-term make-extended-lambda extended-lambda-name extended-lambda-comment)
  (export ident make-cond make-lambda make-applic make-block make-sequence
	  literal assignment literal-p ident-p make-macro-lambda macro-lambda-term)
  (export special-term mk-special-term mk-special-term2 
	  special-term-name special-term-data special-term-objects)
  (export leaf-term leaf-term-p error-term-p)
  (export term-p)
  (export term-id)
  (export literal-content)
  (export cond-test cond-f-part cond-t-part)
  (export lambda-ids lambda-body)
  (export applic-fun applic-args)
  (export block-decl block-body)
  (export sequence-content sequence-body sequence-end)
  (export assign-body assign-var)
  (export lambda-id lambda-id-name make-lambda-id)
  
  (export mk-call-next-method-term call-next-method-term)
  ;; declarations

  (defstruct decl syntax-obj
    () 
    predicate decl-p)

  ;; need an abstract class so we can distingush between defn. classes
  (defstruct abs-definition decl
    ((ide initarg ide accessor defn-ide)
     (props initform () initarg props accessor defn-properties))
    predicate abs-definition-p
    )
  
  (defun defn-prop-ref (x arg)
    (let ((xx (assq arg (defn-properties x))))
      (if (null xx)  nil
	(cdr xx))))

  (defun add-defn-prop (defn prop val)
    ((setter defn-properties) defn
     (cons (cons prop val) (defn-properties defn))))

  (defstruct local-definition abs-definition
    ((body initarg body accessor defn-body)
     (mutable initarg mutable accessor defn-mutable-p))
    predicate definition-p)

  (defstruct module-definition local-definition
    ()
    constructor (make-module-definition ide body mutable)
    predicate module-definition-p)

  (defstruct definition local-definition
    ()
    constructor (make-definition ide body mutable))

  (defstruct and-decl decl
    ((decls initarg decls 
	    accessor and-decl-decls))
    constructor (make-and-decl decls)
    predicate and-decl-p)

  (defstruct rec-decl decl
    ((decl initarg decl 
	   accessor rec-decl-decl))
    constructor (make-rec-decl decl)
    predicate rec-decl-p)

  ;; modules
  (defstruct module-block syntax-obj
    ((name initarg name 
	   reader module-name)
     (import-spec initarg imports
		       accessor module-import-spec)
     (syntax-spec initarg syntax accessor module-syntax-spec)
     ;; I know it doesn't exist
     (export-spec initarg exports accessor module-export-spec)
     (body-forms initarg body reader module-body))
    predicate module-p
    constructor (make-module name imports syntax exports body . junk
			     ))

  ;; import/export specification

  (defstruct import-spec syntax-obj
    ()
    predicate import-spec-p)

  (defstruct syntax-spec syntax-obj
    ()
    predicate syntax-spec-p
    constructor (make-syntax-spex))

  (defstruct export-spec syntax-obj
    ()
    predicate export-spec-p)

  (defstruct export-directive export-spec
    ((name initarg name reader export-spec-name))
    constructor (make-export-directive name))

  (defstruct expose-directive export-spec
    ((mod-name initarg name reader expose-spec-importer))
    constructor (make-expose-directive name))

  (defstruct import-directive import-spec
    ((name initarg name
	   reader import-directive-name))
    constructor (make-import-directive name))
  
  (defstruct rename-directive import-spec
    ((name-list initarg name-list reader rename-name-list)
     (imports initarg name reader rename-imports))
    constructor (make-rename-directive name-list name))

  (defstruct except-directive import-spec
    ((name-list initarg name-list reader except-name-list)
     (name initarg name reader except-imports))
    constructor (make-except-directive name-list name))

  (defstruct only-directive import-spec
    ((name-list initarg name-list reader only-name-list)
     (name initarg name reader only-imports))
    constructor (make-only-directive name-list name))

  (defstruct union-directive import-spec
    ((content initarg content reader union-content))
    constructor (make-union-directive content))

  (export definition decl and-decl rec-decl module-definition)
  (export decl-p and-decl-p rec-decl-p definition-p abs-definition abs-definition-p
	  local-definition)
  (export make-definition defn-ide defn-body defn-mutable-p defn-properties defn-prop-ref add-defn-prop)
  (export make-module-definition module-definition-p)
  (export and-decl-decls make-and-decl)
  (export rec-decl-decl make-rec-decl)
  
  (export  module-block module-import-spec
	   module-syntax-spec
	   module-export-spec module-body)
  
  (export import-spec  import-spec-p syntax-spec-p make-syntax-spex 
	  make-module module-name module-p 
	  export-spec export-directive export-spec-name make-export-directive
	  import-directive import-directive-name
	  make-import-directive rename-directive
	  rename-imports rename-name-list rename-imports make-rename-directive 
	  except-directive except-name-list  except-imports 
	  make-except-directive
	  only-directive only-name-list  only-imports make-only-directive
	  union-directive union-content make-union-directive)
  
  (export expose-directive expose-spec-importer make-expose-directive)
  
  ;; printing methods (undefined for now)
  
  (defgeneric print-term (ob stream))
  (defgeneric print-decl (ob stream))

  (export print-term)
  (export print-decl)
  ;; end module
  )


