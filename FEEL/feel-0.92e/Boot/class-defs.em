;; Eulisp Module
;; Author: pab
;; File: <class>defs.em
;; Date: Mon Oct  5 13:20:58 1992
;;
;; Project:
;; Description: 
;;

(defmodule class-defs
  (   
   class-macs
   )
  ()
  (reset-classes)

  (define-prim-class <object> ()
    ()
    )

  (define-prim-class <class> (<object>)
    ((instance-size position 0
		    accessor class-instance-size)
     (name position 1 
	   accessor class-name)
     (super-classes position 2
		    accessor class-direct-superclasses)
     (subclasses position 3
		 accessor class-direct-subclasses)
     ;; actually slot-table...
     (local-slot-descriptions position 4
		       accessor class-local-slot-descriptions)
     (slot-descriptions position 5
		accessor class-slot-descriptions)
     (non-local-descriptions position 6
		   accessor class-non-local-slot-descriptions)
     (precedence position 7
		 accessor class-precedence-list)
     (initargs  position 8
		accessor class-initargs)
     (spare position 9
	    accessor class-spare))
    direct-initargs (direct-superclasses direct-slot-descriptions direct-initargs name)
    constructor make-class
    metaclass <class>)
  
  
  ;; Telos classes:
  (define-prim-class <instantiable-class> (<class>)
    ()
    allocate t)

  (define-prim-class <si-class> (<instantiable-class>)
    ()
    allocate t)
  
  (define-prim-class <abstract-class> (<class>)
    ()
    metaclass <class>
    allocate t)

  (define-prim-class <structure-class> (<si-class>)
    ()
    metaclass <class>
    allocate t)

  (define-prim-class <mi-class> (<instantiable-class>)
    ()
    allocate t)

  (define-prim-class <slot-description-class> (<class>)
    ()
    metaclass <class>
    allocate t)

  (define-prim-class <structure> (<object>)
    ()
    metaclass <structure-class>
    allocate t)
     
  ;; from slots.h
  (define-prim-class <slot-description> (<object>)
    ((name position 0
	   accessor slot-description-name
	   initarg name)
     (position position 1
	       initarg position
	       accessor slot-description-position)
     ;; ack!
     (initarg position 2
	      initarg initarg
	      ;;initform unbound-slot-value
	      accessor slot-description-initarg)
     (initfunction position 3
		   initarg initfunction
		   ;;initform unbound-slot-value
		   accessor slot-description-initfunction)
     (slot-writer position 4
		  initarg writer
		  accessor slot-description-slot-writer)
     (slot-reader position 5
		  initarg reader
		  accessor slot-description-slot-reader))
    direct-initargs (name position initfunction reader writer initarg)
    constructor make-slot-description
    metaclass <slot-description-class>
    allocate t)

  ;; should never be instatiated by new protocol
  (define-prim-class <local-slot-description> (<slot-description>)
    ()
    allocate t)
  
  (define-prim-class <unreadable-slot-description> (<local-slot-description>)
    ()
    allocate t
    metaclass <slot-description-class>)

  ;; function meta-hierarchy

  (define-prim-class <funcallable-object-class> (<class>)
    ()
    metaclass <class>)

  (define-prim-class <generic-class> (<funcallable-object-class>)
    ()
    metaclass <class>)

   (define-prim-class <bytefunction-class> (<funcallable-object-class>)
     ()
     metaclass <class>)

  (define-prim-class <funcallable-object> (<object>)
    ()
    metaclass <funcallable-object-class>
    allocate t
    )

  (define-prim-class <function> (<funcallable-object>)
     ;; usually a lisp <object>, but can be NULL!
    ((argtype position 0 class <unreadable-slot-description>)
     (env position 1 class <unreadable-slot-description>)
     (xxx position 2 class <unreadable-slot-description>)
     (name position 3 accessor function-name)
     (home position 4 accessor function-home))
    metaclass <funcallable-object-class>)
  
  (define-prim-class <i-function> (<function>)
    ((body position 5 accessor i-function-body))
    metaclass <funcallable-object-class>)

  (define-prim-class <c-function> (<function>)
    ()
    metaclass <funcallable-object-class>)

  ;;(compile-time 
  (define-prim-class <bytefunction> (<funcallable-object>)
    ((env position 0
	  accessor bytefunction-env)
     (offset position 1
	     accessor bytefunction-offset)
     (nargs position 2
	    accessor bytefunction-nargs)
     (globals position 3
	      accessor bytefunction-globals)
     (setter position 4
	     accessor bytefunction-setter))
    metaclass <bytefunction-class>)

  (define-prim-class <extended-bytefunction> (<bytefunction>)
    ((info position 5 accessor extended-bytefunction-info))
    metaclass <bytefunction-class>)
   ;;)


  ;; from generics.h
  ;; OK. The standard version
  (define-prim-class <generic-function> (<funcallable-object>)
    ((name position 0
	   accessor generic-name
	   initarg name)
     (discriminator position 1
	   accessor generic-discriminator)
     (argtype position 2
	      initarg argtype
	      accessor generic-argtype)
     (fast-cache position 3
		 accessor generic-fast-cache)
     (slow-cache position 4
		 accessor generic-slow-cache)
     (method-table position 5
		   accessor generic-method-table)
     ;; actually, this is a hack so that I can do generic-range.
     ;; contains range and method-class
     (method-description position 6
		   accessor generic-method-description)
     ;; used to be discriminator...
     (method-lookup-function position 7
		    accessor generic-method-lookup-function)
     (discrimination-depth position 8
			   accessor generic-discrimination-depth)
     (setter position 9
	     accessor generic-setter))
    direct-initargs (method-class methods domain)
    metaclass <generic-class>)
  

  (define-prim-class <method-class> (<class>)
    ()
    metaclass <class>)
  
  (define-prim-class <method> (<object>)
    ((method-qualifier position 0
		       accessor method-qualifier)
     (signature position 1
		accessor method-signature)
     (generic-function position 2
	   initform ()
	   accessor method-generic-function)
     (function position 3
	       initarg function
	       accessor method-function)
     (fixed position 4
	    accessor method-fixed))
    direct-initargs (domain range function signature)
    metaclass <method-class>)

  (define-prim-class <condition-class> (<class>)
    ()
    metaclass <class>)

  ;; from error.h
  
  (define-prim-class <condition> (<object>)
    ((message position 0
	      initarg message
	      accessor condition-message)
     (error-value initarg error-value 
		  position 1 
		  accessor condition-error-value))
    direct-initargs (message error-value)
    metaclass <condition-class>)

  (define-prim-class <Internal-Error> (<condition>)
    ()
    metaclass <condition-class>)
  
  (define-prim-class <clock-tick> (<condition>)
    ()
    metaclass <condition-class>)

  (define-prim-class <invalid-operator> (<condition>)
    ((args position 2 initarg args accessor invalid-operator-args)
     (op position 3 initarg op accessor invalid-operator-op))
    allocate t
    metaclass <condition-class>)

  ;; threads and the like
  
  (define-prim-class <thread-class> (<class>)
    ()
    metaclass <class>)

  (define-prim-class <thread> (<object>)
    ((data position 0 )
     (state position 1  accessor thread-internal-state)
     (fun position 2 class <unreadable-slot-description>)
     (args position 3 accessor thread-args)
     (value position 4 )
     (cochain position 5 class <unreadable-slot-description> accessor thread-cochain)
     (signal-list position 6 accessor thread-signals))
    metaclass <thread-class>)

  ;; primitive classes 

  (define-prim-class <primitive-class> (<class>)
    ()
    metaclass <class>)

  (define-prim-class <character> (<object>)
    ()
    metaclass <primitive-class>)

  
  (define-prim-class <symbol> (<object>)
    ()
    metaclass <primitive-class>)


  (define-prim-class <weak-wrapper> (<object>)
    ()
    metaclass <primitive-class>)

  (define-prim-class <continuation> (<funcallable-object>)
    ()
    metaclass <primitive-class>)

  (with-sockets
   (define-prim-class <socket> (<object>)
     ()
     metaclass <primitive-class>)

   (define-prim-class <listener> (<object>)
     ()
     metaclass <primitive-class>)
   )

  ;; Collections
  (define-prim-class <collection> (<object>)
    ()
    allocate t
    metaclass <abstract-class>
    )

  (define-prim-class <table> (<collection>)
    ((table-values position 0 initform () accessor table-values)
     (table-population position 1 initform 0 accessor table-population)
     (table-threshold position 2 initform 14 accessor table-threshold)
     (table-filled position 3 accessor table-filled)
     (table-comparator position 4 initarg comparator initform () accessor table-comparator)
     (table-hash-function position 5 initarg hash-function initform () accessor table-hash-function)
     (table-fill position 6 initarg fill initform () accessor table-fill))
    metaclass <class>
    )

  ;; and sequences..
  (define-prim-class <sequence> (<collection>)
    ()
    metaclass <abstract-class>
    allocate t
    )
  
  (define-prim-class <string> (<sequence>)
    ()
    metaclass <primitive-class>)

  ;; Could be variable-sized-class, but we don't have
  ;; a use for that yet...

  (define-prim-class <vector-class> (<primitive-class>)
    ()
    metaclass <class>
    allocate t)

  (define-prim-class <vector> (<sequence>)
    ()
    metaclass <vector-class>)

  ;; number hierarchy...
  
  (define-prim-class <number-class> (<primitive-class>)
    ()
    metaclass <class>
    allocate t)

  (define-prim-class <number> (<object>)
    ()
    metaclass <number-class>)

  (define-prim-class <float> (<number>)
    ()
    allocate t
    )

  (define-prim-class <double-float> (<float>)
    ()
    metaclass <number-class>)

  (define-prim-class <integer> (<number>)
    ()
    allocate t
    metaclass <number-class>)

  (define-prim-class <fixint> (<integer>)
    ()
    metaclass <number-class>)
  
  ;; lists --- the other way...

  (define-prim-class <list> (<sequence>)
    ()
    metaclass <abstract-class>
    allocate t)
  
;  (define-prim-class <pair> (<list>)
  (define-prim-class <cons> (<list>)
    ()
    metaclass <primitive-class>)
  
  (define-prim-class <null> (<list>)
    ()
    metaclass <primitive-class>)

  (define-prim-class <special-method> (<object>)
    ((id initarg id position 0 accessor sm-id))
    direct-initargs (id)
    allocate t)


  ;; end module
  )
