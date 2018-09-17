;; Eulisp Module
;; Author: pab
;; File: telos1.em
;; Date: Wed Dec 16 20:09:58 1992
;;
;; Project:
;; Description: 
;;

(defmodule telos1
  (
   init  
   defs  macros0  extras0 
   class-names
   gens
   list
   )
  ()
  
  ;; The Mop itself. 
  
  (export allocate initialize make
	  ;; core protocol
	  compatible-superclasses-p
	  compatible-superclass-p
	  compute-slot-reader
	  compute-slot-descriptions
	  compute-inherited-slot-descriptions
	  compute-specialized-slot-description
	  compute-specialized-slot-description-class
	  compute-defined-slot-description
	  compute-defined-slot-description-class
	  metaclass-default-slot-description-class
	  compute-slot-reader
	  compute-slot-writer
	  ensure-slot-reader
	  ensure-slot-writer
	  compute-and-ensure-slot-accessors
	  compute-primitive-writer-using-slot-description
	  compute-primitive-writer-using-class
	  compute-primitive-reader-using-slot-description
	  compute-primitive-reader-using-class
	  compute-precedence-list 
	  compute-initargs
	  compute-inherited-initargs 
	  add-subclass

	  ;; class/slot access
	  class-slot-descriptions
	  slot-description-slot-reader
	  slot-description-slot-writer
	  class-initargs
	  class-instance-size
	  class-precedence-list
	  ;; generic accessors
	  find-method
	  generic-function-methods
	  generic-method-class

	  find-slot-description
	  find-slot-reader
	  find-slot-writer

	  )
  
  (defconstant false nil)
  ;; more accessors

  (defun mapcan (f l)
    (fold (lambda (x lst)
	    (nconc lst (f x)))
	  l
	  nil))
  
  (defun nary-string-append x
    (fold (lambda (s x)
	    (string-append x
			   (cond ((symbolp s)
				  (symbol-unbraced-name s))
				 ((stringp s) s)
				 (t (symbol-unbraced-name (class-name (class-of s)))))))
	  x
	  ""))
    
  ;; debug vars
  (deflocal xx ())
  (deflocal xxx ())

  (defun match-sigs (sig meths)
    (cond ((not (consp meths)) ())
	  ((list-equal sig (method-signature (car meths))) (car meths))
	  (t (match-sigs sig (cdr meths)))))

  (defpredicate local-sd-p <local-slot-description>)

  ;; efficiency hacks..
  
  (defconstant readers
    (make-initialized-vector  
     primitive-slot-ref-0  primitive-slot-ref-1     primitive-slot-ref-2   primitive-slot-ref-3
     primitive-slot-ref-4  primitive-slot-ref-5     primitive-slot-ref-6   primitive-slot-ref-7
     primitive-slot-ref-8  primitive-slot-ref-9))

  (defconstant writers
    (make-initialized-vector
     primitive-set-slot-ref-0  primitive-set-slot-ref-1     primitive-set-slot-ref-2   primitive-set-slot-ref-3
     primitive-set-slot-ref-4  primitive-set-slot-ref-5     primitive-set-slot-ref-6   primitive-set-slot-ref-7
     primitive-set-slot-ref-8  primitive-set-slot-ref-9))

  (defun %compute-reader (n)
    (if (< n 10)
	(vector-ref readers n)
      (lambda (x) (primitive-slot-ref x n))))

  (defun %compute-writer (n)
    (if (< n 10)
	(vector-ref writers n)
      (lambda (x y) (primitive-set-slot-ref x n y))))



  (defmethod initialize ((cl <class>) initlist)
    ;; Initialisation is in 3 phases
    (let ((cl (call-next-method)))
      (let ((direct-superclasses (scan-args 'direct-superclasses initlist
					    (default-argument (list <object>))))
	    (direct-slot-descriptions (scan-args 'direct-slot-descriptions
						 initlist null-argument))
	    (direct-initargs (scan-args 'direct-initargs initlist null-argument)))
	;; 1. Compatability
	(unless (compatible-superclasses-p cl direct-superclasses)
	  (error "Incompatible superclasses." <incompatible-superclasses>
		 cl direct-superclasses))
	((setter class-direct-superclasses) cl direct-superclasses)
	((setter class-name) cl (scan-args 'name initlist (default-argument '<anonymous>)))
	((setter class-precedence-list) cl 
	 (compute-precedence-list cl direct-superclasses))
	;; 2a compute cpl
	(let ((initargs 
	       ;; 2b initargs
	       (compute-initargs cl 
				 direct-initargs
				 (compute-inherited-initargs cl direct-superclasses)))
	      ;; 2c slot descriptions, also, we do the ensuring here too...
	      (ensured-effective-sds 
	       (let ((inherited-slots
		      (compute-inherited-slot-descriptions cl direct-slot-descriptions 
							   direct-superclasses)))
		 (compute-and-ensure-slot-accessors 
		  cl
		  (compute-slot-descriptions cl direct-slot-descriptions
					     inherited-slots)
		  inherited-slots))))
	  ;; 3. Make it all readable....
	  ((setter class-initargs) cl initargs)
	  ((setter class-slot-descriptions) cl ensured-effective-sds)
	  ;; Sort the slot descriptions...
	  (labels
	   ((sort-slots (lst local nonlocal nlocal)
			(cond ((null lst) 
			       ((setter class-local-slot-descriptions) cl (reverse local))
			       ((setter class-non-local-slot-descriptions) cl nonlocal)
			       ((setter class-instance-size) cl nlocal))
			      ((sane-slot-description-p cl (car lst) ensured-effective-sds)
			       (sort-slots (cdr lst)
					   (cons (make-init-slot nlocal (car lst)) local) 
					   nonlocal (+ nlocal 1)))
			      ((subclassp (class-of (car lst)) <local-slot-description>)
			       (sort-slots (cdr lst) (cons nil local) (cons (car lst) nonlocal) (+ nlocal 1)))
			      (t (sort-slots (cdr lst) local (cons (car lst) nonlocal) nlocal))))
	    (make-init-slot (n sd)
			    (cons (slot-description-initarg sd)
				  (slot-description-initfunction sd))))
	   (sort-slots (class-slot-descriptions cl) nil nil 0))
	  ;; Add the super-classes
	  (mapc (lambda (super) (add-subclass super cl) )
		direct-superclasses)
	  ((setter class-direct-subclasses) cl nil)
	  (set-type cl class-type)
	  cl))))
  
  ;; class compatability
  
  (defgeneric compatible-superclasses-p (class list))
  (defgeneric compatible-superclass-p (class list))

  (defmethod compatible-superclasses-p ((cl <class>) (direct-superclasses <cons>))
    (and (= (list-length direct-superclasses) 1)
         (compatible-superclass-p cl (car direct-superclasses))))

  (defmethod compatible-superclasses-p ((cl <mi-class>) (direct-superclasses <cons>))
    (labels ((loop (superclasses)
		   (cond
		    ((null superclasses) t)
		    ((compatible-superclass-p cl (car superclasses))
		     (loop (cdr superclasses)))
		    (t false))))
	    (loop direct-superclasses)))

 (defmethod compatible-superclasses-p ((cl <class>) lst)
   (if (null lst)
       (error "I need some superclasses...~%" <incompatible-superclasses> 'error-value 'none-specified)
     (if (null (cdr lst)) t
       (error "Too many superclasses for single-inheritance class" <incompatible-superclasses>
	      'error-value (cons cl lst)))))

 
 (defmethod compatible-superclass-p ((subclass <class>) (superclass <class>)) t)
 
 ;; Precedence lists
 (defgeneric compute-precedence-list (class list))
 
 (defmethod compute-precedence-list ((cl <class>) (direct-superclasses <cons>))
   ;;(format t "comp-cpl: ~a~a~%" cl direct-superclasses)
    (let ((super-cpl (class-precedence-list (car direct-superclasses))))
      (if (memq cl super-cpl)
	  (error "Circular hierarchy [How did you manage that?]"
		 <illegal-inheritance-hierarchy>
		 cl super-cpl)
	(cons cl super-cpl))))

 (defun detect (p l)
   (cond ((null l) ())
	 ((p (car l)))
	 (t (detect p (cdr l)))))

 (defun tsort (lsts)
   (if (null lsts) nil
     (let ((firsts (mapcar car lsts))
	   (rests (mapcar cdr lsts)))
       (let ((leasts (mapcan (lambda (elt)
			       (if (detect (lambda (l) (memq elt l)) rests)
				   nil
				 (list elt)))
			     (remove-duplicates firsts))))
	 (append leasts
		 (tsort (mapcan (lambda (l)
				  (if (null l) nil
				    (if (memq (car l) leasts) 
					(if (cdr l) (list (cdr l)) nil)
				      (list l))))
				lsts)))))))

 (defmethod compute-precedence-list ((cl <mi-class>)  (direct-superclasses <cons>))
   (cons cl (remove-duplicates-from-end (depth-first-preorder direct-superclasses))))

 (defun depth-first-preorder (lst)
   (if (null lst) nil
     (cons (car lst)
	   (append (depth-first-preorder (cdr (class-precedence-list (car lst))))
		   (depth-first-preorder (cdr lst))))))
	     

;; computing initargs 
 
 (defgeneric compute-initargs (class direct inherited))
 (defgeneric compute-inherited-initargs (class list))

 (defmethod compute-inherited-initargs ((cl <class>) (direct-superclasses <cons>))
    (class-initargs (car direct-superclasses)))

  (defmethod compute-inherited-initargs ((cl <mi-class>) (direct-superclasses <cons>))
    (mapappend class-initargs direct-superclasses))
  
  (defmethod compute-initargs ((cl <class>) direct-initargs inherited-initargs)
    (labels ((fold (direct-initargs result)
		   (cond
		    ((null direct-initargs) result)
		    ((memq (car direct-initargs) result) 
		     (fold (cdr direct-initargs) result))
		    (t (fold (cdr direct-initargs) (cons (car direct-initargs)
							 result))))))
	    (fold direct-initargs inherited-initargs)))

  (defmethod compute-initargs ((cl <mi-class>) direct-initargs inherited-initargs)
    (remove-duplicates
     (append direct-initargs inherited-initargs)))
   
  ;; computing slot-descriptions

  (defgeneric compute-slot-descriptions (class direct-sds inherited-sds))
  (defgeneric compute-inherited-slot-descriptions (class direct-sds superclasses))
  (defgeneric compute-specialized-slot-description (class inherited-sd direct-sd))
  (defgeneric compute-specialized-slot-description-class (class inherited-sd direct-sd))
  (defgeneric compute-defined-slot-description (class direct-sd))
  (defgeneric compute-defined-slot-description-class (class direct-sd))

;;  (defmethod compute-inherited-slot-descriptions ((cl <class>) direct-sds superclasses)
;;    (mapcar list (class-slot-descriptions (car superclasses))))
  
  ;; Don't really like any of this. Should be able to specify a slot more exactly
  ;; than by name --- name.class or something.
  (defmethod compute-inherited-slot-descriptions ((cl <class>) direct-sds superclasses)
    (reverse (mapcar cdr 
	    (fold (lambda (inherited collect)
		    (let ((seen (assq (slot-description-name inherited) collect)))
		      (if (null seen)
			  (cons (list (slot-description-name inherited) inherited) collect)
			(progn ((setter cdr) seen (cons inherited (cdr seen)))
			       collect))))
		  (fold append (mapcar class-slot-descriptions superclasses) nil)
		  nil))))
		    
  (defmethod compute-slot-descriptions ((cl <class>) direct-sds inherited-sds)
    (let ((old-sd-names (mapcan (lambda (sds)
				  (mapcar slot-description-name sds))
				inherited-sds))
	  (new-sd-plist (mapcan (lambda (spec)
				  (list (cons (scan-args 'name spec null-argument) spec)))
				direct-sds)))
      (append
       (mapcar (lambda (sds)
		 (compute-specialized-slot-description
		  cl
		  sds
		  (let ((x (assq (slot-description-name (car sds)) new-sd-plist)))
		    (if (null x) nil
		      (cdr x)))))
	       inherited-sds)
       (mapcan (lambda (named-spec)
		 (if (memq (car named-spec) old-sd-names)
		     nil
		   (list (compute-defined-slot-description cl (cdr named-spec)))))
	       new-sd-plist))))
  
  (defmethod compute-specialized-slot-description ((cl <class>) sds spec)
    (let ((sd (car sds))
	  (sdclass (compute-specialized-slot-description-class cl sds spec)))
      (if (null spec)
	  (if (eq (class-of sd) sdclass) sd
	    (let ((new (make sdclass 
			     'name (slot-description-name sd)
			     'reader (slot-description-slot-reader sd)
			     'writer (slot-description-slot-writer sd))))
	      (initialize-specialized-slot-description new sds)))
	(let ((new (apply make sdclass
			  'reader (slot-description-slot-reader sd)
			  'writer (slot-description-slot-writer sd)
			  'parent-sds sds 
			  spec)))
	  (initialize-specialized-slot-description new sds)))))

  (defgeneric initialize-specialized-slot-description ((sd <slot-description>) parent-sds))

  ;; Tacky really. This part of the protocol _must_ assume mi.
  (defmethod initialize-specialized-slot-description ((sd <slot-description>) parent-sds)
    (if (eq (slot-description-initfunction sd) unbound-slot-value)
	((setter slot-description-initfunction) sd
	 (or (detect (lambda (sd) 
		       (if (eq (slot-description-initfunction sd) unbound-slot-value) nil
			 (slot-description-initfunction sd)))
		     parent-sds)
	     unbound-slot-value))
      nil)
    (if (eq (slot-description-initarg sd) unbound-slot-value)
	((setter slot-description-initarg) sd
	 (or (detect (lambda (sd) 
		       (if (eq (slot-description-initarg sd) unbound-slot-value) nil
			 (slot-description-initarg sd)))
		     parent-sds)
	     unbound-slot-value))
      nil)
    sd)

  (defmethod compute-specialized-slot-description-class ((cl <class>) sds spec)
    (scan-args 'slot-class spec (default-argument (class-of (car sds)))))

  (defmethod compute-defined-slot-description ((cl <class>) defn)
    (initialize (allocate (compute-defined-slot-description-class cl defn) defn)
		defn))

  (defmethod compute-defined-slot-description-class ((cl <class>) defn)
    (scan-args 'slot-class defn (lambda (a b)
				  (metaclass-default-slot-description-class cl))))
  
  (defgeneric metaclass-default-slot-description-class (class))
  (defmethod metaclass-default-slot-description-class ((cl <class>))
    <local-slot-description>)

  (defgeneric compute-and-ensure-slot-accessors (class list list))

  (defmethod compute-and-ensure-slot-accessors ((cl <class>) effective-sds inherited-sds)
    (fold (lambda (sd n)
	    (cond ((not (subclassp (class-of sd) <local-slot-description>)) n)
		  ;;((subclassp (class-of (slot-description-position sd)) number)
		  ;; (+ n 1))
		  (t ((setter slot-description-position) sd n)
		     (+ n 1))))
	  effective-sds 0)
    (mapc (lambda (effective-sd)
            (unless (accessors-exist-p effective-sd)
	      ;;(format t "Slot: ~a~%" effective-sd)
              ((setter slot-description-slot-reader) 
               effective-sd
               (compute-slot-reader cl effective-sd))
              ((setter slot-description-slot-writer) 
               effective-sd
               (compute-slot-writer cl effective-sd)))
            (ensure-slot-reader cl 
                                effective-sd
                                effective-sds 
                                (slot-description-slot-reader
                                 effective-sd))
            (ensure-slot-writer cl 
                                effective-sd 
                                effective-sds
                                (slot-description-slot-writer
                                 effective-sd)))
          effective-sds)
    effective-sds)

  (defmethod compute-and-ensure-slot-accessors ((cl <mi-class>) effective-sds inherited-sds)
    (call-next-method) ;; deals with effective-sds
    (mapc
     (lambda (inherited-sds)
       ;; If we are merging 2 slot-descriptions, life becomes tricky.
       (let ((real-sd (car (member (slot-description-name (car inherited-sds))
				   effective-sds
				   (lambda (name sd) 
				     (eq (slot-description-name sd) 
					 name))))))
	 (mapc (lambda (inherited-sd)
		 (ensure-slot-reader cl 
				     real-sd
				     effective-sds 
				     (slot-description-slot-reader inherited-sd))
		 (ensure-slot-writer cl 
				     real-sd
				     effective-sds
				     (slot-description-slot-writer inherited-sd)))
	       inherited-sds)))
     inherited-sds)
    effective-sds)
  
  (defun accessors-exist-p (sd) 
    (and (not (eq (slot-description-slot-reader sd) unbound-slot-value))
	 (not (eq (slot-description-slot-writer sd) unbound-slot-value))))

  (defgeneric compute-slot-reader (class slot-description))

  (defmethod compute-slot-reader ((cl <class>) sd)
    (make <generic-function>
	  'lambda-list '(obj) 'method-class <method> 'argtype 1
	  'name (make-symbol (nary-string-append (symbol-unbraced-name (class-name cl)) "-" (slot-description-name sd)))
	  'domain (list <object>))) ;; could be cl

  ;;(make generic-function 'lambda-list '(obj) 'method-class method) ; 'signature (list cl)
  
  (defgeneric compute-slot-writer (class slot-description))

  (defmethod compute-slot-writer ((cl <class>) sd )
    ;; 'signature (list cl object)
    (make <generic-function> 'lambda-list '(obj val) 'method-class <method> 'argtype 2
	  'name (make-symbol (nary-string-append (symbol-unbraced-name (class-name cl))
						 "-" (slot-description-name sd) "-setter"))
	  'domain (list <object> <object>))) ;; could be cl

  ;;(make generic-function 'lambda-list '(obj val) 'method-class method)  ; 'signature (list cl object)

  (defgeneric ensure-slot-reader (class slot-description <cons> function))
  (defmethod ensure-slot-reader ((cl <class>) (sd <slot-description>) 
                                 (effective-sds <cons>) (gfn <generic-function>))
    ;; in case of single inheritance and local slots only, all slot positions
    ;; remain valid in subclasses. Thus, inherited reader methods remain valid
    ;; as well.
    (when (null (generic-function-methods gfn))
      (let ((reader
             (compute-primitive-reader-using-slot-description sd cl
                                                              effective-sds)))
	;; XXX: Should use a vector lookup for these..
        (add-method gfn
                    (make <method>
                          'signature (list cl)
                          'function (if (eq (class-of reader) <c-function>)
					reader
				      (method-lambda (obj)
						     (reader obj)))))
	gfn)))

  (defmethod ensure-slot-reader ((cl <class>) (sd <slot-description>)
				 (effective-sds <cons>) (xx <function>))
    xx)

  (defmethod ensure-slot-reader ((cl <mi-class>) (sd <slot-description>) 
                                 (effective-sds <cons>) (gfn <generic-function>))
    ;; in case of multiple inheritance slot positions may become invalid in
    ;; subclasses. A more specific method should be added in that case.
    ;; Here is a solution where for each subclass a new method is added.
    (format t "Ensure: ~a ~a ~a~%" cl (slot-description-name sd) gfn)
    (unless (find-method gfn (list cl))
      (let ((reader
             (compute-primitive-reader-using-slot-description sd cl
                                                              effective-sds)))
        (add-method gfn
                    (make <method>
                          'signature (list cl)
                          'function (if (eq (class-of reader) <c-function>) 
					reader
				      (method-lambda (obj)
						     (reader obj))))))))
  
  (defgeneric ensure-slot-writer (class slot-description <cons> function))

  (defmethod ensure-slot-writer ((cl <class>) (sd <slot-description>) 
                                 (effective-sds <cons>) (gfn <generic-function>))
    ;; in case of single inheritance and local slots only, all slot positions
    ;; remain valid in subclasses. Thus, inherited writer methods remain valid
    ;; as well.
    (when (null (generic-function-methods gfn))
      (let ((writer
             (compute-primitive-writer-using-slot-description sd cl
                                                              effective-sds)))
        (add-method gfn
                    (make <method>
                          'signature (list cl <object>)
                          'function (if (eq (class-of writer) <c-function>)
					writer
				      (method-lambda (obj val)
						     (writer obj val))))))))

  (defmethod ensure-slot-writer ((cl <class>) (sd <slot-description>) 
                                 (effective-sds <cons>) (xx <function>))
    xx)

  (defmethod ensure-slot-writer ((cl <mi-class>) (sd <slot-description>) 
                                 (effective-sds <cons>) (gfn <generic-function>))
    ;; in case of multiple inheritance slot positions may become invalid in
    ;; subclasses. A more specific method should be added in that case.
    ;; Here is a solution where for each subclass a new method is added.
    (unless (find-method gfn (list cl <object>))
      (let ((writer
             (compute-primitive-writer-using-slot-description sd cl
                                                              effective-sds)))
        (add-method gfn
                    (make <method>
                          'signature (list cl <object>)
                          'function (if (eq (class-of writer) <c-function>)
					writer
				      (method-lambda (obj val)
						     (writer obj val))))))))
  
  (defgeneric compute-primitive-reader-using-slot-description (slot-description
                                                               class
                                                               <cons>))
  (defmethod compute-primitive-reader-using-slot-description ((sd <slot-description>)
							      (cl <class>)
							      (effective-sds <cons>))
    (compute-primitive-reader-using-class cl sd effective-sds))
  
  (defgeneric compute-primitive-writer-using-slot-description (slot-description
                                                               class
                                                               <cons>))

  (defmethod compute-primitive-writer-using-slot-description ((sd <slot-description>)
							      (cl <class>)
							      (effective-sds <cons>))
    (compute-primitive-writer-using-class cl sd effective-sds))
  
  (defgeneric compute-primitive-reader-using-class (class slot-description <cons>))
  (defmethod compute-primitive-reader-using-class ((cl <class>) (sd <local-slot-description>) (effective-sds <cons>))
    ;; assumption: sd is element of effective-sds
    (let ((local-sds (collect local-sd-p effective-sds)))
      (let ((pos (position sd local-sds eq)))
	(%compute-reader pos))))
	    

  (defmethod compute-primitive-reader-using-class ((cl <mi-class>) (sd <local-slot-description>) (effective-sds <cons>))
    ;; sd may be an inherited slot description, i.e. not element of effective-sds
    (let ((local-sds (collect local-sd-p effective-sds)))
      (let ((pos (position sd local-sds
			   (lambda (sd1 sd2) 
			     (eq (slot-description-name sd1)
				 (slot-description-name sd2))))))
	(%compute-reader pos))))
  
  (defgeneric compute-primitive-writer-using-class (class slot-description <cons>))
  (defmethod compute-primitive-writer-using-class ((cl <class>) 
						   (sd <local-slot-description>)
						   (effective-sds <cons>))
    ;; assumption: sd is element of effective-sds
    (let ((pos (position sd (collect local-sd-p effective-sds) eq)))
      (%compute-writer pos)))

  (defmethod compute-primitive-writer-using-class ((cl <mi-class>) (sd <local-slot-description>) (effective-sds <cons>))
    ;; sd may be an inherited slot description, i.e. not element of effective-sds
    (let ((pos (position sd (collect local-sd-p effective-sds)
                         (lambda (sd1 sd2) 
                           (eq (slot-description-name sd1)
                               (slot-description-name sd2))))))
      (%compute-writer pos)))
  
  (defgeneric add-subclass (cl sub))

  (defmethod add-subclass ((cl <class>) sub)
    ((setter class-direct-subclasses) cl (cons sub (class-direct-subclasses cl))))

  ;; Non-essential methods...
  ;; does an sd have a sensible reader+writer
    (defun sane-slot-description-p (cl slot ensured-effective-sds)
      (and (eq (class-of slot) <local-slot-description>)
	   (= (length 
	       ((generic-method-lookup-function 
		 compute-primitive-writer-using-slot-description)
		(list slot cl ensured-effective-sds)))
	      1)
	   (= (length 
	       ((generic-method-lookup-function 
		 compute-primitive-reader-using-slot-description)
		(list slot cl ensured-effective-sds)))
	      1)
	   (= (length 
	       ((generic-method-lookup-function 
		 compute-primitive-writer-using-class)
		(list cl slot ensured-effective-sds)))
	      1)
	   (= (length 
	       ((generic-method-lookup-function 
		 compute-primitive-reader-using-class)
		(list cl slot ensured-effective-sds)))
	      1)))

  ;; define defstruct efficiency hacks

  (defmethod compute-and-ensure-slot-accessors ((c <structure-class>) effective-sds inherited-sds)
    (labels ((aux (lst n)
		  (cond ((null lst) nil)
			((accessors-exist-p (car lst))
			 (aux (cdr lst) (+ n 1)))
			(t ((setter slot-description-slot-reader) (car lst)
			    (make-structure-reader c n))
			   ((setter slot-description-slot-writer) (car lst)
			    (make-structure-writer c n))
			   ((setter slot-description-position) (car lst) n)
			   (aux (cdr lst) (+ n 1))))))
	    (aux effective-sds 0)
	    effective-sds))
  
  (defmethod compatible-superclass-p ((cl <structure>) (cl <structure>))
    t)

  (defmethod compatible-superclass-p ((cl <structure>) cl)
    nil)

  (defmethod add-subclass ((x <structure-class>) new)
    (if (subclassp new <structure>)
	(call-next-method)
      (error "Can't subclass structure with a non-structure"
	     <illegal-inheritance-hierarchy>
	     'error-value new)))

  ;; and a new initialize hack for structures
  (add-method initialize (make <method>
			       'signature (list <structure> <object>)
			       'function initialize-local-slots))
  
  ;; utilities
  (defun position (obj lst fn)
    (labels ((loop (lst pos)
		   (cond
		    ((null lst) (error "Object not in the list." <element-not-found> 'error-value 
				       (list obj lst fn)))
		    ((fn obj (car lst)) pos)
		    (t (loop (cdr lst) (+ pos 1))))))
	    (loop lst 0)))

  (defun remove-duplicates (elements)
    (labels ((fold (elements result)
		   (cond
		    ((null elements) result)
		    ((member (car elements) result eq)
		     (fold (cdr elements) result))
		    (t (fold (cdr elements) (cons (car elements) result))))))
	    (reverse (fold elements '()))))

  (defun remove-duplicates-from-end (elements)
    (labels ((fold (elements result)
		   (cond
		    ((null elements) result)
		    ((member (car elements) result eq) (fold (cdr elements) result))
		    (t (fold (cdr elements) (cons (car elements) result))))))
	    (fold (reverse elements) '())))
  
  (defun mapappend (fn args)
    (labels ((loop (lst result)
		   (cond
		    ((null lst) result)
		    ;;((null (cdr lst)) (append result (fn (car lst))))
		    (t (loop (cdr lst) (append result (fn (car lst))))))))
	    (loop args '())))

  (defun list-equal (lst1 lst2)
    (labels ((loop (l1 l2)
		   (cond ((null l1)
			  (if (null l2)
			      t
			    nil))
			 ((null l2)
			  nil)
			 ((eq (car l1) (car l2))
			  (loop (cdr l1) (cdr l2)))
			 (t nil))))
	    (loop lst1 lst2)))

  (defun collect (fn lst)
    (cond ((null lst) nil)
	  ((fn (car lst))
	   (cons (car lst)
		 (collect fn (cdr lst))))
	  (t (collect fn (cdr lst)))))

  
;; Finally, we define the error classes 

  (defclass <incompatible-superclasses> (<condition>)
    ()
    class <condition-class>)

  (defclass <telos-cannot-happen> (<condition>)
    ()
    class <condition-class>)

  (defclass <illegal-inheritance-hierarchy> (<condition>)
    ()
    class <condition-class>)

  (defclass <element-not-found> (<condition>)
    ()
    class <condition-class>)
  
  (defclass <no-applicable-method> (<condition>)
    ((sig initarg sig))
    class <condition-class>)
  
  (set-no-applicable-method <no-applicable-method>)
  (export <incompatible-superclasses> <telos-cannot-happen>
	  <illegal-inheritance-hierarchy> <element-not-found>
	  <no-applicable-method>)
  ;; Real (non-simple) functions...

  (defgeneric find-slot-description (class name))
  
  (defmethod find-slot-description ((cl <class>) name)
    (if (eq cl (class-of nil))
	(error "<null> has no slots. please try again"
	       'error-value (cons cl name))
      (labels ((aux (slots)
		    (cond ((null slots)
			   (error "no slot description" <element-not-found> 'error-value (cons cl name)))
			  ((eq (slot-description-name (car slots)) name)
			   (car slots))
			  (t (aux (cdr slots))))))
	      (aux (class-slot-descriptions cl)))))
  
  (defun find-slot-reader (class name)
    (slot-description-slot-reader (find-slot-description class name)))

  (defun find-slot-writer (class name)
    (slot-description-slot-writer (find-slot-description class name)))
  
  (defclass <not-yet-implemented> (<condition>)
    ()
    class <condition-class>)
  (export <not-yet-implemented>)

  (defun nyi (msg) (error msg <not-yet-implemented>))
  (export nyi)

  (defclass <collection-condition> (<condition>)
    ()
    class <condition-class>)
  (export <collection-condition>)
  
  (defclass <format-error> (<condition>)
    ()
    class <condition-class>)
  (defclass <scan-mismatch> (<condition>)
    ()
    class <condition-class>)
  (export <format-error> <scan-mismatch>)

  (defclass <stream-error> (<condition>)
    ()
    class <condition-class>)
  (export <stream-error>)

  )
