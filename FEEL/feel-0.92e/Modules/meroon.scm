;;; $Id: meroon.scm,v 1.13 1992/05/23 08:47:59 queinnec Exp $
;;; Copyright (c) 1990, 91, 92 by Christian Queinnec. All rights reserved.

;;; This program is distributed in the hope that it will be useful.
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted, so long as the following
;;; conditions are met:
;;;      o credit to the authors is acknowledged following current
;;;        academic behaviour
;;;      o no fees or compensation are charged for use, copies, or
;;;        access to this software
;;;      o this copyright notice is included intact.
;;; This software is made available AS IS, and no warranty is made about 
;;; the software or its performance. 

;;; Bug descriptions, use reports, comments or suggestions are welcome.
;;; Send them to    <queinnec@poly.polytechnique.fr>   
;;;        or to    <Christian.Queinnec@inria.fr>
;;; or to:
;;;      Christian Queinnec
;;;      Laboratoire d'Informatique de l'X
;;;      Ecole Polytechnique
;;;      91128 Palaiseau
;;;      France

;;;************************************************
;;;    Small, Efficient and Innovative Class System
;;;       Christian Queinnec  
;;;   \'Ecole Polytechnique & INRIA-Rocquencourt
;;;   91128 Palaiseau Cedex --- France
;;;************************************************

;;; In order to load or compile this file, you must load or compile the
;;; appropriate prologue. Some important facts are:
;;; -a- All errors are mapped onto the function oo-error.
;;; -b- macros are defined by the define-oo-macro macro.
;;; -c- Use of macros such as when and until.
;;; -d- some utility functions like mapcan, oo-concat or flat are used.
;;; -e- oo-apply corresponds to a n-ary apply operator.
;;; -f- macros use the gensym function.
;;; The actual prologues are meroon.pcs for PC-Scheme
;;;                          meroon.s2c for Scheme->C

;;; This vector records existing classes. Any class can be retrieved by
;;; its number (which acts as an index in this vector). 
(define *classes* (vector #f #f #f #f #f #f #f #f #f #f))

;;; the number of classes in *classes*
(define *class-number* 0)

;;; This vector records all existing generic functions.
;;; This is not good for the GC, but is portable since there is no
;;; need to open the guts of the representation of a generic function
;;; to find the dispatch table. A generic function is represented by
;;; a regular Scheme function AND an instance of class Generic kept
;;; within this vector. This generic object is accessed by its name
;;; which must also be the name of the generic function (sigh).
(define *generics* (vector #f #f #f #f #f #f #f #f))

;;; The number of generic functions in *generics*
(define *generic-number* 0)

;;; The list of traced generic functions. This list is an AList 
;;; mapping names to alterated fields of traced generic functions.
(define *traced-generics* (list))
(define *traced-generic-names* (list))

;;; (lit fn end '(e1 e2 .. eN)) -> (fn 'e1 (fn 'e2 .. (fn eN end)..))
(define (lit fn end list)
  (if (pair? list)
      (fn (car list) 
          (lit fn end (cdr list)) )
      end ) )

;;; (vector-map f #(v1 v2 .. vN)) -> (begin (f v1) (f v2) .. (f vN))
(define (vector-map fn v)
  (let ((n (vector-length v)))
    (define (mapvector i)
      (when (< i n)
            (fn (vector-ref v i))
            (mapvector (+ 1 i)) ) )
    (mapvector 0) ) )

;;; return a new bigger vector
(define (vector-extend s)
  (let* ((n (vector-length s))
         (r (make-vector (+ n 1 (quotient n 2)) #f)) )
    (vector-copy! s r 0 n)
    r ) )

;;; Copy vector old[start..end[ into vector new[start..end[
(define (vector-copy! old new start end)
  (let copy ((i start))
    (when (< i end)
          (vector-set! new i (vector-ref old i))
          (copy (+ i 1)) ) ) )

;;; look for an object located in a vector which, when applied key on it,
;;; returns name. If such object does not exist apply default on name.
;;; This function mimics Common Lisp find function on sequence.
(define (sequence-find name s key default)
  (let look ((i 0))
    (if (and (< i (vector-length s))
             (vector-ref s i) )
        (if (eq? name (key (vector-ref s i)))
            (vector-ref s i)
            (look (+ i 1)) )
        (default name) ) ) )

;;; Check list of fields. The syntax is actually quite simple:
;;;    field ::= field-name 
;;;           |  (= field-name . field-options)
;;;           |  (* field-name . field-options)
;;; Each field is converted into a field-descriptor
;;; The options variable represents the class options that might
;;; affect the parsing of fields.
(define (parse-fields fields options)
  (map (lambda (field)
         (cond ((symbol? field)
                (parse-default-field field options) )
               ((pair? field)
                (case (car field)
                  ((mono =) (parse-mono-field (cdr field) options))
                  ((poly *) (parse-poly-field (cdr field) options))
                  (else (oo-error "Unrecognized kind of field" field)) ) )
               (else (oo-error "Invalid description of field" field)) ) )
       fields ) )

;;; Default slot-reader or -writer
(define (default-slot-accessor o . args)
  (oo-error "Cannot access :prototype class") )

;;; These parsers can be extended to handle options such as 
;;; typed slot, variant fields ...
;;; By default, all field are mutable (unless specified by :immutable)
(define (parse-default-field name options)
  (if (memq ':immutable options)
      (make-mono-field name default-slot-accessor)
      (make-mutable-mono-field 
       name default-slot-accessor default-slot-accessor ) ) )

;;; Both mono- and poly- fields recognize the :mutable option.
(define (parse-mono-field parms options)
  (if (or (memq ':immutable options)
          (memq ':immutable (cdr parms)) )
      (make-mono-field (car parms) default-slot-accessor)
      (make-mutable-mono-field 
       (car parms) default-slot-accessor default-slot-accessor ) ) )

(define (parse-poly-field parms options)
  (if (or (memq ':immutable options)
          (memq ':immutable (cdr parms)) )
      (make-poly-field 
       (car parms) default-slot-accessor default-slot-accessor )
      (make-mutable-poly-field 
       (car parms) default-slot-accessor default-slot-accessor 
                   default-slot-accessor ) ) )

;;; The macro that defines a class with a name, a super class and 
;;; the extra fields (that may be repeated).
(define-oo-macro (define-class name super own-fields . options)
  (unless (symbol? name) (oo-error "Not a name" name))
  (let* ((super-class 
          (symbol->class 
           super
           (lambda (name) (oo-error "No such super class" name)) ))
         (own-fields (parse-fields own-fields options))
         (g (gensym))
         (fields (append-fields (class-fields super-class) own-fields)) )
    (unless (symbol->class name (lambda (name) #f))
            ;; Create the class if not already existing
            ;; so that it can be subclassed at macroexpand-time
            (set-class-fields! (create-subclass name super-class)
                               fields ) )
    (if (memq ':prototype options)
        ;; The :prototype option is used to register the class not to
        ;; generate code. It is mainly used for separate compilation
        `',name
        ;; Generate all the related functions
        `(register-class 
          ',name 
          ',super 
          (lambda (,g)
            (set-class-fields! 
             ,g 
             (append (class-fields (class-super ,g))
                     (list ,@(map generate-field own-fields)) ) )
            ,(generate-allocator name fields g)
            ,(generate-maker name fields g)
            ,@(generate-readers name fields 1 g)
            ,@(generate-writers name fields 1 g)
            ,@(generate-lengthers name fields 1 g)
            ,(generate-predicate name g)
            ,g ) ) ) ) )

(define (register-class name super-name initialize)
  (let ((c (create-subclass name (symbol->class super-name))))
    (initialize c)
    name ) )

;;; check and append new fields to a list of fields. Fields cannot
;;; be redefined within subclasses.
(define (append-fields fields own-fields)
  (for-each (lambda (field)
              (define (lookup fields)
                (if (pair? fields)
                    (or (eq? (field-name field) (field-name (car fields)))
                        (lookup (cdr fields)) )
                    #f ) )
              (when (lookup fields)
                    (oo-error "Field redefinition" field) ) )
            own-fields )
  (append fields own-fields) )

;;; generates a predicate that recognizes instances. Since we coexist
;;; with Scheme it is not fully safe (see object? definition).
(define (generate-predicate name class)
  `(begin
     (set-class-predicate! 
      ,class (lambda (o) (and (object? o)
                              (is-a? o ,class) )) )
     (set! ,(oo-concat name "?") (class-predicate ,class)) ) )

;;; generates a maker that takes all the arguments to fill the instance.
;;; An instance is represented by a vector which first coordinate is the
;;; number of its class. This leads to extra indirections for object->class
;;; but avoids to print circular structures.
;;; A maker function takes poly-field values preceded by their number.
;;; Thus the maker of (define-class polygon object (name (* items))) may be
;;; called as follows (make-polygon "name" 3 item1 item2 item3).
(define (generate-maker name fields class)
  `(begin
     (set-class-maker!
      ,class 
      ,(if (mapcan need-parameter-for-field fields)
           `(lambda args
              (check-arguments (class-fields ,class) args)
              (oo-apply vector (class-number ,class) args) )
           (let ((args (map field-name fields)))
             `(lambda ,args
                (vector (class-number ,class) ,@args) ) ) ) )
     (set! ,(oo-concat 'make- name) (class-maker ,class)) ) )

;;; Check repeated fields specifications.
;;; NOTE It is a feature of Scheme->C that local variables do not shadow
;;; global compiler macros such as map. So the internal function is 
;;; renamed mapcdr.
(define (check-arguments fields args)
  (define (mapcdr fn args fields)
    (if (pair? fields)
        (mapcdr fn (fn (car fields) args) (cdr fields))
        args ) )
  (or (null? (mapcdr check-argument-for-field args fields))
      (oo-error "Too much arguments" args) ) )

;;; generates an addition. Generated additions are binary and if they
;;; contain a number it is in first argument.
(define (generate-plus a b)
  (cond ((number? a)
         (cond ((number? b) (+ a b))
               ((and (pair? b)          ; b = (+ aa bb)
                     (eq? (car b) '+) )
                (cond ((number? (cadr b))
                       `(+ ,(+ a (cadr b)) ,(caddr b)) )
                      (else `(+ ,a ,b)) ) )
               (else `(+ ,a ,b)) ) )
        (else `(+ ,a ,b)) ) )
  
;;; generates a function that takes the various sizes of the repeated 
;;; fields and return an empty (but structured) instance.
;;; Can be removed if no field is mutable since then to allocate instances
;;; is useless since there are immutable.
(define (generate-allocator name fields class)
  (let ((sizes (mapcan need-parameter-for-field fields)))
    `(begin 
       (set-class-allocator!
        ,class (lambda ,sizes
                 (let ((r (make-vector
                           ,(lit generate-plus
                                 (generate-plus 1 (length fields))
                                 sizes ))))
                   (vector-set! r 0 (class-number ,class))
                   ,@(generate-skeletizer 'r fields 1)
                   (initialize! r) ) ) )
       (set! ,(oo-concat 'allocate- name) 
             (class-allocator ,class) ) ) ) )

;;; generates functions that read slots (repeated or not).
(define (generate-readers name fields index class)
  (if (pair? fields)
      (generate-reader-for-field (car fields) name (cdr fields) index class)
      '() ) )

;;; generates writers (code similar to generate-readers)
(define (generate-writers name fields index class)
  (if (pair? fields)
      (generate-writer-for-field (car fields) name (cdr fields) index class)
      '() ) )

;;; generates for each repeated fields the function that returns the 
;;; number of items in an instance.
(define (generate-lengthers name fields index class)
  (if (pair? fields)
      (generate-lengther-for-field (car fields) name (cdr fields) index class)
      '() ) )

;;; Generates the code that structures vector-name ie which puts 
;;; the right sizes at the right places.
(define (generate-skeletizer name fields index)
  (if (pair? fields)
      (generate-skeletizer-for-field (car fields) name (cdr fields) index)
      '() ) )

;;; Build by hand the initial set of accessors needed for the bootstrap
(define class-name (lambda (o) (vector-ref o 1)))
(define class-number (lambda (o) (vector-ref o 2)))
(define class-fields (lambda (o) (vector-ref o 3)))
(define set-class-fields! (lambda (o v) (vector-set! o 3 v)))
(define class-super (lambda (o) (vector-ref o 4)))
(define set-class-super! (lambda (o v) (vector-set! o 4 v)))
(define class-subclasses (lambda (o) (vector-ref o 5)))
(define set-class-subclasses! (lambda (o v) (vector-set! o 5 v)))
(define class-predicate (lambda (o) (vector-ref o 6)))
(define set-class-predicate! (lambda (o v) (vector-set! o 6 v)))
(define class-allocator (lambda (o) (vector-ref o 7)))
(define set-class-allocator! (lambda (o v) (vector-set! o 7 v)))
(define class-maker (lambda (o) (vector-ref o 8)))
(define set-class-maker! (lambda (o v) (vector-set! o 8 v)))

(define field-name (lambda (o) (vector-ref o 1)))
(define field-reader (lambda (o) (vector-ref o 2)))
(define set-field-reader! (lambda (o v) (vector-set! o 2 v)))
(define poly-field-lengther (lambda (o) (vector-ref o 3)))
(define set-poly-field-lengther! (lambda (o v) (vector-set! o 3 v)))
(define mutable-mono-field-writer (lambda (o) (vector-ref o 3)))
(define set-mutable-mono-field-writer! (lambda (o v) (vector-set! o 3 v)))
(define mutable-poly-field-writer (lambda (o) (vector-ref o 4)))
(define set-mutable-poly-field-writer! (lambda (o v) (vector-set! o 4 v)))

(define generic-name (lambda (o) (vector-ref o 1)))
(define set-generic-name! (lambda (o v) (vector-set! o 1 v)))
(define generic-discriminator (lambda (o) (vector-ref o 2)))
(define set-generic-discriminator! (lambda (o v) (vector-set! o 2 v)))
(define generic-dispatch-table (lambda (o) (vector-ref o 3)))
(define set-generic-dispatch-table! (lambda (o v) (vector-set! o 3 v)))
(define generic-default (lambda (o) (vector-ref o 4)))
(define set-generic-default! (lambda (o v) (vector-set! o 4 v)))
(define generic-variables (lambda (o) (vector-ref o 5)))
(define set-generic-variables! (lambda (o v) (vector-set! o 5 v)))

(define allocate-generic 'wait)
(define make-mono-field 'wait)
(define make-poly-field 'wait)
(define make-mutable-mono-field 'wait)
(define make-mutable-poly-field 'wait)

;;; Build by hand the initial net of classes with all the relationship
;;; needed to bootstrap the rest of the file.
(let* ((mkmf (lambda (name) ; create a mutable-mono-field (by hand)
               (vector 6 name 'wait 'wait) ))
       (o (vector 1                     ; internal index (Object is a class)
                  'Object               ; name
                  0                     ; number
                  (list)                ; fields
                  #f                    ; super (here there is no parent)
                  (list 1 2 3)          ; subclasses
                  'wait                 ; predicate
                  'wait                 ; allocator
                  'wait                 ; maker
                  ))
       (c (vector 1
                  'Class
                  1
                  (map mkmf 
                       '(name number fields super subclasses
                              predicate allocator maker ) )
                  o
                  (list)
                  'wait
                  'wait
                  'wait ))
       (g (vector 1
                  'Generic
                  2
                  (map mkmf
                       '(name discriminator dispatch-table default
                              variables ) )
                  o
                  (list)
                  'wait
                  'wait
                  'wait ))
       (ff (vector 1
                   'Field
                   3
                   (map mkmf '(name reader))
                   o
                   (list 4 5)
                   'wait
                   'wait
                   'wait ))
       (fm (vector 1
                   'Mono-Field
                   4
                   (map mkmf '(name reader))
                   ff
                   (list 6)
                   'wait
                   'wait
                   'wait ))
       (fp (vector 1
                   'Poly-Field
                   5
                   (map mkmf '(name reader lengther))
                   ff
                   (list 7)
                   'wait
                   'wait
                   'wait ))
       (mfm (vector 1
                   'Mutable-Mono-Field
                   6
                   (map mkmf '(name reader writer))
                   fm
                   (list)
                   'wait
                   'wait
                   'wait ))
       (mfp (vector 1
                   'Mutable-Poly-Field
                   7
                   (map mkmf '(name reader lengther writer))
                   fp
                   (list)
                   'wait
                   'wait
                   'wait )) )
  (vector-set! *classes* 0 o)
  (vector-set! *classes* 1 c)
  (vector-set! *classes* 2 g)
  (vector-set! *classes* 3 ff)
  (vector-set! *classes* 4 fm)
  (vector-set! *classes* 5 fp)
  (vector-set! *classes* 6 mfm)
  (vector-set! *classes* 7 mfp)
  (set! *class-number* 8)
  (set! allocate-generic 
        (lambda () 
          (let ((v (make-vector (+ 1 (length (class-fields g))))))
            (vector-set! v 0 (class-number g))
            v ) ) )
  (set! make-mono-field 
        (lambda (name reader)
          (vector (class-number fm) name reader)) )
  (set! make-poly-field 
        (lambda (name reader lengther)
          (vector (class-number fp) name reader lengther)) )
  (set! make-mutable-mono-field 
        (lambda (name reader writer)
          (vector (class-number mfm) name reader writer)) )
  (set! make-mutable-poly-field 
        (lambda (name reader lengther writer)
          (vector (class-number mfp) name reader lengther writer )) )
  'genesis )

;;; extracts the internal number of a class from within an instance.
(define (object->class-number o)
  (vector-ref o 0) )

;;; return the class of an object (class-of in other OOL).
(define (object->class o)
  (vector-ref *classes* (object->class-number o)) )

;;; return the ith known class.
(define (number->class i)
  (vector-ref *classes* i) )

;;; returns the class named name or apply default on this name.
;;; This is an expensive function since it uses sequence-find.
(define (symbol->class name . default)
  (sequence-find
   name *classes* 
   class-name
   (if (pair? default)
       (car default)
       (lambda (name) (oo-error "No such class" name)) ) ) )

;;; returns the generic instance named name or apply default on this name.
;;; This is an expensive function since it uses sequence-find.
(define (symbol->generic name . default)
  (sequence-find 
   name *generics* 
   generic-name
   (if (pair? default)
       (car default)
       (lambda (name) (oo-error "No such generic" name)) ) ) )

;;; look for a named field in a class
(define (retrieve-field name class)
  (define (lookup fields)
    (if (pair? fields)
        (if (eq? name (field-name (car fields)))
            (car fields)
            (lookup (cdr fields)) )
        #f ) )
  (lookup (class-fields class)) )

;;; The following function is temporarily defined and 
;;; is used only during the bootstrap. It will be turned 
;;; into a generic function that the user can customize.
(define (initialize! o) o)              ; Temporary definition

(define (create-subclass name super)
  (let ((c (symbol->class name (lambda (name) #f))))
    (if c
        ;; patch the already existing class, leave intact its methods.
        (let ((old-super (class-super c)))
          (set-class-subclasses! 
           old-super (remove (class-number c)
                             (class-subclasses old-super) ) )
          (set-class-super! c super)
          (set-class-subclasses! c (list))
          (set-class-subclasses!
           super (cons (class-number c) (class-subclasses super)) )
          c )
        ;; build a fresh class.
        (let ((o (make-Class name *class-number* 'wait super (list)
                             'wait 'wait 'wait )))
          (when (>= *class-number* (vector-length *classes*))
                (extend-classes-number!) )
          (set-class-subclasses!
           super (cons *class-number* (class-subclasses super)) )
          (vector-set! *classes* *class-number* o)
          (vector-map 
           (lambda (g)
             (when g
                   (vector-set! (generic-dispatch-table g)
                                *class-number*
                                (vector-ref (generic-dispatch-table g)
                                            (class-number super) ) ) ) )
           *generics* )
          (set! *class-number* (+ 1 *class-number*))
          o ) ) ) )

;;; extend the vector of classes as well as the dispatch tables of all
;;; generic functions (whether traced or not).
(define (extend-classes-number!)
  (vector-map (lambda (g)
                (when g
                      (set-generic-dispatch-table!
                       g (vector-extend (generic-dispatch-table g)) ) ) )
              *generics* )
  (set! *classes* (vector-extend *classes*))
  #f )

;;; tests if the instance O belongs to class. 
;;; O is assumed to be an object.
(define (is-a? o class)
  (let up ((c (object->class o)))
    (or (eq? class c)
        (and (class-super c)
             (up (class-super c)) ) ) ) )

;;; find a pair in specs while flattening specs into a variable-list:
;;; call k on these two results. Useful for parsing define-generic
;;; and define-method.
(define (parse-disc specs k)
  (let ((disc #f)
        (variables 'wait) )
    (set! variables
          (let search ((vars specs))
            (if (pair? vars)
                (if (pair? (car vars))
                    (begin (set! disc (car vars))
                           (cons (caar vars) (search (cdr vars))) )
                    (cons (car vars) (search (cdr vars))) )
                vars ) ) )
    (if disc (k disc variables)
        (oo-error "No discrimination variable" specs) ) ) )

;;; (define-generic (foo x (y) z) [ . default-body])
;;; Methods are represented by fixed arity functions.
(define-oo-macro (define-generic call . body)
  (unless (pair? (cdr call)) (oo-error "Niladic generic function" call))
  (parse-disc 
   (cdr call)
   (lambda (disc-spec variables)
     (let ((generic (gensym))
           (discriminator (gensym))
           (default (gensym)) )
       `(set! ,(car call) 
              (letrec 
                  ((,generic (create-generic ',(car call)))
                   (,discriminator
                    (lambda ,variables
                      ((if (object? ,(car disc-spec)) 
                           (vector-ref
                            (generic-dispatch-table ,generic)
                            (object->class-number ,(car disc-spec)) )
                           ,default )
                       . ,(flat variables) ) ) )
                   (,default
                     (lambda ,(flat variables) 
                       ,(if (pair? body)
                            `(begin . ,body)
                            `(oo-error "No method for" 
                                       ',(car call) 
                                       . ,(flat variables) ) ) ) ) )
                (set-generic-variables! ,generic ',(cdr call))
                (set-generic-discriminator! ,generic ,discriminator)
                (set-generic-default! ,generic ,default)
                (set-generic-dispatch-table!
                 ,generic (make-vector (vector-length *classes*) ,default) )
                ;; This is the original discriminator bound to the name
                ;; of the generic function.
                (lambda ,variables
                  (if (eq? ,discriminator (generic-discriminator ,generic))
                      ;; Normal (and fast) case
                      ((if (object? ,(car disc-spec)) 
                           (vector-ref
                            (generic-dispatch-table ,generic)
                            (object->class-number ,(car disc-spec)) )
                           ,default )
                       . ,(flat variables) )
                      ;; only useful when a function is traced:
                      ,(if (null? (cdr (last-pair variables)))
                           `((generic-discriminator ,generic)
                             . ,variables )
                           `(oo-apply (generic-discriminator ,generic)
                                      . ,(flat variables) ) ) ) ) ) ) ) ) ) )

;;; This function installs two functions named before and after 
;;; to advise the use of a generic instance. Before is called on 
;;; the input arguments while after is invoked on the result.
(define (generic-trace generic before after)
  (let* ((couple (assq (generic-name generic) *traced-generics*))
         (traced 
          (or couple
              (list (generic-name generic)
                    (generic-discriminator generic) ) ) )
         (old-discriminator (cadr traced)) )
    (set-generic-discriminator!
     generic
     (lambda args
       (apply before args)
       (after (apply old-discriminator args)) ) )
    (unless couple
            (set! *traced-generics* (cons traced *traced-generics*)) )
    generic ) )

;;; remove the tracing functions associated to a generic instance.
(define (generic-untrace generic)
  (let ((name (generic-name generic))
        (head (cons 1936 *traced-generics*)) )
    (define (look l)
      (cond ((null? (cdr l)) #t)
            ((eq? name (car (cadr l)))
             (set-generic-discriminator!
              generic (cadr (cadr l)) )
             (set-cdr! l (cddr l)) )
            (else (look (cdr l))) ) )
    (look head)
    (set! *traced-generics* (cdr head)) )
  generic )

;;; Creates a new generic instance (trying to reuse an old one).
(define (create-generic name)
  (or (symbol->generic name (lambda (name) #f) )
      (let ((g (allocate-generic)))
        (set-generic-name! g name)
        (when (>= *generic-number* (vector-length *generics*))
              (set! *generics* (vector-extend *generics*)) )
        (vector-set! *generics* *generic-number* g)
        (set! *generic-number* (+ 1 *generic-number*))
        g ) ) )

;;; (define-method (foo x (y class) z) . body)
;;; Even if the list of variables is dotted, the corresponding
;;; method has a fix arity.
(define-oo-macro (define-method call . body)
  (parse-disc 
   (cdr call)
   (lambda (disc-spec variables)
     (unless (pair? (cdr disc-spec))
             (oo-error "No class mentionned" disc-spec) )
     (let ((g (gensym)) (c (gensym)))
       `(register-method 
         ',(car call)
         ',(cadr disc-spec)
         ',(cdr call)
         (lambda (,g ,c)
           (lambda ,(flat variables)
             ;; Call the super-method
             (define (call-next-method)
               ((vector-ref (generic-dispatch-table ,g)
                            (class-number (class-super ,c)) )
                . ,(flat variables) ) )
             . ,body ) ) ) ) ) ) )

;;; Officially register the method.
(define (register-method generic-name
                         discriminating-class-name
                         variable-list
                         method-maker )
  (let ((g (symbol->generic generic-name))
        (c (symbol->class discriminating-class-name)) )
    (unless (coherent-variables? (generic-variables g) variable-list)
            (oo-error "Non congruent lambda-lists" 
                       (generic-variables g) variable-list ) )
    (add-method! g c (method-maker g c)) ) )


;;; Add a method (a function) to a generic associated with a given class.
;;; The method is propagated to all subclasses which do not redefine it.
(define (add-method! generic class method)
  (define (propagate class old-method)
    (when (eq? old-method (vector-ref (generic-dispatch-table generic)
                                      (class-number class) ))
          (vector-set! (generic-dispatch-table generic)
                       (class-number class)
                       method )
          (for-each (lambda (c) (propagate (number->class c) old-method))
                    (class-subclasses class) ) ) )
  (propagate class (vector-ref (generic-dispatch-table generic)
                               (class-number class) ))
  method )

;;; Test the congruence of two variable lists. They may contain a final
;;; n-ary variable.
(define (coherent-variables? la lb)
  (if (pair? la)
      (if (pair? lb)
          (and (or ;; similar discriminating variable
                   (and (pair? (car la))
                        (pair? (car lb)) )
                   ;; similar regular variable
                   (and (symbol? (car la))
                        (symbol? (car lb)) ) )
               (coherent-variables? (cdr la) (cdr lb)) )
          #f )
      (or (and (null? la) (null? lb))
          ;; similar dotted variable
          (and (symbol? la) (symbol? lb)) ) ) )

;;; These generic functions are used by field-descriptors to generate
;;; the code of their related functions. 

;;; takes a field and generates the code to rebuild it
(define-generic (generate-field (field)))

(define-method (generate-field (o Mono-Field))
  `(make-mono-field ',(field-name o) default-slot-accessor) )

(define-method (generate-field (o Poly-Field))
  `(make-poly-field ',(field-name o) 
                    default-slot-accessor 
                    default-slot-accessor ) )

(define-method (generate-field (o Mutable-Mono-Field))
  `(make-mutable-mono-field ',(field-name o) 
                            default-slot-accessor 
                            default-slot-accessor ) )

(define-method (generate-field (o Mutable-Poly-Field))
  `(make-mutable-poly-field ',(field-name o) 
                            default-slot-accessor
                            default-slot-accessor
                            default-slot-accessor ) )

;;; Check if a prefix of args can be accepted by field, return the
;;; arguments not taken. Used in the run-time of make-<instance>.
(define-generic (check-argument-for-field (field) args))

(define-method (check-argument-for-field (o Mono-Field) args)
  (if (pair? args)
      (cdr args)
      (oo-error "Too less arguments" o) ) )

(define-method (check-argument-for-field (o Poly-Field) args)
  (if (and (pair? args)
           (number? (car args)) )
      (if (and (>= (car args) 0)
               (>= (length (cdr args)) (car args)) )
          (list-tail (cdr args) (car args))
          (oo-error "Incomplete repetition" o) )
      (oo-error "Incorrect repetition factor" (car args) o) ) )

;;; return a list of waited parameters for each field.
(define-generic (need-parameter-for-field (field)))

;;; a mono-field does not expect anything
(define-method (need-parameter-for-field (o Mono-Field))
  (list) )

;;; a poly-field waits for a repetition factor indicating how much slots
;;; must be allocated in the instance.
(define-method (need-parameter-for-field (o Poly-Field))
  (list (field-name o)) )

;;; returns the code of the reader of field.
(define-generic (generate-reader-for-field (field) name fields index class))

(define-method (generate-reader-for-field (o Mono-Field) 
                                          name fields index class )
  (cons `(begin 
           (set! ,(oo-concat name "-" (field-name o))
                    (lambda (o) (vector-ref o ,index)) )
           (set-field-reader!
            (retrieve-field ',(field-name o) ,class)
            ,(oo-concat name "-" (field-name o)) ) )
        (generate-readers name fields (generate-plus 1 index) class) ) )

(define-method (generate-reader-for-field (o Poly-Field) 
                                          name fields index class )
  (cons `(begin
           (set! ,(oo-concat name "-" (field-name o))
                    (lambda (o i)
                      (if (and (>= i 0) (< i (vector-ref o ,index)))
                          (vector-ref 
                           o ,(generate-plus
                               1 (generate-plus index `i) ) )
                          (oo-error "Index out of range" 
                                    o ',(field-name o) i ) ) ) )
           (set-field-reader!
            (retrieve-field ',(field-name o) ,class)
            ,(oo-concat name "-" (field-name o)) ) )
        (generate-readers name 
                          fields
                          (generate-plus 
                           1 (generate-plus 
                              index `(vector-ref o ,index) ) )
                          class ) ) )

;;; generate a function to write a field.
(define-generic (generate-writer-for-field (field) name fields index class))

(define-method (generate-writer-for-field (o Field) 
                                          name fields index class )
  (generate-writers name fields (generate-plus 1 index) class) )

(define-method (generate-writer-for-field (o Mutable-Mono-Field) 
                                          name fields index class )
  (cons `(begin
           (set! ,(oo-concat 'set- name "-" (field-name o) "!")
                    (lambda (o v)
                      (vector-set! o ,index v) ) )
           (set-mutable-mono-field-writer!
            (retrieve-field ',(field-name o) ,class)
            ,(oo-concat 'set- name "-" (field-name o) "!") ) )
        (generate-writers name fields (generate-plus 1 index) class) ) )

(define-method (generate-writer-for-field (o Mutable-Poly-Field) 
                                          name fields index class )
  (cons `(begin
           (set! ,(oo-concat 'set- name "-" (field-name o) "!")
                    (lambda (o i v)
                      (if (and (>= i 0) (< i (vector-ref o ,index)))
                          (vector-set! o ,(generate-plus
                                           1 (generate-plus index `i) )
                                       v )
                          (oo-error "Index out of range" 
                                    o ',(field-name o) i ) ) ) )
           (set-mutable-poly-field-writer!
            (retrieve-field ',(field-name o) ,class)
            ,(oo-concat 'set- name "-" (field-name o) "!") ) )
        (generate-writers name 
                          fields
                          (generate-plus 
                           1 (generate-plus 
                              index `(vector-ref o ,index) ) )
                          class ) ) )

;;; generates a function to compute the length of a poly-field
(define-generic (generate-lengther-for-field (field) name fields index class))

(define-method (generate-lengther-for-field (o Mono-Field) 
                                            name fields index class )
  (generate-lengthers name fields (generate-plus 1 index) class) )

(define-method (generate-lengther-for-field (o Poly-Field) 
                                            name fields index class )
  (cons `(begin
           (set! ,(oo-concat name "-" (field-name o) "-" 'length)
                    (lambda (o) (vector-ref o ,index)) )
           (set-poly-field-lengther!
            (retrieve-field ',(field-name o) ,class)
            ,(oo-concat name "-" (field-name o) "-" 'length) ) )
        (generate-lengthers 
         name 
         fields
         (generate-plus
          1 (generate-plus 
             index `(vector-ref o ,index) ) )
         class ) ) )

;;; return the code that structures a fresh instance. Afterthat the 
;;; instance is still empty (fields contain NIL) but has a skeleton
;;; ie may be inquired for the lengths of inner poly-fields.
(define-generic (generate-skeletizer-for-field (field) name fields index))

(define-method (generate-skeletizer-for-field (o Mono-Field) name fields index)
  (generate-skeletizer name fields (generate-plus 1 index)) )

(define-method (generate-skeletizer-for-field (o Poly-Field) name fields index)
  (cons `(vector-set! ,name ,index ,(field-name o))
        (generate-skeletizer name 
                             fields
                             (generate-plus
                              1 (generate-plus 
                                 index (field-name o)) ) ) ) )


;;; The fundamental classes.
;;;=========================

;;; Object is too intricate to be regenerated, so it is hand-built.
;;;(define-class Object no-super ())

;;; rather weak !
(define (object? o)
  (and (vector? o)
       (number? (object->class-number o)) ) )

(define (make-object)
  (vector (class-number (symbol->class 'Object))) )

(define allocate-object make-object)
(let ((o (symbol->class 'Object)))
  (set-class-predicate! o object?)
  (set-class-allocator! o allocate-object)
  (set-class-maker! o make-object)
  "Object built" )

;;; The other fundamental classes can now be automatically regenerated.

(define-class Class Object 
  (name number fields super subclasses predicate allocator maker))

(define-class Generic Object 
  (name discriminator dispatch-table default variables) )

(define-class Field Object (name reader))

(define-class Mono-Field Field ())

(define-class Poly-Field Field (lengther))

(define-class Mutable-Mono-Field Mono-Field (writer))

(define-class Mutable-Poly-Field Poly-Field (writer))

;;; Library of basic generic functions
;;;====================================

;;; Automatically called on new instances and predefined to do nothing.
;;; It is there to be customized on your classes.
(define-generic (initialize! (o)) o)

;;; Field-writer is a simple name 
(define-generic (field-writer (o)))

(define-method (field-writer (o Mutable-Mono-Field))
  (mutable-mono-field-writer o) )

(define-method (field-writer (o Mutable-Poly-Field))
  (mutable-poly-field-writer o) )

;;;==================================
;;; Show printed images of instances.

(define-generic (show (o) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (cond ((symbol? o) (display o stream))
          ((boolean? o) (display (if o "#T" "#F") stream))
          ((null? o) (display "()" stream))
          ((string? o) (display o stream))
          ((number? o) (display o stream))
          ((pair? o) (show-list o stream))
          ((vector? o) (show-vector o stream))
          ((procedure? o) (display o stream))
          ((char? o) (display o stream))
          (else (oo-error "Cannot show" o)) ) ) )

(define (show-list o . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (define (show-content o)
      (show (car o) stream)
      (cond ((null? (cdr o)) #t)
            ((pair? (cdr o)) (display " " stream)
                             (show-content (cdr o)) )
            (else (display " . " stream)
                  (show (cdr o) stream) ) ) )
    (display "(" stream)
    (show-content o)
    (display ")" stream) ) )

(define (show-vector o . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port)))
        (n (vector-length o)) )
    (define (show-content i)
      (when (< i n) 
            (show (vector-ref o i) stream)
            (display " " stream)
            (show-content (+ 1 i)) ) )
    (display "#(" stream)
    (show-content 0)
    (display ")" stream) ) )

(define-method (show (o Object) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<a " stream)
    (display (class-name (object->class o)) stream)
    (display ">" stream)
    o ) )

(define-method (show (o Class) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<Class: " stream)
    (display (class-name o) stream)
    (display ">" stream)
    o ) )

(define-method (show (o Mono-Field) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<Field=" stream)
    (when (mutable-mono-field? o) (display "!" stream))
    (display (field-name o) stream)
    (display ">" stream)
    o ) )

(define-method (show (o Poly-Field) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<Field*" stream)
    (when (mutable-poly-field? o) (display "!" stream))
    (display (field-name o) stream)
    (display ">" stream)
    o ) )

;;;==========================================================
;;; This generic function returns a copy of any Meroon object. 
;;; This is only a shallow (no recursive) copy.

(define-generic (clone (o)))

(define-method (clone (o Object))
  (let* ((n (vector-length o))
         (r (make-vector n)) )
    (vector-copy! o r 0 n)
    r ) )

;;; Library of useful functions.
;;;==============================
;;; This function displays the inheritance tree of classes.

(define (show-hierarchy . args)
  (let* ((class-name (if (pair? args) (car args) 'Object))
         (stream (if (pair? args) (cdr args) '()))
         (stream (if (pair? stream) (car stream) (current-output-port))) )
    (define (show-class c indent)
      (do ((i 0 (+ 1 i)))
          ((>= i indent))
        (display " " stream) )
      (show c stream)
      (newline stream)
      (for-each (lambda (c) (show-class (number->class c) (+ indent 1)))
                (class-subclasses c) ) )
    (display "Subclass tree of " stream)
    (display class-name stream)
    (newline stream)
    (show-class (symbol->class class-name) 0)
    #t ) )

;;; Show all different methods attached to a generic function.
(define (show-generic generic-name . stream)
  (let* ((stream (if (pair? stream) (car stream) (current-output-port)))
         (generic (symbol->generic generic-name))
         (dispatch-table (generic-dispatch-table generic)) )
    (define (show-method c super-method indent)
      (let ((current-method (vector-ref dispatch-table (class-number c))))
        (unless (eq? super-method current-method)
          (do ((i 0 (+ 1 i)))
              ((>= i indent))
            (display " " stream) )
          (show c stream)
          (newline stream) )
        (for-each (lambda (c)
                    (show-method (number->class c)
                                 current-method
                                 (+ indent 1) ) )
                  (class-subclasses c) ) ) )
    (display "Methods on " stream)
    (display generic-name stream)
    (newline stream)
    (show-method (symbol->class 'object) 
                 (generic-default generic)
                 0 )
    #t ) )

;;;==========================================================
;;; A better tracing facility with meaningful defaults

(define (show-generic-trace . names)
  (for-each
   (lambda (name)
     (if (member name *traced-generic-names*)
         (oo-error "Already traced" name)
         (begin
           (set! *traced-generic-names* (cons name *traced-generic-names*))
           (generic-trace (symbol->generic name)
                          (lambda args
                            (show name)
                            (display "<<")
                            (for-each (lambda (arg) 
                                        (display " ")
                                        (show arg) )
                                      args )
                            (newline) )
                          (lambda (result)
                            (show name)
                            (display ">> ")
                            (show result)
                            (newline)
                            result ) ) ) ) )
   names ) )

(define (show-generic-untrace . names)
  (when (and (null? names) (pair? *traced-generic-names*))
        (set! names *traced-generic-names*) )
  (for-each (lambda (name)
              (set! *traced-generic-names* 
                    (remove name *traced-generic-names*) )
              (generic-untrace (symbol->generic name)) )
            names ) )


;;;=============================================================
;;; Notes pour moi-meme:
;;; Moche d'utiliser des set! pour creer la classe.
;;; si Native-Pair etait une classe, comment en heriterait-t-on ?
;;; encapsuler mieux le code engendre par define-XX afin de limiter
;;; le nombre de symboles inconnus engendres par Scheme2C par exemple.
