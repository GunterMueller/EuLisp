(defmodule meroon (scheme) ()

  (display "Hold your horses...\n")
  (load "Modules/meroon.euscm")
  (load "Modules/meroon.scm")
  (display "Hokay.\n")

  (export
   
    define-generic
    define-method
    define-class

    ;; Class
    Class?
    make-Class
    allocate-Class
      Class-name Class-number Class-fields Class-super Class-subclasses
      Class-predicate Class-allocator Class-maker

    ;; Generic
    Generic?
    make-Generic
    allocate-Generic
      Generic-name Generic-discriminator Generic-dispatch-table
      Generic-default Generic-variables

    ;; Field
    Field?
    make-Field
    allocate-Field
      Field-name Field-reader

    ;; Mono-Field
    make-Mono-Field
    Mono-Field?

    ;; Poly-Field
    make-Poly-Field
    Poly-Field?

    ;; Mutable-Mono-Field
    make-Mutable-Mono-Field
    Mutable-Mono-Field?

    ;; Mutable-Poly-Field
    make-Mutable-Poly-Field
    Mutable-Poly-Field?

    show
    clone
    initialize!

    object->class
    symbol->class
    number->class
    symbol->generic
    
  )

)

;; eof
