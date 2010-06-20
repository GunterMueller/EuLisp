;;; EuLisp system 'youtoo'
;;;   Interface file for module ex-import

(definterface ex-import
  (import (i-all sx-obj sx-node p-env cg-interf)
   syntax (_macros _i-aux0 _sx-obj0)
   full-import (i-error i-notify i-param i-level1 boot1 boot symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level1 aux-table i-all sx-obj2 sx-obj1 sx-obj p-env i-ffi sx-node i-modify cg-interf)
   export (
    ((name . import-module) (pos . 7) (origin ex-import . import-module))
    ((name . expand-import) (pos . 6) (origin ex-import . expand-import))
    ((name . expand-old-imports) (pos . 2) (origin ex-import . expand-old-imports))
    ((name . register-imported-module) (pos . 5) (origin ex-import . register-imported-module))
   )
   local-literals (
    (top-level . 78)
    (expand-old-imports . 77)
    (import-binding . 76)
    (register-imported-module . 75)
    (expand-import . 74)
    (import-module . 73)
    (install-import-expander . 72)
    (import-expander . 71)
    (make-prefix . 70)
    ("no import expander ~a available" . 67)
    ("no import expander ~a available" . 66)
    ("redefinition of expander ~a" . 62)
    (|(method G002136)| . 60)
    ("  Import module ~a ..." . 59)
    ("cannot import lexical module ~a" . 57)
    ("compile time error condition: " . 56)
    ("external binding ~a not available in module ~a" . 50)
    (prefix . 47)
    (rename . 46)
    (except . 45)
    (only . 44)
    (|(method G002186)| . 42)
    ("bad only import syntax" . 40)
    ("compile time error condition: " . 39)
    (|(method G002212)| . 35)
    ("bad except import syntax" . 33)
    ("compile time error condition: " . 32)
    (|(method G002240)| . 28)
    ("bad rename import syntax" . 26)
    ("compile time error condition: " . 25)
    (*actual-module* . 20)
    (|(method G002272)| . 19)
    (anonymous . 18)
    (ct-error-value: . 16)
    ("bad prefix import syntax" . 15)
    ("compile time error condition: " . 14)
   )
   literals (
   )
))
