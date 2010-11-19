;;; EuLisp system 'youtoo'
;;;   Interface file for module cg-bycode2

(definterface cg-bycode2
  (import (i-level-1 cg-bycode)
   syntax (_macros _cg-bycode0 _sx-obj0)
   full-import (aux-table level-1 telos mop-defcl mop-meth mop-gf mop-inspect mop-init mop-class mop-key mop-prim mop-access mop-alloc bit condition event thread dynamic let-cc callback string convert copy integer number fpi collect compare character float stream stream1 lock stream2 socket list format convert1 vector table1 table read handler random stream3 symbol boot boot1 i-level-1 cg-bycode)
   export (
    ((name . bytecode-args!) (pos . 2) (origin cg-bycode . bytecode-args!) (inline (G004496 (stack-ref 1) (static-fpi-byte-ref 4) (stack-ref 2) (binding-ref ? <bytecode>) (set-primitive-relative-ref) (nobble 2))))
    ((name . bytecode-size!) (pos . 4) (origin cg-bycode . bytecode-size!) (inline (G004484 (stack-ref 1) (static-ref1) (stack-ref 2) (binding-ref ? <bytecode>) (set-primitive-relative-ref) (nobble 2))))
    ((name . bytecode-name!) (pos . 17) (origin cg-bycode . bytecode-name!) (inline (G004500 (stack-ref 1) (static-fpi-byte-ref 5) (stack-ref 2) (binding-ref ? <bytecode>) (set-primitive-relative-ref) (nobble 2))))
    ((name . bytecode-code?) (pos . 13) (origin cg-bycode . bytecode-code?) (inline (G004490 (static-fpi-byte-ref 3) (binding-ref ? <bytecode>) (primitive-relative-ref))))
    ((name . bytecode-modus!) (pos . 9) (origin cg-bycode . bytecode-modus!) (inline (G004480 (stack-ref 1) (static-ref0) (stack-ref 2) (binding-ref ? <bytecode>) (set-primitive-relative-ref) (nobble 2))))
    ((name . bytecode-args?) (pos . 7) (origin cg-bycode . bytecode-args?) (inline (G004494 (static-fpi-byte-ref 4) (binding-ref ? <bytecode>) (primitive-relative-ref))))
    ((name . bytecode-code!) (pos . 14) (origin cg-bycode . bytecode-code!) (inline (G004492 (stack-ref 1) (static-fpi-byte-ref 3) (stack-ref 2) (binding-ref ? <bytecode>) (set-primitive-relative-ref) (nobble 2))))
    ((name . bytecode-properties!) (pos . 10) (origin cg-bycode . bytecode-properties!) (inline (G004488 (stack-ref 1) (static-ref2) (stack-ref 2) (binding-ref ? <bytecode>) (set-primitive-relative-ref) (nobble 2))))
    ((name . bytecode-name?) (pos . 11) (origin cg-bycode . bytecode-name?) (inline (G004498 (static-fpi-byte-ref 5) (binding-ref ? <bytecode>) (primitive-relative-ref))))
    ((name . get-bytecode) (pos . 18) (origin cg-bycode . get-bytecode) (class . constant))
    ((name . get-stream-primitive) (pos . 8) (origin cg-bycode . get-stream-primitive) (class . constant))
    ((name . bytecode-size?) (pos . 12) (origin cg-bycode . bytecode-size?) (inline (G004482 (static-ref1) (binding-ref ? <bytecode>) (primitive-relative-ref))))
    ((name . <bytecode>) (pos . 5) (origin cg-bycode . <bytecode>) (class . constant))
    ((name . bytecode?) (pos . 16) (origin cg-bycode . bytecode?))
    ((name . bytecode-modus?) (pos . 15) (origin cg-bycode . bytecode-modus?) (inline (G004478 (static-ref0) (binding-ref ? <bytecode>) (primitive-relative-ref))))
    ((name . bytecode-properties?) (pos . 3) (origin cg-bycode . bytecode-properties?) (inline (G004486 (static-ref2) (binding-ref ? <bytecode>) (primitive-relative-ref))))
    ((name . get-register) (pos . 6) (origin cg-bycode . get-register) (class . constant))
   )
   local-literals (
    (top-level . 159)
    (symbols . 157)
    (keywords . 156)
    (next-methods . 155)
    (callbacks . 154)
    (vector-class . 153)
    (table-class . 152)
    (symbol-class . 151)
    (string-ref-class . 150)
    (string-class . 149)
    (null-class . 148)
    (method-class . 147)
    (lambda-class . 146)
    (keyword-class . 145)
    (generic-class . 144)
    (fpi-ref-class . 143)
    (fpi-class . 142)
    (double-float-ref-class . 141)
    (double-float-class . 140)
    (cons-class . 139)
    (character-class . 138)
    (byte-vector-class . 137)
    (argv . 136)
    (argc . 135)
    (((in obj1 obj2 obj3) (out obj3)) . 134)
    (memq . 133)
    (((in obj1 obj2 obj3) (out obj3)) . 132)
    (iniq . 131)
    (((in obj1 obj2 obj3) (out obj3)) . 130)
    (obj3 . 129)
    (assq . 128)
    (((in obj1) (out obj2)) . 127)
    (intern . 126)
    (((in obj)) . 125)
    (exit . 124)
    (((in int) (out char)) . 123)
    (fpi-as-character . 122)
    (((in char) (out int)) . 121)
    (char . 120)
    (character-as-fpi . 119)
    (((in obj)) . 118)
    (set-state-ref . 117)
    (((out obj)) . 116)
    (state-ref . 115)
    (((in obj1 obj2)) . 114)
    (restore-state . 113)
    (((in obj1) (out obj1)) . 112)
    (fill-state . 111)
    (((in obj1) (out obj1)) . 110)
    (restore-thread-state . 109)
    (((in obj1) (out obj1)) . 108)
    (fill-thread-state . 107)
    (((in) (out obj)) . 106)
    (unflush-stacks . 105)
    (((in obj) (out obj)) . 104)
    (test-and-set-lock . 103)
    (((in obj1 obj2) (out obj)) . 102)
    (eql . 101)
    (((in obj1 obj2) (out obj)) . 100)
    (eq . 99)
    (dealloc . 98)
    (((in obj)) . 97)
    ((byte byte) . 96)
    (set-display-ref . 95)
    (((out obj)) . 94)
    ((byte byte) . 93)
    (display-ref . 92)
    ((byte) . 91)
    (alloc . 90)
    (return2 . 89)
    (return1 . 88)
    (return0 . 87)
    ((byte) . 86)
    (return . 85)
    (check-arguments2 . 84)
    (check-arguments1 . 83)
    (check-arguments0 . 82)
    (check-arguments-1 . 81)
    (check-arguments-2 . 80)
    ((byte) . 79)
    (check-arguments . 78)
    (((in obj1 obj2) (out obj)) . 77)
    (write-object . 76)
    (((in int *) (out obj)) . 75)
    (int . 74)
    ((ff) . 73)
    (ff . 72)
    (call-foreign-function . 71)
    (((in obj1 obj2) (out obj)) . 70)
    (obj2 . 69)
    (obj1 . 68)
    (apply . 67)
    (((in *) (out obj)) . 66)
    ((byte byte) . 65)
    (tail-call-operator . 64)
    (((in *) (out obj)) . 63)
    ((byte) . 62)
    (call-operator . 61)
    (((in obj obj) (out obj)) . 60)
    ((byte) . 59)
    (make-lambda . 58)
    (((in *) (out obj)) . 57)
    ((byte byte) . 56)
    (tail-call-next-method . 55)
    (((in *) (out obj)) . 54)
    (out . 53)
    (* . 52)
    ((byte) . 51)
    (call-next-method . 50)
    (((in obj)) . 49)
    ((byte) . 48)
    (branch-nil-long-pos . 47)
    (((in obj)) . 46)
    ((byte) . 45)
    (branch-nil-pos . 44)
    (((in obj)) . 43)
    ((byte) . 42)
    (branch-nil-neg . 41)
    (((in obj)) . 40)
    ((byte) . 39)
    (branch-nil-long-neg . 38)
    (((in obj)) . 37)
    ((label) . 36)
    (branch-nil . 35)
    (((in obj)) . 34)
    ((byte) . 33)
    (branch-true-long-pos . 32)
    (((in obj)) . 31)
    ((byte) . 30)
    (branch-true-pos . 29)
    (((in obj)) . 28)
    ((byte) . 27)
    (branch-true-neg . 26)
    (((in obj)) . 25)
    ((byte) . 24)
    (branch-true-long-neg . 23)
    (((in obj)) . 22)
    (obj . 21)
    (in . 20)
    ((label) . 19)
    (branch-true . 18)
    ((byte) . 17)
    (branch-long-pos . 16)
    ((byte) . 15)
    (branch-pos . 14)
    ((byte) . 13)
    (branch-neg . 12)
    ((byte) . 11)
    (byte . 10)
    (branch-long-neg . 9)
    (properties: . 8)
    (code: . 7)
    ((label) . 6)
    (label . 5)
    (args: . 4)
    (branch . 3)
    (name: . 2)
   )
   literals (
   )
))
