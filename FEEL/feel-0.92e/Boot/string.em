;; Eulisp Module
;; Author: pab
;; File: string.em
;; Date: Wed Jun 30 12:27:51 1993
;;
;; Project:
;; Description: 
;;

(defmodule string
  (init extras0 macros0 defs gens character)
  ()

  (export <string>)

;;; ----------
;;; default methods for initial-state, next-state, current-element and
;;; (setter current-element) work for <string>

  (defmethod element ((v <string>) (s <fixint>)) (string-ref v s))

  (defmethod (setter element) ((c <string>) (s <fixint>) v)
    ((setter string-ref) c s v))

  (defmethod current-key ((c <string>) (s <fixint>)) s)

;;; ----------
;;; specified methods

  (defmethod binary< ((s1 <string>) (s2 <string>)) (string-lt s1 s2))

  (defmethod as-lowercase ((s <string>)) (map as-lowercase s))

;;; (defgeneric as-lowercase! (s))
;;; 
;;; (export as-uppercase!)
;;; 
;;; (defmethod as-lowercase! ((s <string>)) (map-into s as-lowercase s))

  (defmethod as-uppercase ((s <string>)) (map as-uppercase s))

;;; (defgeneric as-uppercase! (s))
;;; 
;;; (export as-uppercase!)
;;; 
;;; (defmethod as-uppercase! ((s <string>)) (map-into s as-uppercase s))

  (defmethod size ((c <string>)) (length c))

  (defmethod deep-copy ((s <string>))
    (shallow-copy s))

  (defmethod clone ((s <string>))
    (string-copy s))

  ;; end module
  )
