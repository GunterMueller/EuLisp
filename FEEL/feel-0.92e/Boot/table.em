;; Eulisp Module
;; Author: pab
;; File: table.em
;; Date: Wed Jun 30 12:46:18 1993
;;
;; Project:
;; Description: 
;;

(defmodule table
  (gens init extras0 macros0 defs
	)
  ()
  
  (export tablep table-ref setter-table-ref)

  (defpredicate tablep <table>)

  (defgeneric table-ref (table key)
    method (((x <table>) key)
	    (sys-table-ref x key)))

  (defgeneric setter-table-ref (table key value)
    method (((x <table>) key value)
	    ((setter sys-table-ref) x key value)))
  
  ((setter setter) table-ref setter-table-ref)

  (defmethod initial-state ((tab <table>))
    (accumulate
     (lambda (a x)
       (if (null x) a (cons (car x) a)))
     ()
     (table-values tab)))

  (defmethod next-state ((tab <table>) (s <null>)) ())

  (defmethod next-state ((tab <table>) (s <cons>)) (cdr s))

  (defmethod final-state ((tab <table>)) (initial-state tab))

  (defmethod previous-state ((tab <table>) (s <null>)) ())

  (defmethod previous-state ((tab <table>) (s <cons>)) (cdr s))

  (defmethod current-element ((tab <table>) (s <cons>))
    (table-ref tab (car s)))

  (defmethod (setter current-element) ((c <table>) (s <cons>) v)
    ((setter table-ref) c (car s) v))

  (defmethod current-key ((tab <table>) (s <cons>)) (car s))

  (defmethod key-sequence ((tab <table>)) (initial-state tab))

  (defmethod element ((tab <table>) k)
    (table-ref tab k))

  (defmethod (setter element) ((tab <table>) k v)
    ((setter table-ref) tab k v))

  (defmethod size ((tab <table>)) (table-population tab))

  (defmethod clone ((tab <table>))
    (make <table> 
	  'hash-function (table-hash-function tab)
	  'comparator (table-comparator tab)
	  'table-fill (table-fill tab)))

  (defun clear-table (tab) 
    ((setter table-population) tab 0)
    (fill (table-values tab) ()))

  (export clear-table)

  (defmethod gf-remove (object (table <table>) comp)
    (let ((k (find-key table (lambda (v) (comp v object)))))
      (if k
	  (let ((r (shallow-copy table)))
	    (table-delete r k)
	    r)
	table)))

  (defmethod gf-delete (object (table <table>) comp)
    (let ((k (find-key table (lambda (v) (comp v object)))))
      (when k (table-delete table k))
      table))

  ;; end module
  )
