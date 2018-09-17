;; Eulisp Module
;; Author: pab
;; File: character.em
;; Date: Wed Jun 30 12:32:03 1993
;;
;; Project:
;; Description: 
;;

(defmodule character
  (init 
   extras0
   gens
   defs
   macros0
   table
   characters
   )
  ()

  (deflocal lc-chars "abcdefghijklmnopqrstuvwxyz")
  (deflocal uc-chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

  (deflocal to-lower (make <table> 'comparator eql 'hash-function generic-hash))
  (deflocal to-upper (make <table> 'comparator eql 'hash-function generic-hash))
  
  (export characterp)

  (labels
   ((loop (i table from to)
	  (if (< i 0)
	      ()
	    (progn
	      ((setter table-ref) table (string-ref from i) (string-ref to i))
	      (loop (- i 1) table from to)))))
   (loop (- (string-length uc-chars) 1) to-lower uc-chars lc-chars)
   (loop (- (string-length lc-chars) 1) to-upper lc-chars uc-chars))

  (defgeneric as-lowercase (x))

  (export as-lowercase)

  (defmethod as-lowercase ((c <character>)) (or (table-ref to-lower c) c))

  (defgeneric as-uppercase (x))

  (export as-uppercase)

  (defmethod as-uppercase ((c <character>)) (or (table-ref to-upper c) c))

  (defmethod = ((c1 <character>) (c2 <character>))
    (equal c1 c2))

  (defun upperp (c) (member c uc-chars))

  (defun lowerp (c) (member c lc-chars))

  (defun digitp (c) (member c "0123456789"))

  (defmethod binary< ((c1 <character>) (c2 <character>))
    (cond
     ((and (upperp c1) (upperp c2))
      (< (character-to-integer c1) (character-to-integer c2)))
     ((and (lowerp c1) (lowerp c2))
      (< (character-to-integer c1) (character-to-integer c2)))
     ((and (digitp c1) (digitp c2))
      (< (character-to-integer c1) (character-to-integer c2)))
     (t ())))

  ;; end module
  )
