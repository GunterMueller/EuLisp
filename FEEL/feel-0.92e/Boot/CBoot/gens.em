;; Eulisp Module
;; Author: pab
;; File: gens.em
;; Date: Tue Jun 29 20:59:56 1993
;;
;; Project:
;; Description: 
;;   All generic functions defined in the standard 
;;   that have not yet been defined go here.

(defmodule gens
  (init
   defs
   extras0
   macros0
   )
  ()
       
  

;; ----------
;; functions for collections

(defgeneric size (o))
(export  size  emptyp  member  gf-member  do  gf-do  anyp
	 gf-any  map  gf-map  accumulate  accumulate1
	 fill gf-fill-range gf-fill-from
	 concatenate  gf-concatenate  concatenate-as  
	 gf-concatenate-as  deep-copy  shallow-copy reverse clone
	 first second third last sort delete gf-delete remove gf-remove)

(defgeneric emptyp (o))

(defun member (v c . f) (gf-member v c (if f (car f) eq)))

(defgeneric gf-member (v c f))

(defun do (f c . cs) (gf-do f c cs))

(defgeneric gf-do (f c cs))

(defun anyp (f c . cs) (gf-any f c cs))

(defgeneric gf-any (f c cs))

(defun map (f c . cs) (gf-map f c cs))

(defgeneric gf-map (f c cs))

(defgeneric accumulate (f i c))

(defgeneric accumulate1 (f c))

(defun fill (mc v . where)
  (if (null where)
      (gf-fill-range mc v 0 (- (size mc) 1))
    (if (null (cdr where))
	(gf-fill-from mc v (car where))
      (gf-fill-range mc v (car where) (cadr where)))))

(defgeneric gf-fill-range (mc v start end))

(defgeneric gf-fill-from (mc v where))

(defun concatenate (sequence . more-sequences)
  (gf-concatenate sequence more-sequences))

(defgeneric gf-concatenate (sequence more-sequences))

(defun concatenate-as (class sequence . more-sequences)
  (gf-concatenate-as class sequence more-sequences))

(defgeneric gf-concatenate-as (class sequence more-sequences))

(defgeneric deep-copy (x))

(defgeneric shallow-copy (x))

(defgeneric clone (x))

(defgeneric reverse (sequence))

(defgeneric first (sequence))

(defgeneric second (sequence))

(defgeneric third (sequence))

(defgeneric last (sequence))

(defgeneric sort (sequence comparator))

(defun delete (object collection . comparator)
  (gf-delete object collection (if (null comparator) eql (car comparator))))

(defgeneric gf-delete (object collection comparator))

(defun remove (object collection . comparator)
  (gf-remove object collection (if (null comparator) eql (car comparator))))

(defgeneric gf-remove (object collection comparator))

;; ----------
;; p123

(defgeneric element (c s))

(defgeneric (setter element) (c s v))

(defgeneric current-key (c s))

(defgeneric key-sequence (c))

(export element current-key key-sequence)

  ;; Iteration protocol

(defgeneric initial-state (c))

(defmethod initial-state (c) (if (= 0 (size c)) () 0))

(defgeneric next-state (c s))

(defmethod next-state (c s) (if (< (+ s 1) (size c)) (+ s 1) ()))

(defgeneric final-state (c))

(defmethod final-state (c) (if (= 0 (size c)) () (- (size c) 1)))

(defgeneric previous-state (c s))

(defmethod previous-state (c s) (if (= s 0) () (- s 1)))

(defgeneric current-element (c s))

(defmethod current-element (c s) (element c s))

(defgeneric (setter current-element) (c s v))

(defmethod (setter current-element) (c s v) ((setter element) c s v))

(defgeneric copy-state (c s))

(defmethod copy-state (c s) s)

(export initial-state next-state final-state previous-state
	current-element copy-state)

(defun find-key (collection function . optionals)
  (let ((skip 0) (failure ()))
    (when optionals
      (setq skip (car optionals))
      (setq optionals (cdr optionals)))
    (when optionals
      (setq failure (car optionals))
      (setq optionals (cdr optionals)))
    (gf-find-key collection function skip failure)))

(defgeneric gf-find-key ((c <collection>) f s r))

(export find-key gf-find-key)

  ;; end module
)
