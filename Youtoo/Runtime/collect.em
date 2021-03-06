;;; Copyright 1997 A. Kind & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: collections
;;;  Library: level-1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule collect
  (syntax (_telos0)
   import (telos
           compare)
   export (name
           <collection>
           <sequence>
           collectionp
           sequence?
           accumulate
           accumulate1
           any?
           all?
           do
           fill
           find
           map
           member
           select
           element
           delete
           remove
           reset
           emptyp
           size
           reverse
           sort
           concatenate
           slice))

;;;-----------------------------------------------------------------------------
;;; Name
;;;-----------------------------------------------------------------------------
(defgeneric name (x)
  method: (((x <symbol>)) (symbol-name x))
  method: (((x <keyword>)) (keyword-name x))
  method: (((x <function>)) (function-name x))
  method: (((x <class>)) (class-name x))
  method: (((x <slot>)) (slot-name x)))

;;;-----------------------------------------------------------------------------
;;; Classes: <collection> and <sequence>
;;;-----------------------------------------------------------------------------
(defclass <collection> <object> ()
  abstract?: t)

(defclass <sequence> <collection> ()
  keywords: (size: fill-value:)
  abstract?: t)

;;;-----------------------------------------------------------------------------
;;; Predicates
;;    Return t (not x when generated) in positive case
;;;-----------------------------------------------------------------------------
(defgeneric collectionp (x)
  method: (((x <object>)) ())
  method: (((x <collection>)) t))

(defgeneric sequence? (x)
  method: (((x <object>)) ())
  method: (((x <sequence>)) t))

(defgeneric emptyp (o))

(defmethod binary= ((c1 <collection>) (c2 <collection>))
  (and (eq (class-of c1) (class-of c2))
       (if (list? c1)
           (all? binary= c1 c2)
         (and (fpi-binary= (size c1) (size c2))
              (all? binary= c1 c2)))))

;;;-----------------------------------------------------------------------------
;;; Iteration
;;    Note that "do" and "map" methods for <collection> are in vector.em
;;    because they depend on conversion to <vector>.
;;;-----------------------------------------------------------------------------
(defgeneric accumulate (f i c))
(defgeneric accumulate1 (f c))
(defgeneric any? (f c . cs))
(defgeneric all? (f c . cs))
(defgeneric do (f c . cs))
(defgeneric fill (c x . keys))
(defgeneric find (v c . f))
(defgeneric map (f c . cs))
(defgeneric member (v c . f))
(defgeneric select (f c . cs))

(defmethod any? ((fun <function>) (c <collection>) . cs)
  (let* ((ccs (cons c cs))
         (n (apply min (map size ccs))))
    (labels
     ((loop (i)
            (and (fpi-binary< i n)
                 (or (apply fun (map (lambda (x) (element x i)) ccs))
                     (loop (fpi-binary+ i 1))))))
     (loop 0))))

(defmethod all? ((fun <function>) (c <collection>) . cs)
  (let* ((ccs (cons c cs))
         (n (apply min (map size ccs))))
    (labels
     ((loop (i)
            (if (fpi-binary< i n)
                (and (apply fun (map (lambda (x) (element x i)) ccs))
                     (loop (fpi-binary+ i 1)))
              t)))
     (loop 0))))

(defmethod fill ((c <collection>) x . keys)
  (error () "fill not yet implemented"))

(defmethod find (x (c <collection>) . preds)
  (apply member x c preds))

;;;-----------------------------------------------------------------------------
;;; Access
;;;-----------------------------------------------------------------------------
(defgeneric element (c s))
(defgeneric (setter element) (c s v))
(defgeneric delete (c s . pred)) ;; destructive
(defgeneric remove (c s . pred)) ;; non-destructive
(defgeneric reset (x)) ;; also used for streams

;;;-----------------------------------------------------------------------------
;;; Size
;;;-----------------------------------------------------------------------------
(defgeneric size (c))

;;;-----------------------------------------------------------------------------
;;; Reverse
;;;-----------------------------------------------------------------------------
(defgeneric reverse (c))

;;;-----------------------------------------------------------------------------
;;; Sort
;;;-----------------------------------------------------------------------------
(defgeneric sort (c . comp))

;;;-----------------------------------------------------------------------------
;;; Concatenate
;;;-----------------------------------------------------------------------------
(defgeneric concatenate (c . cs))

;;;-----------------------------------------------------------------------------
;;; Slice
;;;-----------------------------------------------------------------------------
(defgeneric slice (c s e))

;;;-----------------------------------------------------------------------------
)  ;; End of module collect
;;;-----------------------------------------------------------------------------
