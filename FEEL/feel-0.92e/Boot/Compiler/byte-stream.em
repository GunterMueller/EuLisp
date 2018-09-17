;; Eulisp Module
;; Author: pab
;; File: byte-stream.em
;; Date: Fri Dec  6 00:04:49 1991
;;
;; Project:
;; Description: 
;;   Generic streams
;;   allow a reader and a writer to communicate. 
;;   sorta like a pipe really.
;; Use:
;;   interface to bytecode generator


(defmodule byte-stream
  (standard0
   list-fns
         
   )
  ()
     
  (defstruct stream ()
    ()
    predicate stream-p)
  
  (defstruct end-of-stream-object ()
    ()
    predicate eos-p)

  (deflocal *end-of-stream* (make-instance end-of-stream-object))
  (defun end-of-stream () *end-of-stream*)

  (defgeneric write-stream (stream ob))

  (defgeneric read-stream (stream ob))

  (defgeneric write-stream-list (stream lst))

  (defmethod write-stream-list ((str stream) lst)
    (fold (lambda (o s)
	    (write-stream str o))
	  lst
	  str))

  (export  read-stream write-stream write-stream-list end-of-stream stream-p)

  ;; Writer driven stream
  ;; a write forces evaluation of the reader function.

  (defstruct writer-stream stream 
    ((reader-function initarg reader-fn
		      reader writer-stream-function))
    constructor (make-writer-stream reader-fn))

  (defmethod write-stream ((w writer-stream) x)
    ((writer-stream-function w) w x))

  (defmethod read-stream ((w writer-stream) x)
    (error "Writer stream cannot be read~%" <clock-tick>))

  (defstruct simple-stream ()
    ((content initform nil 
	      accessor simple-stream-content)
     (last initform nil
	   accessor simple-stream-last))
    constructor (make-simple-stream))

  (defmethod write-stream ((str simple-stream) ob)
    (if (null (simple-stream-content str))
	(let ((xx (list ob)))
	  ((setter simple-stream-content) str xx)
	  ((setter simple-stream-last) str xx)
	  str)
      (progn ((setter cdr) (simple-stream-last str) (list ob))
	     ((setter simple-stream-last) str (cdr (simple-stream-last str)))
	     str)))

  (defmethod write-stream-list ((str simple-stream) l)
    (cond ((null l) str)
	  ((null (simple-stream-content str))
	   ((setter simple-stream-content) str l)
	   ((setter simple-stream-last) str (last-pair l))
	   str)
	  (t ((setter cdr) (simple-stream-last str) l)
	     ((setter simple-stream-last) str (last-pair l))
	     str)))
	      
  (defmethod (converter pair) ((str simple-stream))
    (simple-stream-content str))
  
  (defstruct filter-stream stream
    ((filter initarg filter 
	     reader filter-stream-filter)
     (stream initarg stream 
	     reader filter-stream-stream))
    constructor (make-filter-stream stream filter))

  (defmethod write-stream ((str filter-stream) ob)
    (write-stream (filter-stream-stream str) 
		  ((filter-stream-filter str) ob)))

  (defmethod (converter pair) ((xx filter-stream))
    (convert (filter-stream-stream xx) pair))

  
  (export writer-stream make-writer-stream make-simple-stream make-filter-stream)
  ;; end module
  )
