;; Eulisp Module
;; Author: pab
;; File: pretty.em
;; Date: Fri Jul  9 12:28:15 1993
;;
;; Project:
;; Description: 
;;  pretty printing for lists. Not defuns.

(defmodule pretty
  (eulisp0         
   )
  ()
	
  (defstruct <tracking-stream> ()
    ((stream initarg stream reader tracking-stream-stream)
     (pos initform 0 accessor tracking-stream-pos)
     (lst initform nil accessor tlst))
    constructor (make-tracking-stream stream))

  (defun inc-posn (stream n)
    ((setter tracking-stream-pos) stream
     (+ n (tracking-stream-pos stream))))
  
  (defun zero-posn (stream)
    ((setter tracking-stream-pos) stream 0))

  (defmethod output ((stream <tracking-stream>) (o <character>))
    (if (eq o #\newline)
	(zero-posn stream)
      (inc-posn stream 1))
    ((setter tlst) stream (cons o (tlst stream)))
    (output (tracking-stream-stream stream) o)
    o)

  ;; much too lazy---should really
  ;; check for newlines in here too 
  ;; may be better to have line-buffering though.
  (defmethod output ((stream <tracking-stream>) (s <string>))
    (do (lambda (c)
	  (output stream c))
	s))
  
  (defmethod flush ((stream <tracking-stream>))
    (flush (tracking-stream-stream stream)))

  ;; pretty printer
  (defconstant *width* 60)
  
  (defun pretty-print (form . stream)
    (let ((stream (make-tracking-stream
		   (if (null stream)
		       (standard-output-stream)
		     (car stream)))))
      (newline stream)
      (pp-aux form 0 stream)))
  
  (defconstant pp pretty-print)

  (defgeneric pp-aux (form off stream))
  
  (defmethod pp-aux ((x <object>) off stream)
    (generic-write x stream))
  
  ;; Utterly broken quick hack
  (defmethod pp-aux ((form <cons>) off stream)
    (if (not (consp (car form)))
        (progn (newline stream)
               (generic-prin (make-string off) stream))
      nil)
  (generic-prin "(" stream)
    (let ((here (tracking-stream-pos stream)))
      (labels ((aux (forms )
		    (if (consp forms)
			(progn (pp-aux (car forms) here stream)
			       (when (consp (cdr forms)) (generic-prin " " stream))
			       (if (> (tracking-stream-pos stream) *width*)
				   (skip-down-to here)
				 nil)
			       (aux (cdr forms)))
		      (if (null forms)
			  nil
			(progn (if (> (tracking-stream-pos stream) (- *width* 3))
				   (skip-down-to here)
				 nil)
			       (generic-prin " . " stream)
			       (pp-aux forms here stream)))))
	       (skip-down-to (x)
			     (progn (newline stream)
				    (generic-prin (make-string x) stream))))
	      (aux form)
	      (generic-prin ")" stream))))
  
  ;; end module
  )
