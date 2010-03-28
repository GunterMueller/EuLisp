;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;                 OPS5 for EuLisp System 'youtoo'
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; File   : reader-act.em
;;; Date   :  8 Aug 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Desc.  : Functions to set up actions
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defmodule reader-act
;;; Uncomment this block to run under youtoo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN_YOUTOO
(syntax (macros macros-tag) 
import (level1 basic prod action ops-out)) 
;;; END_YOUTOO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uncomment this block to run under euscheme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN_EUSCHEME
;;  (import (level0 reader-vars prod action ops-out))
;;; END_EUSCHEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defconstant ops-warn format)
;;; read-action
;;; Process an action
   (defun read-action (reader action prod)
     ;;(format ops-out "action: ~a~%" action)
     (let* ((type (car action))
            (act (cond
                  ((eql type 'make) (read-make-action (cdr action)))
                  ((eql type 'remove) (read-remove-action (cdr action)))
                  ((eql type 'modify) (read-modify-action (cdr action)))
                  ((eql type 'write) (read-write-action (cdr action)))
                  ((eql type 'bind) (read-bind-action (cdr action)))
                  ((eql type 'halt) (read-halt-action (cdr action)))
                  (t (ops-warn t "Action ~a not supported~%" type)))))
            (set-prod-actions prod (append (prod-actions prod) (list act))))
     reader)
   (defun read-make-action (action)
     ;;(format ops-out "make: ~a~%" action)
     (make-make-action (car action) (cdr action)))
   (defun read-remove-action (action)
     ;;(format ops-out "remove: ~a~%" action)
     (make-remove-action action))
   (defun read-modify-action (action)
     ;;(format ops-out "modify: ~a~%" action)
     (make-modify-action (car action) (cdr action)))
   (defun read-write-action (action)
     ;;(format ops-out "write: ~a~%" action)
     (make-write-action action))
   
   (defun read-bind-action (action)
     ;;(format ops-out "bind: ~a~%" action)
     (make-bind-action (car action) (cadr action)))
   
   (defun read-halt-action (action)
     ;;(format ops-out "halt: ~a~%" action)
     (make-halt-action))
   (export read-action)
) ;; module: reader-act