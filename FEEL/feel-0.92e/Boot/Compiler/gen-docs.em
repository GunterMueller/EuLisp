;; Eulisp Module
;; Author: pab
;; File: gen-docs.em
;; Date: Mon Jul 12 19:15:28 1993
;;
;; Project:
;; Description: 
;;  Tracks documentation strings. Basically a hack.
;;  Initialized and used by gen-code. File finalised by output

(defmodule gen-docs
  (eulisp0
   fn-docs         
   abs-syntx
   )
  ()

  (export initialize-docs add-documented-entry complete-docs docs-name)
  
  (deflocal *the-directory* nil)
  (deflocal *entry-count* 0)
  (deflocal *module-name* nil)

  (defun initialize-docs (name)
    "Create the necessary junk"
    (setq *module-name* name)
    (setq *the-directory* (make-directory (format nil "~a.doc" name))))
  
  (defun docs-name () *module-name*)
  ;; returns id of entry
  (defun add-documented-entry (name doc)
    (let ((count *entry-count*))
      (output-record *the-directory* (make-info name doc))
      (setq *entry-count* (+ *entry-count* 1))
      count))

  (defun complete-docs ()
    (finalise-dbase *the-directory*)
    (setq *the-directory* nil))
  
  ;; end module
  )
