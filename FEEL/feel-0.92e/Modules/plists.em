
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;  EuLisp Module                     Copyright (C) University of Bath 1991  ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule plists (standard0) ()

  (deflocal main-table (make <table> ))

  (defun put (id key val)
    (let ((prop-table (or 
		        (table-ref main-table id)
			(progn
			  ((setter table-ref) main-table id (make <table>))
			  (table-ref main-table id)))))
      ((setter table-ref) prop-table key val)
      val))

  (export put)

  (defun get (id key)
    (let ((tab (table-ref main-table id)))
      (if (null tab) nil
	(table-ref tab key))))

  (export get)

  ((setter setter) get put)

  (defun remprop (id key)
    (let ((tab (table-ref main-table id)))
      (if (null tab) nil
	(let ((ans (table-ref tab key)))
					; May be a new table
	  ;;((setter table-ref) main-table id (table-delete tab key))
	  ans))))

  (export remprop)

  (defun symbol-props (id)
    (let ((tab (table-ref main-table id)))
      (if (null tab) nil
	(let ((ans nil))
	  (map-table
	     (lambda (tag prop) (setq ans (cons tag (cons prop ans))))
	     tab)
	  ans))))

  (defun table-delete (t x)
    ((setter table-ref) t x nil))

  (defun kill-props (id)
    ((setter table-ref) main-table id nil))

  (export symbol-props kill-props)
)
