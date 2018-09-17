
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;  EuLisp Module                     Copyright (C) University of Bath 1991  ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule plists (standard0) ()

  (deflocal main-table (make-table eq))

  (defun put (id key val)
    (let ((prop-table (or 
		        (table-ref main-table id)
			(progn
			  ((setter table-ref) main-table id (make-table eq))
			  (table-ref main-table id)))))
      ((setter table-ref) prop-table key val)
      val))

  (export put)

  (defun get (id key)
    (let ((tab (table-ref main-table id)))
      (if tab (table-ref tab key) nil)))

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
