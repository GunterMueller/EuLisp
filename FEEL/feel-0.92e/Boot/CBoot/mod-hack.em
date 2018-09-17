;; Eulisp Module
;; Author: pab
;; File: mod-hack.em
;; Date: Tue Dec 22 16:58:48 1992
;;
;; Project:
;; Description: 
;;

(defmodule mod-hack
        (standard0
         module-operators
	 root
         )
        ()
	(defun add-load-path (x)
	  ((dynamic-access (get-module 'root) 'set-load-path) (cons x ((dynamic-access (get-module 'root) 'load-path)))))
	
	(export add-load-path)
	
	(defun xx-add-load-path (x)
	  nil)

	(export xx-add-load-path)
      ;; end module
      )
