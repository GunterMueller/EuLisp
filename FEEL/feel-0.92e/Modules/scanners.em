;; Eulisp Module
;; Author: pab
;; File: scanners.em
;; Date: Tue Apr 27 15:29:00 1993
;;
;; Project:
;; Description: 
;;

(defmodule scanners
  ((only (scan-args 
	  required-argument
	  null-argument 
	  default-argument)
	 init)
   )
  ()
  
  (export scan-args required-argument null-argument default-argument)
  ;; end module
  )
