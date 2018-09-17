;; Eulisp Module
;; Author: pete broadbery
;; File: mod-cache.em
;; Date: 18/sep/1991
;;
;; Project:
;; Description: 
;; reads and caches the exports of a module 
;;

(defmodule mod-cache 
  ((except (fold) standard)
   list-fns
   
   comp-utl
   )
  ()
  
  ;; Badly named. Something holding the property list of a module.

  (defstruct import-module ()
    ((props initarg props accessor imported-module-props))
    constructor (make-imported-module props))

  (export import-module)

  (defconstant mod-exports (mk-finder))

  (defconstant *default-interface-path*
    ".:/net/brad/denton_export/denton/You/NewYou/Interfaces:/net/brad/denton_export/denton/You/Interfaces")

  (defconstant *interface-path* (make-search-path 
				 "FEEL_INTF_PATH" #\:
				 *default-interface-path*))


  (defun read-names (stream)
    (let* ((raw-names (read stream))
	   (mod (make-imported-module raw-names)))
      (mapcar (lambda (info)
		(list (cdr (assq 'name info))
		      info mod))
	      (cdr (assq 'exported-ids raw-names)))))

  ;; Interface function
  (defun read-exportations (modname)
    (format t "Reading: ~a~%" modname)
    (or (mod-exports modname)
	(let ((file (path-open *interface-path*
			       (interface-file-name modname))))
	  (unwind-protect
	    (let ((exps (read-names file)))
	      ((setter mod-exports) modname exps)
	      exps)
	    (close file)))))
    
  (export read-exportations
	  imported-module-props
	  get-module-stream)
  ;; end module
  )
