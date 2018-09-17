;; Eulisp Module
;; Author: pab
;; File: iface.em
;; Date: Thu Feb 20 17:01:27 1992
;;
;; Project: Compiler
;; Description: 
;;     Dumps out an interface file given a module-block

(defmodule iface
  (standard0
   list-fns
   
   props
   syntx-env
   comp-utl
   syntx-utl
   )
  ()
  
  (export write-interface-file)

  (defun write-interface-file (mod-blk)
    (let ((name (interface-file-name (module-name mod-blk))))
      (let ((stream (open name 'output t)))
	(unwind-protect (write (make-interface-description mod-blk) 
			       stream)
	  (close stream)))))

  (defun make-interface-description (blk)
    (list (list 'name (module-name blk))
	  (list 'dependencies (make-dependencies blk))
	  (cons 'exported-ids (make-exported-ids blk))))
  
  (defun make-dependencies (blk)
    (module-dependencies blk))
  
  (defgeneric sub-specs (x ))

  (defmethod sub-specs ((x import-directive))
    nil)

  (defmethod sub-specs ((x union-directive))
    (union-content x))

  (defmethod sub-specs ((x rename-directive))
    (list (rename-imports x)))

  (defmethod sub-specs ((x only-directive))
    (list (only-imports x)))

  (defmethod sub-specs ((x except-directive))
    (list (except-imports x)))

  ;; old dependency grabbing
  (defgeneric get-dep-list (x)
    methods ((((x import-spec))
	      (fold (lambda (a l)
		      (append (get-dep-list a) l))
		    (sub-specs x)
		    nil))
	     (((x import-directive))
	      (list (import-directive-name x)))))
  
  (defun make-exported-ids (blk)
    (let ((ids (module-exports blk)))
      (mapcar make-iface-export ids)))
  
  (defgeneric make-iface-export (decl))

  (defmethod make-iface-export ((decl imported-definition))
    (defn-properties decl))

  (defmethod make-iface-export ((decl module-definition))
    (decl-class decl))

    ;; end module
    )
