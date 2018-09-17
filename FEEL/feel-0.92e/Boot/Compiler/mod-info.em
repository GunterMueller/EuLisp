;; Eulisp Module
;; Author: pab
;; File: mk-modinfo.em
;; Date: Mon Nov 25 14:03:51 1991
;;
;; Project:
;; Description: 
;;   adds some useful information into the module structure

(defmodule mod-info
  ((except (fold scan-args) standard)
   list-fns
   scan-args

   syntx-env
   pass
   props
   )
  ()
  
  (defclass Module-State-Error (<condition>)
    ((msg initarg msg initform "")
     (values initarg values initform nil))
    metaclass <condition-class>
    )

  (export Module-State-Error)

  (defstruct module-info import-module
    ((imports initarg imports 
	      accessor module-info-imports)
     (class initarg class
	    accessor module-info-class)
     (exports initarg exports
	      accessor module-info-exports)
     (definitions initarg definitions 
                  accessor module-info-defns))
    constructor (make-module-info imports exports))



  (defun set-module-interface (module initargs)
    (let ((imports (scan-args 'import-list initargs ()))
	  (module-ops (get-mod-ops module)))
      ;;(format t "Mod ops: ~a~%" module-ops)
      ((setter module-imports) module imports)
      ((setter module-flat-imports) module 
       (flatten-imports (module-import-spec module)))
      ((setter module-declarations) module (car module-ops))
      (let ((exports (find-mod-exports (append (car module-ops) 
					       (module-imports-list imports))
				       (cdr module-ops)
				       module)))
	((setter module-exports) module exports)
	module
	)))
  
 
  (export set-module-interface)
  (export module-info-defns module-info-imports module-info-class)

  ;; returns (export-stmts . local-decls)
  (defun get-mod-ops (module)
    (generic-get-mod-ops module))

  (defgeneric generic-get-mod-ops (decl))

  (defmethod generic-get-mod-ops ((x syntax-obj))
    (fold (lambda (x so-far)
	    (cons (add-decls (car x) (car so-far))
		  (append (cdr x) (cdr so-far))))
	  (let ((xx (mapcar
		     generic-get-mod-ops 
		     (subcomponents x))))
	    xx)
	  (cons nil nil)))
  
  (defun add-decls (lst var-lst)
    (if (null lst) var-lst
      (let ((name (defn-ide (car lst))))
	(if (detect (lambda (x) 
		      (eq (defn-ide x) name))
		    var-lst)
	    (progn (cerror "Thing declared twice" Module-State-Error 
			   'values (list name)
			   'msg "** ~a defined more than once~%")
		   ;; if the error is continued...
		   (add-decls (cdr lst) var-lst))
	  (add-decls (cdr lst) (cons (car lst) var-lst))))))
  
  (defmethod generic-get-mod-ops ((decl module-definition))
    (cons (list decl) nil))
  
  (defmethod generic-get-mod-ops ((exp export-spec))
    (cons nil (list exp)))

  ;; No, you can't have them inside lambdas...
  (defmethod generic-get-mod-ops ((exp lambda-term))
    (cons nil nil))

  ;; try to find out what must be kept
  ;; returns list of module-infos
  
  (defun find-mod-exports (defns export-spec module)
    (fold (lambda (export-ob so-far) 
	    (append (make-module-export export-ob defns module) so-far))
	  export-spec
	  nil))

  (defgeneric make-module-export (exporter defns module))

  (defmethod make-module-export ((exp export-directive) defns module)
    (fold (lambda (defn new-exps)
	    (if (memq (defn-ide defn) (export-spec-name exp))
		(cons defn new-exps)
	      new-exps))
	  defns
	  nil))

  (defmethod make-module-export ((imp import-directive) defns module)
    (error "Import in module body" <Internal-Error>))

  (defmethod make-module-export ((exp expose-directive) defns module)
    (let* ((yy (format t "(Expose: ~a~%" exp))
	   (xx (read-imports (expose-spec-importer exp))))
      (format t "~a)" xx)
      (module-imports-list xx)))

  ;; accessors to retrieve bindings
  ;; returns nil if not to be found

  (defun find-module-import (module name)
    (find-name (module-imports module) name))

  ;; Print method
  (defmethod print-decl ((defn imported-definition) stream)
    (format stream "'~a'" (cons (import-home defn) (import-real-name defn))))

  (export find-module-import imported-definition import-home import-real-name)

  
  ;; end module
  )
