;; Eulisp Module
;; Author: pete broadbery
;; File: syntax.em
;; Date: 30/jun/1991
;;
;; Project:
;; Description: 
;;  Transforms a piece of eulisp into an
;;  abstract syntax tree
;;

(defmodule syntax 
  (standard0
   list-fns
   syntx-env
   scan-args
   )
  ()
  
  ;;
  ;; Translator into abstract syntax...
  ;; covers most cases now (I hope)
  ;;
  (defclass Syntax-Error (<condition>)
    (msg values)
    )

  (export Syntax-Error)
  (defconstant get-import-translator (mk-finder))
  (defconstant get-module-translator (mk-finder))
  
  ;; cop out at the moment...
  (defun find-translator (syntax-env ob)
    (syntax-env ob))
  
  (defun translate-expr (syntax-env form)
    (syntax-env form))
    
  (defun translate (ob)
    (translate-expr (std-module-translator ())
		    ob))

  ;; could be generic...
  (defun std-module-translator (env)
    (lambda (form)
      (cond ((eq form t) (literal form))
	    ;; nil always evaluates to ()!
	    ((eq form 'nil) (literal nil))
	    ((symbolp form) (ident form))
	    ((atom form) (literal form))
	    (t;; otherwize...
	     (let ((tran (get-module-translator-1 (car form))))
	       (if tran 
		   (tran (std-module-translator env) (cdr form))
		 (let ((macro (find-macro env (car form))))
		   (if macro 
		       (progn (format t  "Macro: ~a~%" (car form))
			      (translate-expr (std-module-translator env)
					      (compile-macro-expand (expander (cdr macro))
								    (cdr form))))
		     (translate-applic (std-module-translator env) form)))))))))
  
  (defun get-module-translator-1 (exp)
    (cond ((atom exp)
	   (get-module-translator exp))
	  ((eq (car exp) 'lambda)
	   (rewrite-inline-lambda (cdr exp)))
	  (t nil)))

  ;; really only nasty 'cos someone may do ((lambda x ...) 1 2 3 4)
  (defun rewrite-inline-lambda (lambda-term)
    (labels ((rewrite-args (args values)
			   (cond ((null args) nil)
				 ((atom args) 
				  (list (list args (cons 'list values))))
				 (t (cons (list (car args) (car values))
					  (rewrite-args (cdr args) (cdr values)))))))
      (lambda (tran args)
	(translate-expr tran
			(print `(let ,(rewrite-args (car lambda-term) args)
				  ,@(cdr lambda-term)))))))


  (defun import-translator (form)
    ((get-import-translator (car form)) import-translator (cdr form)))

  (defun translate-applic (syntax-env form)
    (make-applic (translate-expr syntax-env (car form))
		 (mapcar (lambda (form) (translate-expr syntax-env form))
			 (cdr form))))
  
  ((setter get-module-translator) 'progn
   (lambda (syntax-env txt)
     (make-sequence
      (mapcar (lambda (ob)
		(translate-expr syntax-env ob))
	      (if (null txt) (list nil) txt)))))

  (defun end-mapcar (fn lst)
    (if (consp lst)
	(cons (fn (car lst)) (end-mapcar fn (cdr lst)))
      (if lst (fn lst) ())))

  ((setter get-module-translator) 'lambda 
   (lambda (syntax-env txt)
     (make-lambda (end-mapcar make-lambda-id (car txt))
		  (translate-expr syntax-env 
				  (cons 'progn (cdr txt)))
		  )))
  
  ((setter get-module-translator) '%extended-lambda
   (lambda (syntax-env txt)
     (make-extended-lambda (car txt) (cadr txt)
			   (end-mapcar make-lambda-id (caddr txt))
			   (translate-expr syntax-env
					   (cons 'progn 
						 (cdddr txt)))
			   )))

  ((setter get-module-translator) 'let 
   (lambda (syntax-env txt)
     (make-block (make-and-decl
		  (mapcar (lambda (decl)
			    (make-definition (car decl)
					     (translate-expr syntax-env (cadr decl))
					     t))
			  (car txt)))
		 (translate-expr syntax-env 
				 (cons 'progn (cdr txt))))))


  ;; the real labels...
  ((setter get-module-translator) 'labels
   (lambda (syntax-env txt)
     (make-block 
      (make-rec-decl
       (make-and-decl
	(mapcar (lambda (decl)
		  (make-definition (car decl)
				   (translate-expr syntax-env (cons 'lambda (cdr decl)))
				   nil))
		(car txt))))
      (translate-expr syntax-env 
		      (cons 'progn (cdr txt))) )))
		      
  ((setter get-module-translator) 'if
   (lambda (syntax-env txt)
     (if (or (null txt) 
	     (null (cdr txt))
	     (null (cddr txt)))
	 (progn (cerror "Syntax in if statement" Syntax-Error
			'msg "**Invalid if expression: ~a~%"
			'values (cons 'if txt))
		(make-error-term))
       (make-cond (translate-expr syntax-env (car txt))
		  (translate-expr syntax-env (cadr txt))
		  (translate-expr syntax-env (caddr txt))))))
  

  ((setter get-module-translator) 'defun 
   (lambda (syntax-env txt)
     (make-block (make-rec-decl (make-module-definition 
				 (car txt) 
				 (let ((doc-text (defun-code txt)))
				   (translate-expr syntax-env
						   `(%extended-lambda ,(car txt) ,(car doc-text) ,@(cdr doc-text))))
				 nil))
		 (translate-expr syntax-env (car txt)))))
  
  (defun defun-code (txt)
    (if (and (stringp (caddr txt))
	     (not (null (cdddr txt))))
	(cons (caddr txt)
	      (cons (cadr txt) 
		    (cdddr txt)))
      (cons "" (cdr txt))))

  ;; A long shot, but it might just work...
  ((setter get-module-translator) 'defmacro
   (lambda (syntax-env txt)
     (make-block (make-rec-decl (make-module-definition 
				 (car txt) 
				 (make-macro-lambda (end-mapcar make-lambda-id (cadr txt))
						    (translate-expr syntax-env
								    (cons 'progn (cddr txt))))
				 nil))
		 (translate-expr syntax-env (car txt)))))

  ;; defconstant + deflocal return their values...
  ((setter get-module-translator) 'defconstant
   (lambda (syntax-env txt)
     (make-block
      (make-module-definition 
       (car txt) 
       (translate-expr syntax-env (if (cdr txt) (cadr txt) nil))
       nil)
      (translate-expr syntax-env (car txt)))))

  ((setter get-module-translator) 'deflocal
   (lambda (syntax-env txt)
     (make-block
      (make-module-definition 
       (car txt) 
       (translate-expr 
	syntax-env 
	(if (cdr txt) (cadr txt) nil))
       t)
      (translate-expr syntax-env (car txt)))))

  ((setter get-module-translator) 'call-next-method
   (lambda (syntax-env txt)
     (mk-call-next-method-term)))
	
  ((setter get-module-translator) '%Compiler-special
   (lambda (syntax-env txt)
     (mk-special-term  (car txt) (cdr txt))))
  
  ((setter get-module-translator) '%Compiler-special-object
   (lambda (syntx-env txt)
     (mk-special-term2 (car txt) (cadr txt) (mapcar (lambda (exp)
						      (translate-expr syntx-env exp))
						    (cddr txt)))))
  ;; wronginsh
  ((setter get-module-translator) 'export
   (lambda (syntax-env txt)
     (make-export-directive 
      txt)))

  ((setter get-module-translator) 'expose
   (lambda (syntax-env txt)
     (make-expose-directive 
      (translate-expr import-translator 
		      (hack-imports txt)))))

  ((setter get-module-translator) 'setq
   (lambda (syntax-env txt)
     (assignment (translate-expr syntax-env (car txt))
		 (translate-expr syntax-env (cadr txt)))))

  ((setter get-module-translator) 'quote
   (lambda (syntax-env txt)
     (literal (car txt))))
  
  
  (defun find-imports (lst)
    (let ((new (scan-args 'import lst nil)))
      (or new lst)))

  ((setter get-module-translator) 'defmodule
   (lambda (syntax-env txt)
     (let* ((import-expr (translate-expr import-translator
					 (hack-imports (find-imports (cadr txt)))))
	    (new-env (make-local-syntax syntax-env import-expr)))
       (make-module (car txt)
		    import-expr
		    nil ;; syntax
		    nil ;; exports
		    (make-sequence (mapcar (lambda (x) 
					     (translate-expr (cdr new-env) x))
					   (cdddr txt)))
		    'import-list (car new-env)))))
  
  ;; syntax depends heavily on imports :-(  )
  (defun make-local-syntax (mod-env imports)
    (let ((i (read-imports imports)))
      (cons i (std-module-translator i))))
  
  ;; judiciously add a few imports...
  (defun hack-imports (import-spec)
    (cons 'union
	  (mapcar (lambda (x) 
		    (cond ((symbolp x) (list 'import x))
			  (t x)))
		  import-spec)))

  ((setter get-import-translator) 'import 
   (lambda (syntax-env txt)
     (make-import-directive (car txt))))

  ((setter get-import-translator) 'rename
   (lambda (syntax-env txt)
     (make-rename-directive (car txt)
			    (translate-expr syntax-env (hack-imports (cdr txt))))))
  ((setter get-import-translator) 'except
   (lambda (syntax-env txt)
     (make-except-directive (car txt)
			    (translate-expr syntax-env (hack-imports (cdr txt))))))

  ((setter get-import-translator) 'only
   (lambda (syntax-env txt)
     (make-only-directive (car txt)
			  (translate-expr syntax-env (hack-imports (cdr txt))))))

  ((setter get-import-translator) 'union
   (lambda (syntax-env txt)
     (make-union-directive 
      (mapcar (lambda (x) 
		(translate-expr syntax-env x))
	      txt))))
  ;;
  ;; printing the beastie
  ;;

  (defmethod print-term ((tm syntax-obj) stream)
    (format stream "#<term: ~a>" (class-name (class-of stream))))

  (defmethod print-term ((tm ident-term) stream)
    (prin (term-id tm) stream))

  (defmethod print-term ((tm literal-term) stream)
    (write (literal-content tm) stream))

  (defmethod print-term ((tm special-term) stream)
    (format stream "(%special:~a ~a)" 
	    (special-term-name tm)
	    (special-term-data tm)))

  (defmethod print-term ((cond condition-term) stream)
    (prin "(if " stream)
    (print-term (cond-test cond) stream)
    (prin " " stream)
    (print-term (cond-t-part cond) stream)
    (prin " " stream)
    (print-term (cond-f-part cond) stream)
    (prin ")" stream))

  (defmethod print-term ((l-term lambda-term) stream)
    (prin "(lambda " stream)
    (prin (lambda-ids l-term) stream)
    (print-term (lambda-body l-term) stream)
    (prin ")" stream))

  (defmethod print-term ((applic applic-term) stream)
    (prin "(" stream)
    (print-term (applic-fun applic) stream)
    (mapcar (lambda (term)
	      (prin " " stream)
	      (print-term term stream))
	    (applic-args applic))
    (prin ")" stream))

  (defmethod print-term ((seq sequence) stream)
    (prin "(progn" stream)
    (mapcar (lambda (term)
	      (prin " " stream)
	      (print-term term stream))
	    (sequence-content seq))
    (prin ")" stream))

  (defmethod print-term ((assign assignment-term) stream)
    (prin "(setq " stream)
    (prin (assign-var assign) stream)
    (print-term (assign-body assign) stream)
    (prin ")" stream))

  (defmethod print-term ((blck block-term) stream)
    (prin "(let (" stream)
    (print-decl (block-decl blck) stream)
    (prin ") " stream)
    (print-term (block-body blck) stream)
    (prin ")" stream))

  (defmethod print-term ((blck call-next-method-term) stream)
    ;;(format stream "(call-next-method)")
    (call-next-method))

  (defmethod print-term ((exp export-directive) stream)
    (prin "(exp: " stream)
    (print (export-spec-name exp))
    (prin ")"))

  (defmethod print-decl ((decl and-decl) stream)
    (mapcar (lambda (x) (print-decl x stream))
	    (and-decl-decls decl)))

  (defmethod print-decl ((decl rec-decl) stream)
    (prin "rec (" stream)
    (print-decl (rec-decl-decl decl) stream)
    (prin ")" stream))

 (defmethod generic-prin ((x term) stream)
    (prin "#<Term: " stream)
    (print-term x stream)
    (prin ">"))

  (defmethod generic-prin ((x decl) stream)
    (prin "#<decl:" stream)
    (print-decl x stream)
    (prin ">"))
  (export translate get-module-translator)

  ;; Test function

  (defun test () 
    (let* ((file (open "test.em"))
	   (forms (read file)))
      (close file)
      (translate forms)))
  (export test)


  ;; end module
  )
