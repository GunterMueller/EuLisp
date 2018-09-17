;; Eulisp Module
;; Author: pete broadbery
;; File: pass-0.em
;; Date: 3/sep/1991
;;
;; Project:
;; Description: 
;; sets up the lexical contour map of the syntax-tree
;; adds simple type info to lambda expressions --- we could just work
;;  this one out.
;;

(defmodule pass-0 
  ((except (scan-args fold) standard)
   list-fns
   scan-args

   abs-syntx
   syntx-utl
   mod-info
   props
   pass

   stop
   )
  ()
  
  (export Module-State-Error)

  (defmethod make-syntactic-properties ((x module-block) lst)
    (format t "Module: initlist: ~a" lst)
    (call-next-method)
    ((setter enclosing-block) (module-body x) x)
    (set-enclosing-block (module-body x) x)
    (set-module-interface x lst)
    )

  (defmethod make-syntactic-properties ((x block-obj) lst)
    (call-next-method)
    (set-closure x))

  (defmethod make-syntactic-properties ((x lambda-term) lst)
    (call-next-method)
    (let ((ids (scan-args 'id-list lst nil)))
      (let ((args (count-nargs ids)))
	((setter lambda-nargs) x args)
	((setter lambda-ids) x (lose-nary ids)))))

  ;; ugh.
  (defun count-nargs (lst)
    (cond ((null lst) (cons () 0))
	  ((atom lst) (cons t 1))
	  (t (let ((x (count-nargs (cdr lst))))
	       (cons (car x)
		     (+ (cdr x) 1))))))

  (defun lose-nary (lst)
    (cond ((null lst) lst)
	  ((atom lst) (list lst))
	  (t (cons (car lst) (lose-nary (cdr lst))))))

  (defmethod make-syntactic-properties ((x rec-decl) lst)
    (call-next-method)
    (set-closure x))

  (defun set-closure (blk)
    (mapcar (lambda (x) 
	      ((setter enclosing-block) x blk)
	      (set-enclosing-block x blk))
	    (internal-blocks blk)))
    
  (defgeneric internal-blocks (obj)
    methods ((((x block-obj))
	      (list (block-body x)))
	     (((x rec-decl))
	      (list (rec-decl-decl x)))))

  (defgeneric set-enclosing-block (obj enclose))

  (defmethod set-enclosing-block ((x syntax-obj) y)
    (mapc (lambda (sub) 
	    ((setter enclosing-block) sub y))
	  (subcomponents x))
    (mapc (lambda (sub) (set-enclosing-block sub y))
	  (subcomponents x)))
  
  ;; we have to setup the declaration term only 
  ;; --- rest is done bt the pass that began on this structure.

  (defmethod set-enclosing-block ((x block-term) y)
    ((setter enclosing-block) (block-decl x) y)
    (set-enclosing-block (block-decl x) y)
    nil)
  
  (defmethod set-enclosing-block ((x rec-decl) y)
    (if (not (eq (enclosing-block (rec-decl-decl x)) x))
	(stop (list x y))
      nil)
    nil)

  (defmethod set-enclosing-block ((x lambda-term) y)
    (if (not (eq (enclosing-block (lambda-body x)) x))
	(stop (list x y))
      nil)
    nil)

  (defmethod set-enclosing-block ((app applic-term) y)
    (call-next-method)
    )

  (defmethod set-enclosing-block ((cnm call-next-method-term) block)
    ((setter applic-fun) cnm (literal 1))
    ((setter applic-args) cnm (list (literal 1)))
    (call-next-method))

  
  ;; pass-1 
  ;; 
  ;; sets the tail flag on the appropriate calls
  ;; links vbles to thier bindings
  ;; sets closure/use flags
  (defconstant annotator (make-compiler-pass 'annotate))
  (defun annotate-tree (tree)
    (annotator tree nil))
  
  (export annotate-tree)

  (defmethod annotator ((cond condition-term) tail-flag)
    ((setter term-tail-call) cond tail-flag)
    (annotator (cond-test cond) nil)
    (annotator (cond-t-part cond)  tail-flag)
    (annotator (cond-f-part cond) tail-flag))

  ;; flow of control...
  (defmethod annotator ((bl block-term) tail-flag)
    ((setter term-tail-call) bl tail-flag)
    (annotator (block-decl bl) nil)
    (annotator (block-body bl) tail-flag))

  (defmethod annotator ((seq sequence) tail-flag)
    ((setter term-tail-call) seq tail-flag)
    (mapcar (lambda (ob)
	      (annotator ob nil))
	    (sequence-body seq))
    (annotator (sequence-end seq) tail-flag))

  (defmethod annotator ((lam lambda-term) tail-flag)
    ((setter term-tail-call) lam tail-flag)
    ((setter lambda-closed-p) (enclosing-lambda lam) t)
    (if nil ;;(inline-lambda lam)
	(annotator (lambda-body lam) tail-flag)
      (annotator (lambda-body lam) t)))

  (deflocal app-count 0)
  (deflocal tail-count 0)
  (defmethod annotator ((app applic-term) tail-flag)
    ((setter term-tail-call) app tail-flag)
    ((setter term-fn-call-loc) (applic-fun app) t)
    (setq app-count (+ app-count 1))
    (if tail-flag 
	(setq tail-count (+ tail-count 1))
      nil)
    (mapcar (lambda (obj)
	      (annotator obj nil))
	    (subcomponents app)))

  (defmethod annotator ((trm term) tail-flag)
    ((setter term-tail-call) trm tail-flag)
    (call-next-method))
  
  (defmethod annotator ((as assignment-term) tail-flag)
    ((setter term-tail-call) as tail-flag)
    ((setter binding-mutable) (assign-var as) t)
    (annotator (assign-var as) nil)
    (annotator (assign-body as) nil))

  ;; Assumes method function is the nearest.
  (defmethod annotator ((cnm call-next-method-term) tail-flag)
      (mapc (lambda (id) (annotator id nil)) 
	    (subcomponents cnm))
      (call-next-method))
  
  (defun make-ident-term (id enclose)
    (let ((xx (ident id)))
      ((setter enclosing-block) xx enclose)
      xx))

  ;; locals hacking

  (defmethod annotator ((id ident-term) tail-flag)
    ((setter term-tail-call) id tail-flag)
    (let ((id-decl (lookup-id (enclosing-block id)
			 (term-id id) 
			 ())))
      ((setter ident-defblock) id id-decl))
    (if (not (term-fn-call-loc id)) 
	((setter binding-as-arg) (ident-decl id) t)
      nil))
    

  (defmethod annotator ((cnm call-next-method-term) tail-flag)
    (let ((meth-lambda (find-method-lambda cnm)))
      ((setter applic-fun) cnm (mk-special-term 'call-next-method-internal nil))
      ((setter applic-args) cnm
       (mapcar ident (mapcar lambda-id-name
			     (lambda-ids meth-lambda))))
      ((setter enclosing-block) (applic-fun cnm) (enclosing-block cnm))
      (mapcar (lambda (x) ((setter enclosing-block) x (enclosing-block cnm)))
	      (applic-args cnm) )
      (call-next-method)))

  (defmethod annotator ((special special-term) tail-flag)
    (call-next-method)
    (cond ((eq (special-term-name special) 'add-property)
	   (add-defn-prop (ident-decl (car (special-term-objects special)))
			  (car (special-term-data special))
			  (cadr (special-term-data special))))
	  (t nil)))

  (defgeneric find-method-lambda (blck)
    ;; what about inlines...
    methods ((((x lambda-term)) x) 
	     (((x block-term)) 
	      (find-method-lambda
	       (enclosing-block x)))
	     (((x term))
	      (find-method-lambda (enclosing-block x)))))

  (defgeneric lookup-id (object name closed-flag))

  (defmethod lookup-id ((obj lambda-term) name closed)
    (or (detect (lambda (x)
		  ((setter enclosing-block) x obj)
		  (if (eq name (lambda-id-name x))
		      (progn ((setter binding-used) x t)
			     (if closed
				 ((setter binding-closed) x t)
			       ())
			     (cons x obj))
		    ()))
		(lambda-ids obj))
	(let ((bind (lookup-id (enclosing-block obj) name
			       (or closed t)))) ;; should be (not (inline-lambda obj))
	  (if (and (not (module-p (cdr bind)))
		   (not (null (cdr bind)))
		   (not (module-definition-p (cdr bind)))
		   (binding-closed (car bind)))
	      ((setter lambda-closed-p) obj t)
	    ())
	  bind)))

  (defmethod lookup-id ((obj block-term) name closed)
    (or (detect (lambda (x)
		  (if (eq name (defn-ide x))
		      (progn ((setter binding-used) x t)
			     (if closed
				 ((setter binding-closed) x t)
			       ())
			     (cons x obj))
		    ()))
		(block-definition-names (block-decl obj)))
	(lookup-id (enclosing-block obj) name closed)))

  (defmethod lookup-id ((obj rec-decl) name closed)
    (or (detect (lambda (x)
		  (if (eq name (defn-ide x))
		      (progn ((setter binding-used) x t)
			     (if closed
				 ((setter binding-closed) x t)
			       ())
			     (cons x obj))
		    ()))
		 (block-definition-names (rec-decl-decl obj)))
	(lookup-id (enclosing-block obj) name closed)))

  (defun block-definition-names (b)
    (find-decls b))
  

  (defmethod lookup-id ((obj module-block) name closed)
    (or (detect (lambda (x)
		  (if (eq name (defn-ide x))
		      (cons x obj)
		    nil))
		(module-declarations obj))
	(let ((xx (find-module-import obj name)))
	  (if (null xx)
	      nil
	    (progn (add-dependency obj (cdr xx))
		   (cons (cdr xx) obj))))
	(if (eq name 'nil)
	    (cons nil nil)
	  ())
	(if (eq name t)
	    (cons t nil)
	  ())
	(cerror "unknown-binding: ~a " Module-State-Error
		'msg "**Unknown binding: ~a~%"
		'values (list name))
	;; Error handler comes back here if it wants
	(cons (make-error-term) nil)))

  (defmethod print-decl ((defn local-definition) stream)
    (prin "(" stream)
    (cond ((binding-closed defn)
	   (format  stream "{~a}" (defn-ide defn)))
	  ((binding-used defn)
	   (prin (defn-ide defn) stream))
	  (t (format stream "-~a-" (defn-ide defn))))
    (prin " " stream)
    (print-term (defn-body defn) stream)
    (prin ")" stream))
  

  (defmethod generic-prin ((lid lambda-id) stream)
    (cond ((binding-closed lid)
	   (format stream "{~a}" (lambda-id-name lid)))
	  ((binding-used lid)
	   (prin (lambda-id-name lid) stream))
	  (t (format stream "-~a-" (lambda-id-name lid)))))
    
  ;; end module
)
  


