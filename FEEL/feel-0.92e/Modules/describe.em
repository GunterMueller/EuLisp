;;; describe various objects
;;; version 1.0
;;; RJB May 92

(defmodule describe

  ((rename ((function-lambda-list fll)) eulisp0)
   fn-docs)
  ()

  ; first fix fll
  (defgeneric function-lambda-list (fun))

  (defmethod function-lambda-list ((f <object>))
    (fll f))

  (defmethod function-lambda-list ((gf <generic-function>))
    (let ((meths (generic-function-methods gf)))
      (if (atom meths)
	  "unknown"
	  (mkargs (length (method-signature (car meths)))))))

  (defmethod function-lambda-list ((c <continuation>))
    '(a))

  (defun mkargs (n)
    (if (= n 0) ()
	(cons (vector-ref #(@ a b c d e f g h i j k l m n o
			      p q r s t u v w x y z) n)
	      (mkargs (- n 1)))))

  (defgeneric describe (obj))

  (defmethod describe ((cl <class>))
    (format t "The class ~a is an instance of ~a~%"
	    cl (class-of cl))
    (format t "class precedence list: ~a~%"
	    (class-precedence-list cl))
    (format t "direct superclasses:   ~a~%"
	    (class-direct-superclasses cl))
    (format t "direct subclasses:     ~a~%"
	    (class-direct-subclasses cl))
    (when (class-slot-descriptions cl)
      (format t "direct slots~%------------~%")
      (mapcar describe-slot
	      (class-slot-descriptions cl)))
;    (when (class-constructors cl)
;      (format t "------------~%")
;      (format t "class constructors:~%")
;      (mapcar print (class-constructors cl)))
    t)

  (defmethod describe ((inst <object>))
    (format t "~a is an instance of ~a~%"
	    inst (class-of inst))
    (describe-slot-values (class-slot-descriptions (class-of inst))
			  inst)
    t)    

  (defun describe-slot (sl)
    (format t "slot name: ~a~%"
	    (slot-description-name sl))
    (format t "position:  ~a~%"
	    (slot-description-position sl))
    (format t "initarg:  ~a~%"
	    (slot-description-initarg sl)))

  (defun describe-slot-values (slotds inst)
    (when slotds
	  (let ((name (slot-description-name (car slotds))))
	    (format t "slot ~a: ~a~%"
		    name
		    (protected-slot-value inst name))
	    (describe-slot-values (cdr slotds) inst))))

  (defun protected-slot-value (inst name)
    (let/cc leave
            (with-handler
             (lambda (cond cont)
               (leave "**Error---unreadable slot")
	       )
             ((slot-description-slot-reader (find-slot-description (class-of inst) name)) inst))))

  (defmethod describe ((f <function>))
    (call-next-method)
    (format t "argument list: ~a~%" (function-lambda-list f))
    t)

  (defmethod describe ((gf <generic-function>))
    (call-next-method)
    (format t "methods signatures:~%")
    (mapcar (lambda (m)
	      (format t "~a~%" (method-signature m)))
	    (generic-function-methods gf))
    t)

  (defmethod describe ((m <method>))
    (call-next-method)
;    (format t "generic function: ~a~%" (method-generic-function m))      
    (format t "signature: ~a~%" (method-signature m))
    t)

  (defmethod describe ((th <thread>))
    (call-next-method)
    (format t "thread state: ~a~%" (thread-state th))
    t)

  (defmethod describe ((sl <slot-description>))
    (call-next-method)
    (describe-slot sl)
    t)

  ; semaphores

  ; now export 
  (export describe)
  
  ;; show -- simple non-generic describe
  (defun show (x) 
    (format t "~a [~a]~%" x (class-of x))
    (mapc (lambda (s)
	    (format t "~a: ~a~%" (slot-description-name s)
		    ((slot-description-slot-reader s) x)))
	  (class-slot-descriptions (class-of x))))

  (export show)

  (defun class-slots (cl)
    (mapcar slot-description-name
	    (class-slot-descriptions cl)))

  (defun class-hierarchy slots?
    (do-class-hierarchy (list <object>) 0 slots?))
  
  (defun class-hierarchy-1 ()
    (do-class-hierarchy (list <object>) 0 nil))

  (defun do-class-hierarchy (objlist depth flag)
    (print-indent (car objlist) depth)
    (if (class-slots (car objlist))
	(when flag
	  (prin "slots: ")
	  (print-indent (class-slots (car objlist)) depth))
        nil)
    (if (class-direct-subclasses (car objlist))
	(do-class-hierarchy (class-direct-subclasses (car objlist))
			    (+ depth 4) flag)
        nil)
    (if (cdr objlist)
	(do-class-hierarchy (cdr objlist) depth flag)
        nil))

  (defun print-indent (obj depth)
    (if (= depth 0)
	(print obj)
        (progn
	  (prin " ")
	  (print-indent obj (- depth 1)))))

  (export class-hierarchy class-hierarchy-1)

  ;; Additional method for printing bytefunctions
  
  (defconstant bf-slots (class-slot-descriptions <bytefunction>))

  (defconstant bf-readers (mapcar slot-description-slot-reader bf-slots))

  (defconstant bf-names (mapcar slot-description-name bf-slots))

  ; avoid printing globals slot
  (defmethod describe ((bf <bytefunction>))
    (format t "~a is an instance of ~a~%" bf (class-of bf))
    (map (lambda (n r)
	   (format t "slot ~s: ~s~%" n
		   (if (eq n 'globals) #(...) (r bf))))
	 bf-names bf-readers)
    t)

  (defmethod describe ((ebf <extended-bytefunction>))
    (format t "~a is an instance of ~a~%" ebf (class-of ebf))
    (format t "defined in: ~a.em~%" (car (extended-bytefunction-info ebf)))
    (print "documentation:")
    (print (bytefunction-info ebf))
    t)

  (defmethod generic-prin ((ebf <extended-bytefunction>) stream)
    (format stream "#<~a: ~a[~a.em] ~u>" 
	    (symbol-unbraced-name (class-name (class-of ebf) ))
	    (bytefunction-name ebf)
	    (bytefunction-location ebf)
	    ebf))
  
  (defun bytefunction-location (ebf)
    (car (extended-bytefunction-info ebf)))

  (defun bytefunction-name (ebf)
    (let ((info (extended-bytefunction-info ebf)))
      (let ((file (find-doc-file (car info))))
	(if (null file) "{unknown}"
	  (let ((name (read-name file (cdr info))))
	    (close file)
	    name)))))

  (defun bytefunction-info (ebf)
    (let ((info (extended-bytefunction-info ebf)))
      (let ((file (find-doc-file (car info))))
	(if (null file) ""
	  (let ((junk (read-entry file (cdr info))))
	    (close file)
	    (cdr junk))))))

  (defun find-doc-file (name)
    (let/cc out
	    (with-handler (lambda (cond cont)
			    (out nil))
			  (path-open (make-search-path  "FEEL_DOCS_PATH"
							#\: ".")
				     (format nil "~a.doc" name)
				     'eos-action (lambda (s) ".")))))
    
  (defun make-search-path (shell-var separator default)
    (let ((sp (or (getenv shell-var) default))
	  (sp-length 0))
      (if (null sp)
	  (list ".")
	(labels (
		 (dissect-path (index previous-index index-pairs)
		    (if (= index sp-length)
			(cons (cons previous-index (- index 1)) index-pairs)
		      (if (equal (string-ref sp index) separator)
			  (dissect-path
			   (+ index 1)
			   (+ index 1)
			   (cons (cons previous-index (- index 1)) index-pairs))
			(dissect-path (+ index 1) previous-index index-pairs)))))
		(setq sp-length (string-length sp))
		(reverse
		 (mapcar (lambda (start-finish)
			   (substring sp (car start-finish) (cdr start-finish)))
			 (dissect-path 0 0 ())))))))

  (defun path-open (pathlist name . options)
    (let/cc succeed
	    (mapc (lambda (path)
		    (let/cc fail
			    (with-handler (lambda (a b) (fail ()))
					  (succeed (apply open (format nil "~a/~a" path name) options)))))
		  pathlist)
	    (error
	     (format nil "path-open: cannot open stream for (~a) ~a" pathlist name)
	     <Internal-Error>)
	    nil))


)
