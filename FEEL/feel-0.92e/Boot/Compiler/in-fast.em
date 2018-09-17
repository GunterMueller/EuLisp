;; Eulisp Module
;; Author: pab
;; File: in-fast.em
;; Date: Mon Nov 23 18:24:30 1992
;;
;; Project:
;; Description: 
;;   Like combine, but quick and dirty

(defmodule in-fast
  (standard0
   list-fns

   (only (all-registered-modules) boot)
   (only (boot-module-list load-bytecodes) bci)
   module-operators
   byte-stream 
   comp-utl
   bci
   root
   )
  ()

  (export load-module set-debug)

  (defclass Link-Error (<condition>)
    ()
    metaclass <condition-class>)

  (defun load-module (name)
    (module-name (read-fastbytes name)))

  (deflocal aa ())
  (deflocal debug ())
  (defun set-debug (x)
    (setq debug x))

  (defun read-fastbytes (name)
    (let ((file (path-open *objs-path-list* (fast-file-name name) 'input t)))
      (let* ((deps (read file))
	     (loaded-modules (load-dependencies deps (find-loaded-modules))))
	(setq aa loaded-modules)
	(let* ((nslots (read file))
	       (code-len (read file))
	       (statics (read file)))
	  (format t "~a: ~a bytes ~a statics~%" name code-len  (+ (list-length statics) nslots))
	  (add-code-vector1 (convert (read-nbytes code-len (make-simple-stream) file loaded-modules) pair)
			    code-len
			    statics
			    (+  1 nslots (list-length statics)))))))
  
  (defun add-code-vector1 (a b c d)
    (if debug
	(progn ;;(stop (list a b c d))
	       (set-bc-global 3 t))
      ())
    (add-code-vector a b c d))

  (defun load-dependencies (deps loaded)
    (cond ((null deps) loaded)
	  ((assq (car deps) loaded)
	   (load-dependencies (cdr deps) loaded))
	  (t (read-fastbytes (car deps))
	     (load-dependencies (cdr deps)
				(find-loaded-modules)))))

  (defun read-nbytes (n stream file loaded-mods)
    ;;(prin ".")
    (if (zerop n) 
	(progn (close file) 
	       stream)
      (let ((next (read file)))
	(cond ((numberp next)
	       (read-nbytes (- n 1)
		(write-stream stream next)
		file
		loaded-mods))
	      ((consp next)
	       (let ((inline-bytes (resolve-ref loaded-mods next)))
		 (read-nbytes (- n (list-length inline-bytes))
			      (write-stream-list stream inline-bytes)
			      file
			      loaded-mods)))))))
    
  ;; making 4 bytes from integers.

  (defun int2bytes (x)
    (let ((sign (< x 0))
	  (val (abs x)))
      (let* ((v1 (/ val 256))
	     (v2 (/ v1 256))
	     (v3 (/ v2 256)))
	(list (modulo v2 256)
	      (modulo v1 256)
	      (modulo val 256)
	      (if sign 1 0)))))

  (defconstant *name-table* (make-table eq))
  
  (defun resolve-ref (loaded-mods obj)
    (let ((val (table-ref *name-table* (cdr obj))))
      (if (and val (eq (car val) (car obj)))
	  (copy-list (cdr val))
	(let ((new (simple-resolve-ref loaded-mods obj)))
	  ((setter table-ref) *name-table* (cdr obj) (cons (car obj) new))
	  (copy-list new)))))

  (defun simple-resolve-ref (loaded-mods obj)
    (let ((mod (assq (car obj) loaded-mods)))
      (if (null mod)
	  (error "Module not found" Link-Error 'error-value obj)
	(let ((bind (module-binding-location (caddr mod) (cdr obj)))
	      (posn (cadr mod)))
	  (if (null bind)
	      (error "Binding not found" Link-Error 'error-value obj)
	    (progn ;;(format t "~a->(~a ~a)~%" obj posn bind)
		   (nconc (int2bytes posn)
			  (int2bytes bind))))))))


  
  ;; loaded module list:
  ;; (name posn module).
  
  (defun find-loaded-modules ()
    (let ((c 31)
	  (last nil))
      (append (mapcar (lambda (new-mod) 
			(list (module-name (car new-mod))
			      (if (eq (cdr new-mod) last) c
				(progn (setq last (cdr new-mod))
				       (setq c (+ c 1))))
			      (car new-mod)))
		      (reverse (all-registered-modules)))
	    (make-loaded-list (boot-module-list)))))

  (defun make-loaded-list (modules)
    (labels ((add-module (lst count)
			 (cond ((null lst) nil)
			       ((null (car lst)) 
				(cons (list '%-no-module-% count nil)
				      (add-module (cdr lst) (+ 1 count))))
			       (t (cons (list (module-name (car lst)) count (car lst))
					(add-module (cdr lst) (+ 1 count)))))))
	    (add-module modules 0)))
  
  (deflocal *path-list* (list (cons ".fm" load-module) (cons ".em" dynamic-load-module)))

  (deflocal *objs-path-list* (make-search-path "FEEL_OBJS_PATH" #\: "."))

  (defun load-any-module (name)
    (load-aux name *path-list* *objs-path-list*))

  (defun load-aux (name ext-list path-list)
    (if (null ext-list) nil
      (let ((file (safe-path-test path-list 
				  (format nil "~a~a" name (caar ext-list)) )))
	(if file
	    ((cdar ext-list) name)
	  (load-aux name (cdr ext-list) path-list)))))
  
  (defun safe-path-test (path name)
    (let/cc escape
	    (with-handler (lambda (cond cont)
			    (escape nil))
			  (let ((file (path-open path name)))
			    (close file)
			    t))))
  (set-eum-function load-any-module)
  ;; end module
  )
