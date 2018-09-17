;; Eulisp Module
;; Author: pab
;; File: combine.em
;; Date: Tue Mar 31 14:40:14 1992
;;
;; Project:
;; Description: 
;;   Function that takes a bunch of modules and combines 
;;   them in to a single file.
;;

(defmodule mycombine
  ((except (fold) standard)
   (only (pair) standard0)
   list-fns
   byte-stream

   comp-defn
   instruct
   assem
   link2
   module-operators
   ;;(only (all-registered-modules) boot)
   (only (boot-module-list load-bytecodes) bci)
   )
  ()

  ;; conditions
  (defcondition Link-Error ())

  ;; constant located modules --- this is reloaded every time round. 
  ;; if you add modules, you may have to re-combine the bytecodes.

  (defconstant *global-module-locs* 
    (boot-module-list))

  ;; read some .sc files
  (defun get-mods (lst)
    (mapcar (lambda (name)
	      (read-object sc-compile-unit (sc-file-name name) (search-path)
			   ))
	    lst))

  ;; map is a list of (n-statics (list of old-static/new-static pairs) new-statics)
  ;; mod-data is a list giving the new locations of bindings. It is updated in place
      
  (defun combine-modules-aux (name context mod-list lst)
    (let* ((mods (get-mods lst))
	   (map (compute-static-map (mapcar sc-statics mods)))
	   (mod-data (get-mod-data mods mod-list)))
      (let* ((codes (make-code mods lst (cadr map) mod-data))
	     (combined-code  (reify-code-list (make-linkable-code codes (mapcar sc-length mods))))
	     (tslots (fold + (mapcar sc-nslots mods) 0))
	     (xx (setq mods nil))
	     (yy (setq codes nil)))
	(write-combined-code name 
			     combined-code
			     context
			     mod-list
			     (car map)
			     (+ tslots (car map)))
	(write-static-file name
			   (caddr map)))))

  (defun make-code (mods names maps mod-data)
    (if (null mods) nil
      (cons (munge-module-code (car maps) (sc-code (car mods)) (list (car names)) mod-data)
	    (make-code (cdr mods) (cdr names) (cdr maps) mod-data))))

  (defun combine-modules (name lst)
    (combine-modules-aux name
			 32 ;; FIRST_USER_CODE
			 (mk-module-data *global-module-locs* nil)
			 lst))

  (deflocal *mod-path-list* (make-search-path "FEEL_LOAD_PATH" #\: "."))

  (defun combine-mods-with-file (name file lst)
    (let ((stream (path-open *mod-path-list* file)))
      (let ((internal-mods (read stream)))
	(close stream)
	(combine-modules-aux name
			     32 ;; FIRST_USER_CODE
			     internal-mods
			     lst))))
  
  (defun combine-user-modules (name lst)
    (combine-modules name (append *std-modules* lst)))

  (defun combine-user-modules-with-file (name file lst)
    (combine-mods-with-file name file (append *std-modules* lst)))
  (export combine-modules combine-mods-with-file combine-user-modules)

  (defun load-modules (list)
    (combine-with-current-state 'temp-file list)
    (load-bytecodes (convert 'temp-file <string>)))

  (defun load-module (name)
    (mapcar (lambda (a) (print a) (load-modules (list a)))
	    (reverse (get-module-deps name))))

  (defun combine-with-current-state (name lst)
    (combine-modules-aux name
			 (next-context (all-registered-modules))
			 (mk-module-data *global-module-locs* (all-registered-modules))
			 lst))

  (defun make-boot-code (file name)
    (combine-mods-with-file name file *std-modules*))

  (defconstant link-version (local-var 'bsd))

  (deflocal *std-modules* '(boot init  macros0 extras0
			    defs lock  standard0))
  

  (defconstant *std-search-path* 
    ".:/net/brad/denton_export/denton/You/NewYou/Compiler:/net//brad/denton_export/denton/You/NewYou/Interfaces")

  (defconstant *generic-search-path* 
    ".:/net/brad/denton_export/denton/You/NewYou/Compiler/Generic:/net//brad/denton_export/denton/You/NewYou/Interfaces/Generic")

  (defun search-path ()
    (let ((path (cond ((eq (link-version) 'generic)
		       (format nil "~a:~a" *generic-search-path*  *std-search-path*))
		      (t *std-search-path*))))
      (make-search-path "FEEL_OBJS_PATH" #\: path)))
    

  (defun module-loaded-p (name)
    (or (memq name (mapcar module-name (cdr *global-module-locs*)))
	(memq name (mapcar (lambda (x) (module-name (car x))) (all-registered-modules)))))

  (defun write-combined-code (name code context loaded-mods nstatics tslots)
    (let* ((file (open (encapsulated-byte-file-name name) 'output t))
	   (stream (make-writer-stream (lambda (s x) 
					 (write-nl x file)
					 s))))
      (print "ASCIIBYTES" file)
      (write-nl (+ 1 tslots) file)
      (write-nl (cadr code) file)
      (write-bytes code stream context loaded-mods nstatics)
      (close file)))

  (defun write-bytes (code stream context loaded-mods nstatics)
    (let ((get-non-local-id (lambda (obj)
			      (let ((mod (assq (car obj) loaded-mods)))
				(if (null mod)
				    (error "Module not loaded" Link-Error 'error-value (car obj))
				  (list (cadr mod)
					(get-binding-location (cddr mod) (cdr obj))))))))
      (link-vector-to-stream (car code)
			     (cons (mk-local-id-mker context
						     nstatics (lambda (x y) nil))
				   get-non-local-id)
			     (lambda (x) x)
			     stream)))

  
  ;; could be a list or a module...
  (defun get-binding-location (thing name)
    (if (consp (car thing))
	(let ((xx (assq name thing)))
	  (if (null xx) (error "Binding doesn't exist" Link-Error 'error-value (cons name thing))
	    (cdr xx)))
      (module-binding-location (car thing) name)))

  (defun get-mod-data (lst loaded-lst)
    (let ((data-tab (make-table eq))
	  (mod-tab (make-table eq)))
      (print "get mod data")
      (mapc (lambda (sc)
	      (if (atom (sc-names sc))
		  ((setter table-ref) mod-tab (sc-names sc) t)
		(mapc (lambda (x)
			((setter table-ref) mod-tab x t))
		      (sc-names sc))))
	    lst)
      (print 'done)
      (list data-tab mod-tab loaded-lst)))
    
  (defun mk-module-data (global-lst loaded-lst)
    (let ((c (mk-counter 0)))
      (nconc (mapcar (lambda (x)
			(if (null x)
			    (list '%%-no-module-%% (c) nil)
			  (list (module-name x) (c) x)))
		     global-lst)
	      (mapcar (lambda (x) (list (module-name (car x)) (cdr x) (car x)))
		      loaded-lst))))

  (defun next-context (loaded-lst)
    (if (null loaded-lst)
	(list-length *global-module-locs*)
      (+ (fold (lambda (md max)
		 (setq xx md)
		 (if (> (cdr md) max)
		     (cdr md)
		   max))
	       loaded-lst
	       0)
	 1)))
    
  (deflocal xx ())

  (defun make-linkable-code (lists lengths)
    (let ((init-stream (make-simple-stream))
	  (code-stream (make-simple-stream)))
      (labels ((add-code 
		(lists lengths)
		(if (null lists)
		    nil
		  (let ((lab (make-refed-label))
			(rlab (make-refed-label)))
		    (write-stream-list init-stream 
				       (list (push-label (list rlab))
					     (push-fixnum '(0))
					     (push-fixnum '(0)) ;; env
					     (branch (list lab))
					     (i-label (list rlab))
					     (drop '(1))))
		    (write-stream-list code-stream 
				       (list (i-label (list lab))
					     (make-inline-code (car lengths) (car lists))))
		    (add-code (cdr lists) (cdr lengths))))))
	      (add-code lists lengths)
	      (write-stream-list init-stream (cons (return nil)
						   (convert code-stream pair)))
	      (convert init-stream pair))))

	  
  (defun compute-static-map (mod-lst)
    (labels
     ;; gives an a-list mapping orig->combination + new combination-list
     ((extend-tab (static-list static-id mapping new-lst)
       (if (null static-list)
	   (list mapping new-lst)
	 (let ((lstval (assq (car static-list) (cdr new-lst))))
	   (if lstval
	       (extend-tab (cdr static-list) (+ static-id 1)
			   (cons (cons static-id (cdr lstval)) 
				 mapping)
			   new-lst)
	     (extend-tab (cdr static-list) (+ static-id 1)
			 (cons (cons static-id (car new-lst))
			       mapping)
			 (cons (+ (car new-lst) 1)
			       (cons (cons (car static-list)
					   (car new-lst))
				     (cdr new-lst))))))))
      (make-static-maps (mod-lst map-lst so-far)
       (if (null mod-lst)
	   (list (car map-lst) (reverse so-far) (nreverse (mapcar car (cdr map-lst))))
	 (let ((new-junk (extend-tab (car mod-lst) 0 nil map-lst)))
	   (make-static-maps (cdr mod-lst)
			     (cadr new-junk)
			     (cons (car new-junk) so-far))))))
     (make-static-maps mod-lst (cons 1 nil) nil)))

  ;; in-place transformation of a module

  (defun munge-module-code (map code mod-names mod-data)
    (labels ((munge-aux (args code)
	      (cond ((null args)
		     (if (null code)
			 nil
		       (munge-aux (car code) (cdr code))))
		    (t (munge-instruction (car args))
		       (munge-aux (cdr args) code))))
	     (munge-instruction (arg)
	      (cond ((atom arg) arg)
		    ((eq (car arg) (the-static-handle))
		     ((setter cdr) arg
		      (cdr (assoc (cdr arg) map =))))
		    ((eq (car arg) (the-link-handle))
		     (cond ((eq (cadr arg) (the-local-handle))
			    ((setter cdr) arg
			     (cons (the-local-handle)
				   (get-name (car mod-names) (cddr arg) mod-data)))) ;; XXX
			   ((assq (cadr arg) (caddr mod-data))
			    nil)
			   (t
			    ((setter cdr) arg
			     (cons (the-local-handle)
				   (get-name (cadr arg) (cddr arg) mod-data))))))
		    ((eq (car arg) (the-local-handle))
		     ((setter cdr) arg
		      (get-name (car mod-names) (cdr arg) mod-data))))))
	    (print "Munge starts...")
	    (munge-aux (car code) (cdr code)))
    code)
  
  ;; mod-data is an a-list:
  ;; (name . ((internal-name . real-name) *))
  
  (defun get-name (mod-name name mod-data)
    (let ((sym-data (table-ref (car mod-data) name)))
      (cond ((and (null sym-data)
		  (table-ref (cadr mod-data) mod-name))
	     (let ((newsym (gensym)))
	       ((setter table-ref) (car mod-data)
		name (list (cons mod-name newsym)))
	       newsym))
	    ((null sym-data)
	     (error "Unknown module" Link-Error 'error-value mod-name))
	    (t
	     (let ((xx (assq mod-name sym-data)))
	       (cond ((not (null xx))
		      (cdr xx))
		     ((table-ref (cadr mod-data) mod-name)
		      (let ((newsym (gensym)))
			(nconc sym-data
			       (list (cons mod-name newsym)))
			newsym))
		     (t (error "Unknown module 2" Link-Error 'error-value mod-name))))))))

  '(defun get-name (mod-name name mod-data)
    ;; hacked till I get multiple modules right
    (let ((lst (assq mod-name mod-data)))
      (if (null lst)
	  (error "Unresolved reference" Link-Error 'error-value mod-name)
	(let ((xx (assq name (cdr lst))))
	  (if (not (null xx))
	      (cdr xx)
	    (let ((sym (gensym)))
	      (nconc lst (list (cons name sym)))
	      ;;(format t "~a-->~a~%" name sym)
	      sym))))))
	    
  ;; produces: (nstatics statics code)

  (defun make-call-code (call-label)
    (let ((ret-lab (make-refed-label)))
      (list (push-fixnum (list 0))
	    (push-label (list ret-lab))
	    (branch (list call-label))
	    (i-label (list ret-lab))
	    (drop (list 1)))))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Spitting out .ebc and .est files
  
  (defun write-static-file (name statics)
    (let ((static-file (open (encapsulated-static-file-name name) 'output t)))
      (write-nl (+ 1 (list-length statics)) static-file)
      (write-nl nil static-file)
      (mapc (lambda (x) (write-nl x static-file))
	    statics)
      (close static-file)))

  (defun write-nl (x stream)
    (write x stream)
    (newline stream))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Grabbing list of dependencies


  (defun get-module-deps (name)
    (walk-tree (mk-depend-tree name t) nil))
  
  (defun get-all-deps (name)
    (walk-tree (mk-depend-tree name nil) nil))

  ;; return list of modules needed to load given name

  (defun mk-depend-tree (name all)
    (let ((find-tree (mk-finder)))
      (setq xx find-tree)
      (labels ((make-sub-deps (lst res)
	        (cond ((null lst) (nreverse res))
		      ((and all
			    (module-loaded-p (car lst)))
		       (make-sub-deps (cdr lst) res))
		      (t (let ((try1 (find-tree (car lst))))
			   (if try1
			       (make-sub-deps (cdr lst) 
					      (cons try1 res))
			     (let ((tree (make-dep-tree (car lst))))
			       (make-sub-deps (cdr lst)
					      (cons tree res))))))))
	       (make-dep-tree (name)
		(let* ((deps (read-dependencies name))
		       (res (cons name (make-sub-deps deps nil))))
		  ((setter find-tree) name res)
		  res)))
	      (make-dep-tree name ))))

  (defun walk-tree (tree so-far)
    (cons (car tree)
	  (fold (lambda (tree lst)
		  (if (memq (car tree) lst)
		      lst
		    (walk-tree tree lst)))
		(cdr tree)
		so-far)))
		  
  (defconstant *default-interface-path*
    ".:/net/brad/denton_export/denton/You/NewYou/Interfaces:/net/brad/denton_export/denton/You/Interfaces")

  (defconstant *interface-path* (make-search-path 
				 "FEEL_INTF_PATH" #\:
				 *default-interface-path*))

  (defconstant cached-deps (mk-finder))
  
  (defun read-dependencies (name)
    (let ((xx (cached-deps name)))
      (if xx (cdr xx)
	(let ((deps (real-read-dependencies name)))
	  ((setter cached-deps) name (cons nil deps))
	  deps))))
	
  (defun real-read-dependencies (name)
    (let ((file (path-open *interface-path*
			   (interface-file-name name))))
      (let ((deps (assq 'dependencies (read file))))
	(close file)
	(if (cdr deps)
	    (cadr deps)
	  nil))))

  ;; end module
)

