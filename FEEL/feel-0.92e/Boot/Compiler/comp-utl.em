;; Eulisp Module
;; Author: pete broadbery
;; File: comp-utils.em
;; Date: 1/sep/1991
;;
;; Project:
;; Description: 
;;   sundry compiler utliities...
;;

(defmodule comp-utl
  (standard0
   list-fns
   module-operators
   )
  ()
  
  (defclass read-error (<condition>)
    ()
    metaclass <condition-class>)
  (defclass cannot-open-path (<condition>)
    ()
    metaclass <condition-class>)
    
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
	     cannot-open-path)
	    nil))

  (export make-search-path path-open)
  
  ;; macro expansion. 
  ;; low-level
  
  (defun get-expander (module-name name)
    (let ((module (get-module module-name)))
      (dynamic-access module name)))

  (defun interface-file-name (x) (format nil "~a.i" x))

  (defun get-module-stream (x) 
    (open (format nil "~a.em" x)))
  
  (defun fast-file-name (x)
    (format nil "~a.fm" x))

  (export get-expander interface-file-name get-module-stream fast-file-name)

  ;; bytecode file-names

  (defun bytecode-file-name (x) 
    (format nil "~a.bc" x))

  (defun encapsulated-byte-file-name (x)
     (format nil "~a.ebc" x))

  (defun encapsulated-static-file-name (x)
     (format nil "~a.est" x))

  (defun sc-file-name (x)
     (format nil "~a.sc" x))

  (export bytecode-file-name encapsulated-byte-file-name
	  encapsulated-static-file-name
	  sc-file-name)

  ;; Reading and writing files. 
  ;; both cute hacks.

  (defun write-object (x file-name . path)
    (let ((xx (path-open (if path (car path) '("./")) file-name 'output  t)))
      (unwind-protect
	  (safe-write (fold (lambda (slot lst)
			      (cons (car (slot-description-initargs slot))
				    (cons (slot-value x (slot-description-name slot))
					  lst)))
			    (class-slot-descriptions (class-of x))
			    nil)
		      xx)
	(close xx)
	)))
    
  (defun read-object (class file-name . path)
    (let ((file (path-open  (if path (car path) '("./")) file-name)))
      (unwind-protect (apply make-instance class 
			     (safe-read file))
	(close file))))
  
  (export read-object write-object)
  
  (defgeneric safe-write (object stream))

  (defmethod safe-write (object stream)
    (write object stream))
  
  (defun safe-read (stream)
    (read stream))

    
  ;; end module
  )
(defun safe-read (stream)
    (labels ((get-list (stream last-pair)
		       (let ((next (safe-read stream)))
			 (cond ((eq next list-end-symbol)
				nil)
			       ((eq next list-improper-symbol)
				(let* ((next-1 (safe-read stream))
				       (next-2 (safe-read stream)))
				  (if (not (eq next-2 list-end-symbol))
				      (error "Misplaced dot causing error" read-error)
				    ((setter cdr) last-pair next-1))))
			       (t (let ((new-last (cons next nil)))
				    ((setter cdr) last-pair new-last)
				    (get-list stream new-last))))))
	     (start-list (stream)
			 (let ((next (read stream)))
			   (cond ((eq next list-start-symbol)
				  (let ((first-pair (cons (start-list stream) nil)))
				    (get-list stream first-pair)
				    first-pair))
				 ((eq next list-end-symbol)
				  (let ((first-pair (cons nil nil)))
				    (get-list stream first-pair)
				    first-pair))
				 ((eq next list-improper-symbol)
				  (error "improper list" read-error))
				 (t (let ((first-pair (cons next nil)))
				      (get-list stream first-pair)
				      first-pair))))))
	    (let ((obj (read stream)))
	      (if (not (eq obj list-start-symbol))
		  obj
		(start-list stream)))))

  (defun list-too-long-p (x) t) 

  (defun space (stream) 
    (prin " " stream))

;  (defmethod safe-write ((lst <pair>) stream)
  (defmethod safe-write ((lst <cons>) stream)
    (labels ((do-safe-write (lst stream)
			    (cond ((null lst)
				   (write list-end-symbol stream)
				   (space stream))
				  ((atom lst)
				   (write list-improper-symbol stream)
				   (space stream)
				   (safe-write lst stream)
				   (space stream)
				   (write list-end-symbol stream)
				   (space stream))
				  (t
				   (safe-write (car lst) stream)
				   (space stream)
				   (do-safe-write (cdr lst) stream)))))
	    (if (list-too-long-p lst)
		(progn (write list-start-symbol stream)
		       (space stream)
		       (do-safe-write lst stream))
	      (write stream lst))))

  ;; read hacks for big lists
  ;; Note: we cannot say 'foo for obvious reasons
  (defconstant list-start-symbol (make-symbol "%_List_start_%"))
  (defconstant list-end-symbol (make-symbol "%_List_end_%"))
  (defconstant list-improper-symbol (make-symbol "%_dotty_thing_%"))

