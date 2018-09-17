;; Eulisp Module
;; Author: pab
;; File: link2.em
;; Date: Fri Apr 24 11:29:55 1992
;;
;; Project:
;; Description: 
;;   Short linker for self-install data

(defmodule link2
  (standard0
   list-fns
   
   comp-defn
   byte-stream
   )
  ()

  (deflocal xx nil)

  (defun link-vector (lst istate static-link)
    (let ((out (make-simple-stream)))
      (convert (link-vector-to-stream lst istate static-link out)
	       pair)))

  (defun link-vector-to-stream (lst istate static-link stream)
    (labels ((link-stuff (i lst stream)
			 (if (null i)
			     (if (null lst)
				 stream
			       (link-stuff (car lst) (cdr lst) stream))
			   (progn;;(setq xx i)
			     (link-stuff (cdr i) lst 
					 (write-stream-list stream (link-object (car i)))))))
	     ;; do strange things to an argument
	     (link-object  (obj)
			   (cond ((numberp obj)
				  (list obj))
				 ((eq (car obj) (the-link-handle))
				  (let ((val (if (eq (cadr obj) (the-local-handle))
						 (get-local-id istate (cddr obj))
					       (get-non-local-id istate (cdr obj)))))
				    (nconc (int2bytes (car val))
					   (int2bytes (cadr val)))))
				 ((eq (car obj) (the-long-handle))
				  (int2bytes (cadr obj)))
				 ((eq (car obj) (the-static-handle))
				  (int2bytes (static-link (cdr obj))))
				 ((eq (car obj) (the-local-handle))
				  (int2bytes (cadr (get-local-id istate (cdr obj)))))
				 (t (format t "~a~%" obj)
				    (error "Whups" <clock-tick>)))))
	    (link-stuff nil lst stream)))

  (defun get-non-local-id (state binding)
    ((cdr state) binding))

  (defun mk-local-id-mker (mod-id start mod-setter)
    (let ((tab (make-table eq))
	  (count (mk-counter start)))
      (lambda (name)
	(let ((xx (table-ref tab name)))
	  (if (null xx)
	      (let ((c (count)))
		(format t "(~a->~a)" name c)
		((setter table-ref) tab name c)
		(mod-setter name c)
		(list mod-id c))
	    (list mod-id xx))))))

  (defun get-local-id (state id)
    ((car state) id))

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
	  
	    
  (export link-vector mk-local-id-mker link-vector-to-stream)

  ;; end module
  )
