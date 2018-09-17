;
;; EuLisp prog macro
;

(defmodule prog (standard0) ()

  ;
  ;; Do the meat of the problem in tagbody...
  ;;
  ;;  Go for serious continuation abuse here in order to limit stack
  ;;  use in a compiler that can't deal with tail in general.
  ;

  (defconstant *tagbody-dispatcher-name* (gensym))

  (defun tagbody-until-label (forms)
    (cond ((null forms) nil)
	  ((symbolp (car forms)) (tagbody-until-label (cdr forms)))
	  (t (cons (car forms) (tagbody-until-label (cdr forms))))))

  (defun tagbody-forms-before-a-label (forms)
    (cond ((null forms) nil)
	  ((symbolp (car forms)) nil)
	  (t (cons (car forms) (tagbody-forms-before-a-label (cdr forms))))))
	
  (defun tagbody-label-forms (forms)
    (cond ((null forms) nil)
	  ((symbolp (car forms))
	   (cons
	    (cons (car forms) (cons () (tagbody-until-label (cdr forms))))
	    (tagbody-label-forms (cdr forms))))
	  (t (tagbody-label-forms (cdr forms)))))

  (defun tagbody-first-label (forms)
    (cond ((null forms) nil)
	  ((symbolp (car forms)) (car forms))
	  (t (tagbody-first-label (cdr forms)))))

  (defmacro ctagbody forms
    (let ((tag-label-forms (tagbody-label-forms forms)))
      (if (null tag-label-forms) ; No labels at all
	`(let/cc return ,@forms)
	`(let/cc return 
	   (let ((,*tagbody-dispatcher-name* ()))
	     (labels 
	       ,tag-label-forms
	       (let/cc dropped-out
		 (labels
	           ((dispatcher (fn)
		      (dispatcher 
		        (let/cc called
			  (setq ,*tagbody-dispatcher-name* called)
			  (dropped-out (fn))))))
 		   ; these must see the tags
		   (dispatcher 
		     (let/cc panic
		       (setq ,*tagbody-dispatcher-name* panic)
		       ,@(tagbody-forms-before-a-label forms)
                       ,(tagbody-first-label forms)))))))))))

  (defmacro cgo (name)
    `(go ,name))
       
  (defmacro cprog (vars . forms)
    `(let/cc return
       ((lambda ,vars (ctagbody ,@forms)) ,@(mapcar (lambda (a) ()) vars))))

  (export ctagbody cgo cprog)

)


	  
