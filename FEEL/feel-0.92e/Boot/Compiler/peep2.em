;; Eulisp Module
;; Author: pab
;; File: peep2.em
;; Date: Fri May  8 20:00:39 1992
;;
;; Project:
;; Description: 
;;

(defmodule peep2
  (standard0
   list-fns
   byte-stream 
   
   instruct 
   comp-rules
   peephole
   )
  ()
  

  ;; returns '(nochange lst)
  ;;         '(change newlst rest)
  
  ;; cont is: (prev-list fn)
  (defun optimize-lst (lst)
    (let ((stream (make-simple-stream)))
      (labels ((move-right (conts lst prev-code)
			   (if (null lst)
			       (list 'no-change prev-code lst)
			     (let ((extended-conts (extend-conts (car lst) 
								 (cons (list prev-code peep-test)
								       conts))))
			       (cond ((not (null (car extended-conts)))
				      ;;(format t "New code: ~a~%" (car extended-conts))
				      (list 'new 
					    (append (make-prev-code (car extended-conts))
						    (cdr lst))))
				     ((null (cdr extended-conts))
				      (list 'no-change
					    (cons (car lst) prev-code)
					    (cdr lst)))
				     (t (move-right (cdr extended-conts)
						    (cdr lst)
						    (cons (car lst) prev-code)))))))
	       (opt-aux (lst)
			(if (null lst) nil
			  (let ((res (move-right nil lst nil)))
			    (cond ((eq (car res) 'no-change)
				   (format t "Code: ~a~%" (reverse (cadr res)))
				   (write-stream-list stream (nreverse (cadr res)))
				   (opt-aux (caddr res)))
				  (t ;;(format t "Hacking again: ~a~%" (cadr res))
				     (opt-aux (cadr res))))))))
	      (opt-aux lst)
	      (convert stream pair))))
  
  (export optimize-lst)

  (defun extend-conts (i lst)
    ;;(format t "(Extend: ~a ~a~%" 
    ;;(list-length i) 
    ;;i)
    (labels ((aux (conts code res)
		  ;;(format t "cont: ~a~%" conts)
		  (if (null conts) 
		      (progn ;;(format t "Newcode: ~a~%" code)
			     (cons code res))
		    (let ((ans ((cadar conts) i)))
		      (aux (cdr conts)
			   (append (mapcar (lambda (code)
					     (list (reverse code) (caar conts)))
					   (car ans))
				   code)
			   (if (null (cdr ans))
			       res
			     (progn ;;(format t "new cont: ~a~%" ans)
			       (cons (list (caar conts)
					   (cdr ans))
				     res))))))))
	    (let ((a (aux lst nil nil)))
	      ;;(format t ")~%")
	      a)))

  (defun make-new-cont (i prev)
    (let ((res (peep-test i)))
      (cons (car res)
	    (list prev (cdr res)))))
  
  
  (defun make-prev-code (lst)
    (labels ((aux (lst best-score best)
		  ;;(format t "Select: ~a ~a~%" best (if lst (car lst) nil))
		  (if (null lst)
		      (reverse best)
		    (let ((sc (score-code (caar lst))))
		      (if (< best-score sc)
			  (aux (cdr lst) best-score best)
			(aux (cdr lst) sc (append (caar lst) (cadar lst))))))))
	    (aux (cdr lst)
		 (score-code (caar lst)) 
		 (append (caar lst) (cadar lst)))))
  

  (defun score-code (l) 
    (fold (lambda (i c)
	    (+ (i-cost i) c))
	  l 0))
	  
	  

  ;; end module
  )
