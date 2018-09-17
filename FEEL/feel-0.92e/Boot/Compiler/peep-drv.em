;; Eulisp Module
;; Author: pab
;; File: peep-drv.em
;; Date: Thu May 21 16:01:55 1992
;;
;; Project:
;; Description: 
;;

(defmodule peep-drv
  (standard0
   list-fns
   
   byte-stream
   peephole
   instruct
   )
  ()

  ;;;;;;;;;;;;;;;;;;;;;;
  ;; Driver for all this
  ;;
  
  ;; returns '(nochange lst)
  ;;         '(change newlst rest)
  
  ;; cont is: (prev-list fn)
  (defun optimize-lst (lst micro-opts-wanted)
    (let* ((a-stream (make-simple-stream))
	   (stream (make-peep-stream a-stream micro-opts-wanted)))
      (labels ((move-right (conts lst prev-code)
			   ;;(format t "Move: ~a ~a ~a~%" conts lst prev-code)
			   (if (null lst)
			       (list 'no-change prev-code lst)
			     (let ((extended-conts (extend-conts (car lst) 
								 (cons (list prev-code peep-test)
								       conts))))
			       ;;(format t "Extended: ~a~%" extend-conts)
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
			;;(format t "aux: ~a~%" lst)
			(if (null lst) nil
			  (let ((res (move-right nil lst nil)))
			    (cond ((eq (car res) 'no-change)
				   ;;(format t "Code: ~a~%" (reverse (cadr res)))
				   (write-stream-list stream (nreverse (cadr res)))
				   (opt-aux (caddr res)))
				  (t ;; (format t "Hacking again: ~a~%" (cadr res))
				   (opt-aux (cadr res))))))))
	      (opt-aux lst)
	      (convert stream pair))))
  
  (export optimize-lst)

  ;; create the stream: if peephole optimisation is wanted, make a filter

  (defun make-peep-stream (stream microp)
    (if microp
	(make-filter-stream stream micro-optimise)
      stream))



  (defun extend-conts (i lst)
    ;;(format t "(Extend: ~a ~a~%" i lst)
    (labels ((aux (conts code res)
		  ;;(format t "aux-1: ~a ~a ~a ~a ~%" i conts code res)
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


  ;;;;;
  ;; Micro optimisation
  ;; assumes 1->1 translation.
  
  (defun micro-optimise (i)
    (let ((xx (micro-test i)))
      (if (null (car xx)) i
	(caar (car xx)))))

	  
  ;; end module
  )
