;; Eulisp Module
;; Author: pab
;; File: peephole.em
;; Date: Wed Apr  1 00:51:38 1992
;;
;; Project:
;; Description: 
;;   Simple peephole optimizer
;;   Uses the state-stream to hold partial info
;;   Should be slot in replacement for simple 
;;   stream in compiler

(defmodule peephole
  (standard0
   list-fns
   
   peep-macs
   instruct
   comp-defn
   byte-stream
   )
  ()
  
  ;; lots of optimisations...

  (defconstant peep-test
    (peephole-matcher 
     (one-of (instruct 
	      i-slide-stack (d1 k1)
	      (one-of (next (one-of (instruct 
				     i-slide-stack (d2 k2)
				     (test ((<= k2 k1))
					   (output (i-slide-stack (+ d1 d2 (- k1)) k2))))
				    (instruct drop (n)
					      (test ((>= n k1))
						    (output (drop (+ n (- d1 k1))))))))
		      (test ((= k1 0))
			    (output (drop d1)))
		      (test ((= d1 k1))
			    (output ))))
	     (instruct 
	      nth-ref (n)
	      (do-rules peep-nth-rules))
	     (instruct set-nth (n)
		       (next (instruct i-slide-stack (d k)
				       (one-of ;;(test ((= k 1) (= n (+ d 1))) ;; edge case
					       ;; (output (i-slide-stack (+ n 1) 2)
					       ;;(swap)))
					(test ((> n k) (>= d n)) ;; in between
					      (output (drop 1)
						      (i-slide-stack d k)))
					(test ((> n d))
					      (output (i-slide-stack (+ d 1) (+ k 1))
						      (set-nth (+ (- n d) k))))))))
	     (instruct swap ()
		       (next 
			(one-of (instruct swap ()
					  (output))
				(instruct i-slide-stack (d k)
					  (test ((> k 1))
						(output (i-slide-stack d k) (swap)))))))
	     (instruct set-global (x)
		       (next (instruct push-global (y)
				       (test ((equal x y))
					     (output (set-global x)
						     (nth-ref 0))))))
	     (instruct push-global (x)
		       (one-of (test ((eq (car x) (the-local-handle))
				      (progn (format t "global->local: ~a~%" x) t))
				     (output (push-static x)))
			       (next (one-of (instruct drop (n)
						       (test ((> n 0))
							     (output (drop (- n 1)))))
					     (instruct i-slide-stack (d k)
						       (test ((> d 0)(> k 0))
							     (output (i-slide-stack (- d 1) (- k 1))
								     (push-global x))))))))
	     (instruct push-special (x)
		       (next (one-of (instruct drop (n)
					       (test ((> n 0))
						     (output (drop (- n 1)))))
				     (instruct i-slide-stack (d k)
					       (test ((> d 0)(> k 0))
						     (output (i-slide-stack (- d 1) (- k 1))
							     (push-special x)))))))
	     ;; probably more of this type...
	     (instruct drop (n)
		       (one-of (test ((= n 0))
				     (output))
			       (next (instruct drop (m)
					       (output (drop (+ n m)))))))
	     ;; kill labels where we can
	     (instruct i-label (lab)
		       (test ((and (null (lab-refs lab)) t))
			     (output)))
	     ;; ordinary push-dull-object sorta thing
	     (instruct push-static (x)
	       (next (one-of (instruct drop (n)
				       (test ((> n 0))
					     (output (drop (- n 1)))))
			     (instruct i-slide-stack (d k)
				       (test ((> d 0)(> k 0))
					     (output (i-slide-stack (- d 1) (- k 1))
						     (push-static x)))))))
	     (instruct push-fixnum (x)
	       (next (one-of (instruct drop (n)
				       (test ((> n 0))
					     (output (drop (- n 1)))))
			     (instruct i-slide-stack (d k)
				       (test ((> d 0)(> k 0))
					     (output (i-slide-stack (- d 1) (- k 1))
						     (push-fixnum x)))))))
	     ;; NB Labels are 2 objects..
	     (instruct push-label (x)
	       (next (one-of (instruct drop (n)
				       (test ((> n 1))
					     (output (drop (- n 2)))))
			     (instruct i-slide-stack (d k)
				       (test ((> d 0)(> k 0))
					     (output (i-slide-stack (- d 1) (- k 1))
						     (push-label x)))))))
	     )))

  '(attributes ((in 0)(out 1)(stackop ())(sidep ())) the-instruction
	       (next (one-of (instruct drop (n)
				       (test ((> n 0))
					     (output (drop (- n 1)))))
			     (instruct i-slide-stack (d k)
				       (test ((> d 0)(> k 0))
					     (output (i-slide-stack (- d 1) (- k 1))
						     the-instruction))))))

  (defconstant peep-nth-rules
    (peephole-matcher
     (instruct nth-ref (n)
	       (next (one-of (instruct i-slide-stack (d1 k1)
				 (one-of (test ((> n d1))
					       (output (i-slide-stack (- d1 1) (- k1 1))
						       (nth-ref (+ n (- d1) k1))))
					 (test ((< n (- k1 1)))
					       (output (i-slide-stack (- d1 1) (- k1 1))
						       (nth-ref n)))
					 (test ((= k1 2) (> n 0) (= d1 (+ n 3)))
					       (output (set-nth (+ n 1))
						       (drop (- n 1))))
					 (test ((= n 2) (= k1 2) (= d1 4))
					       (output (i-slide-stack 2 1) (swap)))
					 (test ((= n 0) (= 1 k1) (> d1 1))
					       (output (i-slide-stack (- d1 1) 1)))
					 (test ((= k1 1) (= d1 (+ n 2)))
					       (output (drop n)))
					 (test ((= k1 1) (> n 0))
					       (output (drop n) (i-slide-stack (- d1 (+ n 1)) 1)))
					 (test ((= n 1) (= k1 2))
					       (output (i-slide-stack (- d1 1) 2) (swap)))))
			     (test ((= n 0))
				   (instruct swap ()
					     (output (nth-ref 0))))
			     (instruct drop (m)
				       (test ((> m 0))
					     (output (drop (- m 1)))))
			     (instruct set-nth (m)
				       (test ((= (+ n 1) m))
					     (output)))
			     (instruct nth-ref (m)
				       (test ((> n 0) (= m 1))
					     (next (instruct i-slide-stack (d k)
							     (test ((print (list 'Yow n m d k)) (= d (+ n 3))(= k 2))
								   (output (i-slide-stack n 1))))))))))))

  (defconstant no-rules 
    (peephole-matcher (one-of)))

  (export no-rules peep-test)

  (deflocal *peep-test-fn* peep-test)
  
  ;; Too simple, but...
  
  (defun match-instruct (name i)
    (eq name (i-name i)))

  (export peep-test)

  ;; these must not use the next construct...
  
  (defconstant micro-test 
    (peephole-matcher 
     (one-of (instruct i-slide-stack (n m)
		       (test ((= m 1))
			     (output (i-slide-stack-1 n))))
	     (instruct drop (n)
		       (test ((= n 1))
			     (output (drop-1))))
	     (instruct nth-ref (n)
		       (one-of (test ((= n 0))
				     (output (nth-ref-0)))
			       (test ((= n 3))
				     (output (nth-ref-3)))
			       (test ((= n 1))
				     (output (nth-ref-1)))
			       (test ((= n 2))
				     (output (nth-ref-2)))))
	     (instruct push-fixnum (i)
		       (test ((> i 0)(< i 256))
			     (output (push-small-fixnum i))))
	     (instruct slot-ref (n)
		       (one-of (test ((= n 0))
				     (output (slot-ref-0)))
			       (test ((= n 1))
				     (output (slot-ref-1)))))
	     (instruct set-slot (n)
		       (test ((= n 1))
			     (output (set-slot-1)))))))
  
  (export micro-test)
  
  (defun blocking-instruction-p (op)
    (memq (i-name op) '(i-label branch branch-nil return)))
  
  (export blocking-instruction-p)

)

--------------------------------------------------------------------------------
UNUSED CODE (old version).
replaced by peep-drv.em

  (defun prev (x)
    (cdr x))

  (defun first (x)
    (car x))

  (defstruct state ()
    ((i-list initarg i-list reader state-i-list)
     (fn-list initarg fn-list reader state-fn-list)
     (cost initarg cost reader state-cost))
    constructor (new-state fn-list i-list cost))

  (defun new-triv-state ()
    (new-state nil nil *big-num*))
  (defconstant *big-num* 99999999)

  (defmethod generic-prin ((x state) stream)
    (format stream "<S[~a ~a]: ~a>"
	    (state-cost x)
	    (mapcar (lambda (x) (eq x peep-test)) (state-fn-list x))
	    (state-i-list x)))

  ;; LR walk of instruction stream, backtracking 

  (defun get-cont-info (state i)
    (if (= (state-cost state) *big-num*)
	(cons nil nil)
      (if (null (state-fn-list state))
	  (progn (*peep-test-fn* i))
	((car (state-fn-list state)) i))))

  ;; List of new states given 1 instruction
  (defun gen-all (i state)
    ;;(format t "(Gen all: ~a ~a~%" i state)
    (let ((code-and-continue (get-cont-info state i)))
      ;;(format t "Gen-all: ~a ~a ~a~%" i code-and-continue new-state-lst)
      (let ((new-state-lst (mapcar (lambda (i-list)
				     (all (next-states state) i-list))
				   (car code-and-continue)))
	    (xx (next-states-from-cont state i (cdr code-and-continue))))
	(cons (fold (lambda (ob best)
		      (if (< (state-cost best)
			     (state-cost ob))
			  best
			ob))
		    (mapcar car new-state-lst) 
		    (car xx))
	      (fold append (mapcar cdr new-state-lst) (cdr xx))))))
      
  (defun add-states (new-info state-list)
    (cons (if (< (state-cost (car state-list))
		 (state-cost (car new-info)))
	      (car state-list)
	    (car new-info))
	  (if (null (cdr new-info))
	      (cdr state-list)
	    (my-append (cdr new-info)
		       (cdr state-list)))))

  (defun all (state-pair i-list)
    (format t "All: ~a ~a~%" (car state-pair) i-list)
    (if (null i-list)
	state-pair
      (all (fold add-states 
		 (mapcar (lambda (s) 
			   (gen-all (car i-list) s))
			 state-pair)
		 (cons (new-triv-state) nil))
	   (cdr i-list))))
  
  (defun all-new-states (new-state i-list state-list)
    (if (null i-list) 
	new-state
      (if (null state-list)
	  (all-new-states new-state (cdr i-list) nil)
	(all-new-states (add-states (gen-states (car i-list) 
						(car state-list))
				    new-state)
			i-list
			(cdr state-list)))))

  (defun next-states (state)
    (if (null (state-fn-list state))
	(cons (new-state nil
			 (state-i-list state)
			 (state-cost state))
	      nil)
      (if (null (cdr (state-fn-list state)))
	  (list (new-state (cdr (state-fn-list state))
			   (state-i-list state)
			   (state-cost state))
		(new-state (cons peep-test
				 (cdr (state-fn-list state)))
			   (state-i-list state)
			   (state-cost state)))
	(cons (new-triv-state)
	      (list (new-state (cons peep-test
				     (cdr (state-fn-list state)))
			       (state-i-list state)
			       (state-cost state))
		    (new-state (cdr (state-fn-list state))
			       (state-i-list state)
			       (state-cost state)))))))
  
  (defun next-states-from-cont (state i cont)
    (if cont
	(if (null (state-fn-list state))
	    (cons (new-state nil
			     (cons i (state-i-list state))
			     (+ (i-cost i) 
				(state-cost state)))
		  (list (new-state (cons cont nil)
				   (state-i-list state)
				   (state-cost state))))
	  (cons (new-triv-state)
		(list (new-state (cons cont (cdr (state-fn-list state)))
				 (state-i-list state)
				 (state-cost state))
		      (new-state (cons peep-test
				       (cons cont (cdr (state-fn-list state))))
				 (state-i-list state)
				 (state-cost state)))))
      (if (null (state-fn-list state))
	  (cons (new-state nil
			   (cons i (state-i-list state))
			   (+ (i-cost i) 
			      (state-cost state)))
		nil)
	(cons (new-triv-state) nil))))
      
      '(new-state (list peep-test cont)
		  (state-i-list state)
		  (state-cost state))

  (defun initial-peep-state ()
    (cons (new-state nil nil 0) (list (new-state (list peep-test) nil 0))))

  (defun my-append (a b)
    (cond ((null a)
	   b)
	  ((atom a) (error "Rats" <clock-tick>))
	  (t (append a b))))


  (defun peephole-code-list (lst)
    (let ((stream (make-simple-stream))
	  (zero-state (initial-peep-state)))
      (format t "Peephole optimising...~%")
      (labels ((add-instruct (state lst)
		(let ((newstate (all state (list (car lst)))))
		  (if (null (cdr newstate))
		      (add-code (car newstate) (cdr lst))
		    (add-instruct newstate (cdr lst)))))
	       (add-code (code lst)
		(format t "Made: ~a~%" code)
		(write-stream-list stream (reverse (state-i-list code)))
		(if lst
		    (add-instruct zero-state lst)
		  stream)))
	      (let ((xx (convert (add-instruct zero-state lst) pair)))
		(format t "...Done~%")
		xx))))

  (export peephole-code-list)

  ;; end module
)  
  ;; whop a state until we run out of instructions
  ;; returns state-list
  ;; agenda is: (i-list best state-list)
  (defun gen-by-ilist (agenda best)
    (if (null agenda)
	best
      (let ((i-list (car agenda))
	    (state-list (cdr agenda)))
	(let ((code-and-cont (get-cont-info (car i-list) (car state-list))))
	  (gen-by-ilist (agenda-add (car code-and-cont) 
				    (cdr code-and-cont)
				    agenda)
			
      
)
