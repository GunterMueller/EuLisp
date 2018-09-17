;; A simple Linda implementation
;; RJB March 92

;; (make-linda-pool)
;; (linda-out pool tag . values)
;; (linda-in pool tag . pattern)
;; (linda-in? pool tag . pattern)
;; (linda-read pool tag . pattern)
;; (linda-read? pool tag . pattern)
;; (linda-eval fun . args)

;; linda-in? and linda-read? are non-blocking versions of linda-in
;; and linda-read, returning () if no matching tuple, t otherwise.

;; the pattern (? var) matches anything, and assigns that value to var
;; the pattern ? matches anything, and discards the value
;; tags, and any other patterns are matched literally

;; e.g.
;; (setq pp (make-linda-pool))
;; (linda-out pp 'foo 1 2)
;; (linda-read pp 'foo ? (? x))      setqs x to 2
;; (linda-read? pp 'foo 1 2 3)       returns ()
;; (linda-read pp 'foo 1 2 3)        suspends

(defmodule eulinda 
  ((rename ((lock open-lock)
	    (unlock close-lock))
	   eulisp0) )
   ()

  (deflocal trace-linda? ())

  (defun tril (x) (setq trace-linda? x))

  (defstruct linda-pool ()
    ((lock initform (make <lock>)
	   accessor linda-pool-lock)
     (tuple-table initform (make-linda-tuple-table)
		  accessor linda-pool-tuple-table))
    constructor make-linda-pool)

  (defun print-linda-pool (pool)
    (format t "#< ")
    (map-table
     (lambda (k v) (format t "~a " v))
     (linda-pool-tuple-table pool))
    (format t ">~%"))

  (defun tidy-pattern (pat)
    (cond ((null pat) ())
	  ((eq (car pat) '?)
	   (cons '? (tidy-pattern (cdr pat))))
	  ((and (consp (car pat))
		(eq (caar pat) '?))
	   (cons '? (tidy-pattern (cdr pat))))
	  (t (cons (car pat) (tidy-pattern (cdr pat))))))

  (defun do-setqs-aux (pattern n)
    (cond ((null pattern) ())
	  ((and (consp (car pattern))
		(eq (caar pattern) '?))
	   (cons `(setq ,(cadar pattern) (vector-ref *tuple* ,n))
		 (do-setqs-aux (cdr pattern) (+ n 1))))
	  (t (do-setqs-aux (cdr pattern) (+ n 1)))))

  (defun do-setqs (pattern)
      (do-setqs-aux pattern 0))

  (defmacro linda-in (pool tag . pattern)
    `(let ((*tuple* (convert (linda-tuple-value
			      (linda-in-tuple ,pool ,tag
					      ,@(tidy-pattern pattern)))
			     <vector>)))
       ,@(do-setqs pattern)
       *tuple*))

  (defun linda-in-tuple (pool tag . pattern)
    (when trace-linda? (format t ";; in-ing ~a ~a~%" tag pattern))
    (let ((val (linda-in/read pool tag (tuple tag pattern) in-match)))
      (when trace-linda?
	(format t ";; in'd ~a~%" val))
      val))

  (defmacro linda-in? (pool tag . pattern)
    `(let ((*result* (linda-in?-tuple ,pool ,tag
				      ,@(tidy-pattern pattern))))
       (if (null *result*)
	   ()
	   (let ((*tuple* (convert (linda-tuple-value *result*) <vector>)))
	     ,@(do-setqs pattern)
	     t))))

  (defun linda-in?-tuple (pool tag . pattern)
    (when trace-linda? (format t ";; in?-ing ~a ~a~%" tag pattern))
    (let ((val (linda-in/read? pool tag (tuple tag pattern) in-match)))
      (when trace-linda?
        (format t ";; in?'d ~a~%" val))
      val))

  (defmacro linda-read (pool tag . pattern)
    `(let ((*tuple* (convert (linda-tuple-value
			      (linda-read-tuple ,pool ,tag
				 ,@(tidy-pattern pattern)))
			     <vector>)))
       ,@(do-setqs pattern)
       *tuple*))

  (defun linda-read-tuple (pool tag . pattern)
    (when trace-linda? (format t ";; reading ~a ~a~%" tag pattern))
    (let ((val (linda-in/read pool tag (tuple tag pattern) read-match)))
      (when trace-linda?
	(format t ";; read ~a~%" val))
      val))

  (defmacro linda-read? (pool tag . pattern)
    `(let ((*result* (linda-read?-tuple ,pool ,tag
					,@(tidy-pattern pattern))))
       (if (null *result*)
           ()
           (let ((*tuple* (convert (linda-tuple-value *result*) <vector>)))
             ,@(do-setqs pattern)
             t))))

  (defun linda-read?-tuple (pool tag . pattern)
    (when trace-linda? (format t ";; read?-ing ~a ~a~%" tag pattern))
    (let ((val (linda-in/read? pool tag (tuple tag pattern) read-match)))
      (when trace-linda?
        (format t ";; read?'d ~a~%" val))
      val))

  (defun linda-in/read (pool tag pattern matchfn)
    (let ((lock (linda-pool-lock pool)))
      (open-lock lock)
      (let ((match (matchfn pool tag pattern)))
	(close-lock lock)
	(if (null match)
	    (progn
	      (when trace-linda?
		(format t ";; suspending~%"))
	      (thread-reschedule)
	      (when trace-linda?
		(format t ";; retrying ~a ~a~%" tag
			(linda-tuple-value pattern)))
	      (linda-in/read pool tag pattern matchfn))
	    match))))

  (defun linda-in/read? (pool tag pattern matchfn)
    (let ((lock (linda-pool-lock pool)))
      (open-lock lock)
      (let ((match (matchfn pool tag pattern)))
	(close-lock lock)
	(if (null match)
	    ()
	    match))))

  (defun linda-out (pool tag . rest)
    (when trace-linda? (format t ";; out ~a ~a~%" tag rest))
    (let ((lock (linda-pool-lock pool))
	  (tup (tuple tag rest)))
      (open-lock lock)
      (linda-out-tuple pool tag tup)
      (close-lock lock)
      (thread-reschedule)
      tup))

  (defun make-linda-tuple-table ()
    (make-table eq))

  (defstruct linda-tuple ()
    ((tag initarg tag
	  reader linda-tuple-tag)
     (value initarg value
	    reader linda-tuple-value))
    constructor (tuple tag value))

  (defmethod generic-write ((lt linda-tuple) s)
    (format s "#<linda-tuple: ~a ~a>"
	    (linda-tuple-tag lt)
	    (linda-tuple-value lt)))

  (defmethod generic-prin ((lt linda-tuple) s)
    (format s "#<linda-tuple: ~a ~a>"
            (linda-tuple-tag lt)
            (linda-tuple-value lt)))

  (defun delete1 (obj lis)
    (labels ((aux (end lst)
		  (cond ((null lst)
			 nil)
			((eq obj (car lst))
			 (aux end (cdr lst)))
			(t 
			 ((setter cdr) end (cons (car lst) nil))
			 (aux (cdr end) (cdr lst))))))
	    (if (null lis) nil
	      (if (eq (car lis) obj) 
		  (mapcar (lambda (x) x) lis)
		(progn (let ((first (cons (car lis) nil)))
			 (aux first (cdr lis))
			 first))))))
			 
		       

  (defun in-match (pool tag pattern-tuple)
    (let* ((table (linda-pool-tuple-table pool))
	   (vallist (table-ref table tag))
	   (val (match-in-list (linda-tuple-value pattern-tuple) vallist)))
      (unless (null val)
	((setter table-ref) table tag (delete1 val vallist)))
      val))

  (defun read-match (pool tag pattern-tuple)
    (let* ((table (linda-pool-tuple-table pool))
           (vallist (table-ref table tag)))
      (match-in-list (linda-tuple-value pattern-tuple) vallist)))

  (defun match-in-list (pat vallist)
    (cond ((null vallist) ())
	  ((matchit pat (linda-tuple-value (car vallist))) (car vallist))
	  (t (match-in-list pat (cdr vallist)))))

  (defun matchit (pat val)
    (cond ((null pat) t)
	  ((null val) ())
	  ((equal (car pat) (car val))
	   (matchit (cdr pat) (cdr val)))
	  ((eq (car pat) '?)
	   (matchit (cdr pat) (cdr val)))
	  (t ())))

  ; putting tuple at end allows weak fairness on tuple selection
  ; for a given tag
  (defun linda-out-tuple (pool tag tuple)
    (let* ((table (linda-pool-tuple-table pool))
           (val (table-ref table tag)))
      ((setter table-ref) table tag (nconc val (list tuple)))
      tuple))

  (defun linda-eval (fun . args)
    (when trace-linda?
      (format t ";; eval ~a~%" fun))
    (apply thread-start (make-thread fun) args))

  ; a convenient fiddle
  (defconstant ? '?)

  (export make-linda-pool linda-in linda-read linda-out linda-eval)
  (export linda-in? linda-read?)
  (export linda-in-tuple linda-read-tuple)
  (export linda-in?-tuple linda-read?-tuple)
  (export linda-tuple-value ?)

  (export print-linda-pool tril)

)
