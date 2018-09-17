;; Eulisp Module
;; Author: pab
;; File: collect.em
;; Date: Tue Jun 29 16:05:59 1993
;;
;; Project:
;; Description: 
;;  Actually, all jap's work.

(defmodule collect
  (init
   extras0
   macros0
   defs
   gens
   telos1
   character
   numbers
   )
  ()
  
  ;; the basic collection operations
  (export
   accumulate
   accumulate1
   concatenate
   do
   emptyp
   fill
   map
   size
   sequencep 
   collectionp
   )

  ;; not primitive, but it would be odd to omit them
  (export
   member
   reverse
   )


  ;; ones I had to write to make the rest work
  (export
   anyp
   intersection
   )

  ;; imports to be re-exported
  (export
   element
   )
  
  ;; predicates
  (defpredicate sequencep <sequence>)
  (defpredicate collectionp <collection>)
  
  ;; converter methods

  (defmethod (converter <list>) ((c <collection>))
    ;; converts any kind of collection to a list
    (let ((r ()))
      (labels
       ((loop (s)
	      (if (null s)
		  r
		(progn (setq r (cons (current-element c s) r))
		       (loop (previous-state c s))))))
       (loop (final-state c)))))

  (defmethod (converter <string>) ((c <collection>))
    ;; converts a collection of characters to a string
    (let ((r (make-string (size c))))
      (labels
       ((loop (s1 s2)
	      (cond
	       ((null s1)
		r)
	       ((characterp (current-element c s1))
		((setter current-element) r s2 (current-element c s1))
		(loop (next-state c s1) (next-state r s2)))
	       (t
		(error
		 (format () "list(character)->string: ~a is not a character"
			 (current-element c s1))
		 <Internal-Error>)))))
       (loop (initial-state c) (initial-state r)))))

  (defmethod (converter <table>) ((c <collection>))
    ;; converts any kind of collection to a table
    (let ((r (make <table>
		   'comparator eq
		   'hash-function generic-hash)))
      (labels
       ((loop (s)
	      (if (null s)
		  r
		(progn ((setter element) r (current-key c s) (current-element c s))
		       (loop (next-state c s))))))
       (loop (initial-state c)))))

  (defmethod (converter <vector>) ((c <collection>))
    ;; converts any kind of collection to a vector
    (let ((r (make-vector (size c))))
      (labels
       ((loop (s1 s2)
	      (if (null s1)
		  r
		(progn ((setter current-element) r s2 (current-element c s1))
		       (loop (next-state c s1) (next-state r s2))))))
       (loop (initial-state c) (initial-state r)))))

  ;; default methods for collections

  (defmethod key-sequence ((c <collection>))
    ;; returns a list of integers from 0 .. size of c
    (labels
     ((loop (i s)
	    (if (null s)
		()
	      (cons i (loop (+ i 1) (next-state c s))))))
     (loop 0 (initial-state c))))

  (defmethod emptyp ((c <collection>)) (= 0 (size c)))

  (defmethod gf-member (v (c <collection>) f)
    ;; returns t if the application of f to v and an element of c does
    ;; see list.em for a more efficient list method
    (let/cc k
	    (gf-do
	     (if f
		 (lambda (x) (if (f v x) (k t) ()))
	       (lambda (x) (if (eql v x) (k t) ())))
	     c ())))

  (defmethod gf-do (f (c <collection>) cs)
    ;; default method for iterating over several collections
    ;; simultaneously, applying the function f to the appropriate
    ;; combinations of elements and ignoring the result
    (cond
     ((null cs)
      ;; simplest case of only one iterand
      (labels
       ((loop-1 (s)
		(if (null s)
		    ()
		  (progn (f (current-element c s)) (loop-1 (next-state c s))))))
       (loop-1 (initial-state c))))
     ((null (cdr cs))
      ;; two iterands
      (if (or (not (sequencep c)) (not (sequencep (car cs))))
	  (let ((ks (intersection (key-sequence c) (key-sequence (car cs)))))
	    ;; one or more is a table therefore have to align keys
	    (labels
	     ((loop-2 (s c1 c2)
		      (if (null s)
			  ()
			(progn
			  (f (element c1 (current-element ks s))
			     (element c2 (current-element ks s)))
			  (loop-2 (next-state ks s) c1 c2)))))
	     (loop-2 (initial-state ks) c (car cs))))
	(labels
	 ;; only collections with natural order
	 ((loop-2 (c1 s1 c2 s2)
		  (if (or (null s1) (null s2))
		      ()
		    (progn
		      (f (current-element c1 s1) (current-element c2 s2))
		      (loop-2 c1 (next-state c1 s1) c2 (next-state c2 s2))))))
	 (loop-2 c (initial-state c) (car cs) (initial-state (car cs))))))
     ((anyp (lambda (x) (not (sequencep x))) (cons c cs))
      ;; more than two iterands
      (let ((ks (apply intersection (map key-sequence (cons c cs)))))
	;; and at least one is a table so align keys
	(labels
	 ((loop-n (s cs)
		  (if (null s)
		      ()
		    (progn
		      (apply f
			     (map (lambda (c) (element c (current-element ks s))) cs))
		      (loop-n (next-state ks s) cs)))))
	 (loop-n (initial-state ks) (cons c cs)))))
     (t
      (labels
       ;; only natural order collections
       ((loop-n (cs ss)
		(if (anyp null ss)
		    ()
		  (progn
		    (apply f (map (lambda (c s) (current-element c s)) cs ss))
		    (loop-n cs (map (lambda (c s) (next-state c s)) cs ss))))))
       (loop-n (cons c cs) (map initial-state (cons c cs)))))))

  ;; define this here temporarily until PAB does a more efficient
  ;; version elsewhere

  (defun compose (f g) (lambda l (f (apply g l))))

  (defmethod gf-any (f (c <collection>) cs)
    ;; default method for iterating over several collections testing the
    ;; appropriate combinations of elements using f.  If this once
    ;; returns true, no further elements are processed and t is
    ;; returned.
    (let/cc k
	    (apply do
		   (compose (lambda (x) (if x (k t) ())) f)
		   c cs)))

  (defmethod gf-map (f (c <collection>) cs)
    ;; default method for iterating over several collections
    ;; simultaneously, applying the function f to the appropriate
    ;; combinations of elements and saving the result in an object of
    ;; the same class as c, which is returned as the result.
    ;; this map method only works for sequences...see table.em for one
    ;; which works for collections without natural order and list.em for
    ;; a (slightly) more efficient list version
    ;; JAP 940519
    ;; a major rewrite to cope with the case of the result being shorter
    ;; than the first collection, which means that we can't use clone.
    ;; Temporary-ish hack defines the gf make-result-object: problem is
    ;; that it may need to be extended, but I don't want to export
    ;; this---let's hope the default method works for most cases!
    (if (null cs)
	;; only a single iterand
	(let ((r (clone c))
	      (i 0))
	  (gf-do
	   (compose (lambda (x) ((setter element) r i x) (setq i (+ i 1))) f)
	   c ())
	  r)
      ;; multiple iterands
      (let ((s (accumulate (lambda (a x) (min a (size x))) (size c) cs)))
	(if (= (size c) s)
	    ;; result is the same size as c
	    (let ((r (clone c))
		  (i 0))
	      (apply gf-do
		     (compose
		      (lambda (x) ((setter element) r i x) (setq i (+ i 1)))
		      f)
		     c cs ())
	      r)
	  ;; result is smaller than c
	  (let ((r (make-result-object c s))
		(i 0))
	      (apply gf-do
		     (compose
		      (lambda (x) ((setter element) r i x) (setq i (+ i 1)))
		      f)
		     c cs ())
	      r)))))

  (defgeneric make-result-object (obj size)
    method (((c <object>) s)
	    (make (class-of c) 'size s))
    method (((c <null>) s)
	    ())
    method (((c <cons>) s)
	    (labels
	     ((loop (n) (if (= n 0) () (cons () (loop (- n 1))))))
	     (loop s)))
    method (((c <string>) s)
	    (make-string s #\space))
    method (((c <vector>) s)
	    (make-vector s))
    method (((c <table>) s)
	    (clone c)))

  (defmethod accumulate (f i (c <collection>))
    ;; accumulates and returns the result of applying f to the initial
    ;; value i and the first element of c, then f to the result of that
    ;; and the second, and so on.
    (labels
     ((loop-1 (a s)
	      (if (null s)
		  a
		(loop-1 (f a (current-element c s)) (next-state c s)))))
     (loop-1 i (initial-state c))))

  (defmethod accumulate1 (f (c <collection>))
    ;; as accumulate except that the first value is used the initial
    ;; value and processing then begins with the second value.
    (labels
     ((loop-1 (a s)
	      (if (null s)
		  a
		(loop-1 (f a (current-element c s)) (next-state c s)))))
     (let ((s (initial-state c)))
       (if (null s)
	   ()
	 (loop-1 (current-element c s) (next-state c s))))))

  ;; fill notes
  ;; ----------
  ;; (1) replace start and end by a collection whose elements define the
  ;; keys to be updated with v.  Useful for objects with non-integer
  ;; keys, but implies need for ranges for those with integer keys.
  ;; (2) currently does nothing if start..end falls outside range defined
  ;; by 0..size.  Arguably wrong behaviour for stretchy objects (tables).
  ;; fixed by adding a method for <table> in table.em

  (defmethod gf-fill-from ((mc <sequence>) v c)
    (do (lambda (k) ((setter element) mc k v)) c))

  (defmethod gf-fill-range ((mc <sequence>) v start end)
    (labels
     ((loop (i)
	(if (> i end)
	    ()
	  (progn ((setter element) mc i v) (loop (+ i 1))))))
     (if (and (<= 0 start) (<= start end) (< end (size mc)))
	 (loop start)
       ())))

  ;; concatenate notes
  ;; -----------------
  ;; (1) uses wrong key with tables...special case for tables??
  ;; in fact, Dylan only defines this for sequence...on tables it is
  ;; more like a merge, but what to do about "collisions"?

  (defmethod gf-concatenate-as (class (c <sequence>) cs)
    ;; concatenates the elements of c and cs creating
    ;; an instance of class
    (let* ((sizes (map size (cons c cs)))
	   (r (make-vector (accumulate + 0 sizes)))
	   (rs (initial-state r))
	   (fillptr 0))
      (do
	  (lambda (c l)
	    (labels
	     ((loop (s)
		    (if (null s)
			()
		      (progn
			((setter current-element) r rs (current-element c s))
			(setq rs (next-state r rs))
			(loop (next-state c s))))))
	     (loop (initial-state c))))
	  (cons c cs)
	sizes)
      (convert r class)))

  (defmethod gf-concatenate ((c <collection>) cs)
    ;; see gf-concatenate-as
    (gf-concatenate-as (class-of c) c cs))

  (defmethod reverse ((sequence <sequence>))
    ;; returns a new sequence which has been initialized with the
    ;; elements of the argument sequence in the reverse natural order.
    ;; See also list.em for a more efficient list method.  Works for
    ;; tables but doesn't mean anything...it just makes a copy.
    (let* ((r (shallow-copy sequence))
	   (rs (final-state r)))
      (do
	  (lambda (x)
	    ((setter current-element) r rs x)
	    (setq rs (previous-state r rs)))
	  sequence)
      r))

  (defun intersection (c . cs)
    ;; returns a list whose elements appear in the intersection of the
    ;; collections c and cs.  N-ary case computed by intersection of
    ;; first two, then intersection of subsequent collections with the
    ;; intersection so far.
    (cond
     ((null cs)
      c)
     ((null (cdr cs))
      (accumulate
       (lambda (a x) (if (member x c) (cons x a) a))
       ()
       (car cs)))
     (t
      (accumulate
       (lambda (r ci)
	 (accumulate
	  (lambda (a x) (if (member x r) (cons x a) a))
	  ()
	  ci))
       (accumulate
	(lambda (a x) (if (member x c) (cons x a) a))
	()
	(car cs))
       (cdr cs)))))
  
  ;; Copying protocol
  (defmethod clone ((x <collection>))
    (nyi "Subclass must implement clone"))

  (defmethod shallow-copy ((c <collection>))
    (let ((new (clone c)))
      (labels
        ((loop (s)
	   (if (null s)
	       ()
	     (progn
	       ((setter element) new (current-key c s) (current-element c s))
	       (loop (next-state c s))))))
	(loop (initial-state c))
	new)))

  (defmethod deep-copy ((c <collection>))
    (let ((new (clone c)))
      (labels
        ((loop (s)
	   (if (null s)
	       ()
	     (progn
	       ((setter element) new (current-key c s)
		(deep-copy (current-element c s)))
	       (loop (next-state c s))))))
	(loop (initial-state c))
	new)))

  (defmethod first ((s <sequence>)) (element s 0))

  (defmethod second ((s <sequence>)) (element s 1))

  (defmethod third ((s <sequence>)) (element s 2))

  (defmethod last ((s <sequence>))
    (let ((x (size s)))
      (if (> x 0)
	  (element s (- x 1))
	(error
	 (format () "Argument to last has zero size: ~s\n" s)
	 <Internal-Error>))))

  ;; default method for find-key.  Returns the key associated with
  ;; the element that satisfies f, skipping the first skip elements
  ;; that satisfy f and returning failure if a suitable element was
  ;; not found.
  (defmethod gf-find-key ((c <collection>) f skip failure)
    (labels
      ((loop (s i)
	 (cond
	  ((null s) failure)
	  ((zerop i)
	   (if (f (current-element c s))
	       (current-key c s)
	     (loop (next-state c s) i)))
	  (t (let ((x (f (current-element c s))))
	       (loop (next-state c s) (if x (- i 1) i)))))))
      (loop (initial-state c) skip)))

  ;; default remove method for collections (will it ever be called?)
  (defmethod gf-remove (object (c <collection>) comp)
    (let ((k (find-key c (lambda (v) (comp v object)))))
      (if k
	  (let ((r (shallow-copy c)))
	    (gf-delete k r comp)
	    r)
	c)))

  ;; default remove method for sequences
  (defmethod gf-remove (object (sequence <sequence>) comp)
    (labels
     ((find-object (s ss)
	(cond
	 ((null ss) s)
	 ((comp object (current-element s ss))
	  (let ((r (make <vector> 'size (- (size s) 1))))
	    ;; (format t "object found at index ~s\n" ss)
	    (copy-to-object s (initial-state s) ss r (initial-state r))))
	 (t (find-object s (next-state s ss)))))
      (copy-to-object (s ss os r rs)
        (cond
	 ((null rs)
	  (convert r (class-of sequence)))
	 ((eql os rs)
	  ;; (format t "copied up to ~s\n" rs)
	  (copy-to-end s (next-state s ss) r rs))
	 (t ((setter current-element) r rs (current-element s ss))
	    (copy-to-object s (next-state s ss) os r (next-state r rs)))))
      (copy-to-end (s ss r rs)
        (cond
	 ((null rs)
	  ;; (format t "copied to end\n")
	  (convert r (class-of sequence)))
	 (t ((setter current-element) r rs (current-element s ss))
	    (copy-to-end s (next-state s ss) r (next-state r rs))))))
     (find-object sequence (initial-state sequence))))

  )

(setq a '(0 1 2 3 4))
(setq b '#(0 1 2 3))
(setq c "012")
(setq d (make <table> 'comparator = 'hash-function generic-hash))
((setter element) d 0 'zero)
((setter element) d 1 'one)

(do print a)
(do print b)
(do print c)
(do print d)
(do (lambda (a b) (print (list a b))) a b)
(do (lambda (a b) (print (list a b))) c a)
(do (lambda (a b) (print (list a b))) a d)
(do (lambda (a b) (print (list a b))) d a)
(do (lambda (a b c) (print (list a b c))) a b c)
(do (lambda (a b c d) (print (list a b c d))) a b c d)

