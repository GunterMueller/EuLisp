;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;  EuLisp Module                     Copyright (C) University of Bath 1991  ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule listops

  (
   lists macros0 extras0 calls ccc 
	 defs others generics classes arith
	 (except (null) (expose class-names))
	 (except (vector) (expose vectors)))

  (syntax

;;    (expose lists macros0 extras0 calls ccc defs others)

    (defmacro make-mapper (name combiner part)
      (labels (
        (selector () (if (eq part 'head) 'car 'progn)))
        `(defun ,name (f l1 . ls)
           (labels (
             (mapc1 (l)
               (unless (null l)
                 (,combiner (f (,(selector) l)) (mapc1 (cdr l)))))
             (mapc2 (l1 l2)
               (unless (or (null l1) (null l2))
                 (,combiner
                   (f (,(selector) l1) (,(selector) l2))
                   (mapc2 (cdr l1) (cdr l2)))))
             (mapc3 (l1 l2 l3)
               (unless (or (null l1) (null l2) (null l3))
                 (,combiner
                   (f (,(selector) l1) (,(selector) l2) (,(selector) l3))
                   (mapc3 (cdr l1) (cdr l2) (cdr l3)))))
             (parts-of (v n r)
               (if (= n 0)
                 (cons (,(selector) (vector-ref v 0)) r)
                 (parts-of v (- n 1) (cons (,(selector) (vector-ref v n)) r))))
             (bump-cdr (v n)
               (cond
                 ((= n 0)
                  ((setter vector-ref) v 0 (cdr (vector-ref v 0))))
                 (((setter vector-ref) v n (cdr (vector-ref v n)))
                  (bump-cdr v (- n 1)))
                 (t ())))        
             (mapcn (v)
               (,combiner
                 (apply f (parts-of v (- (vector-length v) 1) ()))
                 (when (bump-cdr v (- (vector-length v) 1)) (mapcn v)))))
           (let ((len (list-length ls)))
             (cond
               ((= len 0) (mapc1 l1))
               ((= len 1) (mapc2 l1 (car ls)))
               ((= len 2) (mapc3 l1 (car ls) (cadr ls)))
               (t (mapcn (make-initialized-vector (cons l1 ls))))))))))

  )

  (expose listops)

  (defcondition listops-error ())

  (make-mapper mapc progn head)

  (make-mapper mapcar cons head)

  (make-mapper mapcan nconc head)

  (make-mapper mapl progn tail)

  (make-mapper maplist cons tail)

  (make-mapper mapcon nconc tail)

  (defun atom (x) (not (consp x)))

  (defun append (l1 l2)
    (if (atom l1) l2 (cons (car l1) (append (cdr l1) l2))))

  (defun assq (k l)
    (cond
      ((null l) ())
      ((eq k (caar l)) (car l))
      (t (assq k (cdr l)))))

  (defun assoc (k l . p)
    (labels 
      ((assoc1 (l)
        (cond
          ((null l) ())
	  ((equal k (caar l)) (car l))
	  (t (assoc1 (cdr l)))))
       (assoc2 (l)
	(cond
          ((null l) ())
	  ((p k (caar l)) (car l))
	  (t (assoc2 (cdr l))))))
       (if (null p)
           (assoc1 l)
           (progn (setq p (car p)) (assoc2 l)))))

  (defun copy-alist (lst)
    (if (null lst) ()
      (cons (cons (caar lst) (cdar lst)) (copy-alist (cdr lst)))))

  (defun copy-list (l)
    (labels 
      ((copy-list-aux (l new)
        (if (atom l) (nconc new l)
          (copy-list-aux (cdr l) (nconc new (cons (car l) ()))))))
      (copy-list-aux l ())))

  (defun last-car (l)
    (cond
      ((atom l) ())
      ((atom (cdr l)) (car l))
      (t (last-car (cdr l)))))

  (defun last-pair (l)
    (cond
      ((atom l) ())
      ((atom (cdr l)) l)
      (t (last-pair (cdr l)))))

  (defun list-tail (lst n)
    (labels 
      ((list-tail-aux (lst n)
        (if (> n 0) (list-tail-aux (cdr lst) (- n 1)) lst)))
      (list-tail-aux lst (- (list-length lst) n)))) 

  (defun list-ref (list n)
    (if (equal n 0) (car list)
      (list-ref (cdr list) (- n 1))))

  ((setter setter) list-ref
    (lambda (list n obj)
      (if (= n 0) ((setter car) list obj)
	(list-ref (cdr list) (- n 1)))))

  (defun memq (i l)
    (cond
      ((atom l) ())
      ((eq i (car l)) l)
      (t (memq i (cdr l)))))

  (defun member (i l . p)
    (labels (
      (member1 (l)
         (cond
           ((atom l) ())
           ((equal i (car l)) l)
           (t (member1 (cdr l)))))
      (member2 (l)
        (cond
          ((atom l) ())
          ((p i (car l)) l)
          (t (member2 (cdr l))))))
      (if (null p)
          (member1 l)
          (progn (setq p (car p)) (member2 l)))))

  (defun nconc (l1 l2) 
    (labels (
      (nconc1 (l)
        (cond
          ((atom l) l2)
          ((atom (cdr l))
           ((setter cdr) l l2)
           l1)
          (t (nconc1 (cdr l))))))
      (nconc1 l1)))
  
  (defun tconc (ptr elem)
    (setq elem (list elem))
    (cond
      ((not (consp ptr))
       (cons elem elem))
      ((null (cdr ptr))
       ((setter cdr) ptr elem)
       ((setter car) ptr elem)
       ptr)
      (t
       ((setter cdr) (cdr ptr) elem)
       ((setter cdr) ptr elem)
       ptr)))
  
  (defun lconc (ptr lst)
    (cond
      ((not (consp ptr))
       (cons lst (last-pair lst)))
      ((null (cdr ptr))
       ((setter cdr) ptr (last-pair lst))
       ((setter car) ptr lst)
       ptr)
      (t
       ((setter cdr) (cdr ptr) lst)
       ((setter cdr) ptr (last-pair lst))
       ptr)))
  
  (defun null (x) (null x))
  
  (defun reverse (l)
    (labels (
      (rev1 (l n)
        (if (null l) n (rev1 (cdr l) (cons (car l) n)))))
      (rev1 l ())))
  
  (defun nreverse (l)
    (labels (
      (nrev1 (l y)
        (if (atom l)
            y
            (nrev1 (cdr l) (progn ((setter cdr) l y) l)))))
      (nrev1 l ())))
  
  (defun deleteq (a b)
     (cond
        ((null b) nil)
        ((eq a (car b)) (cdr b))
        (t (let ((del (deleteq a (cdr b))))
  	    (if (eq del (cdr b)) b (cons (car b) del))))))
  
  (defun delete (a b . p)
    (labels (
      (delete1 (b)
        (cond
           ((null b) ())
           ((p a (car b)) (cdr b))
           (t (let ((del (delete1 (cdr b))))
                 (if (eq del (cdr b)) b (cons (car b) del)))))))
      (setq p (if (null p) equal (car p)))
      (delete1 b)))
  
  (defun substq (a b c)
     (cond
        ((eq c b) a)
        ((atom c) c)
        (t (let ((carc (substq a b (car c)))
                 (cdrc (substq a b (cdr c))))
  	   (if (and (eq carc (car c)) (eq cdrc (cdr c)))
                 c
  	       (cons carc cdrc))))))
  
  (defun subst (a b c . p)
    (labels (
      (subst1 (c)
        (cond
           ((p c b) a)
           ((atom c) c)
           (t (let ((carc (subst1 (car c)))
                    (cdrc (subst1 (cdr c))))
  	      (if (and (eq carc (car c)) (eq cdrc (cdr c)))
                    c
  	          (cons carc cdrc)))))))
      (setq p (if (null p) equal (car p)))
      (subst1 c)))
  
  (defun posq (i l)
    (labels
      ((position-aux (i l c)
         (cond
           ((null l) ())
           ((eq i (car l)) c)
           (t (position-aux i (cdr l) (+ 1 c))))))
      (position-aux i l 0)))
  
  (defun pos (i l . p)
    (labels
      ((position-aux (i l c)
         (cond
           ((null l) ())
           ((p i (car l)) c)
           (t (position-aux i (cdr l) (+ 1 c))))))
      (setq p (if (null p) equal (car p)))
      (position-aux i l 0)))

)

