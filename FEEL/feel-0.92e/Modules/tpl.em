(defmodule tpl (standard) ()

(defconstant *this-module* 'tpl)

(defclass paralation-internal ()
  ((size
    initarg size
    reader p-size)
   (index
    initarg index
    accessor index-internal)
   (shape
    initarg shape
    accessor shape-internal)
   (attributes
    initarg attributes
    accessor attr))
  constructor make-paralation-internal)

(defclass field ()
  ((value
   initarg value
   accessor value)
  (paralation
   initarg paralation
   accessor paralation))
  constructor make-field)

(defun make-paralation (size)
  (let ((new-paralation (make-paralation-internal
			 'size size
			 'index (make-index size))))
    ((setter paralation) (index-internal new-paralation) new-paralation)
    (index-internal new-paralation)))

(defun make-index (size) 
  (let* ((v (make-vector size 0))
	 (new-field (make-field `value v)))
    (labels ((init-field-value (i) 
			       (if (< i 0) nil
				 (progn 
				   ((setter vector-ref) v i i)
				   (init-field-value (- i 1))))))
	    (init-field-value (- size 1)))
    new-field))

(defun index (field)
  (index-internal (paralation field)))

(defmethod generic-prin ((o field) str)
  (format str "#F~a" (convert (value o) <cons>))
  o)

(defmethod generic-write ((o field) str)
  (format str "#F~a" (convert (value o) <cons>))
  o)

(defun mapvec (fn . v)
  (let* ((len (vector-length (car v)))
	 (new (make-vector len nil)))
    (labels 
     ((recurse (i)
        (if (< i 0) nil
	  (progn
	    ((setter vector-ref) new i (apply fn (mkarglst v i)))
	    (recurse (- i 1)))))
      (mkarglst (v i)
	(if v 
	  (cons (vector-ref (car v) i) (mkarglst (cdr v) i))
	  nil)))
     (recurse (- len 1)))
    new))

(defconstant *elwise-index* (gensym))
(defconstant *elt-result* (gensym))
(defconstant *field-vec* (gensym))

(defmacro elwise (fields . body)
  `(let ((,*field-vec* (make-initialized-vector ,@fields)))
     (make-field
      'paralation (paralation ,(car fields))
      'value (apply mapvec 
	     (list
	       (lambda (,*elwise-index* ,@fields)
		 (let ((,*elt-result* ,@body))
		   ,@(do-side-effects fields 0)
		    ,*elt-result*))	
	       (value (index ,(car fields)))
	       ,@(mapcar (lambda (name) `(value ,name)) fields))))))

;(defmacro elwise (fields . body)
;  `(elwise-fun (list ,@fields) (lambda ,fields ,@body))
;)

;(defun elwise-fun (fields lambda-expr)
;  (make-field
;   'paralation (paralation (car fields))
;   'value (apply mapvec
;		 (cons lambda-expr 
;		       (mapcar (lambda (f) (value f)) fields))))
;)

(defun do-side-effects (nl i)
  (if (null nl) nil
    (cons `((setter vector-ref)
	    (value (vector-ref ,*field-vec* ,i))
	    ,*elwise-index*
	    ,(car nl))
	  (do-side-effects (cdr nl) (+ i 1)))))

(defclass mapping ()
  ((from-key
    initarg from-key
    accessor from-key)
   (to-key
    initarg to-key
    accessor to-key))
  constructor make-mapping)

(defun match (from to)
  (make-mapping 
    'from-key (elwise (from) from)
    'to-key (elwise (to) to)))

(defun move (field map with default)
  (let ((key-hash-table (make-table)))
    (labels
      ((key-to-table (from data)
	 (elwise (from data)
	   ((setter table-ref) key-hash-table from
	      (combine with (table-ref key-hash-table from) data))))
       (table-to-key (to)
	 (elwise (to)
	   (if (table-ref key-hash-table to)
	     (table-ref key-hash-table to)
	     default)))
       (combine (fn old new)
	 (if old (if new (fn old new) old)
	   new)))
      (key-to-table (from-key map) field)
      (table-to-key (to-key map)))))

(defun inverse (map)
  (let ((p (index (from-key map))))
    (match (move p map cons nil) p))
)

(defun field-ref (field i)
  (vector-ref (value field) i))

(defun field-length (field)
  (p-size (paralation field)))

(defun vref (field fn)
  (field-ref 
    (move field (match (elwise (field) 0) (make-paralation 1)) fn nil) 0))

(defun shape (field) (shape-internal (paralation field)))

(defun set-shape (field value)
  (progn 
    ((setter shape-internal) (paralation field) value)
    field))

(defun shape-class (field)
  (class-of (field-ref (shape field) 0)))

(defun attributes (field) (attr (paralation field)))

(defun set-attributes (field value) 
  (progn
    ((setter attr) (paralation field) value)
    field))

(defun get (accesor field default)
  (let* ((g-field (shape field))
	 (get-key (elwise (g-field)
		  (accesor g-field))))
    (move field (match (index field) get-key) cons default)))

(defun put (accessor field default)
  (let* ((g-field (shape field))
	 (get-key (elwise (g-field)
		    (accessor g-field))))
    (move field (match get-key (index field)) cons default)))

(defun field-p (object)
  (subclassp (class-of object) field))
 
(export field make-paralation index elwise mapping match move inverse
	field-ref field-length vref shape set-shape shape-class
	attributes set-attributes get put field-p make-field 
	do-side-effects mapvec paralation value
	elwise-fun)
)
