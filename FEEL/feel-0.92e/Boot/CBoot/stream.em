;; Eulisp Module
;; Author: pab
;; File: stream.em
;; Date: Sun Jun  6 18:06:33 1993
;;
;; Project:
;; Description: 
;;

(defmodule stream
  (extras0
   macros0
   numbers
   defs
   telos1
   init
   copy
   )
  ()

  (export generic-prin output open popen flush 
	  close prin print write newline input uninput
	  read-line end-of-stream-p stream-position
          <stream> <char-file-stream> <string-stream>
	  streamp file-stream-p character-stream-p
	  <inappropriate-stream-position> <unexpected-end-of-stream>)
  
  (export scan format)

  (defcondition <unexpected-end-of-stream> <stream-error>)

  (defun default-eos-action (s)
    (error "end of stream" <unexpected-end-of-stream>
           'error-value (if (file-stream-p s)
			    (file-stream-name s)
			    s)))

  (defstruct <stream> ()
    ((action initarg action
	     initform default-eos-action
	     accessor stream-action))
    predicate streamp)

  (defstruct <char-file-stream> <stream>
    ((file accessor file-stream-file)
     (name initarg name accessor file-stream-name)
     (mode accessor file-stream-mode))
    constructor (make-file-stream name action)
    )

  (defstruct <pipe-stream> <char-file-stream>
    ()
    constructor (make-pipe-stream name action)
    )

  ;; generic operations
  (defgeneric close (stream))
  (defgeneric stream-position (stream))
  (defgeneric (setter stream-position) (stream val))
  ;;(defgeneric read (stream))
  (defgeneric input (stream))
  (defgeneric uninput (stream obj))
  (defgeneric read-line (stream))

  (defgeneric file-stream-p (obj))
  (defgeneric character-stream-p (obj))

  (defmethod file-stream-p ((obj <object>)) ())
  (defmethod character-stream-p ((obj <object>)) ())

  (defmethod file-stream-p ((obj <char-file-stream>)) obj)
  (defmethod character-stream-p ((obj <char-file-stream>)) obj)

  ;; Opening a file
  (defun remember (name opts)
    (cond ((atom opts) ())
	  ((eq name (car opts)) (cadr opts))
	  (t (remember name (cdr opts)))))
	  
  (defun open (name . options)
    (let* ((mode (find-mode options))
	   (action (remember 'eos-action options))
	   (new (make-file-stream (convert name <string>)
				  (if action
				      action
				      default-eos-action))))
      ((setter file-stream-file) new (fopen name mode))
      ((setter file-stream-mode) new mode)
      new))

  (defun popen (name . options)
    (let* ((mode (find-mode options))
	   (action (remember 'eos-action options))
	   (new (make-pipe-stream (convert name <string>)
				  (if action
                                      action
                                      default-eos-action))))
      ((setter file-stream-file) new (fpopen name mode))
      ((setter file-stream-mode) new mode)
      new))
  
  (defun find-mode (options)
    (if (null options)
	"r"
      (let ((read-flag (memq 'input options))
	    (write-flag (memq 'output options))
	    (update-flag (memq 'update options))
	    (append-flag (memq 'append options)))
	(cond ((and append-flag update-flag) "a+")
	      (append-flag "a")
	      ((and write-flag (or read-flag update-flag)) "r+")
	      (write-flag "w")
	      (read-flag "r")
	      (t "r")))))

  ;; Writing to a raw file

  (defmethod output ((x <char-file-stream>) o)
    (fput (file-stream-file x) o))

  ;; Sort out standard streams

  ;; reading
  ;; Reading _should_ invoke the input method.
  ;; unfortunately this means much hacking with lex.

  (defmethod generic-read ((stream <char-file-stream>))
    (let ((val (fread (file-stream-file stream))))
      (if (eq val *eof*)
	  ((stream-action stream) stream)
	  val)))
  
  (defmethod input ((stream <char-file-stream>))
    (fread-char (file-stream-file stream)))

  (defmethod uninput ((stream <char-file-stream>) (c <character>))
    (fungetc (file-stream-file stream) c))

  (defun end-of-stream-p (stream)
    (let ((c (input stream)))
      (if (eq c *eof*) t
	(progn (uninput stream c)
	       nil))))

  (defmethod read-line ((s <char-file-stream>))
    (let ((val (fread-line (file-stream-file s))))
      (if (eq val *eof*)
	  ((stream-action s) s)
	  val)))
  
  ;; default method...
  (defmethod read-line ((s <object>))
    (labels ((aux1 (n l)
 		   (let ((c (input s)))
		     (if (eq c #\newline)
			 (let ((string (make-string (+ n 1))))
			   (labels ((aux2 (n l)
					  (if (null l) nil
					    (progn ((setter string-ref) string n (car l))
						   (aux2 (- n 1) (cdr l))))))
				   (aux2 (- n 1) l))
			   ((setter string-ref) string n #\newline)
			   string)
		       (aux1 (+ n 1) (cons c l))))))
	    (aux1 0 nil)))


  ;; Others..
  (defcondition <inappropriate-stream-position> <stream-error>)

  (defmethod (setter stream-position) ((stream <char-file-stream>) pos)
    (when (< pos 0)
	  (error "before start of file" <inappropriate-stream-position>))
    (fseek (file-stream-file stream)
	   (if (eq pos 'stream-end) -1 pos)))

  (defmethod stream-position ((stream <char-file-stream>))
    (ftell (file-stream-file stream)))

  (defmethod close ((stream <char-file-stream>))
    (fclose (file-stream-file stream)))

  (defmethod flush ((stream <char-file-stream>))
    (fflush (file-stream-file stream)))
  
  (defun prin (x . s)
    (generic-prin x (if (null s)
			(standard-output-stream)
		      (car s))))
  
  (defun print (x . s)
    (let ((s (if (null s)
			(standard-output-stream)
		      (car s))) )
      (generic-prin x s)
      (generic-prin #\newline s))
    x)
  
  (defun write (x . s)
    (generic-write x (if (null s)
			(standard-output-stream)
		      (car s))))
  
  (defun newline s
    (prin #\newline (if s (car s) (standard-output-stream))))


  ;; Simple printing
  
  (defmethod generic-prin ((x <class>) s)
    (format s "#<~a: ~a>" 
	    (symbol-unbraced-name (class-name (class-of x)))
	    (symbol-unbraced-name (class-name x)))
    x)
 
  (defmethod generic-prin ((x <cons>) s)
    (print-list x s))

  (defmethod generic-prin ((x <null>) s)
    (generic-prin "()" s)
    nil)
  
  (defmethod generic-prin ((x <symbol>) s)
    (generic-prin (symbol-name x) s)
    x)

  (defmethod generic-prin ((x <string>) s)
    (output s x))

  (defmethod generic-prin ((x <integer>) s)
    (print-fixnum x s) x)

  (defmethod generic-prin ((x <double-float>) s)
    (format s "~g" x)
    x)

  (defmethod generic-prin ((x <character>) s)
    (if (eq x *eof*)
	(generic-prin "<<EOF>>" s)
      (output s x)))

  (defmethod generic-prin ((x <vector>) s)
    (generic-prin "#(" s)
    (let ((l (length x)))
      (cond ((= l 0)
	     nil)
	    (t 
	     (labels ((aux (n)
			   (if (= n (length x)) nil
			     (progn (generic-prin " " s)
				    (generic-prin (vector-ref x n) s)
				    (aux (+ n 1))))))
		     (generic-prin (vector-ref x 0) s)
		     (aux 1)))))
    (generic-prin ")" s))

  (defmethod generic-prin ((x <generic-function>) stream)
    (format stream "#<~a: ~a>" (symbol-unbraced-name (class-name (class-of x))) (generic-name x)))

  (defmethod generic-prin ((x <method>) stream)
    (format stream "#<method: ~a (~a)>" 
	    (if (null (method-generic-function x)) 
		"{unattached}"
	      (generic-name (method-generic-function x)))
	    (mapcar class-name (method-signature x))))

  (defmethod generic-prin ((x <i-function>) s)
    (format s "#<~a: (lambda ~a~l) @ ~a>"
	    (symbol-unbraced-name (class-name (class-of x)))
	    (function-lambda-list x)
	    (lambda (l s)
	      (mapc (lambda (o)
		      (generic-prin " " s)
		      (generic-write o s))
		    l))
	    (i-function-body x)
	    (primitive-slot-ref-0 (function-home x))))

  (defmethod generic-prin ((x <c-function>) s)
    (format s "#<~a: ~a ~a @ ~a>"
	    (symbol-unbraced-name (class-name (class-of x)))
	    (function-name x)
	    (let ((ll (function-lambda-list x)))
	      (if (consp ll)
		  (if (consp (last-pair ll))
		      (length ll)
		    (- (list-length ll) 1))
		-1))
	    (primitive-slot-ref-0 (function-home x))))
	
  (defmethod generic-prin ((x <object>) s)
    (format s "#<~a: ~u>" 
	    (symbol-unbraced-name (class-name (class-of x)))
	    x)
    x)

  ;; generic-write

  (defmethod generic-write ((x <null>) s)
    (generic-prin x s))

  (defun print-escaped-sym (x s)
    (generic-prin "|" s)
    (labels ((aux (n m)
		  (if (= n m) nil
		    (let ((c (string-ref x n) s))
		      (when (or (eq c #\\) (eq c #\|))
			(generic-prin #\\ s))
		      (generic-prin c s)
		      (aux (+ n 1) m)))))
	    (aux 0 (length x)))
    (generic-prin "|" s))
    

  (defmethod generic-write ((x <symbol>) s)
    (let ((name (symbol-name x)))
      (if (escaped-id-p name) 
	  (print-escaped-sym name s)
	(generic-prin name s))))
  
  (defmethod generic-write ((x <vector>) s)
    (generic-prin "#(" s)
    (let ((l (length x)))
      (cond ((= l 0)
	     nil)
	    (t 
	     (labels ((aux (n)
			   (if (= n (length x)) nil
			     (progn (generic-prin " " s)
				    (generic-write (vector-ref x n) s)
				    (aux (+ n 1))))))
		     (generic-write (vector-ref x 0) s)
		     (aux 1)))))
    (generic-prin ")" s))
  
  (defmethod generic-write ((l <cons>) s)
    (generic-prin "(" s)
    (labels ((aux (l)
		  (cond ((consp (cdr l))
			 (generic-write (car l) s)
			 (generic-prin " " s)
			 (aux (cdr l)))
			((null (cdr l))
			 (generic-write (car l) s)
			 (generic-prin ")" s))
			(t (generic-write (car l) s)
			   (generic-prin " . " s)
			   (generic-write (cdr l) s)
			   (generic-prin ")" s)))))
	    (aux l)))

  (defconstant escape-table (make-table))

  ; order important in the chars here
  (labels ((aux (l)
		(if (null l) nil
		  (progn ((setter sys-table-ref) escape-table
			  (car (car l)) (cdr (car l)))
			 (aux (cdr l))))))
	  (aux '((#\^@ . ^@) (#\^a . ^a) (#\^b . ^b) (#\^c . ^c) (#\^d . ^d)
		 (#\^e . ^e) (#\^f . ^f) (#\^g . ^g) (#\^h . ^h) (#\^i . ^i)
		 (#\^j . ^j) (#\^k . ^k) (#\^l . ^l) (#\^m . ^m) (#\^n . ^n)
		 (#\^o . ^o) (#\^p . ^p) (#\^q . ^q) (#\^r . ^r) (#\^s . ^s)
		 (#\^t . ^t) (#\^u . ^u) (#\^v . ^v) (#\^w . ^w) (#\^x . ^x)
		 (#\^y . ^y) (#\^z . ^z) (#\^[ . ^[) (#\^\ . ^\\)
		 (#\^] . ^]) (#\^^ . ^^) (#\^_ . ^_)
		 (#\tab . tab) (#\space . space) (#\return . return)
		 (#\alert . alert) (#\backspace . backspace)
		 (#\delete . delete) (#\formfeed . formfeed)
		 (#\linefeed . linefeed) (#\newline . newline)
		 (#\vertical-tab . vertical-tab))))
  
  (defmethod generic-write ((c <character>) s)
    (generic-prin "#\\" s)
    (if (= (convert c <integer>) -1)	; hack!
	(generic-prin "eof" s)
        (let ((esc (sys-table-ref escape-table c)))
	  (if esc
	      (generic-prin esc s)
	      (generic-prin c s)))))

  (defmethod generic-write ((c <character>) s)
    (generic-prin "#\\" s)
    (let ((n (convert c <integer>)))
      (if (< n 0) 
	  (progn (generic-prin "x00" s)
		 (generic-prin (dec2hex (+ n 256)) s))
	  (let ((esc (sys-table-ref escape-table c)))
	    (if esc
		(generic-prin esc s)
	        (generic-prin c s))))))

  (defconstant string-escapes (make-table))
  (mapc (lambda (l)
	    ((setter sys-table-ref) string-escapes (car l) (cdr l)))
	  '((#\\ . "\\\\") (#\" . "\\\"") (#\newline . "\\n")
	    (#\alert . "\\a") (#\backspace . "\\b")
	    (#\delete . "\\d") (#\formfeed . "\\f")
	    (#\linefeed . "\\n") (#\return . "\\r")
	    (#\tab . "\\t") (#\vertical-tab . "\\v")))
		
  (defconstant hex '(0 1 2 3 4 5 6 7 8 9 a b c d e f))

  ; hacky
  (defun dec2hex (val)
    (format nil "~s~s"
	    (list-ref hex (/ val 16))
	    (list-ref hex (remainder val 16))))

  (defun find-esc (c)
    (let ((val (convert c <integer>)))
      (cond ((< val 0)
	     (format nil "\\x00~a" (dec2hex (+ 256 val))))
	    ((sys-table-ref string-escapes c))
	    ((or (< val 32) (> val 127))
	     (format nil "\\x00~a" (dec2hex val)))
	    (t ()))))

  (defun print-escaped-string (s stream)
    (labels ((aux (n m)
		  (if (= n m)
		      nil
		    (let* ((c (string-ref s n))
			   (esc (find-esc c)))
		      (generic-prin 
		       (if (null esc) c
			 esc)
		       stream)
		      (aux (+ n 1) m)))))
	    (aux 0 (length s))))
			
  (defmethod generic-write ((s <string>) stream)
    (generic-prin "\"" stream)
    (if (escaped-id-p s)
	(print-escaped-string s stream)
      (generic-prin s stream))
    (generic-prin "\"" stream)
    s)

  ;; Format...
  (defun a-formatter (stream args other)
    ;;(GC)
    (if (null args)
	(error "format ~a: insufficient args" <format-error>)
      (progn (generic-prin (car args) stream)
	     (cdr args))))

  (defun s-formatter (stream args other)
    (if (null args)
	(error "format ~s: insufficient args" <format-error>)
      (progn (generic-write (car args) stream)
	     (cdr args))))

  (defun d-formatter (stream args other)
    (if (null args)
	(error "format ~d: insufficient args" <format-error>)
      (progn (print-fixnum (car args) stream)
	     (cdr args))))

  (defun l-formatter (stream args other)
    (if (or (null args) (null (cdr args)))
	(error "format ~l: insufficient args" <format-error>)
      (progn ((car args) (cadr args) stream)
	     (cddr args))))

  (defun t-formatter (stream args other)
    (labels ((ntabs (n)
	       (when (> n 0)
		 (generic-prin #\tab stream)
		 (ntabs (- n 1)))))
      (ntabs (if (null other) 1 (car other))))
    args)

  (defun c-formatter (stream args other)
    (if (null args)
        (error "format ~c: insufficient args" <format-error>)
	(progn (generic-write (car args) stream)
	       (cdr args))))

  (defun simple-formatter (c)
    (lambda (stream args other)
	(progn (output stream c)
	       args)))

  ((setter formatter) #\a a-formatter)
  ((setter formatter) #\A a-formatter)
  ((setter formatter) #\b b-formatter)
  ((setter formatter) #\B b-formatter)
  ((setter formatter) #\c c-formatter)
  ((setter formatter) #\C c-formatter)
  ((setter formatter) #\d d-formatter)
  ((setter formatter) #\D d-formatter)
  
  ((setter formatter) #\e e-formatter)
  ((setter formatter) #\E e-formatter)
  ((setter formatter) #\f f-formatter)
  ((setter formatter) #\F f-formatter)
  ((setter formatter) #\g g-formatter)	; not standard
  ((setter formatter) #\G g-formatter)

  ((setter formatter) #\l l-formatter)	; not standard
  ((setter formatter) #\L l-formatter)

  ((setter formatter) #\o o-formatter)
  ((setter formatter) #\O o-formatter)
  ((setter formatter) #\r r-formatter)
  ((setter formatter) #\R r-formatter)
  ((setter formatter) #\s s-formatter)
  ((setter formatter) #\S s-formatter)
  ((setter formatter) #\u u-formatter)	; not standard
  ((setter formatter) #\U u-formatter)
  ((setter formatter) #\t t-formatter)
  ((setter formatter) #\T t-formatter)
  ((setter formatter) #\x x-formatter)
  ((setter formatter) #\X x-formatter)
  ((setter formatter) #\% (simple-formatter #\newline))
  ((setter formatter) #\| (simple-formatter #\formfeed)) ;not standard
  ;; actually wrong (in some ways...)
  ((setter formatter) #\& (simple-formatter #\newline))
  
  ((setter formatter) #\~ (simple-formatter #\~))
  

  ;; Output side done
  ;; Set things up
  (defun reopen (name fd mode)
    (let ((new (make-file-stream name (lambda (s) *eof*))))
      ((setter file-stream-file) new (freopen fd))
      ((setter file-stream-mode) new mode)
      new))

  ((setter standard-input-stream) (reopen "stdin" 0 "r"))
  ((setter standard-output-stream) (reopen "stdout" 1 "w"))
  ((setter standard-error-stream) (reopen "stderr" 2 "w"))

  ;; Scan...
  (defun substring-copy (sdest ssource start len)
    (labels ((aux (n m)
		  (if (= n m) nil
		    (progn ((setter string-ref) sdest n 
			    (string-ref ssource (+ n start)))
			   (aux (+ n 1) m)))))
	    (aux 0 len)))

  (defconstant whitespace-tab (make-table))
  (mapcar (lambda (c)
	    ((setter sys-table-ref) whitespace-tab c t))
	  '(#\space #\tab #\newline #\return))

  (defun whitespacep (c)
    (sys-table-ref whitespace-tab c))

  (defconstant scan-fns (make-table))
  (defun scanner (c)
    (sys-table-ref scan-fns c))

  ((setter setter) scanner
   (lambda (c x)
     ((setter sys-table-ref) scan-fns c x)))

  (defgeneric scan (stream s))
  
  (defmethod scan ((x <null>) s)
    (scan (standard-input-stream) s))

  (defmethod scan ((x <symbol>) s)
    (if (eq x t)
	(scan (standard-input-stream) s)
      (error "scan: illegal stream" <format-error>)))

  (defun find-scanner (pat n)
    (if (= n (length pat))
	(error "scan: ~ at end of string" <scan-mismatch>)
      (let ((fn (scanner (string-ref pat n))))
	(if (null fn)
	    (error "Scan: unknown scan directive" <scan-mismatch> 'error-value (string-ref pat n))
	  fn))))
  
  (defmethod scan ((s <object>) pat)
    (labels ((aux (last n m result)
		  (cond ((= n m)
			 (scan-literal-string last m)
			 (reverse-list result))
			((whitespacep (string-ref pat n))
			 (eat-whitespace s)
			 (format t "lit: ~s~%" (if (= last n) "" (substring pat last (- n 1))))
			 (scan-literal-string last n)
			 (aux (+ n 1) (+ n 1) m result))
			((eq (string-ref pat n) #\~)
			 (scan-literal-string last n)
			 (let ((scanner (find-scanner pat (+ n 1)) ))
			    (if (null (car scanner))
				(progn (handle-errors (cdr scanner) s result)
				      (aux (+ n 2) (+ n 2) m result))
			     (aux (+ n 2) (+ n 2) m 
				  (cons (handle-errors (cdr scanner) s result) result)))))
			(t (aux last (+ n 1) m result))))
	     (scan-literal-string (start end)
				  (if (= start end) t
				    (let ((c (input s)))
				      (if (eq c (string-ref pat start))
					  (scan-literal-string (+ start 1) end)
					(progn (uninput s c)
					       (error "scan: literal mismatch" <scan-mismatch> )))))))
	    (aux 0 0 (length pat) nil)))
  
  (defun handle-errors (fn stream val)
    (with-handler (lambda (cond cont)
		    ((setter condition-error-value) 
		     cond val))
		  (fn stream)))

  (defun eat-whitespace (stream)
    (let ((c (input stream)))
      (if (whitespacep c)
	  (eat-whitespace stream)
        (uninput stream c))))

  ;; Coded for speed of coding...
  
  ;; buggy: assumes A<a
  (defun make-digit (char)
    (let ((code (convert char <integer>)))
      (cond ((> code (convert #\a <integer>))
	     (- code (convert #\a <integer>)))
	    ((> code (convert #\A <integer>))
	     (- code (convert #\A <integer>)))	  
	    (t (- code (convert #\0 <integer>))))))
	     
  (defun base-scanner (base floatp)
    (lambda (stream)
      (labels ((aux (acc div)
		    (let* ((c (input stream))
			   (digit (make-digit c)))
		      (cond ((and (< digit base)
				  (> digit -1))
			     (if (= div 0)
				 (aux (+ (* acc base) digit) 0)
			       (aux (+ acc (/ digit div)) 
				    (* div base))))
			    ((and (eq c #\.) floatp (= div 0))
			     (aux (convert acc <double-float>)
				  (convert base <double-float>)))
			    (t (progn (uninput stream c)
				      acc)))))
	       (scan-+ve-int (c)
			     (let ((digit-1 (make-digit c)))
			       (if (and (< digit-1 base)
					(> digit-1 -1))
				   (aux digit-1 0)
				 (progn (uninput stream c)
					(error "No digit on stream" <scan-mismatch>)))))
	       (scan-int ()
			 (let ((c (input stream)))
			   (if (eq c #\-)
			       (- (scan-+ve-int (input stream)))
			     (scan-+ve-int c)))))
	      (eat-whitespace stream)
	      (scan-int))))
  
  (defun float-scanner (stream)
    ((base-scanner 10 t) stream))

  ;; assumed to be float..
;  (defun pow (val n)
;    (exp (* n (log val))))
 
  (defun s-scanner (stream)
    (labels ((aux (l)
		  (let ((c (input stream)))
		    (if (whitespacep c)
			(progn (uninput stream c)
			       (fold (lambda (c s)
				       (string-append (convert c <string>)
						      s))
				     l
				     ""))
		      (aux (cons c l))))))
	    (eat-whitespace stream)
	    (aux nil)))

  (defun newline-scanner (s)
    (let ((c (input s)))
      (cond ((eq c #\newline) t)
	    ((eq c *eof*)
	     nil)
	    ((whitespacep c)
	     (newline-scanner s))
	    (t (uninput s c)
	       (error "scan: expected newline" <scan-mismatch> 'error-value c)))))
  
  (defun r-scanner (stream)
    (let ((radix (scan stream "~dr")))
      (let ((v2 ((base-scanner radix nil) stream)))
	v2)))
  
  (defun c-scanner (stream)
    (input stream))

  (defun returning (x) (cons t x))
  (defun non-returning (x) (cons nil x))
  
  ((setter scanner) #\b (returning (base-scanner 2 nil)))
  ((setter scanner) #\c (returning c-scanner))
  ((setter scanner) #\d (returning (base-scanner 10 nil)))
  ((setter scanner) #\f (returning (base-scanner 10 t)))
  ((setter scanner) #\o (returning (base-scanner 8 nil)))
  ((setter scanner) #\r (returning r-scanner))
  ((setter scanner) #\s (returning s-scanner))
  ((setter scanner) #\t (non-returning s-scanner))
  ((setter scanner) #\x (returning (base-scanner 16 nil)))
  ((setter scanner) #\% (non-returning newline-scanner))

  ;; For format nil...
  (defstruct <string-stream> <stream>
    ((content initform nil accessor string-stream-content)
     (size initform 0 accessor string-stream-size))
    constructor make-string-stream)

  (defmethod character-stream-p ((obj <string-stream>)) obj)

  (defmethod output ((x <string-stream>) s)
    (let ((s (if (stringp s) (shallow-copy s)
	       (convert s <string>))))
      ((setter string-stream-content) x
       (cons s (string-stream-content x)))
      ((setter string-stream-size) x 
       (+ (string-stream-size x) (length s)))
      s))

  (defmethod close ((s <string-stream>)) t)

  ;; should be clever and use length field...
  ;; unfortunately, I dont have string-subrange-copy.
  
  (defmethod (converter <string>) ((x <string-stream>))
    (let ((val (fold (lambda (sub str)
		       (string-append sub str))
		     (string-stream-content x)
		     "")))
      ((setter string-stream-content) x ())
      val))

  ((setter format-string-stream-class) <string-stream>)

  (defun test (x)
    (let ((s1  (read-line x)))
      (format t "test string '~a'~%" s1)
      (scan x s1)))
  )


Scan behaviour:

Raise an error on illegal input. Error value is things read in. number reading terminates on an illegal char.
char pointer is left on the char that forced termination. Return value is list of values read.

Formats accepted: 
~b: binary number
~c: Character [may read a whitespace]
~d: decimal number
~f: floating point (e-notation not supported currently)
~o: octal number
~s: string, delimited by whitespace
~t: Read a string, and dump it
~x: hexadecimal number
~%: Match against a newline

