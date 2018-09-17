;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; newschemedef.em
; Full Scheme definition module
; DDeR
; Last change
; Sat Nov 24 15:29:39 GMT 1990

; NB This file is written in EuLisp.  Beware that some Scheme
; functions are visible as they are renamed on import, others 
; because they are defined here, but they shouldn't be used!  
; In principle, the renaming can occur on export.

; BUGS:
; characters module not imported, when it is the functions don't exist
; mapcar doesn't exist

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule schemedefs

  (;(import characters)

           ; Broken now...

   (except (error 
	    memq read read-char peek-char	; for V0.37
	    let
	    substring string vector string-append
	    char-upcase char-downcase vector write-char
	    last-pair
	    let string vector do map
	    ) 
	   (rename (
		    ;; NB new names are exported at the end of this module
		    ;;(characterp char?)				not exported in V0.37
		    ;;(functionp procedure?)			missing in V0.37
		    (character-to-integer char->integer) ; wrong name in V0.37
		    (char-equal char=?)
		    (char< char<?)
		    (char> char>?)
		    (char<= char<=?)
		    (char>= char>=?)
		    (char-upcase feel-char-upcase)
		    (char-downcase feel-char-downcase)
					;(character->integer char->integer)	        ; instead
		    (consp pair?)
		    (symbolp symbol?)
		    (stringp string?)
		    (vectorp vector?)
		    (numberp number?)
		    (eq eq?) 
		    (equal equal?) 
		    (evenp even?)
		    (integer-to-character integer->char) ; wrong name in V0.37
					;(integer->character integer->char)	        ; instead
					;(labels letrec) Bogus!!!
;		    (last-pair last)	; broken on improper lists in V7.04
					;(list-length length)
					;(list-to-string list->string)
					;(mapcar map)
		    (negativep negative?)
		    (null null?)
		    (nconc append!)
					;(number-to-string number->string) 		missing in V0.37
		    (oddp odd?) 			
		    (output-stream-p output-port?)
		    (positivep positive?)
		    (prin display)
		    (standard-input-stream current-input-port)
		    (standard-output-stream current-output-port)
		    (string-append feel-string-append)
					;(string-slice substring)	already called substring in V0.37
		    (substring feel-substring)
		    (symbol-name symbol->string)
		    (make-symbol string->symbol)
		    (write-char feel-write-char)
		    (zerop zero?)

					; these are name clashes and so we prefix them with eulisp-

		    (error eulisp-error)

					; these are V0.37 misfeatures

		    (memq old-memq)	;			misfeature in V0.37
		    (read old-read)	;			misfeature in V0.37
		    (read-char old-read-char) ;		misfeature in V0.37
		    (peek-char old-peek-char) ;		misfeature in V0.37

		    )
		   characters eulisp0)))

  ()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (defmacro iterate (name binds . body)
    `(labels
       ((,name ,(mapcar (lambda (x) (car x)) binds) ,@body))
       (,name ,@(mapcar (lambda (x) (cadr x)) binds))))

  (defmacro let (binds . body)
    (if (symbol? binds)
      `(iterate ,binds ,@body)
      `((lambda ,(map car binds) ,@body)
	,@(map cadr binds))))

  (defmacro case (key . clauses)
    (let ((keyvar '@case-keyvar@))
      `(let ((,keyvar ,key))
	 (cond
	  ,@(map
	     (lambda (clause)
	       (let ((op (car clause))
		     (rest (cdr clause)))
		 (cond
		  ((eq? op 'else) clause)
		  (else
		   (let ((items (if (pair? op) op (list op))))
		     `((or ,@(map (lambda (th)
				       `(eqv? ',th ,keyvar))
				     items))
		       ,@rest))))))
	     clauses)))))

  (defmacro letrec (binds . body)
    `(let ,(map 
	     (lambda (bind) 
	       `(,(car bind) '()))
	     binds)
       ,@(map
	   (lambda (bind)
	     `(set! ,(car bind) ,@(cdr bind)))
	   binds)
       ,@body))

  (defun filter (pred l)
    (cond
      ((null? l) '())
      ((pred (car l)) (cons (car l) (filter pred (cdr l))))
      (t (filter pred (cdr l)))))
        
  (defmacro do (binds condn . body)
    (let ((constant (filter (lambda (bind) (= (length bind) 2)) binds))
	  (stepped (filter (lambda (bind) (= (length bind) 3)) binds)))
      `(let ,constant
	 (let do-loop 
	      ,(map 
		 (lambda (bind) (list (car bind) (cadr bind)))
		 stepped)
	   (if ,(car condn) (begin ,@(cdr condn))
	     (begin
	       ,@body
	       (do-loop 
		 ,@(map (lambda (bind) (caddr bind)) stepped))))))))

  (export let iterate case letrec labels do)

  (deflocal map mapcar)

  (export mapcar)

;;;;;;;;;;;;;;;;;;;;; PATCH SECTION for V0.37 ;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant eof (list '<eof>))

; we now cheat in the standard lexer
(defun open-unschemed-input-file (file)
  (open-input-file file))

;(defun open-unschemed-input-file (file)
;  (popen (format () "/opt/home/kjp/Bin/unscheme < ~a" file)
;	 'input
;	 'eos-action (lambda (s) eof)))

(export open-unschemed-input-file)

(deflocal *true* t)
(deflocal *false* '())

(export *true* *false*)

(defun eqv? (x y)
  (or (eq? x y)
      (and (characterp x) (characterp y) (eq? x y))
      (and (number? x) (number? y) (= x y))))

(export eqv?)
(export negative?)

(defun sorry dummy 
  (eulisp-error "Sorry - unimplemented EuLisp function" schemedef-error))

; These are in EuLisp but are missing in V0.37

; (defun functionp (x) (eq (class-of x) function))
;(defun characterp (x) (eq (class-of x) character))


;(defun abs (x) (if (< x 0) (- x) x))

(defun expt (b n)
  (cond ((= n 0)   1)
        ((evenp n) ((lambda (x) (* x x)) (exp b (/ n 2))))
        (t         (* b (exp b (- n 1))))))

(defun number-to-string (n . radix)
  (unless (null? radix)
    (display "number-to-string: ignoring radix\n"))
  (format nil "~a" n))

;(defconstant lcm sorry)
;(defconstant exp sorry)
;(defconstant tan sorry)
;(defconstant log sorry)
;(defconstant asin sorry)
;(defconstant acos sorry)
;(defconstant atan sorry)

(defconstant numerator sorry)
(defconstant denominator sorry)

; These are in EuLisp but have misfeatures in V0.37

; (defun oddp (x) (not (evenp x)))

(defun memq (item x)		
  (cond ((null? x) '())
        ((eq? item (car x)) x)
        (t (memq item (cdr x)))))

(defun reduce (f args)
  (if (null? (cdr args))
      (car args)
      (f (car args) 
	 (reduce f (cdr args)))))

(defmacro make-stream-optional (name f)
  `(defun ,name port
     (,f (if port (car port) (standard-input-stream)))))

(make-stream-optional read 	old-read)
(make-stream-optional read-char old-read-char)
(make-stream-optional peek-char old-peek-char)

; Do renamings that couldn't be done above

(defconstant char? characterp)
(defconstant procedure? functionp)
(defconstant number->string number-to-string)

(defun substring (s i j)
  (feel-substring s i (- j 1)))

(defun string-append-aux (strings)
  (if (null? strings) ""
    (feel-string-append (car strings) (string-append-aux (cdr strings)))))

(defun string-append strings
  (string-append-aux strings))

(deflocal *case-diff* (- (char->integer #\a) (char->integer #\A)))

(defun char-upcase (x)
  (cond
    ((not (char-alphabetic-p x)) x)
    ((char-upper-case-p x) x)
    (else
      (integer->char (- (char->integer x) *case-diff*)))))

(defun char-downcase (x)
  (cond
    ((not (char-alphabetic-p x)) x)
    ((char-lower-case-p x) x)
    (else
      (integer->char (+ (char->integer x) *case-diff*)))))

(defun eof-object? (x) 
  (eq? x eof))

;(defconstant substring string-slice)	already renamed in V0.37

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Herald for this module appears here

(format t "Full Scheme module (development version).\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Conditions

(defcondition scheme-error ())
(defcondition schemedef-error ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                          ;;;
;;;	D   E   F   I   N   I   T   I   O   N   S            ;;;
;;;                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; define

(defun walk-body (body)
  (if body
      (if (and (pair? (car body))
               (equal? (caar body) 'define))
          (cons (list (if (pair? (cadar body))
                          (car (cadar body))
                          (cadar body))
                      ''unassigned)
                (walk-body (cdr body)))
          (walk-body (cdr body)))
      nil))

;; Broken!!!

'(defmacro define (bind . values)
  (if (pair? bind)
      (let ((name (car bind))
	    (args (cdr bind)))
	   (if (symbol? name)
	       `(progn (setq ,name
			     (let ,(walk-body values)
				  (lambda ,args ,@ values)))
		       ',name)
	       (eulisp-error "define: bad syntax" schemedef-error)))
      (if (symbol? bind)
	  (if values
	      (if (and (pair? (car values))
		       (equal? (caar values) 'lambda))
		  `(progn (setq ,bind
				(let ,(walk-body (cddr (car values)))
				     ,(car values)))
			  ',bind)
		  `(progn (setq ,bind ,(car values))
			  ',bind))
	      `(progn (setq ,bind 'unassigned) 
		      ',bind))
	  (eulisp-error "define: bad identifier" schemedef-error))))

;; Fixed!!

(defmacro define (bind . values)
  (if (pair? bind)
      (let ((name (car bind))
	    (args (cdr bind)))
	   (if (symbol? name)
	       `(progn (setq ,name
			     (lambda ,args 
			       (let ,(walk-body values)
				 ,@values)))
		       ',name)
	       (eulisp-error "define: bad syntax" schemedef-error)))
      (if (symbol? bind)
	  (if values
	      (if (and (pair? (car values))
		       (equal? (caar values) 'lambda))
		  `(progn (setq ,bind
				(let ,(walk-body (cddr (car values)))
				     ,(car values)))
			  ',bind)
		  `(progn (setq ,bind ,(car values))
			  ',bind))
	      `(progn (setq ,bind 'unassigned) 
		      ',bind))
	  (eulisp-error "define: bad identifier" schemedef-error))))

; letd is a let which understands local defines

(defmacro letd (bind . body)
  (let ((bindings (walk-body body)))
       (if bindings
	   `(let ,bindings
		 (let ,bind ,@body))
	   `(let ,bind ,@body))))

(export letd)

(defmacro set! (bind val) `(setq ,bind ,val))
(defmacro begin forms `(progn ,@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Streams

(defconstant the-empty-stream nil)

(defmacro cons-stream (a b) `(cons ,a (delay ,b)))

(defun head (s) (car s))

(defun tail (s) (force (cdr s)))

(defun force (promise) (promise))

(defun empty-stream? (s) (eq? s the-empty-stream))

(defmacro freeze (form) `(lambda () ,form))

(defmacro delay (form) `(make-promise (freeze ,form)))

(defun make-promise (p)
  (let ((run-flag nil) (value nil))
       (lambda ()
	       (if run-flag
		   value
		   (progn (setq run-flag t)
			  (setq value (p)))))))
; hack

(defconstant else t) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Simple ones

(defun inc (x) (+ x 1))	; replaces 1+
(defun dec (x) (- x 1))	; replaces -1+

; in V0.37, (equal? nil 'nil) is false
(defun boolean? (x) (if (or (eq? x t) (eq? x nil) (eq? x 'nil)) t nil))

(defun error (message value)
  (eulisp-error message scheme-error 'error-value value))

; we assume that EuLisp mapcar evaluates in order (though Scheme 
; mapcar doesn't have to)
(defmacro for-each (proc . lists) 
  `(progn (mapcar ,proc ,@lists) t))

(defconstant set-car! (setter car))
(defconstant set-cdr! (setter cdr))
(defconstant string-set! (setter string-ref))
(defconstant vector-set! (setter vector-ref))

(defun call-with-current-continuation (f) (let/cc k (f k)))

(defun list? (l)
  (if (null? l)
      t
      (if (pair? l)
	  (list? (cdr l))
	  nil)))

; files

(defun open-input-file (filename) (open filename
					'input
					'eos-action (lambda (s) eof)))
(defun open-output-file (filename) (open filename 'output))
(defun close-input-port (port) (close port))
(defun close-output-port (port) (close port))

;; Don't have enough streams functions to do this.
(defun input-port? (x)
  t)

(defun output-port? (x)
  t)

; BTW how do these interact with signals, call/cc etc?
(defun call-with-input-file (filename f)
  (let ((port (open-input-file filename)) (value '()))
       (setq value (f port))
       (close port)
       value))

(defun call-with-output-file (filename f)
  (let ((port (open-output-file filename)) (value '()))
       (setq value (f port))
       (close port)
       value))

(defun with-input-from-file (file thunk)
  (let ((old-stream standard-input-stream))
       (let ((new-stream (open-input-file filename)) (value))
	    ((setter standard-input-stream) new-stream)
	    (set! value (thunk))
	    (close new-stream)
	    ((setter standard-input-stream) old-stream)
	    value)))

(defun with-output-to-file (file thunk)
  (let ((old-stream standard-output-stream))
       (let ((new-stream (open-output-file filename)) (value))
	    ((setter standard-output-stream) new-stream)
	    (set! value (thunk))
	    (close new-stream)
	    ((setter standard-output-stream) old-stream)
	    value)))

(defun char-ready? port
  (stream-ready-p (if port (car port) (standard-input-stream))))

; type predicates

(defun integer? (x) (eq? (class-of x) integer))
(defun real? (x) (eq? (class-of x) real))
(defun rational? (x) (eq? (class-of x) rational))
(defun complex? (x) (eq? (class-of x) complex))

;(defun string? (x) (eq? (class-of x) string))
;(defun symbol? (x) (eq? (class-of x) symbol))
;(defun vector? (x) (eq? (class-of x) vector))
;(defun pair? (x) (eq? (class-of x) pair))
;(defun number? (x) (subclassp (class-of x) number))

(defun list->string (l)
  (let ((str (make-string (length l))))
    (let loop ((l l) (i 0))
      (unless (null? l)
        (string-set! str i (car l))
	(loop (cdr l) (+ i 1))))
    str))

(defun string->list (s)
  (let ((len (length s)))
    (let loop ((i 0))
      (if (= i len) '()
	(cons (string-ref s i) (loop (+ 1 i)))))))

(defun string args
  (list->string args))

(deflocal list->vector (converter (class-of #(1))))
(deflocal vector->list (converter (class-of '(1))))

(defun vector stuff
  (list->vector stuff))

; Still have these to define...

; assv
; case
; catch and throw
; char-upcase etc
; do
; memv
; rationalize
; string stuff (including string, string->number)
; transcript
; vector stuff (including vector)

(defun memv (a l) (member? a l eqv?))
(defun assv (a l) (assq a l))

(export memv assv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                          ;;;
;;;	E    X    P    O    R    T    S                      ;;;
;;;                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; EuLisp names which don't need renaming

(export 
	  <
	  <=
	  >
	  >=
	  =
	  +
	  -
	  *
	  /
          abs 
	  and
	  append
	  apply
	  assoc
	  assq
	  asin
	  acos
	  atan
	  car
	  cdr
	  caar
	  cadr
	  cdar
	  cddr
	  caaar
	  caadr
	  cadar
	  caddr
	  cdaar
	  cdadr
	  cddar
	  cdddr
	  caaaar
	  caaadr
	  caadar
	  caaddr
	  cadaar
	  cadadr
	  caddar
	  cadddr
	  cdaaar
	  cdaadr
	  cdadar
	  cdaddr
	  cddaar
	  cddadr
	  cdddar
	  cddddr
	  ceiling
	  char-upcase
	  char-downcase
	  cond
	  cons
	  cos
	  exp
	  expt
	  denominator
	  floor
          gcd
	  lcm
	  length
	  let
	  let*
	  list
	  list-ref
	  log
	  make-string
	  make-vector
	  max
	  min
	  member
	  memq
	  modulo
	  newline
	  not
	  numerator
	  or
	  peek-char
	  print
	  quasiquote
	  quotient
	  read
	  read-char
	  remainder
	  reverse
	  round
	  sin
	  string-copy
	  string-length
	  string-ref
	  tan
	  vector-length
	  vector-ref
	  write
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; EuLisp functions renamed to Scheme in this module
; NB This renaming can be done here, on export, but currently
; appears at the top of the file (as I don't know how to do it
; on export!)

(export
	char?
	char=?
	char<?
	char>?
	char<=?
	char>=?
	char->integer
	current-input-port
	current-output-port
	display
	eof-object?
	eq?
	equal?
	even?
	input-port?
	integer->char
	last-pair
	length
	letrec
	list->string
	list->vector
	map
	null?
	number?
	number->string
	odd?
	output-port?
	pair?
	procedure?	
	string?
	string-append
	string->list
	string->symbol
	substring
	symbol?
	symbol->string
	vector?
	vector->list
	zero?
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Scheme functions defined in this module

(export 
	inc ; in place of 1+
	dec ; in place of -1+

	begin 
	boolean?
	call-with-current-continuation
	call-with-input-file
	call-with-output-file
	char-ready?
	close-input-port 
	close-output-port
	complex?
	cons-stream
	define 
	delay 
        else	; ho hum
	empty-stream? 
	error 
	for-each
	force
	freeze
	head 
	integer? 
	last-pair
	list? 
	;load	this has to be in scheme module (to use eval/cm)
	make-promise 
	open-input-file 
	open-output-file
	rational?
	real? 
	set! 	; could be a renaming of setq if we could rename specials
	set-car! 
	set-cdr! 
	sqrt	; should this be in EuLisp?
	string
	string-set!
	string->list
	tail 
	the-empty-stream
	vector-set!
	vector
	with-input-from-file
	with-output-to-file
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Abelson and Sussman compatibility

;(defun atom? (x) (not (pair? x))) 
; actually, V0.37 has atom but it ain't in EuLisp
(defconstant atom? atom)

(defconstant princ display)

(export atom? princ print)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Compatability with old Schemes

(defconstant prin1 write)
(defconstant call/cc call-with-current-continuation)
(defmacro sequence forms `(progn ,@forms))

(export prin1 call/cc sequence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Real bozo hack at the number system...

(defun exact? (x) 
  (cond
    ((eq? (class-of x) integer) t)
    ((eq? (class-of x) real) ())
    (else ())))

(defun inexact? (x) 
  (cond
    ((eq? (class-of x) integer) ())
    ((eq? (class-of x) real) t)
    (else ())))

(defun exact->inexact (x) (* 1.0 x))
(defun inexact->exact (x) (floor x))


(export exact? inexact? positive? exact->inexact inexact->exact)

(defun write-char (c . port)
  (feel-write-char c (if (null? port) (current-output-port) (car port))))

(export write-char)

(defun list-tail (l n)
  (if (= n 0) l (list-tail (cdr l) (- n 1))))

(export list-tail)

(defun flush-output stuff
  (flush (if (null? stuff) (current-output-port) (car stuff))))

(export flush-output)

(defun last-pair (l)
  (cond
    ((not (pair? l)) (error "last-pair: bogus arg dude!" clock-tick))
    ((not (pair? (cdr l))) l)
    (else (last-pair (cdr l)))))

;; Hacks...

(defstruct <ovector> () 
  ((vector initarg vector accessor ovector-vector))
  constructor (make-ovector-obj vector)
  predicate ovector?)

(define (ovector . stuff)
  (make-ovector-obj (apply vector stuff)))

(define (make-ovector size init)
  (make-ovector-obj (make-vector size init)))

(define (ovector-ref v i)
  (vector-ref (ovector-vector v) i))

(define (ovector-set! v i val)
  (vector-set! (ovector-vector v) i val)
  val)

(export ovector? ovector make-ovector ovector-ref ovector-set!)

(defconstant $t t)
(defconstant $f '())

(export $t $f)

)
; end of newschemedef.em
