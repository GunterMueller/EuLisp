;; Eulisp Module
;; Author: pab
;; File: extras0.em
;; Date: Fri Jan 10 04:17:12 1992
;;
;; Project:
;; Description: 
;;

(defmodule extras0
  (
   init
   (except (car cdr cadr cddr) init)
   macros0
   characters
   ) 
  ()

  (defun not (widget) (null widget))
  
  (export not)

  (defun caar (x) (car (car x)))
  (defun cadr (x) (car (cdr x)))
  (defun cdar (x) (cdr (car x)))
  (defun cddr (x) (cdr (cdr x)))

  (export caar cadr cdar cddr)

  (defun caaar (x) (car (car (car x))))
  (defun caadr (x) (car (car (cdr x))))
  (defun cadar (x) (car (cdr (car x))))
  (defun caddr (x) (car (cdr (cdr x))))
  (defun cdaar (x) (cdr (car (car x))))
  (defun cdadr (x) (cdr (car (cdr x))))
  (defun cddar (x) (cdr (cdr (car x))))
  (defun cdddr (x) (cdr (cdr (cdr x))))

  (export caaar caadr cadar caddr cdaar cdadr cddar cdddr)

  (defun caaaar (x) (car (car (car (car x)))) )
  (defun caaadr (x) (car (car (car (cdr x)))) )
  (defun caadar (x) (car (car (cdr (car x)))) )
  (defun caaddr (x) (car (car (cdr (cdr x)))) )
  (defun cadaar (x) (car (cdr (car (car x)))) )
  (defun cadadr (x) (car (cdr (car (cdr x)))) )
  (defun caddar (x) (car (cdr (cdr (car x)))) )
  (defun cadddr (x) (car (cdr (cdr (cdr x)))) )
  (defun cdaaar (x) (cdr (car (car (car x)))) )
  (defun cdaadr (x) (cdr (car (car (cdr x)))) )
  (defun cdadar (x) (cdr (car (cdr (car x)))) )
  (defun cdaddr (x) (cdr (car (cdr (cdr x)))) )
  (defun cddaar (x) (cdr (cdr (car (car x)))) )
  (defun cddadr (x) (cdr (cdr (car (cdr x)))) )
  (defun cdddar (x) (cdr (cdr (cdr (car x)))) )
  (defun cddddr (x) (cdr (cdr (cdr (cdr x)))) )

  (export caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr 
	  cdaaar cdaadr cdadar cdaddr cddaar cdddar cddadr cddddr)

  (defun eqcar (a b) (cond ((atom a) nil) ((eq (car a) b) t) (t nil)))

  (export eqcar)

  (defun mkquote (x) (list 'quote x))

  (export mkquote)

  (defun assq (a l)
    (cond
     ((null l) nil)
     ((eq a (caar l)) (car l))
     (t (assq a (cdr l)))) )

  (export assq)

  (defun list-ref (l n)
    (if (equal n 0) (car l)
      (list-ref (cdr l) (\- n 1))))

  (export list-ref)

  (defun \@list-ref-update\@ (l n obj)
    (if (equal n 0) ((setter car) l obj)
      (\@list-ref-update\@ (cdr l) (- n 1) obj)))

  (interpret-time 
   (defun reverse-list (l)
     (reverse-aux l nil))
   
   (defun reverse-aux (l so-far)
     (if l (reverse-aux (cdr l)
			(cons (car l) so-far))
       so-far))
   )

  (compile-time 
   (defun reverse-list (l)
     (labels ((rev1 (l n)
		    (if (null l) n
		      (rev1 (cdr l) (cons (car l) n)))))
	     (rev1 l nil)))
   )

  (export reverse-list)

  (defun subst (a b c)
    (cond
     ((equal c b) a)
     ((atom c) c)
     (t 
      ((lambda (carc cdrc)
	 (cond ((and (eq carc (car c)) (eq cdrc (cdr c))) c)
	       (t (cons carc cdrc))))
       (subst a b (car c))
       (subst a b (cdr c))))))

  (export subst)

; JAP 940322: superseded by generic delete/remove
;
;  (defun delete (a b comp)
;    (cond
;     ((null b) nil)
;     ((comp a (car b)) (cdr b))
;     (t ((lambda (del)
;	   (cond ((eq del (cdr b)) b)
;		 (t (cons (car b) del))))
;	 (delete a (cdr b) comp)))))
;
;  (export delete)

  (defun deleteq (a b)
    (cond
     ((null b) nil)
     ((eq a (car b)) (cdr b))
     (t ((lambda (del)
	   (cond ((eq del (cdr b)) b)
		 (t (cons (car b) del))))
	 (deleteq a (cdr b))))))

  (export deleteq)

  ;;
  ;; Missing bits...
  ;;

  (defun list-copy-aux (l new)
    (if l (list-copy-aux (cdr l) (nconc new (cons (car l) nil)))
      new))

  (defun list-copy (l) (list-copy-aux l nil))

  (export list-copy)

  ;; Conversion
  ;; According to the standard (nearly)

  (defconstant *convert-tab* (make-table eq))

  (defun converter (cl)
    (let ((xx (sys-table-ref *convert-tab* cl)))
      (if (not (null xx))
	  xx
	(let ((new-gen (make-converter-generic cl)))
	  ((setter converter) cl new-gen)
	  new-gen))))
	  

  (defun make-converter-generic (cl)
    (let ((gf (make <generic-function>
		    'name (make-symbol (string-append (symbol-unbraced-name (class-name cl)) "-converter"))
		    'lambda-list '(a)
		    'method-class <method>)))
      (add-method gf (make <method>
			   'signature (list cl)
			   'function (method-lambda (o) o)))))

  
  
  
  ((setter setter) converter
   (lambda (cl fn)
     ((setter sys-table-ref) *convert-tab* cl fn)))
  
  
  (defun convert (x cl)
    "(convert obj class)
     Converts obj to be an equivalent object of the specified class.
     Calls (converter class) in order to achieve this"
    ((converter cl) x))
  
  (export converter convert)
  ;; shove in the defined methods...
  ;; Really so trivial that we could use lisp functions...

  (add-method (converter <vector>)
	      (make <method>
		    'signature (list <cons>)
		    'function generic_generic_convert\,Cons\,Vector))

  (add-method (converter <cons>)
	      (make <method>
		    'signature (list <vector>)
		    'function generic_generic_convert\,Vector\,Cons))

  (add-method (converter <vector>)
	      (make <method>
		    'signature (list (class-of nil))
		    'function 
		    (method-lambda (c)
				   (make-vector 0))))

  (add-method (converter <string>)
	      (make <method> 
		    'signature (list <object>)
		    'function (method-lambda (obj)
					     (format nil "~a" obj))))
  (add-method (converter <string>)
	      (make <method>
		    'signature (list <character>)
		    'function (method-lambda (obj)
					     (make-string 1 obj))))

  (add-method (converter <integer>)
	      (make <method>
		    'signature (list <character>)
		    'function character-to-integer))


  ;; Also need to add:
  ;; (allsorts) number from string
  ;; char<--int
  ;; string->pair


  ;; Changing the habit of a lifetime

  (defconstant length (make <generic-function>
			    'name 'length
			    'lambda-list '(l)
			    'method-class <method>))

  (add-method length (make <method>
			   'signature (list <cons>)
			   'function list-length))

  (add-method length (make <method>
			   'signature (list (class-of nil))
			   'function (method-lambda (x) 0)))

  (add-method length (make <method>
			   'signature (list <vector>)
			   'function vector-length))

  (add-method length (make <method>
			   'signature (list <string>)
			   'function string-length))

  (export length)
  
					; more comparison methods...

  ;; Eql.
  ;; Eq. except on numbers
  (defconstant eql (make <generic-function>
			    'name 'eql
			    'lambda-list '(x y)
			    'method-class <method>))

  
  (add-method eql
	      (make <method>
		    'signature (list <object> <object>)
		    'function eq))

  (add-method eql
	      (make <method>
		    'signature (list <number> <number>)
		    'function (method-lambda (x y) (= x y))))


  (export eql)

  ;; equal
  (add-method equal
	      (make <method>
		    'signature (list <object> <object>)
                    ;; add an extra function call because call_method can't
                    ;; handle generics and I can't work out how to fix it
                    ;; safely (JAP)
		    'function (method-lambda (a b) (eql a b))))
  (add-method equal
	      (make <method>
		    'signature (list <cons> <cons>)
		    'function generic_equal\,Cons\,Cons))
  (add-method equal
	      (make <method>
		    'signature (list <vector> <vector>)
		    'function generic_equal\,Vector\,Vector))
  ;; the definition does not prescribe this method, but the system won't 
  ;; boot without it... (JAP)
  (add-method equal
	      (make <method>
		    'signature (list <structure> <structure>)
		    'function generic_equal\,Basic_Structure\,Basic_Structure))
  ;; the definition does not prescribe this method, but the system won't 
  ;; boot without it... (JAP)
  (add-method equal
	      (make <method>
		    'signature (list <class> <class>)
		    'function generic_equal\,Standard_Class\,Standard_Class))

  ;; More copiers
  
;  (add-method copy 
;	      (make <method>
;		    'signature (list <string>)
;		    'function string-copy))

  ;; standard streams
  (defun make-std-stream (n)
    (let ((fn (lambda () (vector-ref (std-streams) n)))
	  (fn-setter (lambda (s) (let ((old (vector-ref (std-streams) n)))
				   ((setter vector-ref) (std-streams) n s) 
				   old))))
      ((setter setter) fn fn-setter)
      fn))

  (defconstant standard-input-stream (make-std-stream 0))
  (defconstant standard-output-stream (make-std-stream 1))
  (defconstant standard-error-stream (make-std-stream 2))

  (export standard-input-stream standard-output-stream standard-error-stream)

  ;; Format 

  (defun formatter (c)
    (vector-ref (std-formatters)
		(convert c <integer>)))

  ((setter setter) formatter
   (lambda (c val)
     ((setter vector-ref) (std-formatters)
      (convert c <integer>) val)))

  (deflocal *sscl* nil)
  (defun format-string-stream-class () *sscl*)

  ((setter setter) format-string-stream-class 
   (lambda (x) (setq *sscl* x)))
  
  (defun format (s msg . args)
    (cond ((null s) 
	   (let ((strm (make (format-string-stream-class))))
	     (internal-format strm msg args)
	     (convert strm <string>)))
	  ((eq s t) (internal-format (standard-output-stream) msg args))
	  (t (internal-format s msg args)))) ; (streamp s)
  
  (export format formatter format-string-stream-class)

  ;; more reflective methods

  (defun mapcan (f l)
    (if (atom l) nil
      (nconc (f (car l))
	     (mapcan f (cdr l)))))

  (defconstant generic-function-methods
    (make <generic-function>
	  'name 'generic-function-methods
	  'lambda-list '(gf)
	  'method-class <method>))

  (export generic-function-methods)

  (defconstant gfm 
    (method-lambda (gf)
		   (labels ((get-method (l)
					(if (atom (cadr l))
					    (list (cadr l))
					  (mapcan get-method (cdr l)))))
			   (mapcan get-method (generic-method-table gf)))))

  (add-method generic-function-methods
	      (make <method>
		    'signature (list <generic-function>)
		    'function gfm))

  (defconstant find-method
    (make <generic-function>
	  'name 'find-method
	  'lambda-list '(gf sig)
	  'method-class <method>))

  (defun match-sigs (sig meths)
    (cond ((atom meths) ())
	  ((equal sig (method-signature (car meths))) (car meths))
	  (t (match-sigs sig (cdr meths)))))

  (add-method find-method
	      (make <method>
		    'signature (list <generic-function> <object>)
		    'function (method-lambda (gf sig)
					     (match-sigs sig (generic-function-methods gf)))))

  (export find-method)

  ;; next version junk....

  (defun make-constructor (cl)
    (lambda a
      (initialize (allocate cl a) a)))

  (export make-constructor)

  ;; add make-predicate...

  (defconstant make-predicate
    (make <generic-function>
	  'name 'make-predicate
	  'lambda-list '(cl)
	  'method-class <method>))


  ;; probably portable
  (add-method make-predicate 
	      (make 
	       <method>
	       'signature (list <class>)
	       'function 
	       (method-lambda (x)
			      (let ((gf (make <generic-function>
					      'name (make-symbol (string-append (symbol-unbraced-name (class-name x)) 
										"-p"))
					      'lambda-list '(obj)
					      'method-class <method>)))
				(add-method gf 
					    (make <method>
						  'signature (list <object>)
						  'function (method-lambda (ob)
									   nil)))
				(add-method gf 
					    (make <method>
						  'signature (list x)
						  'function (method-lambda (ob)
									   t)))
				gf))))
  (export make-predicate)

; JAP 940321: map-table superseded by map (note: use of vector-length breaks
; because table-values used to return the symbol *%_unbound_%*).  JAP changed
; table-values to return () for the generic iteration functions.
;
;  (defun map-table (fn tab)
;    (let ((vector (table-values tab)))
;      (labels ((map (n)
;		    (if (< n 0) nil
;		      (let ((aa (vector-ref vector n)))
;			(if (atom aa) nil
;			  (fn (car aa) (cdr aa)))
;			(map (- n 1))))))
;	      (map (- (vector-length vector) 1)))))
;
; JAP 940321: table-keys now redundant
;
;  (defun table-keys (tab)
;    (let ((lst nil))
;      (map-table (lambda (a b) (setq lst (cons a lst))) tab)
;      lst))

(defun map-table (a b)
  (error
   (format t "map-table has been superseded by map\n")
   <Internal-Error>))

(defun table-keys (tab)
  (error
   (format t "JAP thinks this function is now redundant, but if you think othewise...\n")
   <Internal-Error>))

  (export map-table table-keys)

  ;; Character stuff
  (defconstant char-hash-vector
    #(1 87 49 12 176 178 102 166 121 193 6 84 249 230 44 163
	14 197 213 181 161 85 218 80 64 239 24 226 236 142 38 200
	110 177 104 103 141 253 255 50 77 101 81 18 45 96 31 222
	25 107 190 70 86 237 240  34 72 242 20 214 244 227 149 235
	97 234 57 22 60 250 82 175 208 5 127 199 111 62 135 248
	174 169 211 58 66 154 106 195 245 171 17 187 182 179 0 243
	132 56 148 75 128 133 158 100 130 126 91 13 153 246 216 219
	119 68 223 78 83 88 201 99 122 11 92 32 136 114 52 10
	138 30 48 183 156 35 61 26 143 74 251 94 129 162 63 152
	170 7 115 167 241 206 3 150 55 59 151 220 90 53 23 131
	125 173 15 238 79 95 89 16 105 137 225 224 217 160 37 123
	118 73 2 157 46 116 9 145 134 228 207 212 202 215 69 229
	27 188 67 124 168 252 42 4 29 108 21 247 19 205 39 203
	233 40 186 147 198 192 155 33 164 191 98 204 165 180 117 76
	140 36 210 172 41 54 159 8 185 232 113 196 231 47 146 120
	51 65 28 144 254 221 93 189 194 139 112 43 71 109 184 209
	1))
    
  (add-method generic-hash
	      (make <method>
		    'signature (list <character>)
		    'function 
		    (method-lambda (x)
				   (vector-ref char-hash-vector (convert x <integer>)))))


  )

