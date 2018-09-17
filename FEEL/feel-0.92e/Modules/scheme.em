; Full Scheme

(defmodule scheme (schemedefs
		   ;prettyprint
		   ;trace
		   ) ()
  
  (expose
    
    schemedefs
 
  )

  (export
    
    require
    eval
    load scheme:load

    slib:load require provided?
    slib:form-feed
    slib:tab
    force-output
    slib:error

  )

  (defmacro eval (form)
    `(eval/cm ,form))

  (defmacro load (file)
    `(scheme:load ,file eval/cm))

  (defun scheme:load (filename eval) 
    (eval-forms-in-stream 
      (open-unschemed-input-file filename)
      eval))

  (defun eval-forms-in-stream (port eval)
	 (let ((form (read port)))
	      (cond ((eof-object? form) '(close-input-port port) t)
		    (t (eval form) (eval-forms-in-stream port eval)))))

  ;; Hack for slib code...

  (defmacro slib:load (file)
    `(load 
       (string-append
         (string-append "/net/brad/denton_export/SCM/Lib1.8/" ,file)
         ".scm")))

  (defun require stuff
    (display "required: ") (display stuff)
    (newline)
    stuff)

  (defun provided? (thing)
    (case thing
      ((inexact) *false*)
      (else *false*)))

  ;; The form feed character.
  (define slib:form-feed (integer->char 12))

  ;; The tabulator character.
  (define slib:tab (integer->char 9))

  ;; Flushes an output port (format:force-output output-port).
  (define (force-output port) *true*)

  ;; A null argument closure to jump to the interpreters toplevel continuation;
  ;; format:abort may return and in this case the format returns properly
  ;; and returns #f.
  (define (slib:error . stuff)
    (display "slib error: ") (display stuff)
    (newline)
    ('slib:error))

)

