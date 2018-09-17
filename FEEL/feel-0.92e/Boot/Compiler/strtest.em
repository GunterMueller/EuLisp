;; Eulisp Module
;; Author: pab
;; File: strtest.em
;; Date: Tue Feb 25 21:05:43 1992
;;
;; Project:
;; Description: 
;;

(defmodule strtest
  (standard0
   bci
)
  ()
  

  (defstruct foo ()
    ((a initarg a accessor foo-a))
    constructor make-foo)

  (defstruct bar foo
    ()
    constructor make-bar)

  (defstruct xxx bar
    ()
    )

  (defgeneric yowzer (x))
  
  (defmethod yowzer ((a foo))
    1)

  (defmethod yowzer ((a bar))
    (print "XX" (standard-error-stream))
    (format t "a is: ~a~%" a)
    (+ 1 (call-next-method)))

  (defmethod yowzer ((a xxx))
    (print "going...")
    (call-next-method))

  (defun wibble (a)
    (let ((x (make-instance a)))
      (yowzer x)))

  (defun off ()
    (set-bc-global 3 nil))

  ;; end module
  )
(wibble foo)
(wibble bar)
(wibble xxx)
