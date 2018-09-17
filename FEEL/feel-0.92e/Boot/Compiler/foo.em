;; Eulisp Module
;; Author: pab
;; File: foo.em
;; Date: Thu Apr 30 13:52:20 1992
;;
;; Project:
;; Description: 
;;

(defmodule foo
  (standard0
   list-fns

   byte-stream
   peephole
   )
  ()
  
  (defun make-filter-stream ()
    (let ((state (make-initial-peep-state)))
      (make-filter-stream
       (peep-opt (lambda () state))
       (close-state (lambda () state))
       (make-simple-stream))))
  
  

  )
