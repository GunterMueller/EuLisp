;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright (c) University of Bath, 1993
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule null
  (init extras0 macros0 gens defs)
   ()

   (export <null> null)
   
   (defmethod size ((c <null>)) 0)
   
   (defmethod shallow-copy ((c <null>)) ())

   (defmethod initial-state ((c <null>))
     nil)

   (defmethod gf-concatenate ((c <null>) cs)
     (cond
       ((null cs) ())
       ((null (car cs)) ())
       (t (gf-concatenate-as <list> (car cs)(cdr cs)))))

   )
