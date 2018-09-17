; while and for written tail recursively
; with (break) for early exit
; RJB 10 Feb 92

(defmodule loops (standard0) ()

  (defmacro while (condition . body)
    `(let/cc break			; (syntax break)
       (labels ((| do it again | ()
		 (when ,condition
		   ,@body
		   (| do it again |))))
	       (| do it again |))))

  (defmacro for (init condition inc . body)
    `(progn
       ,init
       (while ,condition
	 ,@body
	 ,inc)))

  (export while for)

)

;  (defmacro for (init condition inc . body)
;    `(let/cc break			; (syntax break)
;       (labels ((| do it again | ()
;		 (when ,condition
;		   ,@body
;		   ,inc
;		   (| do it again |))))
;	  ,init
;	  (| do it again |))))

