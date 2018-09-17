(defmodule ack (standard0) ()

  (export f)

  (defun f (x)
    (labels
      ((wop (y) x))
      wop))

)
