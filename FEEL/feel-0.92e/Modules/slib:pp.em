(defmodule slib:pp (scheme) ()

  (slib:load "genwrite")
  (slib:load "pp")
  (slib:load "pp2str")

  (export
   
    pretty-print
    pretty-print-to-string

  )

)

;; eof
