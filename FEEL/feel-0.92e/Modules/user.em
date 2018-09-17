;; Eulisp Module
;; Author: pab
;; File: user.em
;; Date: Mon Jul 12 10:35:15 1993
;;
;; Project:
;; Description: 
;;    Very boring module.
;;    This is where it all starts

(defmodule user

  (import (eulisp0 describe))

  ()

  (defmethod output ((x <string>) (s <string>))
    (fput x s))

  (defmethod output ((x <string>) (s <character>))
    (fput x s))

)

