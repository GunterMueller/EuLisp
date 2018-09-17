(defmodule wait

;;  (import (extras0 macros0 defs init))

  (import (thread stream defs init telos1))

  ;; defs for defgeneric, defmethod
  ;; init for various fundamental things including <generic-function>
  ;; telos1 for <not-yet-implemented>

  (defconstant ticks-per-second 0.0)

  (defgeneric wait (obj timeout))

  (defmethod wait ((th <thread>) (timeout <integer>))
    (if (= timeout 0)
	;; poll check
	(eq (thread-state th) 'returned)
      ;; sleep for timeout or until thread returns
      (error "cannot wait with non-zero timeout" <not-yet-implemented>)))

  (defmethod wait ((th <thread>) (timeout <symbol>))
    (thread-value th)
    t)

  (defmethod wait ((str <char-file-stream>) (timeout <integer>))
    (if (= timeout 0)
	;; poll check
	(input_available str)
      ;; sleep for timeout or until input is available
      (error "cannot wait with non-zero timeout" <not-yet-implemented>)))

  (defmethod wait ((str <char-file-stream>) (timeout <symbol>))
    (error "cannot wait indefinitely" <not-yet-implemented>))

  (export ticks-per-second wait)

)
