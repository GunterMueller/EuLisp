(defmodule patch

  (eulisp0)

  ()

  (defmethod initial-state ((x <table>))
    (if (= (size x) 0) () (table-keys x)))

  (defmethod key-sequence ((x <table>))
    (initial-state x))

  (defmethod input ((stream <char-file-stream>))
    (let ((val (fread-char (file-stream-file stream))))
	(if (eql val #\x00ff)
	  ((stream-action stream) stream)
	  val)))


)


