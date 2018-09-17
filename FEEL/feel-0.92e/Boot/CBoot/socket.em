;; Eulisp Module
;; Author: pab
;; File: socket.em
;; Date: Thu Jul  1 13:09:07 1993
;;
;; Project:
;; Description: 
;;

(defmodule socket
  (init
   macros0
   extras0
   telos1
   stream
   defs
   sockets
   table
   )
  ()
  
  (expose sockets)
  
  (defmethod output ((x <socket>) (s <string>))
    (socket-write-string x s))

  (defmethod output ((x <socket>) (c <character>))
    (socket-write-string x (convert c <string>)))
  
  (defmethod input ((x <socket>))
    (let ((c (socket-read-char x)))
;      (format t "{~a}" c)
      c))

  (defmethod uninput ((x <socket>) (c <character>))
;    (format t "unread: ~a~%" c)
    (socket-unread-char x c))
  
  (defmethod close ((x <socket>))
    (close-socket x))

  (defmethod flush ((x <socket>))
    nil)
  
  ;; end module
  )
