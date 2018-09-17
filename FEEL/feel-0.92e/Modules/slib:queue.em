(defmodule slib:queue (scheme slib:record) ()

  (slib:load "queue")

  (export
   
    make-queue
    queue?
    queue-empty?
    queue-front
    queue-rear
    queue-push!
    enqueue!
    dequeue!
    queue-pop!

  )

)

;; eof
