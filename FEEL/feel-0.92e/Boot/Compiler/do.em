(defmodule do
  (standard0)
  ()

  (defmacro do (var-init-step-forms end-test-result . body)
    (let ((vars (mapcar car var-init-step-forms))
          (inits (mapcar cadr var-init-step-forms))
          (steps (mapcar caddr var-init-step-forms))
          (end-test (car end-test-result))
          (results (cdr end-test-result)))
    `(let/cc return
       (labels (
         (do-loop ,vars
           (if ,end-test
               (progn ,@results)
               (progn ,@body (do-loop ,@steps)))))
         (do-loop ,@inits)))))

  (export do)

)
