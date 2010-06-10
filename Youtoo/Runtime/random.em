;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind
;;; Description: Pseudo-random number generator
;;;-----------------------------------------------------------------------------
(defmodule random
  (syntax (_telos0)
          import (telos)
          export (random *random-max* random-seed random-true-nil))

;;;-----------------------------------------------------------------------------
;;; Set *random-max*
;;;-----------------------------------------------------------------------------
  (defconstant *random-max* (eul_rand_max))
  (defextern eul_rand_max () <int>)

;;;-----------------------------------------------------------------------------
;;; Return a random value between 0 and random-max
;;;-----------------------------------------------------------------------------
  (defextern rand () <int>)

  (defun random x
    (if x
        (int-binary/ (rand) (int-binary/ *random-max* (car x)))
      (rand)))

;;;-----------------------------------------------------------------------------
;;; The function random-seed uses the argument as a seed for a
;;; new sequence of pseudo-random numbers to be returned by subsequent
;;; calls to the function random
;;;-----------------------------------------------------------------------------
  (defextern eul_srand (<int>) <int>)

  (defun random-seed (x) (eul_srand x))

;;;-----------------------------------------------------------------------------
;;; Fifty-fifty function
;;;-----------------------------------------------------------------------------
  ;; The result of (random 2) should always be a fixnum (<int> class),
  ;; so use int-zerop, which is available during bootstrapping, instead of
  ;; zerop, the generic function.
  (defun random-true-nil ()
    (int-zerop (random 2)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------