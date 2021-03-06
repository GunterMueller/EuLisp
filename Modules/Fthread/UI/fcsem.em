;;; Copyright 1996 A. Kind & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: Interface to Unix International (aka Solaris) Semaphores (UI)
;;;  Library: fthread (foreign thread interface)
;;;  Authors: Andreas Kind, Liam Wickins
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    See fthread.em
;;;-----------------------------------------------------------------------------

(defmodule fcsem
  (syntax (syntax-1)
   import (level-1)
   export (<csemaphore>
           csemaphorep
           cwait
           csignal))

;;;-----------------------------------------------------------------------------
;;; Class <csemaphore>
;;;-----------------------------------------------------------------------------
(defclass <csemaphore> ()
  ((handle accessor: csemaphore-handle))
  predicate: csemaphorep
  keywords: (counter:))

(defmethod initialize ((csem <csemaphore>) inits)
  (call-next-method)
  (let ((n (init-list-ref inits counter:)))
    (if (integer? n)
        ((setter csemaphore-handle) csem (eul_sema_create n))
      ;; Basically a lock
      ((setter csemaphore-handle) csem (eul_sema_create 1)))
    csem))

(defextern eul_sema_create (<fpi>) ptr)

;;;-----------------------------------------------------------------------------
;;; Wait
;;;-----------------------------------------------------------------------------
(defun cwait (csem)
  (eul_sema_wait (csemaphore-handle csem))
  csem)

(defextern eul_sema_wait (ptr) ptr "sema_wait")

;;;-----------------------------------------------------------------------------
;;; Signal
;;;-----------------------------------------------------------------------------
(defun csignal (csem)
  (eul_sema_signal (csemaphore-handle csem))
  csem)

(defextern eul_sema_signal (ptr) ptr "sema_post")

;;;-----------------------------------------------------------------------------
)  ;; End of module fcsem
;;;-----------------------------------------------------------------------------
