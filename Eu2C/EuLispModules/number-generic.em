;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: number-generic
;;;  Description:
;;    Provides all generic functions defined for subclasses of class <number>.
;;;  Notes:
;;    This module provides some generic functions with its default EuLisp
;;    names. They are defined in number-generic-i with names which appear as
;;    more readable C-identifiers in the generated C-Code.
;;;  Authors: Ingo Mohr
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule number-generic
  (import (eulisp-kernel)
   syntax (eulisp-kernel)
   expose ((rename ((binary-plus binary+)
                    (binary-minus binary-)
                    (binary-div binary/)
                    (binary-mult binary*)
                    (binary-rem binary%))
                   number-generic-i)))

;;;-----------------------------------------------------------------------------
)  ;; End of module number-generic
;;;-----------------------------------------------------------------------------
