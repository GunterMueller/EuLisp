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
;;; Title: syntax definitions for c-code
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;    The import of format is only necessary in the common lisp environment.
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module c-code-syntax
(import ((except (format)
                 level-0)
         (only (format)
               common-lisp))
 syntax (level-0
         dynamic))

(defmacro write-code (format-string . args)
  `(format (dynamic code-output) ,format-string ,@args))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
