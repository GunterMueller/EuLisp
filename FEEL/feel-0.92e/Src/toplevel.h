/* ******************************************************************** */
/* toplevel.h        Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* toplevel syntactic forms' headers                                    */
/* ******************************************************************** */

LispObject TL_define(LispObject *,LispObject,LispObject);
LispObject TL_defun(LispObject *,LispObject,LispObject);
LispObject TL_defmacro(LispObject *,LispObject,LispObject);
LispObject TL_deflex(LispObject *,LispObject,LispObject);
LispObject TL_defconstant(LispObject *,LispObject,LispObject);
LispObject TL_defvar(LispObject*,LispObject,LispObject);
