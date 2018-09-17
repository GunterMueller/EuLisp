/* ******************************************************************** */
/*  calls.h          Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* explicit funcalls							*/
/* ******************************************************************** */

extern LispObject Fn_apply(LispObject*);
extern LispObject apply1(LispObject*);
extern LispObject apply2(LispObject*);
extern LispObject apply3(LispObject *);

extern EUDECL(macroexpand_1);

extern EUDECL( Sf_macroexpand_1);
extern EUDECL(Sf_macroexpand);

extern void initialise_calls(LispObject*);

