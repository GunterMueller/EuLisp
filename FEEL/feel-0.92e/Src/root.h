/* ******************************************************************** */
/*  root.h           Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* The root level operations protos                                     */
/* ******************************************************************** */

extern MODULE Module_root;

LispObject load_module(LispObject*);
extern EUDECL( Rf_load_module);
extern EUDECL( Rf_start_module);
extern EUDECL( Rf_enter_module);
extern EUDECL( Rf_loaded_modules);


extern void initialise_root(LispObject*);
