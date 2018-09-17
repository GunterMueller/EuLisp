/* ******************************************************************** */
/* semaphores.h      Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Lisp semaphores       		                                */
/* ******************************************************************** */

extern LispObject Fn_semaphorep(LispObject *);
extern LispObject Fn_make_semaphore(LispObject*);
extern LispObject Fn_initialize_semaphore(LispObject*);
extern LispObject Fn_open_semaphore(LispObject*);
extern LispObject Fn_close_semaphore(LispObject*);

extern void initialise_semaphores(LispObject*);

