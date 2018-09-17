/* ******************************************************************** */
/*  garbage.h        Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Garbage colection module prototypes	                                */
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, December 1989
 */

#ifndef GARBAGE_H
#define GARBAGE_H

#define GC_FAILED (-1)

extern void initialise_garbage(LispObject*);
extern void runtime_initialise_garbage_collector(LispObject*);
extern void ON_collect(void);
extern void OFF_collect(void);
extern int garbage_collect(LispObject *);

extern SYSTEM_GLOBAL(int,GC_register);
extern SYSTEM_GLOBAL(LispObject **,GC_static_protect_stack);
extern SYSTEM_GLOBAL(int,GC_static_protect_stack_ptr);


#endif
