/* ******************************************************************** */
/*  error.h          Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Error and Signal handling	                                        */
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, April 1989
 */


#ifndef ERROR_H
#define ERROR_H

#ifndef SETJMP_H
#define SETJMP_H
#include <setjmp.h>
#endif

#define is_condition(op) (EUCALL_2(Fn_subclassp,classof(op),Default_Condition)!=nil)

LispObject CallError(LispObject *, char *, LispObject, int);
void signal_message(LispObject *,int,char *,LispObject);
void signal_heap_failure(LispObject *, int);

extern LispObject function_default_handler;
/* condition representation */

#define condition_message(x) (slotref((x),0))
#define condition_error_value(x) (slotref((x),1))


/* The conditions, used as index into table */

#define INTERNAL_ERROR			(0)

#define CLOCK_TICK			(1)

#define HEAP_EXHAUSTED                  (2)

#define Reader_CallError(sp,msg,val,foo) \
  (ON_collect(), CallError(sp,msg,val,foo))

#endif /* ERROR_H */
/* end of error.h */
