/* ******************************************************************** */
/* init_elvira.c     Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Interpreter elvira.			                                */
/* ******************************************************************** */

/*
 * $Id: el_linkins.c,v 1.3 1994/04/25 12:37:12 djb Exp $
 *
 * $Log: el_linkins.c,v $
 * Revision 1.3  1994/04/25  12:37:12  djb
 * Dave Halls fix -- #include<..> to #include"..."
 * This is where the files were migrated from AddOns to Src
 *
 * Revision 1.2  1994/02/08  11:25:39  djb
 * Env->LispObject
 *
 */

/* No Elvira as yet... */

#include "irun.h"
#include "allocate.h"
#include "garbage.h"

#define FRAMEBUG(x) 

LispObject dlp,dp;

LispObject elvira_slowcall_object;

LispObject Slowcall(LispObject *stacktop)
{
  LispObject res;

  if (elvira_slowcall_object == nil)
    CallError(stacktop,"slowcall: object to call unknown",nil,NONCONTINUABLE);

  res = module_mv_apply_1(stacktop);
  elvira_slowcall_object = NULL;

  return(res);
}

/* Copy the current display onto the heap if necessary */

void transfer_display_to_heap(LispObject *stacktop)
{
  if (dp != nil) {

    if (FRAME_TYPE(dp) == nil) {    /* Copy it to the heap */
      LispObject temp;
      int i;

      STACK(dp);
      temp = (LispObject) allocate_vector(stacktop,dp->VECTOR.length);
      UNSTACK(1);

      for (i = dp->VECTOR.length-1; i > 0; --i) 
	VREF(temp,i) = VREF(dp,i);

      VREF(temp,0) = lisptrue; /* Heap frame */

      dlp = dp = temp;
    }

  }
}

LispObject allocate_e_function(LispObject *stacktop,
	       LispObject mod,LispObject (*fun)(LispObject*),int args)
{
  LispObject f;
#if 0
FRAMEBUG(printf("Grabbing function object %d\n",args); fflush(stdout);)

  f = allocate_module_function(stacktop,mod,nil,fun,args);
  lval_typeof(f) = TYPE_E_FUNCTION;

  STACK_TMP(f);
  transfer_display_to_heap(stacktop);
  UNSTACK_TMP(f);
  
  f->C_FUNCTION.env = dp; /* Right? */

FRAMEBUG(printf("Grabbed function object %d\n",args); fflush(stdout);)
#endif
  return(f);
}

/****** THIS CANNOT POSSIBLY WORK ********/
void init_stack_frame(LispObject frame,int n)
{
  int i;

FRAMEBUG(printf("Initialising stack frame %d\n",n); fflush(stdout);)

  lval_typeof(frame) = TYPE_VECTOR;
  gcof(frame) = -1;
  lval_classof(frame) = Vector;

  frame->VECTOR.length = n+2;

  FRAME_TYPE(frame) = nil; /* Stack frame */
  LAST_FRAME(frame) = nil;

  for (i=0; i<n; ++i) VREF(frame,i+2) = nil;

FRAMEBUG(printf("Initialised stack frame %d\n",n); fflush(stdout);)
}
  
LispObject allocate_e_macro(LispObject *stacktop,
			    LispObject mod,
			    LispObject (*fun)(LispObject*),int args)
{
  LispObject f;
#if 0
  f = allocate_module_function(stacktop,mod,nil,fun,args);
  
  lval_typeof(f) = TYPE_E_MACRO;
  f->C_FUNCTION.env = dp; /* Right? */
#endif
  return(f);
}

LispObject *dynamic_ref(LispObject name)
{
  LispObject ee = DYNAMIC_ENV();

  while (ee != NULL)
    if (ee->ENV.variable == name) 
      return(&(ee->ENV.value));
    else
      ee = ee->ENV.next;

  if (name->SYMBOL.gvalue != NULL) 
    return(&(name->SYMBOL.gvalue));
  else
    CallError("dynamic: name unbound",name,NONCONTINUABLE);

  return(&nil);
}

LispObject dynamic_setq(LispObject name,LispObject value)
{
  LispObject ee = DYNAMIC_ENV();

  while (ee != NULL)
    if (ee->ENV.variable == name) 
      return(ee->ENV.value = value);
    else
      ee = ee->ENV.next;

  if (name->SYMBOL.gvalue != NULL) 
    return(name->SYMBOL.gvalue = value);
  else
    CallError("dynamic-setq: name unbound",name,NONCONTINUABLE);

  return(nil);
}
      
/*
void initialise_elvira_modules(LispObject *stacktop) 
{
  dp = nil;

  ELVIRA_INIT_CALL();
}
*/



