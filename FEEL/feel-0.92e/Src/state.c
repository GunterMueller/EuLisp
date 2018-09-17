/* ******************************************************************** */
/* state.c           Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Lisp state           		                                */
/* ******************************************************************** */

/*
 * $Id: state.c,v 1.2 1994/01/26 11:59:05 djb Exp $
 *
 * $Log: state.c,v $
 * Revision 1.2  1994/01/26  11:59:05  djb
 * removed redundant DGC tidy_stacks code
 *
 * Revision 1.1  1994/01/25  13:45:08  djb
 * Initial revision
 *
 * Revision 1.1  1994/01/25  13:29:33  djb
 * Initial revision
 *
 * Revision 1.8  1992/11/26  16:05:43  pab
 * Lost Envs
 *
 * Revision 1.7  1992/07/13  13:15:56  djb
 * ifdef DGC (compacting mark+sweep collector)
 * then zero unused portions of c and gc-stack
 * before gc (tidy_stacks())
 *
 * Revision 1.6  1992/01/29  13:48:20  pab
 * additional debug info for sysV
 *
 * Revision 1.5  1992/01/05  22:48:22  pab
 * Minor bug fixes, plus BSD version
 *
 * Revision 1.4  1991/12/22  15:14:35  pab
 * Xmas revision
 *
 * Revision 1.3  1991/11/15  13:45:35  pab
 * copyalloc rev 0.01
 *
 * Revision 1.2  1991/09/11  12:07:42  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:50:01  pab
 * Initial revision
 *
 * Revision 1.6  1991/02/13  18:25:07  kjp
 * Pass.
 *
 */

/*
 * Change Log:
 *   Version 1, May 1990
 */

/*

 * This holds the "state" data and operations - should be system
 * independant and encapsulte ALL continuation operations...

 */

#include "funcalls.h"
#include "defs.h"
#include "structs.h"
#include "error.h"
#include "global.h"

#include "calls.h"
#include "modboot.h"
#include "allocate.h"
#include "modules.h"
#include "threads.h"

#include "state.h"

/* Fixed outside of a context switch... */

SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject,state_current_thread);
SYSTEM_THREAD_SPECIFIC_DECLARATION(int*,state_stack_base);
SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject*,state_gc_stack_base);

/* Forever wandering... */

SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject*,state_gc_stack_pointer);
SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject,state_dynamic_env);
SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject,state_last_continue);
SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject,state_handler_stack);

SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject,dp);
SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject,dlp);

/* Notionally, the registers hold the machine state */

/*
 * Loads the lisp specific state of the world into a continuation struct
 */

LispObject save_state(LispObject *stacktop,LispObject cont)
{
#ifndef NODEBUG
  extern int gc_paranoia;

  if (gc_paranoia)
    fprintf(stderr,"{Save: 0x%x->0x%x[%d]}",
	    SYSTEM_THREAD_SPECIFIC_VALUE(state_gc_stack_base),
	    stacktop,(stacktop-SYSTEM_THREAD_SPECIFIC_VALUE(state_gc_stack_base))/sizeof(LispObject));
#endif
  cont->CONTINUE.gc_stack_pointer 
    = stacktop;

  cont->CONTINUE.dynamic_env
    = SYSTEM_THREAD_SPECIFIC_VALUE(state_dynamic_env);

  cont->CONTINUE.last_continue
    = SYSTEM_THREAD_SPECIFIC_VALUE(state_last_continue);

  cont->CONTINUE.handler_stack
    = SYSTEM_THREAD_SPECIFIC_VALUE(state_handler_stack);

  cont->CONTINUE.dp 
    = SYSTEM_THREAD_SPECIFIC_VALUE(dp);

  return(cont);

}

/*
 * Similarly, the other way around...
 */

void change_state(LispObject cont)
{
  
  SYSTEM_THREAD_SPECIFIC_VALUE(state_gc_stack_pointer)
    = cont->CONTINUE.gc_stack_pointer;

  SYSTEM_THREAD_SPECIFIC_VALUE(state_dynamic_env)
    = cont->CONTINUE.dynamic_env;

  SYSTEM_THREAD_SPECIFIC_VALUE(state_last_continue)
    = cont->CONTINUE.last_continue;

  SYSTEM_THREAD_SPECIFIC_VALUE(state_handler_stack)
    = cont->CONTINUE.handler_stack;

  SYSTEM_THREAD_SPECIFIC_VALUE(dp)
    = cont->CONTINUE.dp;

  SYSTEM_THREAD_SPECIFIC_VALUE(dlp)
    = cont->CONTINUE.dp;
}

/*

 * Set a continuation...
 *
 * Note: these are just the lisp equivalents of setjmp and longjmp -
 *       they do not deal with killing other continuations apart from
 *       themselves or handling unwind protects.

 * Note also that all this hackery is required to provide abstraction
 * 'cos were it a standard function call, the stack would get nobbled.

 */

int set_continue_1(LispObject *stacktop,LispObject cont)
{

  cont->CONTINUE.thread = SYSTEM_THREAD_SPECIFIC_VALUE(state_current_thread);

  save_state(stacktop,cont);

  cont->CONTINUE.value = nil;

  return(TRUE);

}

int set_continue_2(LispObject cont)
{

  /* Fix last continue... */

  LAST_CONTINUE() = cont;

  /* All set... */

  cont->CONTINUE.live = TRUE;

  return(FALSE);

}

void call_continue(LispObject *stacktop,LispObject cont,LispObject value)
{
  
  if (!is_continue(cont)) {
    printf("****BAD CONTINUATION**** type %d - waiting...\n",typeof(cont));
    fflush(stdout);
    exit(1);
  }

  if (cont->CONTINUE.thread 
      != SYSTEM_THREAD_SPECIFIC_VALUE(state_current_thread))
    {	
      fprintf(stderr,"Wrong thread: %x[%d] %x[%d]\n",SYSTEM_THREAD_SPECIFIC_VALUE(state_current_thread),
	      SYSTEM_THREAD_SPECIFIC_VALUE(state_current_thread)->THREAD.header.gc,
	      cont->CONTINUE.thread,cont->CONTINUE.thread->THREAD.header.gc);
      CallError(stacktop,"call continuation: wrong thread",cont,NONCONTINUABLE);
    }

  cont->CONTINUE.live = FALSE;

  /* Already on current thread... */

  change_state(cont);

  cont->CONTINUE.value = value;


  longjmp(cont->CONTINUE.machine_state,TRUE);

}

/*

 * Load a thread into the system ready for execution...

 * returns the new GC stacktop
 */

LispObject* load_thread(LispObject thread)
{

  CURRENT_THREAD() = thread;

  STACK_BASE()    = thread_stack_base(thread);
  GC_STACK_BASE() = thread_gc_stack_base(thread);
  
  /* Just the flexible stuff left... */

  change_state(thread->THREAD.state);

  return (thread->THREAD.state->CONTINUE.gc_stack_pointer);
}

