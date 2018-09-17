/* ******************************************************************** */
/* semaphores.c      Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Lisp semaphores       		                                */
/* ******************************************************************** */

/*
 * $Id: semaphores.c,v 1.1 1994/01/25 13:45:08 djb Exp $
 *
 * $Log: semaphores.c,v $
 * Revision 1.1  1994/01/25  13:45:08  djb
 * Initial revision
 *
 * Revision 1.1  1994/01/25  13:29:33  djb
 * Initial revision
 *
 * Revision 1.8  1992/06/09  14:05:48  pab
 * fixed includes
 *
 * Revision 1.7  1992/05/28  11:27:36  pab
 * changed value vector (unused)
 *
 * Revision 1.6  1992/05/19  11:26:08  pab
 * fixed to be strings
 *
 * Revision 1.5  1992/01/29  13:46:17  pab
 * sysV fixes
 *
 * Revision 1.4  1992/01/09  22:29:01  pab
 * Fixed for low tag ints
 *
 * Revision 1.3  1992/01/05  22:48:18  pab
 * Minor bug fixes, plus BSD version
 *
 * Revision 1.2  1991/09/11  12:07:34  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:49:55  pab
 * Initial revision
 *
 * Revision 1.4  1991/03/27  18:25:06  kjp
 * Changes + arg parity correction.
 *
 * Revision 1.3  1991/02/13  18:24:43  kjp
 * Pass.
 *
 */

/*
 * Change Log:
 *   Version 1, April 1990
 */

#include "defs.h"
#include "structs.h"
#include "funcalls.h"
#include "error.h"

#include "global.h"

#include "calls.h"
#include "modboot.h"
#include "allocate.h"
#include "modules.h"
#include "threads.h"

#ifdef MACHINE_ANY

EUFUN_0( Fn_make_semaphore)
{
  fprintf(stderr, "*** locks not supported\n");
  return nil;
}
EUFUN_CLOSE

EUFUN_1( Fn_no_semaphore, sem)
{
  fprintf(stderr, "*** locks not supported\n");
  return lisptrue;		/* see lock-with-signals in lock.em */
}
EUFUN_CLOSE

#else

#define semaphoreof(x) ((SystemSemaphore*) (stringof(x)))
/* Generator... */

EUFUN_0( Fn_make_semaphore)
{
  LispObject retval;

  retval = allocate_string(stacktop,"",sizeof(SystemSemaphore));

  system_initialise_semaphore(semaphoreof(retval));

  return(retval);

}
EUFUN_CLOSE

/* Initialiser... */

EUFUN_1( Fn_primitive_initialize_semaphore, sem)
{

  if (!is_string(sem))
    CallError(stacktop,
	      "initialize-semaphore: non semaphore",sem,NONCONTINUABLE);

  /* System specific call... */

  system_initialise_semaphore(semaphoreof(sem));

  /* Trusting OK... */

  return(sem);

}
EUFUN_CLOSE

/* Opener... */

EUFUN_1( Fn_open_semaphore, sem)
{
  if (!is_string(sem))
    CallError(stacktop,"open-semaphore: non semaphore",sem,NONCONTINUABLE);

  /* System specific call... */

  while (!system_maybe_open_semaphore(stacktop,(semaphoreof(ARG_0(stackbase)))))
    {
      if (thread_signalled(CURRENT_THREAD())
	  ||SYSTEM_GLOBAL_VALUE(system_interrupt_flag))
	return nil;
      
      EUCALL_0(Fn_thread_reschedule);
    }

  /* Got it... */

  return(lisptrue);

}
EUFUN_CLOSE

/* Closer... */

EUFUN_1( Fn_close_semaphore, sem)
{

  if (!is_string(sem))
    CallError(stacktop,"close-semaphore: non semaphore",sem,NONCONTINUABLE);

  /* Syspec.. */

  system_close_semaphore((semaphoreof(sem)));

  return(sem);

}
EUFUN_CLOSE

static SYSTEM_GLOBAL(SystemSemaphore,test_sem);
static SYSTEM_GLOBAL(int,test_sum);
static SYSTEM_GLOBAL(int,test_total);

static LispObject runner(LispObject *stacktop)
{
  int n;

  for (n=0; n<SYSTEM_GLOBAL_VALUE(test_total); ++n) {
    system_open_semaphore(stacktop,&SYSTEM_GLOBAL_VALUE(test_sem));
    ++SYSTEM_GLOBAL_VALUE(test_sum);
    system_close_semaphore(&SYSTEM_GLOBAL_VALUE(test_sem));
  }

  return(nil);
}

EUFUN_2( Fn_test_internal_semaphore, threads, count)
{
  LispObject th[100];
  int cthreads,i;

  cthreads = intval(threads);

  SYSTEM_GLOBAL_VALUE(test_total) = intval(count);
  SYSTEM_GLOBAL_VALUE(test_sum) = 0;

  for (i=0; i<cthreads; ++i) {
    LispObject xx;
    xx = (LispObject)
      allocate_module_function(stacktop,
			       (LispObject)NULL,(LispObject)NULL,runner,0);
    EUCALLSET_2(th[i], Fn_make_thread, xx, nil);
    EUCALL_2(Fn_thread_start,th[i],nil);
  }

  for (i=0; i<cthreads; ++i) {
    EUCALL_1(Fn_thread_value,th[i]);
  }

  return(allocate_integer(stacktop,SYSTEM_GLOBAL_VALUE(test_sum)));
}
EUFUN_CLOSE

#endif
  
/* *************************************************************** */
/* Initialisation of this section                                  */
/* *************************************************************** */

#ifndef MACHINE_ANY
#define SEMAPHORES_ENTRIES 5
#else
#define SEMAPHORES_ENTRIES 4
#endif

MODULE Module_semaphores;
LispObject Module_semaphores_values[1];

void initialise_semaphores(LispObject *stacktop)
{

  open_module(stacktop,
	      &Module_semaphores,
	      Module_semaphores_values,"sems",SEMAPHORES_ENTRIES);

#ifdef MACHINE_ANY

  (void) make_module_function(stacktop,"make-primitive-semaphore",
			      Fn_make_semaphore,0);
  (void) make_module_function(stacktop,"initialize-primitive-semaphore",
                              Fn_no_semaphore,1);
  (void) make_module_function(stacktop,"open-primitive-semaphore",
			      Fn_no_semaphore,1);
  (void) make_module_function(stacktop,"close-primitive-semaphore",
			      Fn_no_semaphore,1);

#else

  (void) make_module_function(stacktop,"make-primitive-semaphore",Fn_make_semaphore,0);
  (void) make_module_function(stacktop,"initialize-primitive-semaphore",
			      Fn_primitive_initialize_semaphore,1);
  (void) make_module_function(stacktop,"open-primitive-semaphore",Fn_open_semaphore,1);
  (void) make_module_function(stacktop,"close-primitive-semaphore",Fn_close_semaphore,1);

  SYSTEM_INITIALISE_GLOBAL(SystemSemaphore,test_sem,0);
  SYSTEM_INITIALISE_GLOBAL(int,test_sum,0);
  SYSTEM_INITIALISE_GLOBAL(int,test_total,0);

  system_allocate_semaphore(&SYSTEM_GLOBAL_VALUE(test_sem));

  (void) make_module_function(stacktop,"test-internal-semaphores",
			      Fn_test_internal_semaphore,2);

#endif

  close_module();

}
