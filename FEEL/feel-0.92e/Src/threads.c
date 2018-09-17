/* ******************************************************************** */
/* threads.c         Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Lightweight processes		                                */
/* ******************************************************************** */

/*
 * $Id: threads.c,v 1.3 1994/02/18 17:25:06 djb Exp $
 *
 * $Log: threads.c,v $
 * Revision 1.3  1994/02/18  17:25:06  djb
 * changed GC_STACK_RATIO from (4) to (2) otherwise it
 * exploded during heavy duty thread based work-outs.
 *
 * Revision 1.2  1994/01/26  12:00:07  djb
 * removed DGC calls to tidy_stacks
 *
 * Revision 1.1  1994/01/25  13:45:08  djb
 * Initial revision
 *
 * Revision 2.1  1993/01/19  15:41:22  pab
 * New Version
 * SYSV paranoia
 *
 * Revision 1.22.1.1  1993/01/17  18:04:48  pab
 * New Version.
 * Added a volatile, ++paranoia regarding old thread chain
 * reformatted code
 *
 * Revision 1.22  1992/11/26  16:13:18  pab
 * Not much
 *
 * Revision 1.20  1992/08/06  18:15:32  pab
 * init. method -> function
 *
 * Revision 1.19  1992/06/01  13:48:33  pab
 * clipper better fix
 *
 * Revision 1.18  1992/05/28  11:28:47  pab
 * moved initialisation around for compiler
 *
 * Revision 1.17  1992/04/29  12:35:11  pab
 * clipper hack
 *
 * Revision 1.16  1992/03/13  18:10:07  pab
 * SysV fixes (protection around semaphores)
 *
 * Revision 1.15  1992/02/10  12:02:38  pab
 * Debugger addition, plus sysV fix
 *
 * Revision 1.14  1992/02/03  00:38:43  pab
 * pre sysV hack
 *
 * Revision 1.13  1992/01/29  20:10:43  pab
 * fewer exports in Generic version
 *
 * Revision 1.12  1992/01/29  13:51:00  pab
 * sysV fixes
 *
 * Revision 1.11  1992/01/21  22:23:52  pab
 * fixed call to garbage_collect
 *
 * Revision 1.10  1992/01/15  21:23:52  pab
 * Fixed alignment problems; made threads allocate int arrays
 *
 * Revision 1.9  1992/01/09  22:29:10  pab
 * Fixed for low tag ints
 *
 * Revision 1.8  1992/01/07  22:15:37  pab
 * ncc compatable, plus backtrace
 *
 * Revision 1.7  1992/01/07  16:18:35  pab
 * tidy of continuation fns
 *
 * Revision 1.6  1992/01/05  22:48:30  pab
 * Minor bug fixes, plus BSD version
 *
 * Revision 1.5  1991/12/22  15:14:43  pab
 * Xmas revision
 *
 * Revision 1.4  1991/11/15  13:45:47  pab
 * copyalloc rev 0.01
 *
 * Revision 1.3  1991/09/22  19:14:43  pab
 * Fixed obvious bugs
 *
 * Revision 1.2  1991/09/11  12:07:49  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:50:09  pab
 * Initial revision
 *
 * Revision 1.11  1991/06/17  19:01:05  pab
 * Adjusted set_assoc
 *
 * Revision 1.10  1991/06/17  18:58:28  kjp
 * just in case
 *
 * Revision 1.9  1991/04/16  17:59:57  kjp
 * Tidy.
 *
 * Revision 1.8  1991/03/01  15:50:12  kjp
 * Fixed any machine version.
 *
 * Revision 1.7  1991/02/28  14:14:48  kjp
 * Lots of good stuff.
 *
 * Revision 1.6  1991/02/13  18:26:27  kjp
 * Pass.
 *
 */

#define COBUG(x) /* fprintf(stderr,"COBUG:");x;fflush(stderr) */

/*
 * Change Log:
 *   Version 1, April 1990
 */

#include "defs.h"
#include "structs.h"
#include "funcalls.h"

#include "global.h"
#include "error.h"

#include "calls.h"
#include "modboot.h"
#include "symboot.h"

#include "allocate.h"
#include "modules.h"
#include "threads.h"
#include "class.h"
#include "vectors.h"
#include "garbage.h"
#include "streams.h"

extern void free(void*);
extern LispObject Thread_Class;

int command_line_x_debug;

/* *************************************************************** */
/* Simple functions for all machines                               */
/* *************************************************************** */

EUFUN_1( Fn_threadp, obj)
{
  return((is_thread(obj)?lisptrue:nil));
}
EUFUN_CLOSE

EUFUN_0( Fn_current_thread)
{
  return(CURRENT_THREAD());
}
EUFUN_CLOSE

EUFUN_1( Fn_continuationp, obj)
{
  return (is_continue(obj) ? lisptrue : nil);
}
EUFUN_CLOSE

#ifdef MACHINE_ANY

EUFUN_2(Fn_thread_start, thread, args)
{
  CallError(stacktop,
	    "thread-start: threads not supported",thread,NONCONTINUABLE);
  return nil;
}
EUFUN_CLOSE

#else 

/* *************************************************************** */
/* When machines can actually do stuff                             */
/* *************************************************************** */

#define SCHEDBUG(x) /* fprintf(scheduler_debug,"%d:",system_scheduler_number); \
                    x ;fflush(scheduler_debug) ;*/ /*while(getchar()!='\n');*/
#define SDS (scheduler_debug)

#define SET_STATE(th) \
  (set_continue(stacktop,((th)->THREAD.state)))

#define PROCEED(cont,value) \
  stacktop = load_thread(cont->CONTINUE.thread); \
  call_continue(stacktop,cont,value);

#define RUN_THREAD(th) \
  PROCEED(((th->THREAD.state)),th->THREAD.args);

#define RUN_DISPATCHER(arg) \
  { \
    LispObject th = SYSTEM_THREAD_SPECIFIC_VALUE(local_dispatcher_thread); \
    PROCEED(((th->THREAD.state)),arg); \
  }

#define STACK_FIDDLE (16)

#define HOG_THREAD(th)
#define RELEASE_THREAD(th)

/* Queue for default scheduling methods... */

SYSTEM_GLOBAL(LispObject,list_ready_thread_queue);
SYSTEM_GLOBAL(SystemSemaphore,list_ready_thread_queue_sem);
static SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject,local_dispatcher_thread);
static SYSTEM_GLOBAL(LispObject,current_dispatcher_function);
static SYSTEM_GLOBAL(LispObject,list_dispatcher_threads);

/* Stack switch user... */

static SYSTEM_THREAD_SPECIFIC_DECLARATION(jmp_buf,rig_escape);
static SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject,rig_thread);

/* REMEMBER: within this function, we're on the thread's stacks!!! */

void rig_thread_aux()
{
  extern LispObject Fn_apply(LispObject*);

  LispObject *stacktop;
  LispObject xx;
  LispObject thread = SYSTEM_THREAD_SPECIFIC_VALUE(rig_thread);

  if (!setjmp(thread->THREAD.state->CONTINUE.machine_state))
    longjmp(SYSTEM_THREAD_SPECIFIC_VALUE(rig_escape),TRUE);

  stacktop = thread->THREAD.state->CONTINUE.gc_stack_pointer;
  STACK_TMP(thread);
  EUCALLSET_2(xx,
	      Fn_apply,thread->THREAD.fun,thread->THREAD.args);
  UNSTACK_TMP(thread);
  thread->THREAD.value=xx;
  thread_status(thread) = THREAD_RETURNED;

  STACK_TMP(thread);
  SCHEDBUG((fprintf(SDS,"thread returned "),
	    EUCALL_2(Fn_print,thread,SchedOut)));
  UNSTACK_TMP(thread);

  RUN_DISPATCHER(thread);
}
  
LispObject system_thread_rig(LispObject *stacktop, LispObject thread)
{
  int start; /* address to set sp register to */
  /* Allocate the stacks */

  STACK_TMP(thread);
  thread_stack_base(thread)
    = (int *) allocate_stack(stacktop,thread_stack_size(thread)*sizeof(int));
  UNSTACK_TMP(thread);
  STACK_TMP(thread);
  thread_gc_stack_base(thread)
    = (LispObject *) allocate_stack(stacktop,thread_gc_stack_size(thread)*sizeof(int));
  UNSTACK_TMP(thread);
  STACK_TMP(thread);
  thread->THREAD.state->CONTINUE.gc_stack_pointer
    = thread_gc_stack_base(thread);

  if (setjmp(SYSTEM_THREAD_SPECIFIC_VALUE(rig_escape))) return(thread);
  SYSTEM_THREAD_SPECIFIC_VALUE(rig_thread) = thread;
  
  if (thread_stack_base(thread)==NULL)
    CallError(stacktop,"Rig: Got strange thread\n",thread,NONCONTINUABLE);

  /* The ~7 is to align on a nice boundary --- no real point making it a #define */
  start=(int) (thread_stack_base(thread)
                      + thread_stack_size(thread) - STACK_FIDDLE)&(~7);
#ifdef STACK_START_MISALIGNED
  start+=4;
#endif
  /*fprintf(stderr,">>>>>%08x, %08x\n",start,rig_thread_aux);*/
  stack_switch_and_go(start,
		      (int) rig_thread_aux);

  return(nil);
}

/*
 * Free re-usable resources of unrunnable threads... 
 */

void shut_down_thread(LispObject *stacktop,LispObject th)
{
  void deallocate_stack(LispObject *, char *, int);

  th->THREAD.state->CONTINUE.gc_stack_pointer = NULL;
  STACK_TMP(th);
  deallocate_stack(stacktop,(char *) (thread_stack_base(th)), 
		   thread_stack_size(th)*sizeof(int));	
  deallocate_stack(stacktop,(char *) (thread_gc_stack_base(th)),
		   thread_gc_stack_size(th)*sizeof(int));	
  UNSTACK_TMP(th);
  thread_stack_base(th) = NULL;
  thread_gc_stack_base(th) = NULL;

}

/* Simple thread creation... */

#define MIN_THREAD_STACK_SIZE (4*1024)
#define GC_STACK_RATIO        (2)

static SYSTEM_GLOBAL(LispObject,default_thread_stack_size);

EUFUN_0( Fn_default_thread_stack_size)
{
  return(SYSTEM_GLOBAL_VALUE(default_thread_stack_size));
}
EUFUN_CLOSE

EUFUN_1( Fn_default_thread_stack_size_setter, size)
{
  int csize;

  if (!is_fixnum(size))
    CallError(stacktop,"(setter default-thread-stack-size): non-integer",
	      size,NONCONTINUABLE);

  csize = intval(size);

  if (csize < MIN_THREAD_STACK_SIZE)
    CallError(stacktop,"(setter default-thread-stack-size): too small",
	      size,NONCONTINUABLE);

  SYSTEM_GLOBAL_VALUE(default_thread_stack_size) = size;

  return(size);
}
EUFUN_CLOSE

#endif /* ifndef MACHINE_ANY */

static LispObject Cb_signal_callback;

void call_thread_signal(LispObject *stacktop, LispObject thread, int sig)
{
  LispObject i,h;
  STACK_TMP(thread);
  i=allocate_integer(stacktop,sig);
  h=CAR(Cb_signal_callback);
  if (h==nil)	
    CallError(stacktop,"Received interrupt with no handler",i,NONCONTINUABLE);
  UNSTACK_TMP(thread);

  EUCALL_3(apply2,h,thread,i);
}
   
EUFUN_1(Fn_set_sig_handler,fn);
{
  CAR(Cb_signal_callback)=fn;
}
EUFUN_CLOSE

#ifndef MACHINE_ANY

EUFUN_2(Fn_make_thread, fun, args)
{
  LispObject thread;

  if (!is_cons(args)) {

    thread 
      = 
	(LispObject) 
	  allocate_thread(stacktop,
			  intval(SYSTEM_GLOBAL_VALUE(default_thread_stack_size)),
			  intval(SYSTEM_GLOBAL_VALUE(default_thread_stack_size))
			  / GC_STACK_RATIO,
			  0);
  }
  else {
    LispObject size;
    int csize;

    if (!is_fixnum((size = CAR(args))))
      CallError(stacktop,"make-thread: invalid size",size,NONCONTINUABLE);

    csize = intval(size);

    if (csize <= 0)
      CallError(stacktop,"make-thread: negative size",size,NONCONTINUABLE);

    if (csize < MIN_THREAD_STACK_SIZE)
      CallError(stacktop,
		"make-thread: size less than minimun",size,NONCONTINUABLE);

    thread = (LispObject) allocate_thread(stacktop,ALIGN_SIZE(csize),
					  ALIGN_SIZE(csize/GC_STACK_RATIO),0);
  }

  fun = ARG_0(stackbase);
  thread->THREAD.fun = fun;
  thread_status(thread) = THREAD_LIMBO;

  return(thread);
}
EUFUN_CLOSE

EUFUN_1( Fn_thread_reset, th)
{
  if (!is_thread(th))
    CallError(stacktop,"thread-reset: non thread",th,NONCONTINUABLE);

  if (thread_status(th) != THREAD_RETURNED 
       && thread_status(th) != THREAD_ABORTED)
    CallError(stacktop,"thread-reset: thread in use",th,NONCONTINUABLE);

  (void) system_thread_rig(stacktop,th);

  th = ARG_0(stackbase);
  th->THREAD.value = nil;
  thread_status(th) = THREAD_LIMBO;

  return(th);
}
EUFUN_CLOSE

LispObject generic_thread_call;


/* Run on the dispatcher thread... */

EUFUN_1( Fn_next_ready_thread, c)
{
  LispObject thread;

  /* Peek... */

  if (SYSTEM_GLOBAL_VALUE(list_ready_thread_queue) == nil) return(nil);

  /* For real... */

  system_open_semaphore(stacktop,
			&SYSTEM_GLOBAL_VALUE(list_ready_thread_queue_sem));
  if (SYSTEM_GLOBAL_VALUE(list_ready_thread_queue) == nil) {
    system_close_semaphore(&SYSTEM_GLOBAL_VALUE(list_ready_thread_queue_sem));
    return(nil);
  }

  thread = CAR(SYSTEM_GLOBAL_VALUE(list_ready_thread_queue));
  SYSTEM_GLOBAL_VALUE(list_ready_thread_queue)
    = CDR(SYSTEM_GLOBAL_VALUE(list_ready_thread_queue));
  CDR(thread->THREAD.thd_queue)=nil;
  system_close_semaphore(&SYSTEM_GLOBAL_VALUE(list_ready_thread_queue_sem));

  return(thread);
}
EUFUN_CLOSE

EUFUN_1( Fn_run_ready_thread, th)
{

/*
  #ifdef MACHINE_SYSTEMV
  fprintf(stderr,"{R(%x):%x}",system_scheduler_number,(int) th);
  fflush(stderr);
  #endif
*/

  while (( thread_status((volatile LispObject) th)) != THREAD_READY); /* Hedge */

  if (SET_STATE(CURRENT_THREAD())) {
    th=ARG_0(stackbase);
    return(th);
  }
  th=ARG_0(stackbase);
  /* Have we done the stack business yet? */

  if (thread_stack_base(th) == NULL) {
    system_thread_rig(stacktop,th);
    th = ARG_0(stackbase);
  }

  thread_status(th) = THREAD_RUNNING;

  RUN_THREAD(th);

  return(nil); /* Dummy */
}
EUFUN_CLOSE
  
#define SCHEDULER_RETRY_COUNT (1024) /* was 48*1024*/

EUFUN_0( Fn_dispatch)
{
  LispObject from = nil;
  int tries = 0;

 restart:

  /*
  if (SET_STATE(CURRENT_THREAD())) {
    from = CURRENT_THREAD()->THREAD.state->CONTINUE.value;
    goto restart;
  }
  */

  if (is_thread(from)) {

    switch (thread_status(from)) {

     case THREAD_RETURNED:
     case THREAD_ABORTED:

      (void) shut_down_thread(stacktop,from);
      break;

     case THREAD_READY:

      {
	LispObject tmp,cell = nil;
	STACK_TMP(from); 
	if (from->THREAD.thd_queue==nil)
	  {
	    LispObject xx;
	    xx=EUCALL_2(Fn_cons,nil,nil);
	    UNSTACK_TMP(from);
	    STACK_TMP(from);
	    from->THREAD.thd_queue=xx;
	    fprintf(stderr,"{}");
	  }
	system_open_semaphore(stacktop,
		    &SYSTEM_GLOBAL_VALUE(list_ready_thread_queue_sem));
	UNSTACK_TMP(from);
	cell=from->THREAD.thd_queue;
	    
	CAR(cell)=from;
	CDR(cell)=nil;
	EUCALLSET_2(tmp,
		    Fn_nconc,
		    SYSTEM_GLOBAL_VALUE(list_ready_thread_queue),cell);
	SYSTEM_GLOBAL_VALUE(list_ready_thread_queue)=tmp;
	system_close_semaphore(
		    &SYSTEM_GLOBAL_VALUE(list_ready_thread_queue_sem));
	
	break;
      }

     default:

      break;
    }

  }

  SCHEDBUG(printf("Setting dispatch state...\n"); fflush(stdout));

  SCHEDBUG(printf("Dispatching...\n"); fflush(stdout));

  tries = 0;
  while (TRUE) 
    {

      while (tries < SCHEDULER_RETRY_COUNT)
	{
	  LispObject thread;

	  EUCALLSET_1(thread, Fn_next_ready_thread, Thread);
	  if (is_thread(thread)) {
	    EUCALLSET_1(from, Fn_run_ready_thread, thread);
	    STACK_TMP(from);
	    GC_sync_test();
	    UNSTACK_TMP(from);
	    goto restart;
	  }

	  GC_sync_test();

	  ++tries;
	}

      system_sleep_until_kicked();

      GC_sync_test();

      tries = 0;
    }

  return(nil);
}
EUFUN_CLOSE
  
EUFUN_2(Fn_thread_start, thread, args)
{
  COBUG(fprintf(stderr,"In thread-start\n"));

  if (!is_thread(thread))
    CallError(stacktop,
	      "thread-start: non-thread argument",thread,NONCONTINUABLE);

  if (thread_status(thread) != THREAD_LIMBO)
    CallError(stacktop,
	      "thread-start: thread not in limbo",thread,NONCONTINUABLE);

  HOG_THREAD(thread);

  /* Place the args inside and wind her up... */

  thread_status(thread) = THREAD_READY;
  thread->THREAD.args = args;

  RELEASE_THREAD(thread);

  /* Bung it on the ready queue... */

  STACK_TMP(thread);
  system_open_semaphore(stacktop,&SYSTEM_GLOBAL_VALUE(list_ready_thread_queue_sem));
  UNSTACK_TMP(thread);
  {
    LispObject xx;
    STACK_TMP(thread);
    EUCALLSET_2(xx,Fn_cons,thread,nil);
    UNSTACK_TMP(thread);
    
    thread->THREAD.thd_queue=xx;
    EUCALLSET_2(xx,
		Fn_nconc, SYSTEM_GLOBAL_VALUE(list_ready_thread_queue),
		thread->THREAD.thd_queue);
    SYSTEM_GLOBAL_VALUE(list_ready_thread_queue)=xx;
  }
  system_close_semaphore(&SYSTEM_GLOBAL_VALUE(list_ready_thread_queue_sem));

  /* All is cool... */

  /* Poke layabouts... */

  system_kick_sleepers();

  return(ARG_0(stackbase));
}
EUFUN_CLOSE

EUFUN_0( Fn_thread_reschedule)
{
  LispObject thread = CURRENT_THREAD();

  HOG_THREAD(thread);
  if (SET_STATE(thread)) return(nil);
  RELEASE_THREAD(thread);

#ifdef nope /* Mon Mar  2 12:54:29 1992 */
/**/  /* following lines commented out --- probably wrong */
/**/  system_open_semaphore(stacktop,&SYSTEM_GLOBAL_VALUE(list_ready_thread_queue_sem));
/**/  SYSTEM_GLOBAL_VALUE(list_ready_thread_queue)
/**/  = EUCALL_2(Fn_nconc,SYSTEM_GLOBAL_VALUE(list_ready_thread_queue), Fn_cons(thread,nil));
/**/  system_close_semaphore(&SYSTEM_GLOBAL_VALUE(list_ready_thread_queue_sem));
/**/  /**/
#endif /* nope Mon Mar  2 12:54:29 1992 */

  /* Call the dispatcher... */

  thread_status(thread) = THREAD_READY;
  RUN_DISPATCHER(thread);

  return(nil);
}
EUFUN_CLOSE

EUFUN_0( Fn_thread_suspend)
{
  LispObject thread = CURRENT_THREAD();

  /* Must be running */
  STACK_TMP(thread);

  if (SET_STATE(thread))
    {	
      thread=ARG_0(stackbase);
      return(thread->THREAD.args);
    }

  thread_status(thread) = THREAD_LIMBO;

  RUN_DISPATCHER(nil);
  
  if (thread_signalled(thread))
    return nil;
  else
    return lisptrue;
}
EUFUN_CLOSE

EUFUN_0( Fn_abort_thread)
{
  LispObject thread = CURRENT_THREAD();

  HOG_THREAD(thread);
  thread_status(thread) = THREAD_ABORTED;
  RELEASE_THREAD(thread);

  RUN_DISPATCHER(nil);

  return(nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_thread_value, thread)
{
  int wret;
  LispObject cons_hack;
  if (!is_thread(thread))
    CallError(stacktop,"thread-value: non-thread",thread,NONCONTINUABLE);

  while (!thread_signalled(CURRENT_THREAD()))
    {
      switch (thread_status(thread)) 
	{
	case THREAD_RETURNED:  
	  cons_hack=EUCALL_2(Fn_cons,lisptrue,thread->THREAD.value);
	  return cons_hack;
	
	case THREAD_LIMBO:
	case THREAD_RUNNING:
	case THREAD_READY: 
	  EUCALL_0(Fn_thread_reschedule);
	  thread=ARG_0(stackbase);
	  break;

	case THREAD_ABORTED: 
	  CallError(stacktop,
		    "thread_value: thread aborted",thread,NONCONTINUABLE);
	  break;

	default:
	  CallError(stacktop,
		    "thread-value: bad thread status",thread,NONCONTINUABLE);
	}
    }

  cons_hack=EUCALL_2(Fn_cons,nil,nil);
  return cons_hack;
}
EUFUN_CLOSE

EUFUN_2(Fn_set_signalled,thread,value)
{
  thread_signalled(thread) = (value==lisptrue)? 1 : 0;
  
  return value;
}
EUFUN_CLOSE

static LispObject sym_limbo;
static LispObject sym_ready;
static LispObject sym_running;
static LispObject sym_returned;
static LispObject sym_aborted;

EUFUN_1( Fn_thread_state, th)
{
  if (!is_thread(th))
    CallError(stacktop,"thread-state: non-thread",th,NONCONTINUABLE);

  switch (thread_status(th)) {

   case THREAD_LIMBO:    return(sym_limbo);
   case THREAD_READY:    return(sym_ready);
   case THREAD_RUNNING:  return(sym_running);
   case THREAD_RETURNED: return(sym_returned);
   case THREAD_ABORTED:  return(sym_aborted);

   default: CallError(stacktop,"thread-state: weird state",th,NONCONTINUABLE);

  }

  return(nil); /* Dummy */
}
EUFUN_CLOSE

EUFUN_0( Fn_thread_queue)
{
  return(SYSTEM_GLOBAL_VALUE(list_ready_thread_queue));
}
EUFUN_CLOSE

EUFUN_0( Fn_kick)
{
  system_kick_sleepers();
  return(nil);
}
EUFUN_CLOSE

/* *************************************************************** */
/*                        Allocation Methods                       */
/* *************************************************************** */

static LispObject sym_stack_size;

EUFUN_2( Md_allocate_instance_Thread_Class, c, il)
{
  extern LispObject search_keylist(LispObject*,LispObject,LispObject);
  LispObject new,size;
  int i;

  if ((size = search_keylist(stacktop,il,sym_stack_size)) == unbound)
    size = SYSTEM_GLOBAL_VALUE(default_thread_stack_size);
  else {
    
    if (!is_fixnum(size))
      CallError(stacktop,"allocate-instance(thread): non-integer stack size",
		size,NONCONTINUABLE);

    if (intval(size) < MIN_THREAD_STACK_SIZE)
      CallError(stacktop,"allocate-instance(thread): stack size too small",
		size,NONCONTINUABLE);

  }

  new = 
    (LispObject) 
      allocate_thread(stacktop,
		      intval(SYSTEM_GLOBAL_VALUE(default_thread_stack_size)),
		      intval(SYSTEM_GLOBAL_VALUE(default_thread_stack_size))
		         / GC_STACK_RATIO,
		      intval(c->CLASS.local_count));

  lval_classof(new) = ARG_0(stackbase);

  return(new);
}
EUFUN_CLOSE

EUFUN_2( Fn_initialize_thread, t, il)
{
  extern LispObject Md_initialize_instance_1(LispObject*);
  extern LispObject search_keylist(LispObject*,LispObject,LispObject);
  LispObject fun;

  if ((fun = search_keylist(stacktop,il,sym_function)) == unbound)
    CallError(stacktop,"allocate-instance(thread): missing function value",
	      il,NONCONTINUABLE);

  t->THREAD.fun = fun;
  thread_status(t) = THREAD_LIMBO;
  
  return t;
}
EUFUN_CLOSE

#endif /* ifndef MACHINE_ANY */

/* *************************************************************** */
/* Test'n'debug                                                    */
/* *************************************************************** */

#ifndef MACHINE_ANY

LispObject test_reschedule_runner(LispObject* stacktop)
{
  while (TRUE) (void) EUCALL_0(Fn_thread_reschedule);

  return(nil);
}

EUFUN_1( Fn_test_reschedule, n)
{
  int cn;

  cn = intval(n);

  while (cn--) {
    LispObject th;

    th = allocate_module_function(stacktop, NULL, NULL,
				  test_reschedule_runner,0);
    EUCALLSET_2(th, Fn_make_thread, th, nil);

    printf("Test: %x\n",(int) th); fflush(stdout);

    EUCALL_2(Fn_thread_start,th,nil);
  }

  EUCALL_0(Fn_thread_suspend);

  return(nil);
}
EUFUN_CLOSE

EUFUN_0( Fn_test_gc)
{
  
  while (1) garbage_collect(stacktop);

  return(nil);
}
EUFUN_CLOSE

#endif /* ifdef MACHINE_ANY */

/* so we know who we are... Note that this is an expensive function to call*/
EUFUN_0(Fn_feel_arch)
{
#ifdef MACHINE_ANY
  return(get_symbol(stacktop,"generic"));
#elif defined(MACHINE_BSD)
  return(get_symbol(stacktop,"BSD"));
#elif MACHINE_SYSTEMV
  return(get_symbol(stacktop,"System-V"));
#else
  return(get_symbol(stacktop,"something-strange"));
#endif
}
EUFUN_CLOSE
/* *************************************************************** */
/* Initialisation of this section                                  */
/* *************************************************************** */

#ifdef MACHINE_ANY
#define THREADS_ENTRIES 7
#else
#define THREADS_ENTRIES 23
#endif

#define SET_ASSOC(a,b) \
  { LispObject tmp,tmp2; \
    STACK_TMP(a); \
    tmp2=b; \
    UNSTACK_TMP(tmp); \
    set_anon_associate(stacktop,tmp,tmp2); \
  }

MODULE Module_threads;
LispObject Module_threads_values[THREADS_ENTRIES];

void initialise_threads(LispObject *stacktop)
{
  Cb_signal_callback=EUCALL_2(Fn_cons,nil,nil);
  add_root(&Cb_signal_callback);

  open_module(stacktop,
	      &Module_threads,Module_threads_values,"threads",THREADS_ENTRIES);

  (void) make_module_function(stacktop,"threadp",Fn_threadp,1);
  (void) make_module_function(stacktop,"set-sig-handler",Fn_set_sig_handler,1);
  (void) make_module_function(stacktop,"current-thread",Fn_current_thread,0);
  (void) make_module_function(stacktop,"continuationp",Fn_continuationp,1);

  (void) make_module_function(stacktop,"feel-machine-type",Fn_feel_arch,0);

#ifdef MACHINE_ANY
  (void) make_module_entry(stacktop,"*threads-available*",nil);
  (void) make_module_function(stacktop,"thread-start",Fn_thread_start,-2);
#else
  (void) make_module_entry(stacktop,"*threads-available*",
			   allocate_integer(stacktop,RUNNING_PROCESSORS()));
  sym_stack_size = get_symbol(stacktop,"stack-size");
  add_root(&sym_stack_size);
  sym_limbo = get_symbol(stacktop,"limbo");
  add_root(&sym_limbo);
  sym_ready = get_symbol(stacktop,"ready");
  add_root(&sym_ready);
  sym_running = get_symbol(stacktop,"running");
  add_root(&sym_running);
  sym_returned = get_symbol(stacktop,"returned");
  add_root(&sym_returned);
  sym_aborted = get_symbol(stacktop,"aborted");
  add_root(&sym_aborted);

  SYSTEM_INITIALISE_GLOBAL(LispObject,
			   default_thread_stack_size,
			   allocate_integer(stacktop,MY_THREAD_STACK_SIZE));
  ADD_SYSTEM_GLOBAL_ROOT(default_thread_stack_size);

  SYSTEM_INITIALISE_GLOBAL(LispObject,list_ready_thread_queue,nil);
  ADD_SYSTEM_GLOBAL_ROOT(list_ready_thread_queue); 

  SYSTEM_INITIALISE_GLOBAL(LispObject,current_dispatcher_function,nil);
  ADD_SYSTEM_GLOBAL_ROOT(current_dispatcher_function);

  SYSTEM_INITIALISE_GLOBAL(LispObject,list_dispatcher_threads,nil);
  ADD_SYSTEM_GLOBAL_ROOT(list_dispatcher_threads);

  SYSTEM_INITIALISE_GLOBAL(SystemSemaphore,list_ready_thread_queue_sem,0);
  system_allocate_semaphore(&SYSTEM_GLOBAL_VALUE(list_ready_thread_queue_sem));

#if 0 /* Commented out 'cos initializing thread now at lisp level.. */
  (void) make_module_function(stacktop,"make-thread",Fn_make_thread,-2);
#endif
  (void) make_module_function(stacktop,"thread-start",Fn_thread_start,-2);
  (void) make_module_function(stacktop,"thread-set-signalled",Fn_set_signalled,2);
  (void) make_module_function(stacktop,"internal-thread-reschedule",Fn_thread_reschedule,0);

  (void) make_module_function(stacktop,"internal-thread-value",Fn_thread_value,1);
  (void) make_module_function(stacktop,"internal-thread-suspend",Fn_thread_suspend,0);
  (void) make_module_function(stacktop,"generic_allocate_instance,Thread_Class",
			      Md_allocate_instance_Thread_Class,2);
  (void) make_module_function(stacktop,"initialize-thread", Fn_initialize_thread,2);
  
  SYSTEM_GLOBAL_VALUE(current_dispatcher_function)
    = make_unexported_module_function(stacktop,"dispatcher",Fn_dispatch,0);

  (void) make_module_function(stacktop,"kick",Fn_kick,0);

  (void) make_module_function(stacktop,"not-thread-reset",Fn_thread_reset,1);

  (void) make_module_entry(stacktop,"*minimum-stack-size*",
			   allocate_integer(stacktop,MIN_THREAD_STACK_SIZE));

  (void) make_module_function(stacktop,"thread-state",Fn_thread_state,1);
  (void) make_module_function(stacktop,"thread-queue",Fn_thread_queue,0);

  SET_ASSOC(make_module_function(stacktop,"default-thread-stack-size",
				 Fn_default_thread_stack_size,
				 0),
	    make_module_function(stacktop,"(setter default-thread-stack-size)",
				 Fn_default_thread_stack_size_setter,
				 1));
	   
  (void) make_module_function(stacktop,"test-reschedule",Fn_test_reschedule,1);

  (void) make_module_function(stacktop,"test-gc",Fn_test_gc,0);

#endif /* ifdef MACHINE_ANY */

  close_module();

}

#ifndef MACHINE_ANY

static SYSTEM_GLOBAL(int,start_register);

#define DISPATCHER_THREAD_STACK_SIZE (4*1048) /* Woz 4 */
#define DISPATCHER_THREAD_GC_STACK_SIZE (1024)

void runtime_begin_processes(LispObject* stacktop)
{
  extern void rig_gc_thread(LispObject *);
  extern int command_line_processors;
  int i;

  RUNNING_PROCESSORS() 
    = (command_line_processors == 0 ? 1 : command_line_processors);

  rig_gc_thread(stacktop);

  SYSTEM_INITIALISE_GLOBAL(int,start_register,0);

  for (i=0; i<RUNNING_PROCESSORS(); ++i) {
    int val;
    LispObject new_dt,tmp;

    /* Create and register dispatcher thread for each new process... */

    new_dt = allocate_thread(stacktop,
			     DISPATCHER_THREAD_STACK_SIZE,
			     DISPATCHER_THREAD_GC_STACK_SIZE,0);

    new_dt->THREAD.fun = SYSTEM_GLOBAL_VALUE(current_dispatcher_function);

    (void) system_thread_rig(stacktop,new_dt);

    EUCALLSET_2(tmp,
		Fn_cons,new_dt,SYSTEM_GLOBAL_VALUE(list_dispatcher_threads));
    SYSTEM_GLOBAL_VALUE(list_dispatcher_threads)=tmp;
    val = (i == 0 ? 0 : fork());

    if (val == -1) {
      fprintf(stderr,"\nRats: fork wimped out\n\n"); fflush(stderr);
      system_lisp_exit(-1);
    }
    if (val == 0) { /* New! */
      SYSTEM_THREAD_SPECIFIC_VALUE(local_dispatcher_thread) = new_dt;
      add_root(&local_dispatcher_thread);
#ifndef NODEBUG
/*      startdb();*/
#endif
      if (i != 0) {
	runtime_reset_allocator(stacktop);

	break;
      }

    }

    ++SYSTEM_GLOBAL_VALUE(start_register);

  }

  system_register_process(i-1);
  SYSTEM_THREAD_SPECIFIC_VALUE(system_scheduler_number) = i-1;

  /* Wait for it... wait for it... */

  while (SYSTEM_GLOBAL_VALUE(start_register) != RUNNING_PROCESSORS());
  
  ON_collect();

  RUN_DISPATCHER(nil);
}

#endif

