/* ******************************************************************** */
/*  basic.c          Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Basic functions			                                */
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, April 1989
 *      Add many functions - JPff
 *      Add rplaca & rplacd - RJB
 *      Add defmacro - JPff
 *      Introduce GC protection in places - JPff
 *	Wrote NREVERSE for fun - JPff
 *	and ASSOC - JPff
 *	Moved basic.c to generic.c - JPff
 *	Add defconstant and mutability in bindings - JPff
 *      Hacked car & cons on the nil case and fixed the consp 
 *         make_module_function so that it didn't refer to cons !! - (25/10/89) KJP
 *      Altered defun so that its body is a list of forms - (25/10/89) KJP
 */


#include "defs.h"
#include "structs.h"
#include "funcalls.h"

#include "error.h"
#include "global.h"

#include "modboot.h"
#include "specials.h"
#include "weak.h"
#include "streams.h"

#ifdef WITH_SYS_TIMES
#include <sys/types.h> /* For paranoia. no sockets => no types */
#include <sys/times.h>
#endif

#include <sys/time.h>

EUFUN_1( Fn_system, str)
{
    int exit_status;
  if (!is_string(str))
    CallError(stacktop,"system: not a string",str,NONCONTINUABLE);

  exit_status=system(stringof(str));

    return((LispObject) allocate_integer(stacktop,exit_status));
}
EUFUN_CLOSE

EUFUN_1( Fn_getenv, str)
{
  extern char *getenv(char *);
/*  extern int strlen(char *);*/
  char *value;

  if (!is_string(str))
    CallError(stacktop,"getenv: not a string",str,NONCONTINUABLE);

  value = getenv(stringof(str));

  if (value == NULL) return(nil);

  return((LispObject) allocate_string(stacktop,value,strlen(value)));
}
EUFUN_CLOSE

EUFUN_0( Fn_exit)
{
  print_string(stacktop,StdOut(),"\n\nExiting EuLisp\n\n");
  
  system_lisp_exit(0);

  return(nil);
}
EUFUN_CLOSE

EUFUN_0( Fn_make_map)
{
  extern void make_description_file(LispObject *);

  make_description_file(stacktop);

  return(nil);
}
EUFUN_CLOSE

/* Time... */

#include <sys/types.h>

EUFUN_0( Fn_system_time)
{
  extern long time(long *);
  long n;

  (void) time(&n);
  return(allocate_integer(stackbase, (int) n));
}
EUFUN_CLOSE

EUFUN_0( Fn_process_id)
{
  extern int getpid(void);
  int xx;
  xx = getpid();
  return(allocate_integer(stackbase,xx));
}
EUFUN_CLOSE

EUFUN_0( Fn_backtrace)
{
  extern void module_eval_backtrace(LispObject *);
  module_eval_backtrace(stacktop);
  return(nil);
}
EUFUN_CLOSE

EUFUN_0( Fn_cpu_time)
{
  extern long clock(void);
  int xx;
  xx=(int)(clock()/10000);
  return(allocate_integer(stackbase,xx));
}
EUFUN_CLOSE

EUFUN_0( Fn_wall_time)
{
    double t;
    struct timeval tb;
    
    gettimeofday(&tb,NULL);
    t = (double)tb.tv_sec + (((double)tb.tv_usec)/1000000.0);
    return allocate_float( stackbase, t );
}
EUFUN_CLOSE

#ifdef WITH_SYS_TIMES
EUFUN_0(Fn_sys_times)
{
  struct tms time_vals;
  long total_time;
  LispObject vals,tmp;
  
  total_time=times(&time_vals);
  vals=allocate_vector(stacktop,3);
  STACK_TMP(vals);
  tmp=allocate_integer(stacktop,total_time);
  UNSTACK_TMP(vals);
  vref(vals,0)=tmp;
  STACK_TMP(vals);
  tmp=allocate_integer(stacktop,time_vals.tms_utime);
  UNSTACK_TMP(vals);
  vref(vals,1)=tmp;
  STACK_TMP(vals);
  tmp=allocate_integer(stacktop,time_vals.tms_stime);
  UNSTACK_TMP(vals);
  vref(vals,2)=tmp;
  
  return vals;
}
EUFUN_CLOSE
#endif
EUFUN_1( Fn_system_describe, obj)
{
  printf("Address: %x\n",(int) obj);
  printf("Type: %x\n",typeof(obj));
  printf("GC: %x\n",gcof(obj));
  printf("Class: %x\n",(int) classof(obj));
  fflush(stdout);
  return(nil);
}
EUFUN_CLOSE

/* Weak pointers... */

extern LispObject allocate_weak_wrapper(LispObject*, LispObject);

EUFUN_1( Fn_make_weak_wrapper, obj)
{
  LispObject tmp;
  tmp=EUCALL_2(Fn_cons,obj,nil);
  lval_classof(tmp)=Weak_Wrapper;
  lval_typeof(tmp)=TYPE_WEAK_WRAPPER;
  return(tmp);
}
EUFUN_CLOSE

EUFUN_1( Fn_weak_wrapper_ref, w)
{
  if (!is_weak_wrapper(w))
    CallError(stacktop,
	      "weak-wrapper-ref: not a weak wrapper",w,NONCONTINUABLE);

  return(weak_ptr_val(w));
}
EUFUN_CLOSE

EUFUN_2 (Fn_weak_wrapper_ref_setter, w, obj)
{
  if (!is_weak_wrapper(w))
    CallError(stacktop,"(setter weak-wrapper-ref): not a weak wrapper",
	      w,NONCONTINUABLE);  

  weak_ptr_val(w) = obj;

  return(obj);
}
EUFUN_CLOSE

LispObject Cb_GC_hook;

EUFUN_0(Fn_post_gc_callback)
{
    return CAR(Cb_GC_hook);
}
EUFUN_CLOSE

EUFUN_1(Fn_set_post_gc_callback,val)
{
  CAR(Cb_GC_hook)=val;
  return nil;
}
EUFUN_CLOSE
/* *************************************************************** */
/* Initialisation of this section                                  */
/* *************************************************************** */

void initialise_basic(LispObject *stacktop)
{
  LispObject get,set;
  Cb_GC_hook=EUCALL_2(Fn_cons,nil,nil);
  add_root(&Cb_GC_hook);
  (void) make_module_function(stacktop,"special-operator-p",Fn_special_form_p,1);
  get = make_module_function(stacktop,"symbol-dynamic-value",Fn_dynamic,1);
  STACK_TMP(get);
  set = make_unexported_module_function(stacktop,"symbol-dynamic-value-updator",
					Fn_dynamic_setq,2);
  UNSTACK_TMP(get);
  set_anon_associate(stacktop,get,set);

  (void) make_module_function(stacktop,"system",Fn_system,1);
  (void) make_module_function(stacktop,"getenv",Fn_getenv,1);
  (void) make_module_function(stacktop,"exit",Fn_exit,0);
  (void) make_module_function(stacktop,"make-map",Fn_make_map,0);
  (void) make_module_function(stacktop,"system-time",Fn_system_time,0);
  (void) make_module_function(stacktop,"process-id",Fn_process_id,0);
  (void) make_module_function(stacktop,"backtrace",Fn_backtrace,0);
  (void) make_module_function(stacktop,"cpu-time",Fn_cpu_time,0);
  (void) make_module_function(stacktop,"wall-time",Fn_wall_time,0);

  (void) make_module_function(stacktop,"system-print",Fn_system_describe,1);
  (void) make_module_function(stacktop,"make-weak-wrapper",Fn_make_weak_wrapper,1);
  get = make_module_function(stacktop,"weak-wrapper-ref",Fn_weak_wrapper_ref,1);
  STACK_TMP(get);
  set = make_module_function(stacktop,"(setter weak-wrapper-ref)",
			     Fn_weak_wrapper_ref_setter,2);
  UNSTACK_TMP(get);
  set_anon_associate(stacktop,get,set);

  get = make_module_function(stacktop,"post-gc-callback",Fn_post_gc_callback,0);
  STACK_TMP(get);
  set = make_module_function(stacktop,"set-post-gc-callback",Fn_set_post_gc_callback,1);
  UNSTACK_TMP(get);
  set_anon_associate(stacktop,get,set);
#ifdef WITH_SYS_TIMES
  (void) make_module_function(stacktop,"cpu-times",Fn_sys_times,0);
#endif
}
