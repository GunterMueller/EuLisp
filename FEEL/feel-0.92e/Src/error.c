/* ******************************************************************** */
/*  error.c          Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Error and Signal handling	                                        */
/* ******************************************************************** */

/*
 * $Id: error.c,v 1.1 1994/01/25 13:45:08 djb Exp $
 *
 * $Log: error.c,v $
 * Revision 1.1  1994/01/25  13:45:08  djb
 * Initial revision
 *
 * Revision 1.1  1994/01/25  13:29:33  djb
 * Initial revision
 *
 * Revision 2.1  93/01/17  17:25:21  pab
 * 17 Jan 1993 The next generation...
 * 
 * Revision 1.14  1992/11/25  17:20:33  pab
 * error handlers changed to be user-level
 *
 * Revision 1.13  1992/07/22  15:35:05  pab
 * corrected fn_signal
 *
 * Revision 1.12  1992/06/27  05:04:42  kjp
 * False alarm but added this RCS header so it wasn't a complete loss...
 *
 *
 */

/*
 * Change Log:
 *   Version 1, April 1989
 *	Added names of the defined conditions - JPff
 *   Version 2, May 1989
 *	Amalgamated with section condition.c for sanity
 *   Version 3, May 1989
 *      Updated for new ideas on handlers/restarts - RJB
 *      Integrated conditions into the object system - KJP
 *   Version 4, June 1990
 *      Rewrote handlers and signals correctly - KJP
 *        - with-handler special 
 *        - generally rearranged 
 */

#include <stdio.h>
#include <string.h>
#include "defs.h"
#include "structs.h"
#include "funcalls.h"

#include "global.h"
#include "error.h"

#include "bootstrap.h"
#include "slots.h"
#include "class.h"

#include "symboot.h"
#include "modules.h"
#include "specials.h"
#include "modboot.h"
#include "ngenerics.h"
#include "calls.h"
#include "streams.h"
#include "state.h"

#define N_SLOTS_IN_CONDITION 2
/* The error system classes... */

LispObject Condition_Class; 
LispObject Default_Condition;

/* Array for pre-defind conditions... */

LispObject defined_conditions; /* a vector of junk */

extern LispObject unbound;

/*
 * Conditions...
 * Includes generation and defined slot access... 
 */

/* Predicate... */

EUFUN_1( Fn_conditionp, form)
{
  return (is_condition(form) ? lisptrue : nil);
}
EUFUN_CLOSE

/* Generator... */

EUFUN_2( Fn_make_condition, class, initlist)
{
  LispObject ans;
  
  EUCALLSET_2(ans, Fn_subclassp, classof(class),Condition_Class);
  if (ans==nil)
    CallError(stackbase, "make-condition: non condition class",
	      ARG_0(stackbase),NONCONTINUABLE);

  return(Gf_make_instance(stackbase));

}
EUFUN_CLOSE

/* 
 * Signals and Handlers...
 */

/* Heap collapse... */

void signal_heap_failure(LispObject *stacktop, int type)
{
  extern LispObject Fn_abort_thread(LispObject*);
  extern LispObject interpreter_thread;
  extern LispObject read_eval_print_continue;
  /* Cannot allocate in this function... */
  print_string(stacktop,lisptrue,
	  "\nTrapping heap exhaustion condition\n\n");
  
#ifndef MACHINE_ANY

  if (CURRENT_THREAD() == CAR(interpreter_thread)) {
    print_string(stacktop,lisptrue, 
	    "Calculation abandoned - returning to top level...\n\n");
    call_continue(stacktop,CAR(read_eval_print_continue),lisptrue);
  }

  print_string(stacktop,lisptrue,
	       "Thread aborting - wait for other failures...\n\n");
  (void) Fn_abort_thread(stacktop);

#else

  print_string(stacktop,lisptrue,
	  "Calculation abandoned - returning to top level...\n\n");
  call_continue(stacktop,CAR(read_eval_print_continue),lisptrue);

#endif
}

/* Prompt string... */

#define MAX_PROMPT_LENGTH (1024)
char current_prompt_string[MAX_PROMPT_LENGTH];
  
/* Default signal handling... */

static LispObject sym_pling_backtrace;
static LispObject sym_pling_b;
static LispObject sym_pling_quickie;
static LispObject sym_pling_q;
LispObject sym_pling_exit; 
LispObject sym_pling_root;

extern LispObject Gf_generic_write(LispObject*);

void condition_handler(LispObject *stackbase, LispObject cond,LispObject cont)
{
  extern 
    SYSTEM_THREAD_SPECIFIC_DECLARATION(int,system_scheduler_number);
  extern 
    LispObject Gf_generic_prin(LispObject*);
  extern
    void module_eval_backtrace(LispObject *);
  extern
    void quickie_module_eval_backtrace(LispObject *);
  extern
    LispObject get_history_form(LispObject);
  extern
    void put_history_form(LispObject*, LispObject);
  extern
    int get_history_count(void);

  LispObject *stacktop = stackbase;
  LispObject form,value;

  while (TRUE) {
/*    sprintf(current_prompt_string,"eulisp-handler:%x:%s!%d> ",
	    SYSTEM_THREAD_SPECIFIC_VALUE(system_scheduler_number),
	    stringof(SYSTEM_GLOBAL_VALUE(current_interactive_module)
		     ->I_MODULE.name->SYMBOL.pname),
	    get_history_count());*/
    sprintf(current_prompt_string, "error:%s!%d> ",
            stringof(SYSTEM_GLOBAL_VALUE(current_interactive_module)
	             ->I_MODULE.name->SYMBOL.pname),
	    get_history_count());

#ifndef GNUREADLINE
    print_string(stacktop,StdOut(),current_prompt_string);
    generic_apply_1(stacktop,generic_flush,StdOut());
#endif
    
    EUCALLSET_1(form, Fn_read, StdIn());
    form = get_history_form(form);
    put_history_form(stacktop, form);

    if (form == sym_pling_exit || form == q_eof) 
      {
	print_string(stacktop,StdOut(),"\n");
	return;
      }
    if (form == sym_pling_root)
      {
	SYSTEM_GLOBAL_VALUE(current_interactive_module) =
	  get_module(stacktop,sym_root);
	value = nil;
      } 
    else if (form == sym_pling_backtrace || form == sym_pling_b)
      {
	module_eval_backtrace(stacktop);
	value = nil;
      }
    else if (form == sym_pling_quickie || form == sym_pling_q) 
      {
	quickie_module_eval_backtrace(stacktop);
	value = nil;
      }
    else
      EUCALLSET_2(value,process_top_level_form,
		  SYSTEM_GLOBAL_VALUE(current_interactive_module),
		  form);
	
/*    sprintf(current_prompt_string,"eulisp-handler:%x:%s!%d< ",
	    SYSTEM_THREAD_SPECIFIC_VALUE(system_scheduler_number),
	    stringof(SYSTEM_GLOBAL_VALUE(current_interactive_module)
		     ->I_MODULE.name->SYMBOL.pname),
	    get_history_count()-1);*/
    sprintf(current_prompt_string, "error:%s!%d> ",
            stringof(SYSTEM_GLOBAL_VALUE(current_interactive_module)
	             ->I_MODULE.name->SYMBOL.pname),
	    get_history_count()-1);

    print_string(stacktop,StdOut(),current_prompt_string);

    generic_apply_2(stacktop,generic_write,value,StdOut());
    
    print_string(stacktop,StdOut(),"\n\n");
  }
}

LispObject function_bootstrap_handler;
EUFUN_2( Fn_bootstrap_handler, cond, cont)
{
  LispObject slots;

  /* Check for dumb errors... */

  if (!is_condition(cond))
    CallError(stackbase,
	      "Default Handler not given a condition",cond,NONCONTINUABLE);

  if (!is_continue(cont) && cont != nil)
    CallError(stackbase,"Invalid continuation in default handler",cont,
	      NONCONTINUABLE);

  /* Now, display error message... */

  fprintf(stderr,"\nCompiled Elvira initialisation code error!!!\n"); 

  fprintf(stderr,"\nTrapping unhandled "); 
  if (cont == nil)
    fprintf(stderr,"non-continuable \"");
  else
    fprintf(stderr,"continuable \"");

  fprintf(stderr,"error\"");
  fprintf(stderr,"Check for initcode module --- It is needed\n");
  system_lisp_exit(1);
  
  return(nil);			/* dummy return */
}
EUFUN_CLOSE

LispObject function_default_handler;

LispObject Cb_Error_Printout;

static EUFUN_1(Fn_set_print_error_callback,val)
{
  CAR(Cb_Error_Printout)=val;
  return nil;
}
EUFUN_CLOSE


EUFUN_2( Fn_default_handler, cond, cont)
{
  LispObject slots;

  /* Check for dumb errors... */

  if (!is_condition(cond))
    CallError(stackbase,
	      "Default Handler not given a condition",cond,NONCONTINUABLE);

  if (!is_continue(cont) && cont != nil)
    CallError(stackbase,"Invalid continuation in default handler",cont,
	      NONCONTINUABLE);

  /* Now, display error message... */

  /* Should check if it's a heap error... */

  if (CAR(Cb_Error_Printout)==nil)
    {
      fprintf(stderr,"Error called with no handler.\n");
      fprintf(stderr," Error message: %s.",stringof(condition_message(cond)));
    }
  else
    {
      LispObject lst;
      lst=EUCALL_2(Fn_cons,cont,nil);
      lst=EUCALL_2(Fn_cons,ARG_0(stackbase),lst);
      (void) EUCALL_2(Fn_apply,CAR(Cb_Error_Printout),lst);
    }

  {
    extern void module_eval_backtrace(LispObject *);
    extern LispObject Fn_abort_thread(LispObject *);
    extern LispObject read_eval_print_continue;
    extern LispObject interpreter_thread;
    extern void call_continuation(LispObject*,LispObject,LispObject);

    /* Go for auto-backtrace on weird threads */

    cond = ARG_0(stackbase);
    cont = ARG_1(stackbase);
    if (CURRENT_THREAD() == CAR(interpreter_thread)) {
      condition_handler(stacktop,cond,cont);
      call_continuation(stacktop,CAR(read_eval_print_continue),nil);
    }
#ifndef MACHINE_ANY
    
    print_string(stacktop,StdErr(),"ABORTING THREAD: ");
    generic_apply_2(stacktop,generic_write,CURRENT_THREAD(),StdErr());

    print_string(stacktop,StdErr(),"\n\nBacktrace follows...\n");
    module_eval_backtrace(stacktop);
    print_string(stacktop,StdErr(),"Thread aborted.\n\n");
    (void) Fn_abort_thread(stacktop);

#endif

  }

  return(nil);			/* dummy return */
}
EUFUN_CLOSE

/* User signal function... */

EUFUN_2( Fn_signal, cond, cont)
{
  LispObject stack;

  if (cont != nil && !is_continue(cont))
    CallError(stackbase,"signal: non continuation",cont,NONCONTINUABLE);

  if (!is_condition(cond))
    CallError(stackbase,"signal: not a condition",cond,NONCONTINUABLE);

  /* OK, grab a handler and do the business... */

  /* Here be strangeness - handlers are executed in the handler environment
     of their establishment => (I think) just decrementing the handler stack
     as we run along - continuations will re-instate, but keep a copy for
     GC safety... */

  stack = HANDLER_STACK();

  STACK_TMP(stack);
  
  while (is_cons(HANDLER_STACK())) {
    LispObject handle;

    handle = CAR(HANDLER_STACK()); 
    HANDLER_STACK() = CDR(HANDLER_STACK());

    /* Need this 'cos apply allocates... */
    
    if (handle == function_default_handler)
      EUCALL_2(Fn_default_handler,cond,cont);
    else
      EUCALL_3(apply2,handle,cond,cont);
    cond = ARG_0(stackbase);
    cont = ARG_1(stackbase);

    /* Back here means try again... */
  }

  /* Ack! No handler accepted!! */
  EUCALL_2(Fn_default_handler,cond,cont);
#ifdef old /* Mon Jul  6 10:56:55 1992 */
/**/
/**/  UNSTACK_TMP(stack);
/**/
/**/  HANDLER_STACK() = stack;
#endif /* old Mon Jul  6 10:56:55 1992 */

  return(cond);
}
EUFUN_CLOSE

/*
 * Internally used error handling and signalling...
 */

/* Signal condition i with message and one value... */

/* Emergency heap condition... */

LispObject condition_heap_exhausted;

void signal_message(LispObject *stackbase, int i,char *message,LispObject val)
{
  LispObject cond_class;
  LispObject cond;
  LispObject *stacktop = stackbase;
  STACK_TMP(val);

  /* Special case if out of heap... */

  if (i == HEAP_EXHAUSTED) {
    cond = condition_heap_exhausted;
    fprintf(stderr,"Heap wimped out!! Rats.\n");
    system_lisp_exit(1);
  }
  else {
    cond_class = vref(defined_conditions,i);
    cond = (LispObject) allocate_instance(stacktop,cond_class);
  }
  STACK_TMP(cond);
  condition_message(cond) = 
    (LispObject) allocate_string(stacktop,message,strlen(message));
  UNSTACK_TMP(cond);
  UNSTACK_TMP(val);
  condition_error_value(cond) = val;

  STACK_TMP(cond);
  EUCALL_2(Fn_signal,cond,nil);
  UNSTACK_TMP(cond);

  /* Returned => call default... */

  EUCALL_2(Fn_default_handler,cond,nil);

  /* Returned means deep trouble... */

  fprintf(stderr,"INTERNAL ERROR: signal returned on internal call\n");
  fprintf(stderr,"Message was: '%s'\n",message); fflush(stderr);

  system_lisp_exit(1);
}


LispObject CallError(LispObject *stackbase, char *format,LispObject x,int type)
{
  IGNORE(type);
  if (StdErr()==lisptrue)
    {	
      fprintf(stderr,"system error: %s %s\n",format,
	      is_symbol(x) ? stringof(x->SYMBOL.pname) :"");
	
      system_lisp_exit(1);
    }
  signal_message(stackbase, INTERNAL_ERROR,format,x);
  return(nil);
}

EUFUN_3( Fn_cerror, message, cond, args)
{
  LispObject cont,val;

  cont = (LispObject) allocate_continue(stackbase);

  if (set_continue(stacktop,cont)) return(cont->CONTINUE.value);

  STACK_TMP(cont);
  message = ARG_0(stackbase);
  args = ARG_2(stackbase);
  EUCALLSET_2(message, Fn_cons, message, args);
  EUCALLSET_2(message, Fn_cons, sym_message, message);
  cond = ARG_1(stackbase);
  EUCALLSET_2(message, Fn_make_condition, cond, message);
  UNSTACK_TMP(cont);
  EUCALLSET_2(val, Fn_signal, message, cont);
  call_continue(stacktop,cont,val);
  return(val);
}
EUFUN_CLOSE

EUFUN_3( Fn_error, message, cond, args)
{
  LispObject val;

  EUCALLSET_2(message, Fn_cons, message, args);
  EUCALLSET_2(message, Fn_cons, sym_message, message);
  cond = ARG_1(stackbase);
  EUCALLSET_2(message, Fn_make_condition, cond, message);
  EUCALLSET_2(val, Fn_signal, message, nil);
  return(val);
}
EUFUN_CLOSE

/* *************************************************************** */
/* Initialisation of this section                                  */
/* *************************************************************** */

#define ERRORS_ENTRIES 11
MODULE Module_errors;
LispObject Module_errors_values[ERRORS_ENTRIES];

void initialise_error(LispObject *stacktop)
{

  static char* inits[] = {
    "<Internal-Error>",		/* INTERNAL_ERROR */
    "<heap-exhausted>",		/* HEAP_EXHAUSTED */
    "<clock-tick>",		/* CLOCK_TICK */
    0
  };
  int i;

  /* Initialise condition metaclass */

  Condition_Class = (LispObject) allocate_class(stacktop,NULL);
  set_class_size(stacktop,Condition_Class,Standard_Class,0);
  add_root(&Condition_Class);
  
  Default_Condition = (LispObject) allocate_class(stacktop,NULL);
  add_root(&Default_Condition);
  set_class_size(stacktop,Default_Condition,Object, N_SLOTS_IN_CONDITION);

  defined_conditions=allocate_vector(stacktop,99);
  add_root(&defined_conditions);

  for (i=0; inits[i]; i++) {
    LispObject cond_class;
    
    cond_class=allocate_class(stacktop,Condition_Class);
    vref(defined_conditions,i) = cond_class;
    set_class_size(stacktop,vref(defined_conditions,i),Default_Condition,0);
  }

  /* Rig heap failure condition... */

  condition_heap_exhausted = 
    (LispObject) 
      allocate_instance(stacktop,
			 vref(defined_conditions,HEAP_EXHAUSTED));

  add_root(&condition_heap_exhausted);
  sym_pling_backtrace = get_symbol(stacktop,"!backtrace");
  add_root(&sym_pling_backtrace);
  sym_pling_b = get_symbol(stacktop,"!b");
  add_root(&sym_pling_b);
  sym_pling_quickie = get_symbol(stacktop,"!quickie");
  add_root(&sym_pling_quickie);
  sym_pling_q = get_symbol(stacktop,"!q");
  add_root(&sym_pling_q);
  sym_pling_exit = get_symbol(stacktop,"!exit");
  add_root(&sym_pling_exit);
  sym_pling_root = get_symbol(stacktop,"!root");
  add_root(&sym_pling_root);

  open_module(stacktop,
	      &Module_errors,
	      Module_errors_values,
	      "errors",
	      ERRORS_ENTRIES);
  
  make_module_entry(stacktop,"<condition-class>",Condition_Class);
  make_module_entry(stacktop,"<condition>",Default_Condition);
  
  for (i=0; inits[i]; i++)
    make_module_entry(stacktop,inits[i],vref(defined_conditions,i));
       
  (void) make_module_function(stacktop,"conditionp",Fn_conditionp,1);

  (void) make_module_function(stacktop,"make-condition",Fn_make_condition,-2);

  (void) make_module_function(stacktop,"internal-signal",Fn_signal,2);

  function_bootstrap_handler
    = make_unexported_module_function(stacktop,"bootstrap-handler",
				      Fn_bootstrap_handler,2);
  add_root(&function_bootstrap_handler);
  function_default_handler 
    = make_unexported_module_function(stacktop,"default-handler",Fn_default_handler,2);
  add_root(&function_default_handler);

  Cb_Error_Printout=EUCALL_2(Fn_cons,nil,nil);
  add_root(&Cb_Error_Printout);
  (void) make_module_function(stacktop,"set-print-error-callback",Fn_set_print_error_callback,1);


  close_module();
}

