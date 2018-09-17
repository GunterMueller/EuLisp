/* ******************************************************************** */
/*  calls.c          Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* explicit funcalls							*/
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, March 1990 (Compiler rationalisation) 
 */

#include "funcalls.h"
#include "defs.h"
#include "structs.h"

#include "error.h"
#include "global.h"

#include "allocate.h"
#include "lists.h"
#include "modules.h"
#include "modboot.h"
#include "class.h"

#include "calls.h"

EUFUN_1( Fn_functionp, obj)
{
  return(EUCALL_2(Fn_subclassp,classof(obj),Function));
}
EUFUN_CLOSE

EUFUN_1( Fn_real_functionp, obj)
{
  LispObject a;
  EUCALLSET_2(a, Fn_subclassp, classof(obj), Function);
  obj = ARG_0(stackbase);
  return ((a!=nil && !is_macro(obj)) ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_function_lambda_list, form)
{
  if (!is_function(form) && !is_macro(form))
    form = CallError(stacktop,
		     "Not function in function-lambda-list",form,NONCONTINUABLE);
  if (is_i_function(form) || is_i_macro(form)) return (form->I_FUNCTION).bvl;
  if (is_c_function(form)) {
    int args = form->C_FUNCTION.argtype;
    LispObject ans = nil;
    LispObject tt = nil;
    char *name = 
       "@\0a\0b\0c\0d\0e\0f\0g\0h\0i\0j\0k\0l\0m\0n\0o\0p\0q\0r\0s\0t\0u\0v\0w\0x\0y\0z";
    if (args<0) {
      ans = (LispObject)get_symbol(stacktop,"@");
      args = -args-1;
    }
    while (args>0) {
      STACK_TMP(ans);
      tt = (LispObject)get_symbol(stacktop,name+2*args);
      UNSTACK_TMP(ans);
      EUCALLSET_2(ans, Fn_cons, tt, ans);
      args--;
    }
    return ans;
  }
  fprintf(stderr,"What is an e-function??\n");
  return nil;
}
EUFUN_CLOSE

EUFUN_2( Fn_apply, fun, args)
{
  LispObject ret;
  /* args are automatically listed so... */
  EUCALLSET_2(ret,module_mv_apply_1,fun,args);
  return(ret);
}
EUFUN_CLOSE

static LispObject nary_apply_aux(LispObject *stacktop, LispObject l)
{
  LispObject xx;
  if (l == nil) return(nil);
  if (!is_cons(CDR(l))) {
    if (!is_cons(CAR(l)) && CAR(l) != nil)
      CallError(stacktop,"apply: bad list",CAR(l),NONCONTINUABLE);
    else
      return(CAR(l));
  }
  STACK_TMP(CAR(l));
  xx = nary_apply_aux(stacktop,CDR(l));
  UNSTACK_TMP(l);
  return(EUCALL_2(Fn_cons, l, xx));
}

EUFUN_2( Fn_nary_apply, fun, stuff)
{
  LispObject ans;

  ans = nary_apply_aux(stacktop,stuff);
  EUCALLSET_2(ans, Fn_apply, ARG_0(stackbase), ans);
  return(ans);
}
EUFUN_CLOSE

EUFUN_2( apply1, fun, arg)
{
  LispObject ret;
  
  ret=module_apply_args(stackbase+1,1,fun);
#if 0 /* old Thu Jun 10 11:18:57 1993 */
/**/    EUCALLSET_2(arg, Fn_cons,ARG_1(stackbase),nil); /* Multiple valuize */
/**/    EUCALLSET_2(ret,module_mv_apply_1,ARG_0(stackbase),arg);
#endif /* old Thu Jun 10 11:18:57 1993 */
    return ret;
}
EUFUN_CLOSE

EUFUN_3( apply2, fun, arg1, arg2)
{
  LispObject ret;

  ret=module_apply_args(stackbase+1,2,fun);

#if 0  /* old: Thu Jun 10 11:18:36 1993 */
/**/  EUCALLSET_2(arg2,Fn_cons,arg2,nil); /* Multiple valuize */
/**/  EUCALLSET_2(arg1,Fn_cons,ARG_1(stackbase),arg2);
/**/  EUCALLSET_2(ret,module_mv_apply_1,ARG_0(stackbase),arg1);
#endif /* old Thu Jun 10 11:18:36 1993 */
  return ret;
}
EUFUN_CLOSE

EUFUN_4( apply3, fun, arg1, arg2,arg3)
{
  LispObject ret;

  ret=module_apply_args(stackbase+1,3,fun);

  return ret;
}
EUFUN_CLOSE

EUFUN_2( macroexpand_1, mod, exp)
{
  LispObject op,ret;
  LispObject bind;

  if (!is_cons(exp)) {
    EUCALLSET_2(ret, Fn_cons, nil,nil);
    EUCALLSET_2(ret, Fn_cons, ARG_1(stackbase),ret);
    return(ret);
  }

  exp=ARG_1(stackbase);
  op = CAR(exp); 

  if (!is_symbol(op)) {
    EUCALLSET_2(ret, Fn_cons, nil,nil);
    EUCALLSET_2(ret, Fn_cons, ARG_1(stackbase),ret);
    return(ret);
  }

  mod=ARG_0(stackbase);
  /* HACK !!! Should really be imported test */
  bind=GET_BINDING(mod,op);
  if (bind==nil) {
    EUCALLSET_2(ret, Fn_cons, nil,nil);
    EUCALLSET_2(ret, Fn_cons, ARG_1(stackbase),ret);
    return(ret);
  }  

  op = symbol_ref(stacktop,mod,NULL,op);
  
  if (!is_macro(op)) {
    EUCALLSET_2(ret, Fn_cons, nil,nil);
    EUCALLSET_2(ret, Fn_cons, ARG_1(stackbase),ret);
    return(ret);
  }

  /* What a dumb order... I'll rewrite it later (?) */

  EUCALLSET_2(ret,module_mv_apply_1,op,CDR(exp));
  STACK_TMP(ret);
  EUCALLSET_2(exp, Fn_cons, lisptrue, nil);
  UNSTACK_TMP(ret);
  EUCALLSET_2(ret, Fn_cons, ret, exp);
  return(ret);
}
EUFUN_CLOSE

EUFUN_3( Sf_macroexpand_1, mod, env, forms)
{
  LispObject ret;

  if (!is_cons(forms))
    CallError(stacktop,"macroexpand-1: null form",forms,NONCONTINUABLE);

  EUCALLSET_2(ret, macroexpand_1,mod,CAR(forms));

  return(ret);
}
EUFUN_CLOSE

EUFUN_3( Sf_macroexpand, mod, env, forms)
{
  LispObject last,res,exp;

  if (!is_cons(forms))
    CallError(stacktop,"macroexpand: null form",forms,NONCONTINUABLE);

  exp = CAR(forms);
  
  res = nil;

  do {

    last = res;
    STACK_TMP(last);
    mod = ARG_0(stackbase);
    EUCALLSET_2(res, macroexpand_1, mod, exp);
    UNSTACK_TMP(last); 
    exp = CAR(res);

  } while (CAR(CDR(res)) != nil);

  if (last != nil)
    return(last);
  else
    return(res);
}
EUFUN_CLOSE

/* Macroexpand with this macro... */

EUFUN_2( Fn_apply_macro, macro, form)
{	
  LispObject ret;

  if (!is_macro(macro))
    CallError(stacktop,"apply-macro: non-macro",macro,NONCONTINUABLE);

  EUCALLSET_2(ret,module_mv_apply_1,macro,form);	
  return ret;
}
EUFUN_CLOSE

/* Predicate... */

EUFUN_1( Fn_macrop, obj)
{

  return( is_macro(obj) ? lisptrue : nil);

}
EUFUN_CLOSE


/*******	
 * modified handler interactions
 *
 *******/

EUFUN_1(Fn_push_handler,handler)
{
  HANDLER_STACK() = EUCALL_2(Fn_cons,handler,HANDLER_STACK());
  
  return (HANDLER_STACK());
}
EUFUN_CLOSE

EUFUN_0(Fn_pop_handler)
{
  HANDLER_STACK() = CDR(HANDLER_STACK());
  
  return HANDLER_STACK();
}
EUFUN_CLOSE

/* I'll never write a complicated one (in C) */
EUFUN_1(Fn_simple_call_cc,fn)
{
  LispObject cont;
  LispObject args;
  LispObject val;

  cont=allocate_continue(stacktop);
  
  STACK_TMP(cont);

  if (set_continue(stacktop,cont))
    {	/* forcible return */

      UNSTACK_TMP(cont);
      return(cont->CONTINUE.value);
    }

  UNSTACK_TMP(cont);
  STACK_TMP(cont);
  args=EUCALL_2(Fn_cons,cont,nil);

  val=EUCALL_2(module_mv_apply_1,ARG_0(stackbase)/*fn*/,args);
  UNSTACK_TMP(cont);
  unset_continue(cont);
  return(val);

}
EUFUN_CLOSE

EUFUN_2(Fn_unwind_protect, protected_fn, exit_fn)
{
  void call_continuation(LispObject *,LispObject, LispObject);
  LispObject cont,val;

  cont = allocate_continue(stacktop); /* Allocate and freeze */
  protected_fn = ARG_0(stackbase);
  exit_fn = ARG_1(stackbase);
  STACK_TMP(cont);
  if (set_continue(stacktop,cont))
    {
      /** Invoked **/
      EUCALL_2(module_mv_apply_1,ARG_1(stackbase)/*exit_fn*/,nil);
      UNSTACK_TMP(cont);
      call_continuation(stacktop,cont->CONTINUE.target,cont->CONTINUE.value);
    }
      
  cont->CONTINUE.unwind=TRUE;
  val=EUCALL_2(module_mv_apply_1,ARG_0(stackbase)/*protected_fn*/,nil);

  /* kill cont */
  UNSTACK_TMP(cont);
  unset_continue(cont);
  
  /* exit stuff */
  STACK_TMP(val);
  val=EUCALL_2(module_mv_apply_1,ARG_1(stackbase)/*exit_fn*/,nil);
  UNSTACK_TMP(val);

  return val;
}
EUFUN_CLOSE

/*
 * The continuation hacking special forms
 */

void call_continuation(LispObject *stacktop,LispObject cont,LispObject value)
{
  LispObject last;

  /* First, check the continuation's still live... */

  if (!cont->CONTINUE.live)
    CallError(stacktop,"continuation call: dead continuation",cont,NONCONTINUABLE);

  if (cont->CONTINUE.thread != CURRENT_THREAD())
    CallError(stacktop,
	      "continuation call: not on this thread",cont,NONCONTINUABLE);

  /* That's cool, now wander down (up?) the dynamic continuation list
             killing stuff off and looking for unwind protects        */

  last = SYSTEM_THREAD_SPECIFIC_VALUE(state_last_continue);

  while (last != cont) {

    if (last == nil) {
      fprintf(stderr,"AARRRRGGHHH!!!: continuation vanished!");
      exit(1);
    }

    if (last->CONTINUE.unwind) {
      LispObject temp;

      /* We have an unwind continuation */

      /* Leave interesting info for unwind-protect */

      last->CONTINUE.target = cont;
      last->CONTINUE.value = value;

      /* Kill this unwind continuation */

      temp = last;
      last 
	= SYSTEM_THREAD_SPECIFIC_VALUE(state_last_continue) 
	  = temp->CONTINUE.last_continue;

      /* Jump... */

      call_continue(stacktop,temp,value);

    }

    /* Normal continuation - kill it ! */

    {
      LispObject temp;

      temp = last->CONTINUE.last_continue;
      last->CONTINUE.live = FALSE;
      last->CONTINUE.last_continue = nil;
      last = SYSTEM_THREAD_SPECIFIC_VALUE(state_last_continue) = temp;

    }

  }

  /* We've hit our own, so all is hunkydory */

  /* Jump away... */

  call_continue(stacktop,cont,value);

} 

EUFUN_1(Fn_set_no_fn_callback,val)
{

  CAR(Cb_no_function_fn)=val;
  
  return val;
}
EUFUN_CLOSE

/*

 * Initialise calls

 */

#define CALLS_ENTRIES 13
MODULE Module_calls;
LispObject Module_calls_values[CALLS_ENTRIES];

void initialise_calls(LispObject *stacktop)
{
  open_module(stacktop,
	      &Module_calls,
	      Module_calls_values,
	      "calls",
	      CALLS_ENTRIES);

  (void) make_module_function(stacktop,"apply",Fn_nary_apply,-2);
  (void) make_module_special(stacktop,"macroexpand-1",Sf_macroexpand_1);
  (void) make_module_special(stacktop,"macroexpand",Sf_macroexpand);
  (void) make_module_function(stacktop,"apply-macro",Fn_apply_macro,2);
  (void) make_module_function(stacktop,"macrop",Fn_macrop,1);
  (void) make_module_function(stacktop,"functionp",Fn_real_functionp,1);
  (void) make_module_function(stacktop,"function-lambda-list",Fn_function_lambda_list,1);

  (void) make_module_function(stacktop,"push-handler",Fn_push_handler,1);
  (void) make_module_function(stacktop,"pop-handler",Fn_pop_handler,0);
  (void) make_module_function(stacktop,"simple-call/cc",Fn_simple_call_cc,1);
  (void) make_module_function(stacktop,"fn-unwind-protect",Fn_unwind_protect,2);
  (void) make_module_function(stacktop,"set-no-function-callback",Fn_set_no_fn_callback,1);
  (void) make_module_function(stacktop,"get-backtrace-frame",Fn_backtrace_by_arg,1);
  close_module();
}
