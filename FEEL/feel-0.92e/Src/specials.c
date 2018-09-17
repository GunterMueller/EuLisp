/* ******************************************************************** */
/* specials.c        Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Language special forms (NOT toplevel forms)                          */
/* ******************************************************************** */

/*
 * $Id: specials.c,v 1.1 1994/01/25 13:45:08 djb Exp $
 *
 * $Log: specials.c,v $
 * Revision 1.1  1994/01/25  13:45:08  djb
 * Initial revision
 *
 * Revision 1.1  1994/01/25  13:29:33  djb
 * Initial revision
 *
 * Revision 2.1  93/01/17  17:58:13  pab
 * Deleted Env reference
 * 
 * Revision 1.17  1992/11/26  16:05:02  pab
 * Env removal, table changes, etc
 *
 * Revision 1.16  1992/06/12  00:00:55  pab
 * fixed tagbody
 *
 * Revision 1.15  1992/05/28  11:28:26  pab
 * GC protect
 *
 * Revision 1.14  1992/05/19  11:26:37  pab
 * tagbody (blech blech) fixed
 *
 * Revision 1.13  1992/04/30  19:42:18  pab
 * fixed setq(!)
 *
 * Revision 1.12  1992/04/27  21:59:49  pab
 * fixed env stacks
 *
 * Revision 1.11  1992/04/26  21:07:07  pab
 * 'lost ' handler code
 *
 * Revision 1.10  1992/03/07  21:45:16  pab
 * initial continuation changes
 *
 * Revision 1.9  1992/02/10  16:41:09  pab
 * fixed dynamics properly
 *
 * Revision 1.8  1992/01/29  13:47:28  pab
 * bindig fix, gc fix in dynamic let
 *
 * Revision 1.7  1992/01/09  22:29:05  pab
 * Fixed for low tag ints
 *
 * Revision 1.6  1992/01/07  22:13:27  pab
 * *** empty log message ***
 *
 * Revision 1.5  1992/01/05  22:48:20  pab
 * Minor bug fixes, plus BSD version
 *
 * Revision 1.4  1991/12/22  15:14:34  pab
 * Xmas revision
 *
 * Revision 1.3  1991/09/22  19:14:40  pab
 * Fixed obvious bugs
 *
 * Revision 1.2  1991/09/11  12:07:40  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:50:00  pab
 * Initial revision
 *
 * Revision 1.4  1991/02/13  18:28:55  kjp
 * Pass.
 *
 */

/*
 * Change Log:
 *   Version 1, March 1990 (Compiler rationalisation)
 *     New fully working let/cc and unwind-protect - 
 *       all stacks dealt with and dead continuations killed (KJP)
 */

#include "defs.h"
#include "structs.h"
#include "funcalls.h"
#include "error.h"
#include "global.h"

#include "slots.h"
#include "garbage.h"

#include "symboot.h"
#include "modules.h"
#include "root.h"
#include "allocate.h"
#include "specials.h"
#include "toplevel.h"
#include "state.h"
#include "streams.h"

/*

 * We're talking just the non-toplevel restricted special forms here
 * like lambda, setq, and if - the ones always available.

 */

LispObject special_table;

LispObject my_make_special(LispObject *stacktop,
			   char *name, LispObject (*func)())
{
  LispObject ans,tmp;
  
  ans = (LispObject) get_symbol(stacktop,name);
  SYM_CACHE_INIT(ans);
  STACK_TMP(ans);
  tmp = (LispObject) allocate_special(stacktop,ans,func);
  UNSTACK_TMP(ans);
  STACK_TMP(tmp);
  EUCALL_3(Fn_table_ref_setter,special_table,ans,tmp);
  UNSTACK_TMP(tmp);
  return tmp;
}

EUFUN_1( Fn_special_form_p, name)
{
  return(EUCALL_2(Fn_table_ref,special_table,name));
}
EUFUN_CLOSE

LispObject special_lambda;
EUFUN_3( Sf_lambda, mod, env, forms)
{
  LispObject bvl,myforms;
  LispObject ans,walker;
  int i;

  if (forms == nil) {
    CallError(stacktop,"lambda: illegal empty lambda form",nil,NONCONTINUABLE);
  }

  myforms = forms;

  bvl = CAR(myforms); myforms = CDR(myforms);
  STACK_TMP(bvl); STACK_TMP(myforms);

  walker = bvl; i = 0;
  while (is_cons(walker)) {
    walker = CDR(walker);
    ++i;
  }

  if (walker != nil)  /* improper lambda list */
    ans = (LispObject) allocate_i_function(stacktop,mod,env,-i -1);
  else
    ans = (LispObject) allocate_i_function(stacktop,mod,env,i);

  UNSTACK_TMP(myforms); UNSTACK_TMP(bvl);
  ans->I_FUNCTION.bvl  = bvl;
  ans->I_FUNCTION.body = myforms;
  ans->I_FUNCTION.home = ARG_0(stackbase);

  return ans;
}
EUFUN_CLOSE

LispObject special_macro_lambda;
EUFUN_3(Sf_mlambda, mod, env, forms)
{
  LispObject bvl;
  LispObject ans,walker;
  int i;

  if (forms == nil) {
    CallError(stacktop,
	      "macro-lambda: illegal empty macro-lambda form",nil,NONCONTINUABLE);
  }

  bvl = CAR(forms); forms = CDR(forms);
  ARG_2(stackbase)=forms;
  walker = bvl; i = 0;
  while (is_cons(walker)) {
    walker = CDR(walker);
    ++i;
  }
  STACK_TMP(bvl);
  if (walker != nil)  /* improper lambda list */
    ans = (LispObject) allocate_i_function(stacktop,mod,env,-i -1);
  else
    ans = (LispObject) allocate_i_function(stacktop,mod,env,i);

  UNSTACK_TMP(bvl);
  lval_typeof(ans) = TYPE_I_MACRO;
  ans->I_MACRO.bvl  = bvl;
  ans->I_MACRO.body = ARG_2(stackbase)/*forms*/;
  ans->I_MACRO.home = ARG_0(stackbase)/*mod*/;

  return ans;
}
EUFUN_CLOSE

LispObject special_setq;
EUFUN_3( Sf_setq,  mod, env, forms)
{
  LispObject id;

  if (forms == nil) 
    CallError(stacktop,"setq: illegal empty setq form",nil,NONCONTINUABLE);

  id = CAR(forms); forms = CDR(forms);

  if (!is_symbol(id))
    CallError(stacktop,"setq: non-symbolic id",id,NONCONTINUABLE);

  if (CDR(forms)!=nil) 
    CallError(stacktop,"setq: additional setq forms",nil,NONCONTINUABLE);

  while (reserved_symbol_p(id)) {
    id = CallError(stacktop,"setq: reserved symbol",id,CONTINUABLE);
  }
  STACK_TMP(id);
  forms = EUCALL_3(module_eval,mod,env,CAR(forms));
  UNSTACK_TMP(id);
  STACK_TMP(forms);
  STACK_TMP(id);
  env=ARG_1(stackbase);
  while (env != NULL) 
    {
      if (env->ENV.variable == id)
	{
	  if (typeof(env)!=TYPE_FIXENV) 
	    return (env->ENV.value = forms);
	  
	  /* Used to be Fn_equal */
	  if (forms!=env->ENV.value)
	    CallError(stacktop,"setq: immutable binding",id,NONCONTINUABLE);
	  return forms;
	}
      env = (LispObject) env->ENV.next;
    }
  UNSTACK_TMP(id);
  UNSTACK_TMP(forms);
  /* Going for the module environment */
  mod=ARG_0(stackbase);
  STACK_TMP(forms);
  (void) EUCALL_3(module_set,mod,id,forms); /* In the module handler */
  UNSTACK_TMP(forms);
  return(forms);

}
EUFUN_CLOSE

LispObject special_progn;
EUFUN_3( Sf_progn, mod, env, forms)
{
  LispObject ret;

  if (!is_cons(forms))
    CallError(stacktop,"progn: bad forms",forms,NONCONTINUABLE);

  ret = nil; /* Null case return value */

  while (is_cons(forms)) {
    STACK_TMP(CDR(forms));
    ret = EUCALL_3(module_eval,ARG_0(stackbase)/*mod*/,ARG_1(stackbase)/*env*/,CAR(forms));
    UNSTACK_TMP(forms);
  }

  return(ret);
}
EUFUN_CLOSE

LispObject special_if;
EUFUN_3( Sf_if, mod, env, forms)
{
  LispObject pred,alt1,alt2;
  LispObject debug;

  debug = forms;

  if (!is_cons(forms))
    CallError(stacktop,"if: missing predicate",forms,NONCONTINUABLE);

  pred = CAR(forms); forms = CDR(forms);

  if (!is_cons(forms))
    CallError(stacktop,"if: missing consequence",debug,NONCONTINUABLE);

  alt1 = CAR(forms); forms = CDR(forms);

  if (!is_cons(forms))
    CallError(stacktop,"if: missing alternative",debug,NONCONTINUABLE);

  alt2 = CAR(forms); forms = CDR(forms);

  if (forms != nil)
    CallError(stacktop,"if: extraneous forms",forms,NONCONTINUABLE);
  
  STACK_TMP(alt1);
  STACK_TMP(alt2);
  if (EUCALL_3(module_eval,mod,env,pred) != nil) {
    UNSTACK_TMP(alt1); UNSTACK_TMP(alt1);
    return(EUCALL_3(module_eval,ARG_0(stackbase)/*mod*/,ARG_1(stackbase)/*env*/,alt1));
  }
  else {
    UNSTACK_TMP(alt2);
    return(EUCALL_3(module_eval,ARG_0(stackbase)/*mod*/,ARG_1(stackbase)/*env*/,alt2));
  }
}
EUFUN_CLOSE


/*

 * Dynamics...

 */

LispObject special_dynamic_setq;
EUFUN_3( Sf_dynamic_setq, mod, env, forms)
{
  LispObject id,form;
  LispObject walker;

  if (!is_cons(forms))
    CallError(stacktop,"dynamic-setq: missing symbol",forms,NONCONTINUABLE);

  id = CAR(forms); forms = CDR(forms);

  if (!is_symbol(id))
    CallError(stacktop,"dynamic-setq: non-symbolic reference",id,NONCONTINUABLE);

  if (!is_cons(forms)) 
    CallError(stacktop,"dynamic-setq: missing value form",forms,NONCONTINUABLE);

  form = CAR(forms); forms = CDR(forms);

  if (forms != nil)
    CallError(stacktop,"dynamic-setq: extraneous forms",forms,NONCONTINUABLE);

  walker = DYNAMIC_ENV();

  while (walker != NULL) {
    if (walker->ENV.variable == id)
      {
	STACK_TMP(walker);
	form = EUCALL_3(module_eval,mod,env,form);
	UNSTACK_TMP(walker);
	return((walker->ENV.value = form));
      }
    walker = walker->ENV.next;
  }

  if (id->SYMBOL.gvalue == NULL) {
    print_string(stacktop,StdErr(),"****Illegal assignment to undeclared variable: ");
    EUCALL_2(Fn_print,id,StdErr());
    print_string(stacktop,StdErr(),"****Implicit defvar used\n");
  }
  STACK_TMP(id);
  form = EUCALL_3(module_eval,mod,env,form);
  UNSTACK_TMP(id);
  return((id->SYMBOL.gvalue = form));
}
EUFUN_CLOSE

EUFUN_2( Fn_dynamic_setq, id, form)
{
  LispObject walker;

  if (!is_symbol(id))
    CallError(stacktop,"(setter symbol-dynamic-value): non-symbolic reference",id,NONCONTINUABLE);

  walker = DYNAMIC_ENV();

  while (walker != NULL) {
    if (walker->ENV.variable == id) return((walker->ENV.value = form));
    walker = walker->ENV.next;
  }

  if (id->SYMBOL.gvalue == NULL) {
    print_string(stacktop,StdErr(),"****Illegal assignment to undeclared variable: ");
    EUCALL_2(Fn_print,id,StdErr());
    print_string(stacktop,StdErr(),"****Implicit defvar used\n");
  }

  return((id->SYMBOL.gvalue = form));
}
EUFUN_CLOSE

LispObject special_dynamic_set;
EUFUN_3( Sf_dynamic_set, mod, env, forms)
{
  LispObject id,form;
  LispObject walker;

  if (!is_cons(forms))
    CallError(stacktop,"dynamic-set: missing symbol",forms,NONCONTINUABLE);

  id = CAR(forms); forms = CDR(forms);

  id = EUCALL_3(module_eval,mod,env,id);

  if (!is_symbol(id))
    CallError(stacktop,"dynamic-set: non-symbolic reference",id,NONCONTINUABLE);

  if (!is_cons(forms)) 
    CallError(stacktop,"dynamic-set: missing value form",forms,NONCONTINUABLE);

  form = CAR(forms); forms = CDR(forms);

  if (forms != nil)
    CallError(stacktop,"dynamic-set: extraneous forms",forms,NONCONTINUABLE);

  STACK_TMP(id);
  form = EUCALL_3(module_eval,mod,env,form);
  UNSTACK_TMP(id);
  walker = DYNAMIC_ENV();

  while (walker != NULL) {
    if (walker->ENV.variable == id) return((walker->ENV.value = form));
    walker = walker->ENV.next;
  }

  if (id->SYMBOL.gvalue == NULL) {
    print_string(stacktop,StdErr(),"****Illegal assignment to undeclared variable: ");
    EUCALL_2(Fn_print,id,StdErr());
    print_string(stacktop,StdErr(),"****Implicit defvar used\n");
  }

  return((id->SYMBOL.gvalue = form));
}
EUFUN_CLOSE

LispObject special_dynamic_let;
EUFUN_3( Sf_dynamic_let, mod, env, forms)
{
  LispObject bindings;
  LispObject save;

  if (!is_cons(forms))
    CallError(stacktop,"dynamic-let: null forms",forms,NONCONTINUABLE);

  bindings = CAR(forms); forms = CDR(forms);

  if (!is_cons(bindings)) 
    CallError(stacktop,
	      "dynamic-let: invalid binding forms",bindings,NONCONTINUABLE);

  save = DYNAMIC_ENV(); /* Hang on for exit... */
  
  STACK_TMP(save);
  STACK_TMP(forms); 
  while (is_cons(bindings)) {
    LispObject id,val,bind;
    LispObject xx;

    bind = CAR(bindings);
    STACK_TMP(CDR(bindings));
    if (!is_cons(bind))
      CallError(stacktop,
		"dynamic-let: weird binding",bindings,NONCONTINUABLE);

    id = CAR(bind); bind = CDR(bind);

    if (!is_symbol(id)) 
      CallError(stacktop,"dynamic-let: non-symbolic var",id,NONCONTINUABLE);

    if (!is_cons(bind))
      CallError(stacktop,"dynamic-let: weird binding",bindings,NONCONTINUABLE);

    val = CAR(bind);

    STACK_TMP(id);
    val = EUCALL_3(module_eval,ARG_0(stackbase),ARG_1(stackbase),val);
    UNSTACK_TMP(id);

    xx = allocate_env(stacktop,id,val,
		      ((LispObject)(DYNAMIC_ENV())));
    DYNAMIC_ENV()=xx;
    UNSTACK_TMP(bindings);
  }
  UNSTACK_TMP(forms);
  /* Do body... */
  forms = EUCALL_3(Sf_progn,ARG_0(stackbase),ARG_1(stackbase),forms);
  UNSTACK_TMP(save);
  
  DYNAMIC_ENV() = save; /* Repoint */

  return(forms);
}
EUFUN_CLOSE    

EUFUN_1( Fn_dynamic, form)
{
  {
    LispObject ee = DYNAMIC_ENV();
    while (ee!=NULL) {
      if (ee->ENV.variable == form) return ee->ENV.value;
      ee = ee->ENV.next;
    }
  }
  {
    LispObject ans;
    ans =  (form->SYMBOL).gvalue;
    if (ans==NULL) {		/* signal UNBOUND_DYNAMIC_VARIABLE */
      ans = CallError(stacktop,"Unset dynamic variable ",form,CONTINUABLE);
      (form->SYMBOL).gvalue = ans;
    }
    return ans;
  }
}
EUFUN_CLOSE

LispObject special_dynamic;
EUFUN_3( Sf_dynamic, mod, env, form)
{
  IGNORE(mod); IGNORE(env);

  while (!is_symbol(CAR(form)) || CDR(form)!=nil)
    form = CallError(stacktop,"dynamic: Illegal dynamic form ",form,CONTINUABLE);

  form = CAR(form);

  {
    LispObject ee = DYNAMIC_ENV();
    while (ee!=NULL) {
      if (ee->ENV.variable == form) return ee->ENV.value;
      ee = ee->ENV.next;
    }
  }
  {
    LispObject ans;
    ans =  (form->SYMBOL).gvalue;
    if (ans==NULL) {		/* signal UNBOUND_DYNAMIC_VARIABLE */
      ans = CallError(stacktop,"dynamic: unset dynamic variable ",form,CONTINUABLE);
      (form->SYMBOL).gvalue = ans;
    }
    return ans;
  }
}
EUFUN_CLOSE

LispObject special_quote;
EUFUN_3( Sf_quote, mod, env, forms)
{
  IGNORE(mod); IGNORE(env);

  if (!is_cons(forms))
    CallError(stacktop,"quote: bad forms",forms,NONCONTINUABLE);

  return(CAR(forms));
}
EUFUN_CLOSE

/*

 * Handlers...

 */

/* Hack... */

LispObject special_evalcm;
EUFUN_3(Sf_evalcm, mod, env, form)
{
  LispObject ans;

  if (!is_cons(form))
    CallError(stacktop,"eval/cm: no arguments",form,NONCONTINUABLE);

  if (is_cons(CDR(form)))
    CallError(stacktop,"eval/cm: too many arguments",form,NONCONTINUABLE);

  form = EUCALL_3(module_eval,mod,env,form);

  ans = EUCALL_2(process_top_level_form,mod,CAR(form));

  return(ans);
}
EUFUN_CLOSE

/* Tag Body... */

/*

 * 'tagbody'
 *
 *   Plan: Do a naive walk on the body to extract a table of symbols with
 *         following code, rig a continuation for 'go' statements to jump
 *         to and run them in sequence until done...
 
 */

/* ******************** This function cannot be called *************** */
static LispObject tagbody_before_label(LispObject *stacktop,LispObject body)
{
  if (!is_cons(body)) return(nil);
  if (is_symbol(CAR(body))) return(nil);

  return(EUCALL_2(Fn_cons,CAR(body),tagbody_before_label(stacktop,CDR(body))));
}

static LispObject tagbody_suck_symbols(LispObject *stacktop,LispObject body)
{      
  LispObject xx;
  if (!is_cons(body)) return(nil);
  if (is_symbol(CAR(body))) return(tagbody_suck_symbols(stacktop,CDR(body)));

  STACK_TMP(body);
  xx=tagbody_suck_symbols(stacktop,CDR(body));
  UNSTACK_TMP(body);
  return(EUCALL_2(Fn_cons,CAR(body),xx));
}

static LispObject tagbody_handle;

LispObject special_tagbody;
EUFUN_3( Sf_tagbody, mod, env, forms)
{
  LispObject table,cont;
  LispObject walker;
  LispObject before;
  LispObject res;

  table = (LispObject) EUCALL_1(make_table,NULL);
  STACK_TMP(table);
  before = nil;
  before = tagbody_suck_symbols(stacktop,ARG_2(stackbase));
  UNSTACK_TMP(table);
  
  walker = ARG_2(stackbase) /*forms*/;
  while (is_cons(walker)) {
    if (is_symbol(CAR(walker))) break;
    walker = CDR(walker);
  }

  if (is_cons(walker)) 
    {
      LispObject augenv;
      LispObject runbody;

      /* Non-trivial label forms... */
      stacktop+=2;
      ARG_2(stackbase)=before;	/* kill forms*/
      *(stackbase+3)=table;
      *(stackbase+4)=nil;
      STACK_TMP(walker);
      cont = allocate_continue(stacktop);
      *(stackbase+4)=cont;
      
      UNSTACK_TMP(walker);
      do {
	LispObject label, body;
	label = CAR(walker); walker = CDR(walker);
	STACK_TMP(walker);
	STACK_TMP(label);
	body = tagbody_suck_symbols(stacktop,walker);
	UNSTACK_TMP(label);
	EUCALL_3(Fn_table_ref_setter,*(stackbase+3)/*table*/,label,body);
	UNSTACK_TMP(walker);

	while (is_cons(walker))
	  {
	    if (is_symbol(CAR(walker))) break;
	    walker = CDR(walker);
	  }
      } while (is_cons(walker));

      /* Construct the augmented environment... */

      augenv = allocate_env(stacktop,tagbody_handle,*(stackbase+4)/*cont*/,ARG_1(stackbase));
      ARG_1(stackbase)=augenv;

      runbody = ARG_2(stackbase)/*before*/;

      STACK_TMP(augenv);
    reset:

      /* Go continuation... */

      if (set_continue(stacktop,*(stackbase+4)/*cont*/)) {
	
	/* Go has been called... */
	
	runbody = EUCALL_2(Fn_table_ref,*(stackbase+3)/*table*/,(*(stackbase+4))/*cont*/->CONTINUE.value);
	
	if (runbody == nil)
	  CallError(stacktop,
		    "go: no such label",cont->CONTINUE.value,NONCONTINUABLE);
	goto reset;
      }
    
      res = EUCALL_3(Sf_progn,ARG_0(stackbase)/*mod*/,(LispObject)ARG_1(stackbase)/*augenv*/,runbody);
      unset_continue((*(stackbase+4)));

      return(res);
    }
  else
    {	/* The easy way... */
      res = EUCALL_3(Sf_progn,ARG_0(stackbase)/*mod*/,ARG_1(stackbase)/*env*/,before);
      return(res);
    }
}
EUFUN_CLOSE

LispObject special_go;
EUFUN_3( Sf_go, mod, env, forms)
{
  LispObject tag;
  LispObject walker;

  IGNORE(mod);

  if (!is_cons(forms))
    CallError(stacktop,"go: no tag",forms,NONCONTINUABLE);

  tag = CAR(forms);

  if (!is_symbol(tag))
    CallError(stacktop,"go: non-symbolic tag",tag,NONCONTINUABLE);

  walker = env;
  while (walker != NULL) {
    if (walker->ENV.variable == tagbody_handle)
      call_continue(stacktop,walker->ENV.value,tag);
    walker = walker->ENV.next;
  }

  CallError(stacktop,"go: not within tagbody",nil,NONCONTINUABLE);

  return(nil);
}
EUFUN_CLOSE

void initialise_specials(LispObject *stacktop)
{
  special_table = (LispObject) EUCALL_1(make_table,NULL);
  add_root(&special_table);
  
  special_lambda = my_make_special(stacktop,"lambda",Sf_lambda);
  add_root(&special_lambda);
  special_macro_lambda = my_make_special(stacktop,"macro-lambda",Sf_mlambda);
  add_root(&special_macro_lambda);
  special_setq   = my_make_special(stacktop,"setq",Sf_setq);
  add_root(&special_setq);
  special_progn  = my_make_special(stacktop,"progn",Sf_progn);
  add_root(&special_progn);
  special_if     = my_make_special(stacktop,"if",Sf_if);
  add_root(&special_if);
  
/*  last_continue = nil;*/

  special_dynamic_setq = my_make_special(stacktop,"dynamic-setq",Sf_dynamic_setq);
  add_root(&special_dynamic_setq);
  special_dynamic_set  = my_make_special(stacktop,"dynamic-set",Sf_dynamic_set);
  add_root(&special_dynamic_set);
  special_dynamic_let  = my_make_special(stacktop,"dynamic-let",Sf_dynamic_let);
  add_root(&special_dynamic_let);
  special_dynamic      = my_make_special(stacktop,"dynamic",Sf_dynamic);
  add_root(&special_dynamic_let);
  
  special_quote = my_make_special(stacktop,"quote",Sf_quote);
  add_root(&special_quote);
  
  special_tagbody = my_make_special(stacktop,"tagbody",Sf_tagbody);
  add_root(&special_tagbody);
  tagbody_handle = get_symbol(stacktop,"***tagbody-handle***");
  add_root(&tagbody_handle);
  special_go = my_make_special(stacktop,"go",Sf_go);
  add_root(&special_go);
}

