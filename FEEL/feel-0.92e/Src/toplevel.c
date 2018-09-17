/* ******************************************************************** */
/* toplevel.c        Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* toplevel syntactic forms and special forms                           */
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, March 1990 (Compiler rationalisation)
 *     Largely just modulised variants of the originals.
 *   Version 2, August 1990
 *     Added 'define' (kjp)
 */

#include "funcalls.h"
#include "defs.h"
#include "structs.h"

#include "error.h"
#include "global.h"

#include "symboot.h"
#include "allocate.h"
#include "modules.h"
#include "specials.h"
#include "toplevel.h"
#include "streams.h"

/* Language provided toplevel forms */

/*
 * Start with the most fundamental...
 *   The first argument to ALL special forms is now the module it is
 *   called within - not all need it but...
 */

/* Top level defining forms */

LispObject TL_define(LispObject *stacktop,LispObject mod,LispObject forms)
{
  LispObject bind_spec,name,type;
  LispObject ret;

  if (!is_cons(forms))
    CallError(stacktop,"define: no binding spec",forms,NONCONTINUABLE);

  bind_spec = CAR(forms); 

  if (is_symbol(bind_spec)) {
    ret = TL_deflex(stacktop,mod,forms);
    return(ret);
  }

  if (!is_cons(bind_spec))
    CallError(stacktop,"define: invalid binding spec",forms,NONCONTINUABLE);
  
  type = CAR(bind_spec); bind_spec = CDR(bind_spec);

  if (!is_cons(bind_spec))
    CallError(stacktop,"define: invalid binding spec",forms,NONCONTINUABLE);

  name = CAR(bind_spec); bind_spec = CDR(bind_spec);

  if (type == sym_function) {
    LispObject xx;
    STACK_TMP(mod);
    EUCALLSET_2(xx, Fn_cons, name, CDR(forms));
    UNSTACK_TMP(mod);
    ret = TL_defun(stacktop,mod,xx);
    return(ret);
  }

  if (type == sym_macro) {
    LispObject xx;
    STACK_TMP(mod);
    EUCALLSET_2(xx, Fn_cons, name, CDR(forms));
    UNSTACK_TMP(mod);
    ret = TL_defmacro(stacktop,mod,xx);
    return(ret);
  }

  if (type == sym_constant) {
    LispObject xx;
    STACK_TMP(mod);
    EUCALLSET_2(xx, Fn_cons, name, CDR(forms));
    UNSTACK_TMP(mod);
    ret = TL_defconstant(stacktop,mod,xx);
    return(ret);
  }

  CallError(stacktop,"define: unknown binding type",forms,NONCONTINUABLE);

  return(nil);
}

LispObject TL_defun(LispObject *stacktop,LispObject mod,LispObject forms)
{
  LispObject name,fun;

  if (forms == nil)
    CallError(stacktop,"defun form: no function name",nil,NONCONTINUABLE);

  name = CAR(forms); forms = CDR(forms);

  if (!is_symbol(name))
    CallError(stacktop,
	      "defun form: non-symbolic function name",name,NONCONTINUABLE);

  /* Use name for bind and redirect to lambda!! */

  /* What we do here's questionable... */

  STACK_TMP(mod);
  STACK_TMP(name);
  EUCALLSET_3(fun,Sf_lambda,mod,NULL,forms);
  UNSTACK_TMP(name);
  UNSTACK_TMP(mod);
  fun->I_FUNCTION.name = name;

  STACK_TMP(name);
  (void) module_set_new_constant(stacktop,mod,name,fun);
  UNSTACK_TMP(name);

  return(name);
}

LispObject TL_defmacro(LispObject *stacktop,LispObject mod,LispObject forms)
{
  LispObject name, mac;

  if (forms == nil)
    CallError(stacktop,"defmacro form: no macro name",nil,NONCONTINUABLE);

  name = CAR(forms); forms = CDR(forms);

  if (!is_symbol(name))
    CallError(stacktop,
	      "defmacro form: non-symbolic macro name",name,NONCONTINUABLE);

  /* Use name for bind and redirect to lambda!! */

  /* What we do here's questionable... */
  STACK_TMP(mod);
  STACK_TMP(name);
  EUCALLSET_3(mac,Sf_mlambda,mod,NULL,forms);
  UNSTACK_TMP(name);
  UNSTACK_TMP(mod);
  STACK_TMP(name);
  (void) module_set_new_constant(stacktop,mod,name,mac);
  UNSTACK_TMP(name);
  
  return(name);
}

LispObject TL_deflex(LispObject *stacktop,LispObject mod,LispObject forms)
{
  LispObject name,val;

  if (!is_cons(forms))
    CallError(stacktop,"deflocal form: no binding name",nil,NONCONTINUABLE);

  name = CAR(forms); forms = CDR(forms);

  if (!is_symbol(name))
    CallError(stacktop,"deflocal form: non-symbolic binding name",
	      name,NONCONTINUABLE);

  /* What we do here's questionable too... */
  STACK_TMP(mod);
  STACK_TMP(name);
  EUCALLSET_3(val,module_eval,mod,NULL,CAR(forms));
  UNSTACK_TMP(name);
  UNSTACK_TMP(mod);
  STACK_TMP(name);
  (void) module_set_new(stacktop,mod,name,val);
  UNSTACK_TMP(name);

  return(name);
}

LispObject TL_defconstant(LispObject *stacktop,LispObject mod,LispObject forms)
{
  LispObject name,val;

  if (!is_cons(forms))
    CallError(stacktop,"defconstant form: no binding name",nil,NONCONTINUABLE);

  name = CAR(forms); forms = CDR(forms);

  if (!is_symbol(name))
    CallError(stacktop,"defconstant form: non-symbolic binding name",
	      name,NONCONTINUABLE);

  /* What we do here's questionable too... */
  
  STACK_TMP(mod);
  STACK_TMP(name);
  EUCALLSET_3(val,module_eval,mod,NULL,CAR(forms));
  UNSTACK_TMP(name);
  UNSTACK_TMP(mod);
  STACK_TMP(name);
  (void) module_set_new_constant(stacktop,mod,name,val);
  UNSTACK_TMP(name);

  return(name);
}

LispObject TL_defvar(LispObject *stacktop,LispObject mod,LispObject forms)
{
  LispObject id;

  if (!is_cons(forms))
    CallError(stacktop,"defvar: illegal empty defvar form",nil,NONCONTINUABLE);

  id = CAR(forms); forms = CDR(forms);

  if (CDR(forms) != nil)
    CallError(stacktop,"defvar: additional defvar forms",nil,NONCONTINUABLE);

  if (!is_symbol(id))
    CallError(stacktop,"defvar: non-symbolic id",id,NONCONTINUABLE);

  if (reserved_symbol_p(id))
    CallError(stacktop,"defvar: reserved id",id,NONCONTINUABLE);

  STACK_TMP(id);
  EUCALLSET_3(forms,module_eval,mod,NULL,CAR(forms));
  UNSTACK_TMP(id);
  STACK_TMP(forms);
  if ((id->SYMBOL).gvalue !=NULL) {
    print_string(stacktop,StdErr(),"defvar: Illegal re-declaration of ");
    STACK_TMP(id);
    EUCALL_2(Fn_print,id,StdErr());
    UNSTACK_TMP(id);
  }
  UNSTACK_TMP(forms);
  return((id->SYMBOL).gvalue = forms);
}
