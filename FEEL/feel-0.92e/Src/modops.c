/* ******************************************************************** */
/* modops.c          Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Dynamic module manipulation                                          */
/* ******************************************************************** */

/*
 * $Id: modops.c,v 1.3 1994/04/19 10:15:37 djb Exp $
 *
 * $Log: modops.c,v $
 * Revision 1.3  1994/04/19  10:15:37  djb
 * put check on fopen of you.mods -- if it can't open the file,
 * it segv's
 *
 * Revision 1.2  1994/02/08  11:28:58  djb
 * modify_function_env -> function_env_setter
 *
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
#include "modboot.h"
#include "root.h"
#include "table.h"
#include "modops.h"

/* Dynamic module loading... */

EUFUN_1( Fn_dynamic_load_module, name)
{
  extern LispObject load_module(LispObject*);

  if (!is_symbol(name))
    CallError(stacktop,
	      "dynamic-load-module: not a symbolic name",name,NONCONTINUABLE);

  EUCALL_1(load_module,name);

  return(get_module(stacktop,ARG_0(stackbase)));
}
EUFUN_CLOSE

extern LispObject Fn_module_value(LispObject*);

EUFUN_2( Fn_dynamic_accessiblep, mod, sym)
{
  if (!is_symbol(sym))
    CallError(stacktop,"dynamic-accessiblep: non-symbol",sym,NONCONTINUABLE);

  if (!is_i_module(mod) && !is_c_module(mod))
    CallError(stacktop,"dynamic-accessiblep: non-module",mod,NONCONTINUABLE);

  return((module_binding_exists_p(stacktop,mod,sym) ? lisptrue : nil));
}
EUFUN_CLOSE

EUFUN_2( Fn_dynamic_access, mod, sym)
{
  if (!is_symbol(sym))
    CallError(stacktop,"dynamic-access: non-symbol",sym,NONCONTINUABLE);

  if (!is_i_module(mod) && !is_c_module(mod))
    CallError(stacktop,"dynamic-accessible: non-module",mod,NONCONTINUABLE);

  return(EUCALL_2(Fn_module_value,mod,sym));
}
EUFUN_CLOSE

EUFUN_1( Fn_get_module, sym)
{
  LispObject val;

  if (!is_symbol(sym))
    CallError(stacktop,"get-module: non-symbol",sym,NONCONTINUABLE);

  val = get_module(stacktop,sym);

  return(val);
}
EUFUN_CLOSE

EUFUN_1( Fn_module_name, mod)
{
  if (!is_i_module(mod) && !is_c_module(mod))
    CallError(stacktop,"module-name: not a module",mod,NONCONTINUABLE);

  return(mod->I_MODULE.name);
}
EUFUN_CLOSE

EUFUN_1( Fn_module_exports, mod)
{
  if (!is_i_module(mod) && !is_c_module(mod))
    CallError(stacktop,"module-exports: not a module",mod,NONCONTINUABLE);

  return(mod->I_MODULE.exported_names); /* Should copy... */
}
EUFUN_CLOSE

EUFUN_2(Fn_add_module_export, mod, name)
{	
  LispObject xx;

  xx=EUCALL_2(Fn_cons,name, mod->I_MODULE.exported_names);
  mod->I_MODULE.exported_names=xx;
  return nil;
}
EUFUN_CLOSE

/* Module junk for bytecode interpreter */

EUFUN_2(Fn_make_module, name, nbinds )
{
  char *myspace;
  LispObject newmod,tab;
  LispObject binds;
  int i;

#ifdef DGC 
  myspace=(char *)allocate_nbytes(stacktop,sizeof(MODULE),TYPE_C_MODULE);
#else
  myspace=allocate_space(stacktop,sizeof(MODULE));
#endif

  tab=EUCALL_1(make_table,NULL);
  
  newmod=(LispObject) myspace;
  binds=allocate_static_vector(stacktop,intval(nbinds));

  for (i=0; i<intval(nbinds); i++)
    {
      vref(binds,i)=nil; /* NULL maybe */
    }

  lval_classof(newmod)=Object;
  lval_typeof(newmod)=TYPE_C_MODULE;
  /* hack */
#ifndef DGC
  gcof(newmod)=gcof(nil);
  ageof(newmod)=0;
#endif
  newmod->MODULE.name=name;
  newmod->MODULE.imported_modules=nil;
  newmod->MODULE.bindings=tab;
  newmod->MODULE.exported_names=nil;
  newmod->C_MODULE.values=binds;
  newmod->C_MODULE.entry_count=nbinds;
  newmod->C_MODULE.home=nil;
  put_module(stacktop,newmod->MODULE.name,newmod);

  return newmod;
}
EUFUN_CLOSE

static EUFUN_2(Fn_binding_location,mod,name)
{
  LispObject bind;

  bind=GET_BINDING(mod,name);

  return (BINDING_VALUE(bind));
}
EUFUN_CLOSE

static EUFUN_2(Fn_binding_home,mod,name)
{
  LispObject bind;

  bind=GET_BINDING(mod,name);

  return (BINDING_HOME(bind));
}
EUFUN_CLOSE

static EUFUN_4(Fn_add_import,mod,name,inmod,inname)
{
  LispObject bind;

  bind=GET_BINDING(inmod,inname);

  IMPORT_BINDING(ARG_0(stackbase)/*mod*/,ARG_1(stackbase)/*name*/,bind);
  
  return nil;
}
EUFUN_CLOSE

EUFUN_3(Fn_add_binding,mod,name,loc)
{
  
  ADD_BINDING(ARG_0(stackbase)/*mod*/,name,loc,nil);
  
  return nil;
}
EUFUN_CLOSE

static EUFUN_2(Fn_module_val,mod,n)
{
  return (vref((mod->C_MODULE.values),intval(n)));
}
EUFUN_CLOSE

static EUFUN_3(Fn_module_val_setter,mod,n,val)
{
  vref((mod->C_MODULE.values),intval(n))=val;
  
  return nil;
}
EUFUN_CLOSE

/* DJB Type hacks */
/* would be real nice if this was a function */
EUFUN_3(Sf_reify_env,mod,env,form)
{
  LispObject lst=nil;
  LispObject ptr;
  
  ptr=env;

  while (ptr!=NULL)
    {
      LispObject xx;

      STACK_TMP(ptr->ENV.next);
      STACK_TMP(lst);
      xx=EUCALL_2(Fn_cons,ptr->ENV.variable,ptr->ENV.value);
      UNSTACK_TMP(lst);
      lst=EUCALL_2(Fn_cons,xx,lst);
      UNSTACK_TMP(ptr);
    }
  lst=EUCALL_2(Fn_cons,ARG_0(stackbase)->MODULE.name,lst);
  return lst;
}
EUFUN_CLOSE

EUFUN_2(Fn_make_function, envlst, body)
{	/* CAR(body) should be an arglist */

  LispObject env=NULL;
  LispObject mod;
  LispObject ptr=CDR(envlst);
  
  while(ptr!=nil)
    {
      STACK_TMP(CDR(ptr));
      env=allocate_env(stacktop,CAR(CAR(ptr)),CDR(CAR(ptr)), env);
      
      UNSTACK_TMP(ptr);
    }
  
  STACK_TMP(env);
  mod=get_module(stacktop,CAR(ARG_0(stackbase))/*name*/);
  UNSTACK_TMP(env);

  return(EUCALL_3(Sf_lambda,mod,env,ARG_1(stackbase)));
}
EUFUN_CLOSE

static EUFUN_1(Fn_function_body, fn)
{
  if (!is_i_function(fn))
    CallError(stacktop,"Fn_body: not an i-function",fn,NONCONTINUABLE);
  
  /*Should add the lambda-list! */
  return fn->I_FUNCTION.body;
}
EUFUN_CLOSE

EUFUN_1(Fn_function_env, fn)
{
  LispObject lst;
  if (!is_i_function(fn))
    CallError(stacktop,"Fn_body: not an i-function",fn,NONCONTINUABLE);

  lst=EUCALL_1(Fn_listify_env,fn->I_FUNCTION.env);
  lst=EUCALL_2(Fn_cons,(fn->I_FUNCTION.home)->MODULE.name,lst);

  return lst;
}
EUFUN_CLOSE

EUFUN_1(Fn_listify_env,e)
{	
  LispObject ptr, lst;
  
  lst=nil;
  ptr=e;

  while (ptr!=NULL)
    {
      LispObject xx;

      STACK_TMP(ptr->ENV.next);
      STACK_TMP(lst);
      xx=EUCALL_2(Fn_cons,ptr->ENV.variable,ptr->ENV.value);
      UNSTACK_TMP(lst);
      lst=EUCALL_2(Fn_cons,xx,lst);
      UNSTACK_TMP(ptr);
    }
  return lst;

}
EUFUN_CLOSE

EUFUN_2(Fn_function_env_setter, fn, envlst)
{
  LispObject env=NULL;
  LispObject mod;
  LispObject ptr=CDR(envlst);
  
  if (!is_i_function(fn))
    CallError(stacktop,"Fn_body: not an i-function",fn,NONCONTINUABLE);

  while(ptr!=nil)
    {
      STACK_TMP(CDR(ptr));
      env=allocate_env(stacktop,CAR(CAR(ptr)),CDR(CAR(ptr)), env);
      
      UNSTACK_TMP(ptr);
    }
  
  STACK_TMP(env);
  mod=get_module(stacktop,CAR(ARG_1(stackbase))/*name*/);
  UNSTACK_TMP(env);

  fn->I_FUNCTION.env = env;
  fn->I_FUNCTION.home = mod;

  return fn;
}
EUFUN_CLOSE

/* 
 * Gobbing out a description file
 * 
 * Contains location info of all loaded modules
 */

void make_description_file(LispObject *stacktop)
{
#ifdef BCI
  extern LispObject Fn_boot_module_list(LispObject *);

  FILE *file;
  LispObject mods,cmods;
  int i=1;
  /* XXX This needs changing 'cos of demise of table_keys */
  file=fopen("you.mods","w");
  if (file==NULL) CallError(stacktop,
			    "Cannot open you.mods",nil,NONCONTINUABLE);
  mods=Fn_boot_module_list(stacktop);
  mods=CDR(mods);
  fprintf(file,"(\n");
  while (mods!=nil)
    {
      LispObject vals;
      vals=EUCALL_1(Fn_table_parameters,CAR(mods)->MODULE.bindings);
      fprintf(file,"(%s %d",stringof(CAR(mods)->MODULE.name->SYMBOL.pname),i);
      
      while (vals!=nil)
	{
	  fprintf(file," (|%s| . %d)",stringof(CAR(CAR(vals))->SYMBOL.pname), intval(BINDING_VALUE(CDR(CAR(vals)))));
	  vals=CDR(vals);
	}
      fprintf(file,")\n");
      
      i++;
      mods=CDR(mods);
    }
  fprintf(file,")\n");
  return;
#else /* no bci */
  return;
#endif
}

/*
 * Initialisation...
 */

#define MODULE_OPERATORS_ENTRIES 19

MODULE Module_module_operators;
LispObject Module_module_operators_values[MODULE_OPERATORS_ENTRIES];

void initialise_module_operators(LispObject *stacktop)
{
  LispObject get,set;

  open_module(stacktop,
	      &Module_module_operators,
	      Module_module_operators_values,
	      "module-operators",
	      MODULE_OPERATORS_ENTRIES);

  (void) make_module_function(stacktop,
			      "dynamic-load-module",Fn_dynamic_load_module,1);
  (void) make_module_function(stacktop,"dynamic-access",Fn_dynamic_access,2);
  (void) make_module_function(stacktop,
			      "dynamic-accessible-p",Fn_dynamic_accessiblep,2);
  (void) make_module_function(stacktop,"get-module",Fn_get_module,1);
  (void) make_module_function(stacktop,"module-name",Fn_module_name,1);
  (void) make_module_function(stacktop,"module-exports",Fn_module_exports,1);

  (void) make_module_function(stacktop,"add-module-export",Fn_add_module_export,2);
  (void) make_module_function(stacktop,"make-module",Fn_make_module,2);
  (void) make_module_function(stacktop,"module-binding-location",Fn_binding_location,2);
  (void) make_module_function(stacktop,"module-binding-home",Fn_binding_home,2);
  (void) make_module_function(stacktop,"add-module-import",Fn_add_import,4);
  (void) make_module_function(stacktop,"add-module-binding",Fn_add_binding,3);
  (void) make_module_function(stacktop,"module-value",Fn_module_val,2);
  (void) make_module_function(stacktop,"module-value-setter",Fn_module_val_setter,3);
  (void) make_module_special(stacktop,"reify-env",Sf_reify_env);
  (void) make_module_function(stacktop,"make-function",Fn_make_function,2);
  (void) make_module_function(stacktop,"function-body",Fn_function_body,1);
  get = make_module_function(stacktop,"function-env",Fn_function_env,1);
  STACK_TMP(get);
  set = make_unexported_module_function(stacktop,"function-env-setter",Fn_function_env_setter,2);
  UNSTACK_TMP(get);
  set_anon_associate(stacktop,get,set);
  close_module();
}

