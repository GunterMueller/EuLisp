/* ******************************************************************** */
/*  root.c           Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* The root level operations		                                */
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, March 1990 (Compiler rationalisation)
 */

#include <stdio.h>
#include <string.h>

#include "funcalls.h"
#include "defs.h"
#include "structs.h"

#include "error.h"
#include "global.h"
#include "slots.h"
#include "table.h"
#include "garbage.h"

#include "allocate.h"
#include "modboot.h"
#include "symboot.h"
#include "modules.h"
#include "toplevel.h"
#include "root.h"
#include "copy.h"
#include "streams.h"
#include "reader.h"

#define ROOT_ENTRIES 13
MODULE Module_root;
LispObject Module_root_values[ROOT_ENTRIES];

static SYSTEM_GLOBAL(LispObject,list_search_path);
static SYSTEM_GLOBAL(int,load_verbosity);

static LispObject sym_eval_cm,sym_set_cm;
static LispObject Cb_load_user_module;

EUFUN_2( eval_cm_template, env, form)
{
  return(EUCALL_3(module_eval,env->ENV.value,NULL,form));
}
EUFUN_CLOSE

EUFUN_3( set_cm_template, env, sym, val)
{
  if (!is_symbol(sym))
    CallError(stacktop,"set/cm: not a symbol",sym,NONCONTINUABLE);
  printf("No set/cm yet...\n");
  (void) EUCALL_3(module_set,(env)->ENV.value,sym,val);

  return(val);
}
EUFUN_CLOSE

void make_default_module_functions(LispObject *stacktop,LispObject mod)
{
  LispObject f;

  STACK_TMP(mod);
  f = make_anonymous_module_env_function_1(stacktop,mod,eval_cm_template,1,
					   sym_nil,mod);
  UNSTACK_TMP(mod);
  STACK_TMP(mod);
  (void) module_set_new(stacktop,mod,sym_eval_cm,f);		
  UNSTACK_TMP(mod);
  STACK_TMP(mod);
  f = make_anonymous_module_env_function_1(stacktop,mod,set_cm_template,2,
					   sym_nil,mod);
  UNSTACK_TMP(mod);
  (void) module_set_new(stacktop,mod,sym_set_cm,f);

}

EUFUN_3( Rf_defmodule, mod, env, forms)
{
  LispObject name,import_specs,syntax_specs;
  LispObject module,tmp;
  LispObject walker;
  LispObject new_initargs=nil;

  if (!is_cons(forms))
    CallError(stacktop,"defmodule: missing name",nil,NONCONTINUABLE);

  name = CAR(forms); forms = CDR(forms);

  if (!is_symbol(name))
    CallError(stacktop,"defmodule: non-symbolic name",name,NONCONTINUABLE);

  /* Overwrite existing one... */ /* HACK !!! */

  if (!is_cons(forms))
    CallError(stacktop,"defmodule: missing import specs",nil,NONCONTINUABLE);

  import_specs = CAR(forms); forms = CDR(forms);

  if (!is_cons(import_specs) && import_specs != nil)
    CallError(stacktop,
	      "defmodule: bad import spec",import_specs,NONCONTINUABLE);

  walker=import_specs;
  while (walker!=nil)
    { /* new syntax ? --- this is not very extensible, 
	 just bomb when we get 'import thing.  */
      if (CAR(walker)==sym_import)
	{
	  new_initargs=import_specs;
	  import_specs=CAR(CDR(walker));
	  break;
	}
      else /* (cdr nil)=nil */
	walker=CDR(CDR(walker));
    }
  
  if (new_initargs!=nil) 
    {	
      /*syntax_specs=search_keylist(stacktop,new_initargs,sym_syntax);*/
      syntax_specs=nil;
    }
  else
    {

      if (!is_cons(forms))
	CallError(stacktop,"defmodule: missing syntax spec",nil,NONCONTINUABLE);
      syntax_specs = CAR(forms); 
      forms = CDR(forms);
    }
  /* See what sort of syntax we have..*/

  if (syntax_specs != nil)
    CallError(stacktop,
	      "defmodule: non-null syntax spec",syntax_specs,NONCONTINUABLE);
  
  /* Should do the loading here maybe... */ /* HACK !!! */
  STACK_TMP(name); 
  STACK_TMP(forms);
  STACK_TMP(import_specs);
  module = allocate_i_module(stacktop,name);
  STACK_TMP(module);
  tmp=EUCALL_1(make_table,NULL);
  UNSTACK_TMP(module);
  module->I_MODULE.bindings=tmp;
  /* Insert eval/cm and set/cm... */
  STACK_TMP(module);
  make_default_module_functions(stacktop,module);
  UNSTACK_TMP(module);
  /* recover import spec, etc */
  UNSTACK_TMP(import_specs);
  STACK_TMP(module);
  process_import_spec(stacktop,module,import_specs);

  UNSTACK_TMP(module);
  UNSTACK_TMP(forms);
  walker=forms;
  while (walker != nil)
    {
      if (SYSTEM_GLOBAL_VALUE(load_verbosity) > 0 && StdOut()!=nil)
	{
	  STACK_TMP(walker);
	  STACK_TMP(module);
	  print_string(stacktop,StdOut(),"Processing: ");
	  EUCALL_2(Fn_print, CAR(walker),StdOut());
	  UNSTACK_TMP(module);
	  UNSTACK_TMP(walker);
	}
      STACK_TMP(CDR(walker));
      STACK_TMP(module);    
      EUCALL_2(process_top_level_form,module,CAR(walker));
      UNSTACK_TMP(module);
      UNSTACK_TMP(walker);
    }
  UNSTACK_TMP(name);
  STACK_TMP(module);
  put_module(stacktop,name,module);
  UNSTACK_TMP(module);
  return(module);
}
EUFUN_CLOSE

EUFUN_3( Rf_loaded_modules, mod, env, val)
{
  LispObject lst,val;
/**
  *return(EUCALL_1(Fn_table_keys, global_module_table));	
  */
  
  val=EUCALL_1(Fn_table_parameters,global_module_table);
  lst=val;
  while (lst!=nil)
    {	
      CAR(lst)=CAR(CAR(lst));
      lst=CDR(lst);
    }
  return val;
}
EUFUN_CLOSE

EUFUN_3( Rf_load_module, mod, env, form)
{
  IGNORE(mod); IGNORE(env);

  if (!is_cons(form))
    CallError(stacktop,"load-module: invalid arguments",form,NONCONTINUABLE);
  RESET_GLOBAL_STACK();
  return(EUCALL_1(load_module,CAR(form)));
}
EUFUN_CLOSE

EUFUN_3( Rf_reload_module, mod, env, form)
{
  IGNORE(mod); IGNORE(env);

  if (!is_cons(form))
    CallError(stacktop,"reload-module: invalid arguments",form,NONCONTINUABLE);

  /* Hack out original... */

  EUCALL_3(Fn_table_ref_setter, global_module_table,CAR(form),nil);
  return(EUCALL_1(load_module,CAR(ARG_2(stackbase))));
}
EUFUN_CLOSE

static FILE *open_module_file(LispObject *stacktop,LispObject name)
{
  char path[200];
  LispObject walker;
  FILE *fd;

  if (!is_symbol(name))
    CallError(stacktop,
	      "open-module-file: not a symbolic name",name,NONCONTINUABLE);

  walker = SYSTEM_GLOBAL_VALUE(list_search_path);
  while (is_cons(walker)) {
    LispObject dir;

    if (!is_string((dir = CAR(walker))))
      CallError(stacktop,
		"open-module-file: bad search directory",dir,NONCONTINUABLE);

    (void) strcpy(path,stringof(dir));
    (void) strcat(path,DIR_SEP);
    (void) strcat(path,stringof(name->SYMBOL.pname));
    (void) strcat(path,".em");

    if ((fd = fopen(path,"r")) == NULL)
      walker = CDR(walker);
    else
      return fd;
  }

  CallError(stacktop,"open-module-file: unable to find .em file for module",
	    name,NONCONTINUABLE);

  return(NULL); /* Not ever */
}
  
EUFUN_1( load_module, name)
{
  char fname[100];
  LispObject form,ans;
  FILE *stream;

  if (!is_symbol(name)) 
    CallError(stacktop,
	      "load-module: non-symbolic module name",name,NONCONTINUABLE);

  /* Look if it's already loaded */

  if (module_loaded_p(stacktop,name)) return(get_module(stacktop,name));

  stream = open_module_file(stacktop,name);
  
  name=ARG_0(stackbase);
  print_string(stacktop,StdOut(),"Loading module '");
  print_string(stacktop,StdOut(),stringof(name->SYMBOL.pname));
  print_string(stacktop,StdOut(),"'\n");

  form=sys_read(stacktop, stream);

  reader_fclose(stacktop,stream);
  
  if (!is_cons(form))
    CallError(stacktop,
	      "load module: invalid module definition",nil,NONCONTINUABLE);

  if (CAR(form) != sym_defmodule) 
    CallError(stacktop,
	      "load module: invalid module definition/no defmodule",nil,NONCONTINUABLE);

  if(!is_cons(CDR(form)))
    CallError(stacktop,
	      "load module: invalid definintion/bad body",form,NONCONTINUABLE);

  name=ARG_0(stackbase);
  if (CAR(CDR(form)) != name)
    CallError(stacktop,
	      "load module: module badly named",CAR(CDR(form)),NONCONTINUABLE);

  EUCALLSET_3(ans,Rf_defmodule,NULL,NULL,CDR(form));
  
  name=ARG_0(stackbase);
  print_string(stacktop,StdOut(),"Loaded '");
  print_string(stacktop,StdOut(),stringof(name->SYMBOL.pname));
  print_string(stacktop,StdOut(),"'\n");

  return(ans);
}
EUFUN_CLOSE

LispObject load_expanded_module(LispObject *stacktop,LispObject name)
{
  char fname[100];
  LispObject form;
  FILE *stream;

  if (!is_symbol(name)) 
    CallError(stacktop,
	      "load-expanded-module: non-symbolic module name",name,NONCONTINUABLE);

  /* Look if it's already loaded */

  if (module_loaded_p(stacktop,name)) return(get_module(stacktop,name));

  stream = open_module_file(stacktop,name);

  print_string(stacktop,StdOut(),"Loading module '");
  print_string(stacktop,StdOut(),stringof(name->SYMBOL.pname));
  print_string(stacktop,StdOut(),"'\n");

  STACK_TMP(form);
  form=sys_read(stacktop,stream);
  UNSTACK_TMP(form);

  reader_fclose(stacktop,stream);

  if (!is_cons(form))
    CallError(stacktop,
	      "load module: invalid module definition",nil,NONCONTINUABLE);

  if (CAR(form) != sym_defmodule) 
    CallError(stacktop,
	      "load module: invalid module definition",nil,NONCONTINUABLE);

  if(!is_cons(CDR(form)))
    CallError(stacktop,
	      "load module: invalid definintion",form,NONCONTINUABLE);

  if (CAR(CDR(form)) != name)
    CallError(stacktop,
	      "load module: module badly named",CAR(CDR(form)),NONCONTINUABLE);

  return EUCALL_3(Rf_defmodule,NULL,NULL,CDR(form));
}

EUFUN_3( Rf_load_expanded_module, mod, env, forms)
{
  if (!is_cons(forms))
    CallError(stacktop,
	      "load-expanded-module: invalid arguments",forms,NONCONTINUABLE);

  return(load_expanded_module(stacktop,CAR(forms)));
}
EUFUN_CLOSE

EUFUN_3( Rf_start_module, m, env, forms)
{
  LispObject modname,fname;
  LispObject mod;

  if (!is_cons(forms))
    CallError(stacktop,"start-module: invalid arguments",forms,NONCONTINUABLE);

  modname = CAR(forms); forms = CDR(forms);

  if (!is_symbol(modname))
    CallError(stacktop,
	      "start-module: non-symbolic module name",modname,NONCONTINUABLE);

  if (!is_cons(forms))
    CallError(stacktop,
	      "start-module: missing function name",forms,NONCONTINUABLE);

  fname = CAR(forms);

  if (!is_symbol(fname))
    CallError(stacktop,
	      "start-module: non-symbolic function name",fname,NONCONTINUABLE);

  /* forms are hopefully (fname arg1 arg2 ...) */

  /* semantically dubious but... */

  mod = get_module(stacktop,modname);

  if (mod == nil)
    CallError(stacktop,
	      "start-module: module not loaded",modname,NONCONTINUABLE);

  return(EUCALL_3(module_eval,mod,NULL,forms));
}
EUFUN_CLOSE

EUFUN_3( Rf_enter_module, m, env, form)
{
  LispObject name;
  LispObject mod;

  if (!is_cons(form))
    CallError(stacktop,"enter-module: invalid arguments",form,NONCONTINUABLE);

  name = CAR(form);
  if (!is_symbol(name))
    CallError(stacktop,
	      "enter-module: non-symbolic module name",name,NONCONTINUABLE);

  else {
    mod = get_module(stacktop,name);
    STACK_TMP(name);
    if (mod == nil)
      SYSTEM_GLOBAL_VALUE(current_interactive_module) =
	EUCALL_1(load_module,name);
    else
      SYSTEM_GLOBAL_VALUE(current_interactive_module) = mod;
    UNSTACK_TMP(name);
  }

  return(name);
}
EUFUN_CLOSE

EUFUN_3( Rf_module_exports, m, env, form)
{
  LispObject name;
  LispObject mod;

  if (!is_cons(form))
    CallError(stacktop,"module-exports: invalid arguments",form,
	      NONCONTINUABLE);

  name = CAR(form);
  if (!is_symbol(name))
    CallError(stacktop,
              "module-exports: non-symbolic module name",name,NONCONTINUABLE);

  mod = get_module(stacktop,name);
  STACK_TMP(name);
  if (mod == nil) mod = EUCALL_1(load_module,name);
  UNSTACK_TMP(name);

  return mod->MODULE.exported_names;

}
EUFUN_CLOSE

EUFUN_0( Rf_load_quietly)
{
  SYSTEM_GLOBAL_VALUE(load_verbosity) = 0;
  return(nil);
}
EUFUN_CLOSE

EUFUN_0( Rf_load_loudly)
{
  SYSTEM_GLOBAL_VALUE(load_verbosity) = 1;
  return(nil);
}
EUFUN_CLOSE

static EUFUN_0( Fn_load_path)
{
  return(SYSTEM_GLOBAL_VALUE(list_search_path));
}
EUFUN_CLOSE

static EUFUN_1( Fn_load_path_setter, val)
{
  return((SYSTEM_GLOBAL_VALUE(list_search_path) = val));
}
EUFUN_CLOSE

static EUFUN_3( Rf_em, m, e, f)
{
  return Rf_enter_module(stackbase);
}
EUFUN_CLOSE

static EUFUN_3( Rf_rem, m, e, f)
{
  EUCALL_3(Rf_reload_module,m,e,f);
  return Rf_enter_module(stackbase);
}
EUFUN_CLOSE

/* Enter user module */
static EUFUN_3( Rf_eum, m, e, f)
{
  if (CAR(Cb_load_user_module)==nil)
  {
      return Rf_em(stackbase);
  }
  else
  {
      EUCALL_2(apply1, CAR(Cb_load_user_module), CAR(f));
      return Rf_enter_module(stackbase);
  }
  
}
EUFUN_CLOSE

static EUFUN_1( Fn_set_eum_fn, fn)
{
  CAR(Cb_load_user_module)=fn;
  return nil;
}
EUFUN_CLOSE

void initialise_root(LispObject* stacktop)
{
  extern char *getenv(char *);
  extern LispObject Fn_nconc(LispObject*);
  char *path_list;
  
  SYSTEM_INITIALISE_GLOBAL(int,load_verbosity,0);
  SYSTEM_INITIALISE_GLOBAL(LispObject,list_search_path,nil);

  ADD_SYSTEM_GLOBAL_ROOT(list_search_path);
  
  Cb_load_user_module=EUCALL_2(Fn_cons, nil, nil);
  add_root(&Cb_load_user_module);

  /* Initialise the paths... */
  
  path_list = getenv(LOAD_PATH_NAME);

  if (path_list == NULL) {
    SYSTEM_GLOBAL_VALUE(list_search_path) 
      = EUCALL_2(Fn_cons,
		 allocate_string(stacktop,MODULE_PATH,strlen(MODULE_PATH)),
		 SYSTEM_GLOBAL_VALUE(list_search_path));
    SYSTEM_GLOBAL_VALUE(list_search_path) 
      = EUCALL_2(Fn_cons, allocate_string(stacktop,".",1),
		 SYSTEM_GLOBAL_VALUE(list_search_path));
  }
  else {
    char *next, buf[1024];

    path_list = (char *)strcpy(buf,path_list); /* avoid trampling the env */
    
    next = strtok(path_list,":");
    while (next != NULL) {
      LispObject xx;
      xx = allocate_string(stacktop,next,strlen(next));
      EUCALLSET_2(xx, Fn_cons, xx,nil);
      EUCALLSET_2(SYSTEM_GLOBAL_VALUE(list_search_path), 
		  Fn_nconc,SYSTEM_GLOBAL_VALUE(list_search_path), xx);
      next = strtok(NULL,":");
    }
  }

  sym_eval_cm = get_symbol(stacktop,"eval/cm");	
  add_root(&sym_eval_cm);
  sym_set_cm = get_symbol(stacktop,"set/cm");
  add_root(&sym_set_cm);
  {
    extern LispObject my_make_special(LispObject *,char *,LispObject (*)());

    (void) my_make_special(stacktop,"!>",Rf_em);
    (void) my_make_special(stacktop,"!>>",Rf_rem);
    (void) my_make_special(stacktop,"!!>",Rf_eum);
  }

  open_module(stacktop,&Module_root,Module_root_values,"root",ROOT_ENTRIES);

  (void) make_unexported_module_special(stacktop,"defmodule",Rf_defmodule);
  (void) make_unexported_module_special(stacktop,"load-module",Rf_load_module);
  (void) make_unexported_module_special(stacktop,
					"reload-module",Rf_reload_module);
  (void) make_unexported_module_special(stacktop,
					"enter-module",Rf_enter_module);
  (void) make_unexported_module_special(stacktop,
					"loaded-modules",Rf_loaded_modules);
  (void) make_unexported_module_special(stacktop,
					"start-module",Rf_start_module);
  (void) make_unexported_module_special(stacktop,"load-expanded-module",
					Rf_load_expanded_module);
  (void) make_unexported_module_special(stacktop,
					"load-quietly",Rf_load_quietly);
  (void) make_unexported_module_special(stacktop,"load-loudly",Rf_load_loudly);

  (void) make_unexported_module_function(stacktop,"load-path",Fn_load_path,0);
  (void) make_unexported_module_function(stacktop,"set-load-path",
					 Fn_load_path_setter,1);
  (void) make_module_function(stacktop,"set-eum-function",Fn_set_eum_fn,1);

  (void) make_unexported_module_special(stacktop,
					"module-exports", Rf_module_exports);

  close_module();
}
