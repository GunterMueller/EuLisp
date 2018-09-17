/* ******************************************************************** */
/*  modules.c        copyright (c) codemist and university of bath 1989 */
/*                                                                      */
/* creation of modules							*/
/* ******************************************************************** */

/*
 * $Id: modules.c,v 1.2 1994/02/08 11:30:04 djb Exp $
 *
 * $Log: modules.c,v $
 * Revision 1.2  1994/02/08  11:30:04  djb
 * stack paranoia
 *
 * Revision 1.1  1994/01/25  13:45:08  djb
 * Initial revision
 *
 * Revision 2.1  93/01/17  17:25:21  pab
 * 17 Jan 1993 The next generation...
 * 
 * Revision 1.28  1992/11/26  15:58:09  pab
 * Env removal,etc
 *
 * Revision 1.26  1992/05/28  11:26:40  pab
 * not a lot
 *
 * Revision 1.25  1992/05/19  11:25:07  pab
 * bindings exported with write permission, errors msgs improved
 *
 * Revision 1.24  1992/04/27  21:58:15  pab
 * added more BCI dependency, plus corrected listify(c_fn)
 *
 * Revision 1.23  1992/04/26  20:55:02  pab
 * fixes for interpreter
 *
 * Revision 1.22  1992/03/14  16:39:20  pab
 * arg checking (again)
 *
 * Revision 1.21  1992/03/14  14:33:48  pab
 * bytecode optional
 *
 * Revision 1.20  1992/03/07  21:45:16  pab
 * apply changes
 *
 * Revision 1.19  1992/02/27  15:48:17  pab
 * bytecode additions
 *
 * Revision 1.18  1992/02/10  12:06:20  pab
 * new apply functions
 *
 * Revision 1.17  1992/02/02  16:33:47  pab
 * improved backtrace output
 *
 * revision 1.12  1991/04/02  21:25:30  kjp
 * compiler tidying.
 *
 * revision 1.11  1991/03/27  17:37:32  kjp
 * fixed some definition ordering problems.
 *
 * revision 1.10  1991/03/14  14:14:14  fdla
 * *** empty log message ***
 *
 * revision 1.9  1991/03/14  11:43:54  fdla
 * c and elvira function switches expanded (20 args)
 *
 * revision 1.8  1991/03/13  16:57:34  kjp
 * no change.
 *
 * revision 1.7  1991/02/19  18:53:04  kjp
 * (expose spec*) in module body for reexportation.
 *
 * revision 1.6  1991/02/19  17:07:17  kjp
 * updated for new module syntax with full streaming.
 *
 * revision 1.5  1991/02/13  18:24:17  kjp
 * pass.
 *
 */

/*
 * change log:
 *   version 1, may 1989
 *	major rewrite after talking to jap
 *	added include function
 *
 *      threw it all away and did it again 'right' ! kjp (15/3/90)	
 *	Did the same... pab (11/91)
 */
#include <limits.h>

#include "defs.h"
#include "structs.h"
#include "funcalls.h"

#include "error.h"
#include "global.h"


#include "allocate.h"
#include "lists.h"
#include "table.h"
#include "modules.h"
#include "toplevel.h"
#include "symboot.h"
#include "specials.h"
#include "root.h"
#include "class.h"
#include "ngenerics.h"
#include "calls.h"
#include "bvf.h"
#include "threads.h"
#include "streams.h"
#include "reader.h"

/* elsewheres... */
EUDECL(call_generic);
/* in modules.h */
EUDECL(Fn_module_value);
static EUDECL(module_set_new_aux);
EUDECL(register_module_import);
static LispObject export_filter(LispObject *,LispObject,LispObject);
static LispObject union_filter(LispObject *,LispObject,LispObject);
static LispObject filter_import_thang(LispObject*,LispObject,LispObject);
LispObject symbol_ref(LispObject *,LispObject,LispObject,LispObject);
static LispObject sym_include_forms;

SYSTEM_GLOBAL(LispObject,current_interactive_module);

/* global module table --- needed for modops, etc*/

LispObject global_module_table;

/* Callback when we ge a function we can't deal with */
LispObject Cb_no_function_fn;

/* hooking / unhooking */

LispObject put_module(LispObject *stacktop, LispObject name,LispObject module)
{
  if (global_module_table == NULL) {
    fprintf(stderr,"initerror: NULL module table");
    exit(1);
  }
  STACK_TMP(name);
  EUCALL_3(Fn_table_ref_setter, global_module_table,name,module);
  UNSTACK_TMP(name);
  return(name);
}

LispObject get_module(LispObject *stacktop, LispObject name)
{
  ARG_1(stacktop) = name;
  ARG_0(stacktop) = global_module_table;
  return(Fn_table_ref(stacktop));
}

int module_loaded_p(LispObject* stacktop, LispObject name)
{
  return((get_module(stacktop, name) != nil));
}

/* utilities !! */


LispObject module_exports(LispObject mod)
{
  if (is_c_module(mod)) return(mod->C_MODULE.exported_names);
  if (is_i_module(mod)) return(mod->I_MODULE.exported_names);

  CallError(NULL, "module exports: unknown module type",mod,NONCONTINUABLE);

  return(nil);
}

void process_expose_form(LispObject *stacktop,LispObject mod,LispObject forms)
{
  LispObject xx;

  STACK_TMP(mod);
  xx=union_filter(stacktop,forms,mod);
  UNSTACK_TMP(mod);
  (void) export_filter(stacktop,xx,mod);
}    
      
EUFUN_2( process_exports, mod, names)
{

  if (is_c_module(mod))
    CallError(stacktop,
	      "process exports: can't modify compiled module exports",
	      mod,NONCONTINUABLE);

  if (is_i_module(mod)) {
    LispObject walker = names;

    if (names == nil) return nil;

    mod->I_MODULE.bounce_flag = lisptrue;

    while (is_cons(walker)) {

      if (!is_symbol(CAR(walker))) {
	STACK_TMP(walker);
	EUCALL_2(process_top_level_form,ARG_1(stackbase)/*mod*/,CAR(walker)); 
	UNSTACK_TMP(walker);
      }
      walker = CDR(walker);
    }

    mod = ARG_0(stackbase);
    mod->I_MODULE.bounce_flag = nil;

    /* all valid exports */

    walker = ARG_1(stackbase);

    while(is_cons(walker)) {
      if (is_symbol(CAR(walker))) {
	LispObject xx;
	STACK_TMP(walker);
	EUCALLSET_2(xx, Fn_memq,CAR(walker),mod->I_MODULE.exported_names);
	UNSTACK_TMP(walker);
	if (xx == nil) {
	  LispObject xx;
	  mod = ARG_0(stackbase);
	  STACK_TMP(walker);
	  EUCALLSET_2(xx, Fn_cons, CAR(walker),mod->I_MODULE.exported_names);
	  mod = ARG_0(stackbase);
	  mod->I_MODULE.exported_names = xx;
	  UNSTACK_TMP(walker);
	}
      }

      walker = CDR(walker);
    }

    return nil;
  }

  CallError(stacktop, "process exports: non-module arg",mod,NONCONTINUABLE);
}
EUFUN_CLOSE

#ifndef PATH_MAX
#define PATH_MAX 256
#endif
EUFUN_2( process_included_forms, mod, forms)
{
  char buf[PATH_MAX+64];
  LispObject path,read;
  FILE *cstream;

  if (!is_cons(forms))
    CallError(stacktop, "inlude-forms: missing path",forms,NONCONTINUABLE);

  if (!is_string((path = CAR(forms))))
    CallError(stacktop, "include-forms: bad path",path,NONCONTINUABLE);

  cstream = fopen(stringof(path),"r");
  if (cstream == NULL)
    CallError(stacktop, "include-forms: can't open file",path,NONCONTINUABLE);
  
  sprintf(buf,"including \'%s\'\n",stringof(path));
  print_string(stacktop,StdOut(),buf);

  while (1) {
    read=sys_read(stacktop, cstream);
    if (read == q_eof) break;
    EUCALLSET_2(read,process_top_level_form,ARG_0(stackbase),read);
  }

  reader_fclose(stacktop,cstream);
  sprintf(buf,"included \'%s\'\n",stringof(path));
  print_string(stacktop,StdOut(),buf);

}
EUFUN_CLOSE

static LispObject sym_only;
static LispObject sym_except;

static LispObject module_addresses(LispObject *stacktop, LispObject mod)
{
  LispObject exports,addresses;

  addresses = nil;
  exports = mod->I_MODULE.exported_names;

  
  while (is_cons(exports)) {
    LispObject name, xx;
    STACK_TMP(CDR(exports));
    STACK_TMP(mod);
    STACK_TMP(addresses);

    name = CAR(exports);
    
    EUCALLSET_2(xx, Fn_cons, name, mod); /* canonical address */
    EUCALLSET_2(name,Fn_cons, CAR(xx)/*name*/, xx);
    UNSTACK_TMP(addresses);
    EUCALLSET_2(addresses, Fn_cons,name, addresses);
    UNSTACK_TMP(mod);
    UNSTACK_TMP(exports);
  }


  return(addresses);
}

/* filters */

static LispObject only_filter(LispObject *stacktop,
			      LispObject names,LispObject addresses)
{
  LispObject remains;

  remains = nil;

  while (is_cons(addresses)) {

    STACK_TMP(addresses);
    STACK_TMP(remains);
    if (EUCALL_2(Fn_memq,CAR(CAR(addresses)),names) != nil) {
      UNSTACK_TMP(remains);
      STACK_TMP(names);
      EUCALLSET_2(remains, Fn_cons, CAR(addresses),remains);
      UNSTACK_TMP(names);
    }
    else UNSTACK_TMP(remains);

    UNSTACK_TMP(addresses);
    addresses = CDR(addresses);

  }

  return(remains);
}

static LispObject except_filter(LispObject *stacktop,
				LispObject names,LispObject addresses)
{
  LispObject remains;

  remains = nil;

  while (is_cons(addresses)) {

    STACK_TMP(addresses);

    if (EUCALL_2(Fn_memq,CAR(CAR(addresses)),names) == nil) 
      {
	STACK_TMP(names);
	EUCALLSET_2(remains, Fn_cons, CAR(addresses),remains);
	UNSTACK_TMP(names);
      }

    UNSTACK_TMP(addresses);

    addresses = CDR(addresses);

  }

  return(remains);
}

static LispObject name_list_pair(LispObject *stacktop,
				 LispObject k,LispObject l)
{
  while (is_cons(l)) {

    if (!is_cons(CAR(l)))
      CallError(stacktop,
		"module importation: bad rename names",l,NONCONTINUABLE);

    if (k == CAR(CAR(l))) 
      return(CAR(l));
    else
      l = CDR(l);
  }

  return(nil);
}

static LispObject rename_filter(LispObject *stacktop,
				LispObject pairs,LispObject addresses)
{
  LispObject walker;

  walker = addresses;

  while (is_cons(walker)) {
    LispObject pair;
    STACK_TMP(walker);
    pair = name_list_pair(stacktop,CAR(CAR(walker)),pairs);
    UNSTACK_TMP(walker);
    if (pair != nil) { /* to be renamed... */

      CAR(CAR(walker)) = CAR(CDR(pair));

    }

    walker = CDR(walker);
  }
  
  return(addresses);
}

static LispObject
  union_filter(LispObject *stacktop, LispObject list,LispObject context)
{
  LispObject all;

  all = nil;

  while (is_cons(list)) {
    LispObject xx;

    STACK_TMP(CDR(list));
    STACK_TMP(context);
    STACK_TMP(all);
    xx = filter_import_thang(stacktop,CAR(list),context);
    UNSTACK_TMP(all);
    EUCALLSET_2(all, Fn_nconc, xx,all);
    UNSTACK_TMP(context);

    UNSTACK_TMP(list);

  }

  return(all);
}

static LispObject export_filter(LispObject *stacktop,
				LispObject ads,LispObject mod)
{
  LispObject walker;
  
  STACK_TMP(ads);
  walker = ads;

  while (is_cons(walker)) {
    LispObject name;

    name = CAR(CAR(walker)); 

    STACK_TMP(CDR(walker));

    STACK_TMP(mod);
    STACK_TMP(name);
    if (EUCALL_2(Fn_memq,name,mod->I_MODULE.exported_names) == nil)
      {
	LispObject xx;
	UNSTACK_TMP(name);
	EUCALLSET_2(xx, Fn_cons,name,mod->I_MODULE.exported_names);
	UNSTACK_TMP(mod);
	mod->I_MODULE.exported_names = xx;
      }
    else 
      { UNSTACK_TMP(name);	
	UNSTACK_TMP(mod);
      }
    UNSTACK_TMP(walker);

  }

  UNSTACK_TMP(ads);
  return(ads);
}

static void register_filtered_addresses(LispObject *stacktop,
					LispObject ads,LispObject mod)
{
  while (is_cons(ads)) {
    LispObject first;
    
    first = CAR(ads); ads = CDR(ads);
    STACK_TMP(mod);
    STACK_TMP(ads);
    EUCALL_4(register_module_import,mod,
	     CAR(first),CDR(CDR(first)),
	     CAR(CDR(first)));
    UNSTACK_TMP(ads);
    UNSTACK_TMP(mod);
  }
}
    
static LispObject filter_import_thang(
		      LispObject* stacktop, LispObject spec,LispObject context)
{
  LispObject op,xx;

  if (is_symbol(spec)) {
    STACK_TMP(spec);
    EUCALL_1(load_module,spec);
    UNSTACK_TMP(spec);
    xx= get_module(stacktop,spec);
    return(module_addresses(stacktop,xx));
  }

  if (!is_cons(spec)) 
    CallError(stacktop, "module importation: invalid import spec",spec,NONCONTINUABLE);

  op = CAR(spec); spec = CDR(spec);

  if (op == sym_only) {
    
    if (!is_cons(spec))
      CallError(stacktop, "module importation: bad only form",spec,NONCONTINUABLE);
    
    STACK_TMP(CAR(spec));
    xx=union_filter(stacktop, CDR(spec),context);
    UNSTACK_TMP(spec);
    return(only_filter(stacktop,spec,xx));

  }

  if (op == sym_except) {

    if (!is_cons(spec))
      CallError(stacktop, "module importation: bad except form",spec,NONCONTINUABLE);
    STACK_TMP(CAR(spec));
    xx=union_filter(stacktop, CDR(spec),context);
    UNSTACK_TMP(spec);
    return(except_filter(stacktop,spec,xx));

  }

  if (op == sym_rename) {

    if (!is_cons(spec))
      CallError(stacktop, "module importation: bad rename form",spec,NONCONTINUABLE);
    STACK_TMP(CAR(spec));
    xx= union_filter(stacktop, CDR(spec),context);
    UNSTACK_TMP(spec);
    return(rename_filter(stacktop,spec,xx));

  }

  if (op == sym_export) {
    STACK_TMP(spec); STACK_TMP(context);
    xx=union_filter(stacktop, spec,context);
    UNSTACK_TMP(context); UNSTACK_TMP(spec);
    return(export_filter(stacktop,xx,context));

  }

  CallError(stacktop, "module importation: invalid import operation",op,NONCONTINUABLE);

  return(nil);
}

void process_import_form(LispObject *stackbase,LispObject mod,LispObject spec)
{
  LispObject *stacktop=stackbase+1;
  
  ARG_0(stackbase)=mod;

  if (!is_cons(spec))
    CallError(stacktop,
	      "import: invalid NULL import spec",spec,NONCONTINUABLE);

  while (is_cons(spec)) {
    LispObject name = CAR(spec);
    STACK_TMP(CDR(spec));

    if (is_symbol(name)) {
      LispObject inmod,exports;
      
      STACK_TMP(name);
      EUCALL_1(load_module,name);
      UNSTACK_TMP(name);

      inmod = get_module(stacktop,name);
      mod=ARG_0(stackbase);
      exports = module_exports(inmod);

      while (exports != nil) {
	STACK_TMP(mod);
	STACK_TMP(inmod);
	STACK_TMP(CDR(exports));
	EUCALL_4(register_module_import,ARG_0(stackbase)/*mod*/,
		 CAR(exports),inmod,CAR(exports));
	UNSTACK_TMP(exports);
	UNSTACK_TMP(inmod);
	UNSTACK_TMP(mod);
      }

    }
    else {
      
      CallError(stacktop,
		"import: non-symbolic module name",spec,NONCONTINUABLE);

    }

    UNSTACK_TMP(spec);

  }

}

void process_import_spec(LispObject *stacktop, LispObject mod,LispObject spec)
{
  LispObject xx;
  STACK_TMP(mod);
  xx=union_filter(stacktop, spec,mod);
  UNSTACK_TMP(mod);
  register_filtered_addresses(stacktop,xx,mod);
}


EUFUN_2(process_top_level_form, mod, form)
{
  LispObject op;

  /* ok, so here's the game plan -
   
   * for each form, check out the car.
   * if it's not a symbol - crash, probably, for the moment...
   * a symbol means check out any imported macros...
   *   no macros means check out special form key words...
   *     none of them means error.
   * expand macros once and try again.
   * for matching keywords, do the bizness

   */

 top:
  /* interactive hack */

  if (!is_cons(form)) RETURN_EUCALL(EUCALL_3(module_eval,mod,NULL,form));

  op = CAR(form); 

  if (is_symbol(op)) {

    /* really just check for defining forms and 'progn' */

    if (op == sym_progn) {
      LispObject walker,ans = nil;
      walker = form;

      walker = CDR(walker);
      while (is_cons(walker)) {
	STACK_TMP(CDR(walker));
	mod = ARG_0(stackbase);
	EUCALLSET_2(ans, process_top_level_form,mod,CAR(walker));
	UNSTACK_TMP(walker);
      }

      return(ans);
    }

    /*
    if (op == sym_define) {
      return(TL_define(stacktop,mod,CDR(form)));
    }
    */
    if (op == sym_defun)       {
      return(TL_defun(stacktop,mod,CDR(form)));
    }
    if (op == sym_deflocal) {
      return(TL_deflex(stacktop,mod,CDR(form)));
    }
    if (op == sym_defmacro) {
      return(TL_defmacro(stacktop,mod,CDR(form)));
    }

    if (op == sym_defvar) return(TL_defvar(stacktop,mod,CDR(form)));
      
    if (op == sym_defconstant) return(TL_defconstant(stacktop,mod,CDR(form))); 

    if (op == sym_import) {
      process_import_form(stacktop,mod,CDR(form));
      return(nil);
    }

    if (op == sym_expose) {
      process_expose_form(stacktop,mod,CDR(form)); 
      return(nil);
    }

    if (op == sym_export) {
      EUCALL_2(process_exports,mod,CDR(form));
      return(nil);
    }

    if (op == sym_include_forms) {
      EUCALL_2(process_included_forms,mod,CDR(form));
      return(nil);
    }

    /* hell, that'll do for now */

    /* try a macroexpand... */

    EUCALLSET_2(form,macroexpand_1,mod,form);
    
    if (CAR(CDR(form)) != nil) {
      while (CAR(CDR(form))!=nil)
	{ form = CAR(form);
	  mod=ARG_0(stackbase);
	  EUCALLSET_2(form, macroexpand_1,mod,form);
	}
      
      form = CAR(form);
      
      mod=ARG_0(stackbase);
      goto top;
    }

    form = CAR(form);

    /* not a macro... */

    /* ok, so for user-friendliness (ho-ho) just to a module eval */

    mod=ARG_0(stackbase);
    RETURN_EUCALL(EUCALL_3(module_eval,mod,NULL,form));
  }

  /* wasne a symbol - rather than crash, try eval first */

  {
    LispObject ans;

    EUCALLSET_3(ans,module_eval,mod,NULL,form);
    return(ans);
  }
}
EUFUN_CLOSE

/* biggie!! */

LispObject backtrace_handle;
LispObject list_backtrace;

#define PUSH_TRACE(fun,args) \
  { \
    STACK_TMP(args); STACK_TMP(fun); STACK_TMP(backtrace_handle); \
  }

#define SET_TRACE(sp,op,env)	\
{				\
   *(sp)=env;			\
   *((sp)+1)=op;			\
   *((sp)+2)=backtrace_handle;	\
}

void quickie_module_eval_backtrace(LispObject *stacktop)
{
  LispObject *walker;

  print_string(stacktop,StdOut(),"\n");

  for (walker = GC_STACK_BASE(); walker != GC_STACK_POINTER(); ++walker) {
    
    if ((*(walker)) == backtrace_handle) {
      
      print_string(stacktop,StdOut(),"entered: ");
      EUCALL_2(Fn_print, ((*(walker-1)))->FUNCTION.name,StdOut());
    }

  }

  print_string(stacktop,StdOut(),"\n");

}

void module_eval_backtrace(LispObject *stacktop)
{
  LispObject *walker;
  LispObject env;

  for (walker = GC_STACK_BASE(); walker != stacktop; ++walker) {
    
    if (*walker == backtrace_handle) {
      
      print_string(stacktop,StdOut(),"\n");
      print_string(stacktop,StdOut(),"entered: ");
      EUCALL_2(Fn_print,((*(walker-1)))->FUNCTION.name,StdOut());
      print_string(stacktop,StdOut(),"\n");

      if ((*(walker-2)) != NULL && (is_env(*(walker-2)))) {

	for (env = (*(walker-2)); env != NULL; env = env->ENV.next) {

	  print_string(stacktop,StdOut(),"  ");
	  STACK_TMP(env);
	  generic_apply_2(stacktop,generic_prin,env->ENV.variable,StdOut());
	  UNSTACK_TMP(env);
	  STACK_TMP(env);
	  print_string(stacktop,StdOut(),": ");
	  generic_apply_2(stacktop,generic_prin,env->ENV.value,StdOut());
	  print_string(stacktop,StdOut(),"\n");
	  UNSTACK_TMP(env);
	}

      }

    }

  }

  print_string(stacktop,StdOut(),"\n");

}

/* Better backtrace
   * Works by examining vref(arg,0)
   * if == nil begin backtrace, return vector (next end frame)
   * o/w move down 1
   * THIS WILL BREAK FOR LOWTAG SYSTEM !
   */

EUFUN_1(Fn_backtrace_by_arg,arg)
{
  LispObject *walker,*oldtop;
  LispObject val;
  
  /* First time round, we hack the vector */
  if (vref(arg,0)==nil)
    {
      vref(arg,0)=allocate_integer(stacktop,(int)GC_STACK_BASE());
      vref(arg,1)=allocate_integer(stacktop,(int)stacktop);	
    }
  
  oldtop=(LispObject *) intval(vref(arg,1));
  walker=(LispObject *) intval(vref(arg,0));

  while (walker < oldtop)
    {
      if (*walker == backtrace_handle)
	{
	  LispObject ptr;
	  LispObject lst=nil;
	  
	  ptr= *(walker-2);
	  while (ptr!=NULL && is_env(ptr))
	    { /* oh for 1st class envs */
	      LispObject xx;

	      STACK_TMP(ptr->ENV.next);
	      STACK_TMP(lst);
	      xx=EUCALL_2(Fn_cons,ptr->ENV.variable,ptr->ENV.value);
	      UNSTACK_TMP(lst);
	      lst=EUCALL_2(Fn_cons,xx,lst);
	      UNSTACK_TMP(ptr);
	    }
	  val=EUCALL_2(Fn_cons,*(walker-1),lst);
	  
	  /* return new value in vector */
	  vref(ARG_0(stackbase),0)=allocate_integer(stacktop,(int) (walker+1));
	  vref(ARG_0(stackbase),2)=val;
	  return lisptrue;
	}
      
      /* test for bytecode return address.. */
      
      if ( ((int)*walker)&1)
	{
	  val=EUCALL_2(Fn_cons,nil,nil);
	  CAR(val)=*(walker+2);
	  CDR(val)=*(walker+1); /* Context */
	  vref(ARG_0(stackbase),0)=allocate_integer(stacktop,(int) (walker+2));
	  vref(ARG_0(stackbase),2)=val;
	  return lisptrue;
	}
      walker++;
    }
  
  /* no more frames */
  return nil;
}
EUFUN_CLOSE

/*
  *
  * The interpreter lies below 
  */

#define check_if(stmt) /* :-> */

#define ALLOCATE_N_ENVS(var,env) \
{				\
  var=env;				\
  for (i=0 ; i< nargs; i++)		\
    var=allocate_env(stacktop,nil,nil,var); \
}

LispObject module_eval(LispObject *stackbase)
{
  LispObject module_apply_args(LispObject *stackbase, int callargs, LispObject fn);
  LispObject mod,env,form;
  LispObject *stacktop;
  LispObject op;

  mod = ARG_0(stackbase);
  env = ARG_1(stackbase);
  form = ARG_2(stackbase);
  stacktop=stackbase+3;
  STACKS_OK_P(stacktop,form);

  stackbase+=3;	/* Room for trace */
  ARG_0(stackbase)=mod;
  ARG_1(stackbase)=env;
  ARG_2(stackbase)=form;
 toplabel:  
  mod = ARG_0(stackbase);
  env = ARG_1(stackbase);
  form = ARG_2(stackbase);

  stacktop=stackbase+3;

  if (!is_cons(form))
    { /* should check for loose special forms */
      if (is_symbol(form))
	{
	  LispObject tmp=symbol_ref(stacktop,mod,env,form);
	  if (!is_special(tmp)) return(tmp);
	  else	
	    CallError(stacktop,"Invalid use of reserved word",form,NONCONTINUABLE);
	}
      else	
	return form;
    }

  op = CAR(form);

  ARG_3(stackbase)=op;
  stacktop++;

  if (is_symbol(op))
    { 
#ifndef NODEBUG
      { extern int gc_paranoia;
	if (gc_paranoia)
	  fprintf(stderr,"%s\n",stringof(op->SYMBOL.pname));
      }
#endif
      op = symbol_ref(stacktop,mod,env,op);
      ARG_3(stackbase)=op;
    }
  else
    if (is_cons(op))
      {	
	op=EUCALL_3(module_eval,mod,env,op);
	ARG_3(stackbase)=op;
	mod=ARG_0(stackbase);
	env=ARG_1(stackbase);
	form=ARG_2(stackbase);
      }

  if (is_macro(op))
    { 
      LispObject newform;
      
      newform = EUCALL_2(module_mv_apply_1,op,CDR(form));
      /*STACK_TMP(newform);*/
  ATOMIC(stacktop,
	 /*UNSTACK_TMP(newform);*/
      if (!is_cons(newform))
	     form=newform;
      else
	{
	  form=ARG_2(stackbase);
	  CAR(form) = CAR(newform);
	  CDR(form) = CDR(newform);
	}
	 )
	EUTAIL_3(ARG_0(stackbase)/*mod*/,ARG_1(stackbase)/*env*/,form);
    }


  if (is_c_function(op)
#ifdef BCI
      || is_b_function(op)
#endif
      )
    {
      LispObject lastarg;

      LispObject walker, extras = nil;
      int i, args, extra;
      BEGIN_NARY_EUCALL();

      walker = CDR(form);
#ifdef BCI
      args = ((is_c_function(op))
	      ? op->C_FUNCTION.argtype
	      : intval(bytefunction_nargs(op)));
#else
      args = op->C_FUNCTION.argtype;
#endif
      extra = (args < 0);
      args = extra ? -args : args;
      
      if (is_c_function(op))
	if (op->C_FUNCTION.env != NULL)
	  { STACK_TMP(nil); /* space for arg */
	    NARY_PUSH_ARG(op->C_FUNCTION.env);
	  }

      if (args==0)
	{
	  if (walker!=nil)
	    CallError(stacktop,"Too many args to C-fn",op,NONCONTINUABLE);
	  else
	    {
#ifdef BCI	    
	      if (is_b_function(op))
		{
		  return(apply_nary_bytefunction(stackbase,0,op));
		}	
	      else
		return(op->C_FUNCTION.func(stackbase));
#else
	      return(op->C_FUNCTION.func(stackbase));
#endif
	    }
	}
      for (i=0; i < args-1 ; i++)
	{
	  if (walker==nil)
	    CallError(stacktop,"C function wants more args", op, NONCONTINUABLE);
	  STACK_TMP(nil); /* place where arg will go */
	  STACK_TMP(CDR(walker));
	  /* XXX assume 1) CDR(nil)=nil, module_eval(nil)=nil */
	  NARY_PUSH_ARG(EUCALL_3(module_eval,ARG_0(stackbase)/*mod*/,
				 ARG_1(stackbase)/* env */,CAR(walker)));
	  UNSTACK_TMP(walker);
	}

      if (extra)
	{ 
	  LispObject ptr;

	  if (walker!=nil)
	    {
	      LispObject xx;

	      STACK_TMP(CDR(walker));
	      EUCALLSET_3(xx,module_eval,ARG_0(stackbase) /*mod*/,
                              ARG_1(stackbase)/*env*/, CAR(walker));
	      EUCALLSET_2(lastarg,Fn_cons,xx,nil);
	      UNSTACK_TMP(walker);
	      STACK_TMP(lastarg);
	      ptr = lastarg;
	      while(walker!=nil)
		{	
		  STACK_TMP(CDR(walker));
		  STACK_TMP(ptr);
		  EUCALLSET_3(xx, module_eval, ARG_0(stackbase)	/*mod*/, 
			      ARG_1(stackbase)/*env*/, CAR(walker));
		  xx = EUCALL_2(Fn_cons, xx, nil);
		  UNSTACK_TMP(ptr);
		  CDR(ptr)=xx;
		  ptr = CDR(ptr);
		  UNSTACK_TMP(walker);
		}
	      UNSTACK_TMP(lastarg);
	    }
	  else
	    lastarg=nil;
	}
      else
	{
	  if (walker == nil)
	    {
	      CallError(stacktop,
			"C function wants more args", op, NONCONTINUABLE);
	    }

	  if (CDR(walker)!=nil)
	    CallError(stacktop,"Eval: Too many args to 'C-function",CDR(walker),
		      NONCONTINUABLE);
	  EUCALLSET_3(lastarg,module_eval,ARG_0(stackbase)/*mod*/,
		      ARG_1(stackbase)/*env*/,CAR(walker));
	}
      NARY_PUSH_ARG(lastarg);
      op=ARG_3(stackbase);

#ifdef BCI
      if (is_c_function(op))
	return(NARY_EUCALL(op->C_FUNCTION.func));
      else
	{	/* B-function */
	  return(apply_nary_bytefunction(argbase,args,op));
	}
#else
      return(NARY_EUCALL(op->C_FUNCTION.func));
#endif
      END_NARY_EUCALL();
    }

  if (is_generic(op))
    { 
      RETURN_EUCALL(EUCALL_4(call_generic,mod,env,op,CDR(form)));
    }


#if 1 /* I have 2 ways of doing this --- the first is faster (5%) */
  if (is_i_function(op))
    {
      LispObject args, exps, callenv;
      int extra, nargs, i;
      extra = ( op->I_FUNCTION.argtype < 0);
      nargs = extra ? -op->I_FUNCTION.argtype : op->I_FUNCTION.argtype;
      STACK_TMP(op);
      STACK_TMP(op);
      STACK_TMP(form);
      ALLOCATE_N_ENVS(callenv,op->I_FUNCTION.env);
      UNSTACK_TMP(form);
      UNSTACK_TMP(op);
      STACK_TMP(callenv);
      if (nargs == 0)
	{
	  if (CDR(form)!=nil)
	    CallError(stackbase,"Too many args to I-function",op,NONCONTINUABLE);
	}
      else
	{		
	  exps=callenv;
	  for (args = op->I_FUNCTION.bvl ; is_cons(args) ; args=CDR(args))
	    {
	      exps->ENV.variable=CAR(args);
	      exps=exps->ENV.next;
	    }
	  if (extra)
	    exps->ENV.variable=args;
	  
	  exps = CDR(form);
	  for (i=0 ; i<nargs-extra ; i++)
	    {
	      if (exps == nil)
		{
		  CallError(stacktop,
			    "i function wants more args", op, NONCONTINUABLE);
		}
	      else
		{
		  LispObject nextarg;

		  STACK_TMP(CDR(exps));
		  STACK_TMP(callenv);
		  EUCALLSET_3(nextarg,module_eval,
			      ARG_0(stackbase) /*mod*/,
			      ARG_1(stackbase) /*env*/,
			      CAR(exps));
		  UNSTACK_TMP(callenv);
		  callenv->ENV.value = nextarg;
		  callenv = callenv->ENV.next;
		  UNSTACK_TMP(exps);
		}
	      /* end i-function-loop */
	    }
					      
	  /* last arg */

	  if (extra)
	    {
	      LispObject lastarg=nil;

	      STACK_TMP(callenv); /* need this */
	      if (exps!=nil)
		{
		  LispObject xx;
		  LispObject ptr;

		  STACK_TMP(CDR(exps));
		  EUCALLSET_3(xx, module_eval, ARG_0(stackbase) /*mod*/
			      , ARG_1(stackbase) /*env*/, CAR(exps));
		  EUCALLSET_2(lastarg,Fn_cons,xx,nil);
		  UNSTACK_TMP(exps);
		  STACK_TMP(lastarg);
		  ptr = lastarg;
		  while(exps!=nil)
		    {	
		      STACK_TMP(CDR(exps));
		      STACK_TMP(ptr);
		      EUCALLSET_3(xx, module_eval, ARG_0(stackbase) /*mod*/
				  , ARG_1(stackbase) /*env*/, CAR(exps));
		      xx = EUCALL_2(Fn_cons, xx, nil);
		      UNSTACK_TMP(ptr);
		      CDR(ptr)=xx;
		      ptr = CDR(ptr);
		      UNSTACK_TMP(exps);
		    }
		  UNSTACK_TMP(lastarg);
		}
	      else
		lastarg=nil;

	      UNSTACK_TMP(callenv);
	      callenv->ENV.value=lastarg;
	    }
	  else if (exps!=nil)
	    {	
	      UNSTACK_TMP(callenv); UNSTACK_TMP(op);
	      CallError(stackbase,"Too many args to i-function",op,NONCONTINUABLE);
	    }
	}
      UNSTACK_TMP(callenv);
      UNSTACK_TMP(op);
      /* now we call it.., cunningly inlining the progn */

      { LispObject forms = op->I_FUNCTION.body;
	/* Throw it all away */
	stacktop=stackbase;
	SET_TRACE(stackbase-3,op,callenv);

	while (CDR(forms)!=nil)
	  {
	    STACK_TMP(CDR(forms));
	    STACK_TMP(callenv);
	    STACK_TMP(op);
	    EUCALL_3(module_eval,
		     op->I_FUNCTION.home,
		     callenv,
		     CAR(forms));
	    UNSTACK_TMP(op);
	    UNSTACK_TMP(callenv);
	    UNSTACK_TMP(forms);
	  }

	mod = ARG_0(stackbase) = op->I_FUNCTION.home;
	env = ARG_1(stackbase) = callenv;
	form = ARG_2(stackbase) = CAR(forms);
	goto toplabel;
      }
    }
#else
  if (is_i_function(op))
    {
      LispObject args, exps, callenv;
      int extra;

      extra = ( op->I_FUNCTION.argtype < 0);
      callenv = op->I_FUNCTION.env;
      STACK_TMP(op);
      if (op->I_FUNCTION.argtype == 0)
	{
	  if (CDR(form)!=nil)
	    CallError(stackbase,"Too many args to I-function",op,NONCONTINUABLE);
	}
      else
	{	
	  for ((args = op->I_FUNCTION.bvl,
		exps = CDR(form));
	       is_cons(args);
	       (args = CDR(args),
		exps = CDR(exps)))
	    {
	      if (exps == nil)
		{
		  CallError(stacktop,
			    "i function wants more args", op, NONCONTINUABLE);
		}
	      else
		{
		  LispObject nextarg;

		  STACK_TMP(exps);
		  STACK_TMP(args);
		  STACK_TMP(callenv);
		  EUCALLSET_3(nextarg,module_eval,
			      ARG_0(stackbase) /*mod*/,
			      ARG_1(stackbase) /*env*/,
			      CAR(exps));
		  UNSTACK_TMP(callenv);
		  UNSTACK_TMP(args);
		  STACK_TMP(args);
		  callenv = allocate_env(stacktop,CAR(args),
					 nextarg, callenv);
		  UNSTACK_TMP(args);
		  UNSTACK_TMP(exps);

		}
	      /* end i-function-loop */
	    }
					      
	  /* last arg */

	  if (extra)
	    {
	      LispObject lastarg=nil;

	      STACK_TMP(callenv); /* need this */
	      STACK_TMP(args);

	      if (exps!=nil)
		{
		  LispObject xx;
		  LispObject ptr;

		  STACK_TMP(CDR(exps));
		  EUCALLSET_3(xx, module_eval, ARG_0(stackbase) /*mod*/
			      , ARG_1(stackbase) /*env*/, CAR(exps));
		  EUCALLSET_2(lastarg,Fn_cons,xx,nil);
		  UNSTACK_TMP(exps);
		  STACK_TMP(lastarg);
		  ptr = lastarg;
		  while(exps!=nil)
		    {	
		      STACK_TMP(CDR(exps));
		      STACK_TMP(ptr);
		      EUCALLSET_3(xx, module_eval, ARG_0(stackbase) /*mod*/
				  , ARG_1(stackbase) /*env*/, CAR(exps));
		      xx = EUCALL_2(Fn_cons, xx, nil);
		      UNSTACK_TMP(ptr);
		      CDR(ptr)=xx;
		      ptr = CDR(ptr);
		      UNSTACK_TMP(exps);
		    }
		  UNSTACK_TMP(lastarg);
		}
	      else
		lastarg=nil;

	      UNSTACK_TMP(args);
	      UNSTACK_TMP(callenv);
	      callenv = allocate_env(stacktop,args,lastarg, callenv);
	    }
	  else if (exps!=nil)
	    {	
	      UNSTACK_TMP(op);
	      CallError(stackbase,"Too many args to i-function",op,NONCONTINUABLE);
	    }
	}

      UNSTACK_TMP(op);
      /* now we call it.., cunningly inlining the progn */

      { LispObject forms = op->I_FUNCTION.body;
	/* Throw it all away */
	stacktop=stackbase;
	SET_TRACE(stackbase-3,op,callenv);

	while (CDR(forms)!=nil)
	  {
	    STACK_TMP(CDR(forms));
	    STACK_TMP(callenv);
	    STACK_TMP(op);
	    EUCALL_3(module_eval,
		     op->I_FUNCTION.home,
		     callenv,
		     CAR(forms));
	    UNSTACK_TMP(op);
	    UNSTACK_TMP(callenv);
	    UNSTACK_TMP(forms);
	  }

	mod = ARG_0(stackbase) = op->I_FUNCTION.home;
	env = ARG_1(stackbase) = callenv;
	form = ARG_2(stackbase) = CAR(forms);
	goto toplabel;
      }
    }
#endif
  
  
  if (is_special(op))
    {
      if (op==special_progn)
	{ LispObject forms = CDR(form);
	
	  while (CDR(forms)!=nil)
	    {
	      STACK_TMP(CDR(forms));
	      EUCALL_3(module_eval,
		       ARG_0(stackbase)/*mod*/,
		       ARG_1(stackbase)/*env*/,
		       CAR(forms));
	      UNSTACK_TMP(forms);
	    }

	  EUTAIL_3(ARG_0(stackbase)/*mod*/,
		   ARG_1(stackbase)/*env*/,
		   CAR(forms));
	}
      if (op == special_if)
	{	
	  LispObject res,stmt=CDR(form);
	  check_if(stmt);
	  
	  STACK_TMP(CDR(stmt));
	  res = EUCALL_3(module_eval,mod,env,CAR(stmt));
	  if ( res == nil)
	    {
	      UNSTACK_TMP(stmt);
	      EUTAIL_3(ARG_0(stackbase)/*mod*/,ARG_1(stackbase)/*env*/
		       ,CAR(CDR(stmt)));
	    }
	  UNSTACK_TMP(stmt);
	  EUTAIL_3(ARG_0(stackbase)/*mod*/,ARG_1(stackbase)/*env*/,CAR(stmt));
	}

      if (op->SPECIAL.env==NULL)
	RETURN_EUCALL(EUCALL_3(op->SPECIAL.func,mod,env,CDR(form)));
      else
	RETURN_EUCALL(EUCALL_2(op->SPECIAL.func,mod,CDR(form)));
    }

  if (is_continue(op))
    { LispObject res;
      /* CAR(nil)==nil! */
      res = EUCALL_3(module_eval,mod,env,CAR(CDR(form)));
      op=ARG_3(stackbase);
      call_continuation(stacktop,op,res);
      return nil; /* not really */
    }
  
  /* default case.. */
  {
    LispObject *ptr,*argbase,tmp;
    int nargs=0;
    
    argbase=stacktop;
    ptr=argbase;
    *ptr++=op;
    stacktop++;
    ARG_2(stackbase)=CDR(form);
  
    while (is_cons(ARG_2(stackbase)))
      {
	tmp=EUCALL_3(module_eval,mod,env,CAR(ARG_2(stackbase)));
	*ptr=tmp;
	
	mod=ARG_0(stackbase);
	env=ARG_1(stackbase);
	ARG_2(stackbase)=CDR(ARG_2(stackbase));
	stacktop++;
	nargs++;
	ptr++;
      }
    if (ARG_2(stackbase)!=nil)
      CallError(stacktop,"Eval: bad list",nil,NONCONTINUABLE);
    /* should move args down stacktop->stackbase...*/
    return (module_apply_args(argbase,nargs+1,CAR(Cb_no_function_fn)));
  }
}



/* The same, but different... we could be clever + do the tail call properly*/
EUFUN_4( call_generic, mod, env, gf, forms)
{
  LispObject lastarg;
  LispObject walker, extras = nil;
  int i, args, extra;
  BEGIN_NARY_EUCALL();

  walker = forms;
  args = intval(generic_argtype(gf));
  extra = (args < 0);
  args = extra ? -args : args;

  /* Too much cut and paste! */
  for (i=0; i < args-1 ; i++)
    {
      STACK_TMP(nil);		/* place where arg will go */
      STACK_TMP(CDR(walker));
      NARY_PUSH_ARG(EUCALL_3(module_eval,ARG_0(stackbase) /*mod*/,
			     ARG_1(stackbase) /* env */,CAR(walker)));
      UNSTACK_TMP(walker);

      if (walker == nil)
	{
	  CallError(stacktop,
		    "Generic function wants more args", gf, NONCONTINUABLE);
	}
    }

  if (extra)
    { 
      LispObject ptr;

      stacktop=argbase+argcount;

      if (walker!=nil)
	{
	  STACK_TMP(CDR(walker));
	  EUCALLSET_2(lastarg,Fn_cons,CAR(walker),nil);
	  UNSTACK_TMP(walker);
	  STACK_TMP(lastarg);
	  ptr = lastarg;
	  while(walker!=nil)
	    {	
	      LispObject xx;
	      STACK_TMP(CDR(walker));
	      STACK_TMP(ptr);
	      EUCALLSET_3(xx, module_eval, ARG_0(stackbase)/*mod*/, ARG_1(stackbase)/*env*/, CAR(walker));
	      xx = EUCALL_2(Fn_cons, xx, nil);
	      UNSTACK_TMP(ptr);
	      CDR(ptr)=xx;
	      ptr = CDR(ptr);
	      UNSTACK_TMP(walker);
	    }
	  UNSTACK_TMP(lastarg);
	}
      else
	lastarg=nil;
    }
  else
    {	 
      if (CDR(walker)!=nil)
	CallError(stacktop,"Eval: Too many args to Generic-function",CDR(walker),
		  NONCONTINUABLE);
      EUCALLSET_3(lastarg,module_eval,ARG_0(stackbase) /*mod*/,ARG_1(stackbase)/*env*/,CAR(walker));
    }
  NARY_PUSH_ARG(lastarg);
  gf=ARG_2(stackbase);
  return(NARY_EUCALL_1(generic_apply,gf));
  END_NARY_EUCALL();
}
EUFUN_CLOSE

EUFUN_2(module_mv_apply_1,op, form)
{
  LispObject module_apply_args(LispObject *, int , LispObject );
  LispObject *walker=stackbase;
  int n=0;

  while (is_cons(form))
    {
      *walker=CAR(form);
      form=CDR(form);
      walker++;
      n++;
    }

  if (form!=nil)
    CallError(stackbase,"Improper list passed to mv_apply",nil,NONCONTINUABLE);

  return(module_apply_args(stackbase,n,op));
  
}
EUFUN_CLOSE

/* More restatement */
LispObject module_apply_args(LispObject *stackbase, int callargs, LispObject fn)
{
  void listify_args(LispObject *,int ,LispObject *);
  LispObject *stacktop=stackbase+callargs;

  if (is_i_function(fn) || is_i_macro(fn))
    {
      int nargs=fn->I_FUNCTION.argtype;
      LispObject env=fn->I_FUNCTION.env;
      LispObject args;
      LispObject *walker=stackbase;
      int extras;
      
      extras= (nargs<0);
      
      if (nargs==0 && callargs==0)
	RETURN_EUCALL(EUCALL_3(Sf_progn,
			       fn->I_FUNCTION.home,
			       env,
			       fn->I_FUNCTION.body));

      if ( (callargs!=nargs)
	  && (!extras || (extras && callargs < -nargs-1)))
	CallError(stackbase,"apply: i-function called with wrong number of args",fn,NONCONTINUABLE);
      
      STACK_TMP(fn);	/* we stack it twice on the off chance */
      STACK_TMP(fn);	/* it is an nary function called with n-1 args */
      for (args=fn->I_FUNCTION.bvl;
	   is_cons(args);
	   )
	{
	  STACK_TMP(CDR(args));
	  env=allocate_env(stacktop,CAR(args),*walker,env);
	  walker++;
	  UNSTACK_TMP(args);
	}
      if (args!=nil)
	{
	  STACK_TMP(env); STACK_TMP(args);
	  if (callargs!=nargs)
	    listify_args(walker,callargs+nargs+1,stacktop);

	  UNSTACK_TMP(args); UNSTACK_TMP(env);
	  env=allocate_env(stacktop,args,*walker,env);
	}
      UNSTACK_TMP(fn);
#if 0 /* Stack paranioa */
      if (!is_i_function(fn) && !is_i_macro(fn))
	system_lisp_exit(0);
#endif
      RETURN_EUCALL(EUCALL_3(Sf_progn,
			     fn->I_FUNCTION.home,
			     env,
			     fn->I_FUNCTION.body));
      
    }	
  
  if (is_c_function(fn) || is_c_macro(fn) 
#ifdef BCI      
      || is_b_function(fn) || is_b_macro(fn)
#endif
      )
    {
#ifdef BCI
      int nargs=
	((is_c_function(fn)||is_c_macro(fn))
	 ? fn->C_FUNCTION.argtype
	 : intval(bytefunction_nargs(fn)));
#else
      int nargs = fn->C_FUNCTION.argtype;
#endif
      if (is_c_function(fn) && fn->C_FUNCTION.env!=NULL)
	{	/* Whups --- the env needs to be inserted */
	  int i;
	  
	  for (i=callargs; i>=0; i--)
	    stackbase[i+1]=stackbase[i];

	  stackbase[0]=(LispObject)fn->C_FUNCTION.env;
	}
      if (callargs!=nargs)
	{
	  if (nargs<0 && callargs>= -nargs-1)
	    {	
	      int act= -nargs-1;

	      STACK_TMP(fn); /* could be anything --- just to stop the */
	      STACK_TMP(fn); /* value being blatted */
	      listify_args(stackbase+act,callargs-act,stacktop);
	      UNSTACK_TMP(fn);
	    }
	  else
	    CallError(stacktop,"C function called with wrong number of args",fn,NONCONTINUABLE);
	}
#ifdef BCI
      if (is_c_function(fn) || is_c_macro(fn))
	return((fn->C_FUNCTION.func)(stackbase));
      else
	return(apply_nary_bytefunction(stackbase,
				       nargs>0 ? nargs : -nargs,
				       fn));
#else
      return((fn->C_FUNCTION.func)(stackbase));
#endif      
    }			

  if (is_generic(fn))
    {	
      int nargs=intval(generic_argtype(fn));
      
      if (nargs!=callargs)
	CallError(stacktop,"Generic called with wrong number of args",fn,NONCONTINUABLE);

      return(generic_apply(stackbase,fn));
    }

  if (is_continue(fn))
    {
      if (callargs==0)
	{
	  call_continuation(stackbase,fn,nil);
	  return nil; 
	}

      if (callargs==1)
	{
	  call_continuation(stackbase,fn,*stackbase);
	}
      CallError(stackbase,"apply: continuation: too many args",fn,NONCONTINUABLE);
      /* nope */
      return nil;
    }
  
  /* default case */
    if (CAR(Cb_no_function_fn)==nil)
      {
	CallError(stacktop,"Apply: Invalid operator",nil,NONCONTINUABLE);
      }
  else
    {
      int i;
    
      for (i=callargs; i>=0; i--)
	stackbase[i+1]=stackbase[i];

      stackbase[0]=fn;
    
      return(module_apply_args(stackbase,callargs+1,CAR(Cb_no_function_fn)));
    }
}

/* Should be a macro */
void listify_args(LispObject *start,int n,LispObject *stacktop)
{
  int i;
  LispObject lst;

  if (n==0)
    {
      *start=nil;
      return;
    }
  
  lst=allocate_n_conses(stacktop,n);
  CAR(lst)= *start;
  *start = lst;

  start++;
  lst=CDR(lst);
  for (i=1; i<n; i++)
    {
      CAR(lst) = *start;
      lst=CDR(lst);
      start++;
    }
}
#define SYM_REF_DBG(x) /* x;fflush(stderr); */

LispObject symbol_ref(LispObject *stacktop,
		     LispObject mod,LispObject env,LispObject sym)
{
  LispObject walker;
  LispObject spec;

SYM_REF_DBG(fprintf(stderr,"symol_ref with sym '%s'\n",stringof(sym->symbol.pname)));

  /* parameter environment */

  walker = env;

SYM_REF_DBG(fprintf(stderr,"symol_ref env search\n"));

  while (walker != NULL) {
    if (walker->ENV.variable == sym) 
      return(walker->ENV.value);
    else
      walker = walker->ENV.next;
  }

  if (SYM_CACHE_MODULE(sym) == mod)
    return(SYM_CACHE_VALUE(sym));

  /* self evaluating symbols */

  if (sym == sym_nil) return(nil);
  if (sym == lisptrue) return(lisptrue);
  
  /* Check caches */
  /* language constructs and key words */

  STACK_TMP(mod);
  spec=EUCALL_2(Fn_table_ref,special_table,sym);
  UNSTACK_TMP(mod);

  if (spec != nil) 
    {
      SYM_CACHE_SET(sym,mod,spec);
      return spec;	
    }
  
  /* module reference */

  return(EUCALL_2(Fn_module_value,mod,sym));
}


LispObject module_set_new(LispObject *stacktop,LispObject mod,LispObject sym,LispObject val)
{
  return(EUCALL_4(module_set_new_aux,mod,sym,val,lisptrue));
}

LispObject module_set_new_constant(LispObject *stacktop,LispObject mod,
				   LispObject sym,LispObject val)
{
  return(EUCALL_4(module_set_new_aux,mod,sym,val,nil));
}


EUFUN_2(Fn_module_value, mod, sym)
{
  LispObject bind;
  
  bind=GET_BINDING(mod,sym);

  if (bind==nil)
    {
      char buf[256];
      sprintf(buf,"No such binding in module %s",
	      stringof(mod->MODULE.name->SYMBOL.pname));
      CallError(stacktop, buf, sym, NONCONTINUABLE);
    }
  if (is_bind(bind))
    { /* Good value */
      LispObject val;

      if (is_i_module(BINDING_HOME(bind)))
	{
	  val = BINDING_VALUE(bind);
	  SYM_CACHE_SET(sym,mod,val);
	  return val;
	}
      if (is_c_module(BINDING_HOME(bind)))
	{
	  val=vref((BINDING_HOME(bind)->C_MODULE.values),intval(BINDING_VALUE(bind)));
	  SYM_CACHE_SET(sym,mod,val);
	  return val;
	}
      else 
	CallError(stacktop,"Unexpected module type",bind,NONCONTINUABLE);	
    }

  CallError(stacktop,"Unexpected value of binding",bind,NONCONTINUABLE);
  return nil;
}
EUFUN_CLOSE

EUFUN_3(module_set,mod, sym, val)
{
  LispObject bind;

  
  if (is_c_module(mod))
    CallError(stacktop,"module set: can't set in compiled module",sym,NONCONTINUABLE);

  if(reserved_symbol_p(sym))
    CallError(stacktop,"module set: can't set reserved symbol",sym,NONCONTINUABLE);

  bind=GET_BINDING(mod,sym);
  mod=ARG_0(stackbase);
  sym=ARG_1(stackbase);
  val=ARG_2(stackbase);
  if (bind==nil)
    {	/* Be kind and add it anyhow */
      SYM_CACHE_SET(sym,mod,val);

      ADD_BINDING(ARG_0(stackbase)/* mod*/, ARG_1(stackbase)/*sym*/,
		  ARG_2(stackbase)/*val*/,lisptrue);
      return ARG_2(stackbase);
    }
  
  if (BINDING_MUTABLE(bind))
    {
      SYM_CACHE_SET(sym,mod,val);
      BINDING_VALUE(bind)=val;
      return val;
    }
  else
    {
      SYM_CACHE_SET(sym,mod,val);
      
      if (is_c_module(BINDING_HOME(bind)))
	CallError(stacktop,"Binding not assignable.", bind, NONCONTINUABLE);

      print_string(stacktop,StdErr(),"*** Setting immutable binding:");
      print_string(stacktop,StdErr(),stringof(sym->SYMBOL.pname));
      BINDING_VALUE(bind)=val;
      return val;
    }
  
  CallError(stacktop,"module set: How the hell did I get here",sym,NONCONTINUABLE);
  return nil;
}
EUFUN_CLOSE

static EUFUN_4(module_set_new_aux,mod,sym,val,mutability)
{
  LispObject bind;

  if (!is_i_module(mod))
    CallError(stacktop,"Module set new: tried to set in compiled module",sym,NONCONTINUABLE);

  if(reserved_symbol_p(sym))
    CallError(stacktop,"module set: can't set reserved symbol",sym,NONCONTINUABLE);

  bind=GET_BINDING(mod,sym);
  
  if (bind==nil)
    { /* Its a newie */
      SYM_CACHE_INIT(sym);
      SYM_CACHE_SET(sym,mod,val);
      ADD_BINDING(ARG_0(stackbase),ARG_1(stackbase),ARG_2(stackbase),ARG_3(stackbase));
      return ARG_1(stackbase);
    }
  else
    {
      if (BINDING_HOME(bind)==mod)
	{
	  SYM_CACHE_SET(sym,mod,val);
	  BINDING_VALUE(bind)=val;
	  SET_BINDING_MUTABLE(bind,mutability);
	  return sym;
	}
      else
	CallError(stacktop,"Module set new: tried to set over imported binding",sym,NONCONTINUABLE);
    }
  /* NOT ever */
  return nil; 
}
EUFUN_CLOSE

EUFUN_4(register_module_import, mod, name, inmod, inname)
{
  LispObject bind, localbind;
  LispObject xx;
  if (is_c_module(mod))
    CallError(stacktop, "register import: can't import into compiled module",
	      mod,NONCONTINUABLE);

  /* Into canonical form */
  bind=GET_BINDING(inmod,inname);
  if (bind==nil)
    {
      xx=EUCALL_2(Fn_cons,inmod->C_MODULE.name,inname);
      CallError(stacktop,"non-existent binding exported", xx,NONCONTINUABLE);
    }

  /* ok, but is it exported anyhow ? */
  if (!BINDING_EXPORTED(bind))
    {	
      EUCALLSET_2(xx, Fn_memq, inname, module_exports(inmod));
      if (xx == nil)
	CallError(stacktop, "register import: name not exported",inname,
		  NONCONTINUABLE);
      else
	SET_BINDING_EXPORT(bind);
    }
  
  /* See if we have something of the same name */
  localbind=GET_BINDING(mod,name);

  if (localbind==nil)
    { /* add it */
      STACK_TMP(bind);
      SYM_CACHE_INIT(name);
      UNSTACK_TMP(bind);
      IMPORT_BINDING(ARG_0(stackbase),ARG_1(stackbase),bind);
      return nil;
    }
  else 
    {
      if (bind==localbind) /* done this before */
	return nil;
      else 
	{
	  xx=EUCALL_2(Fn_cons, inmod->C_MODULE.name,name);
	  CallError(stacktop,"register import: binding exists locally",xx,NONCONTINUABLE);
	}
    }

  CallError(stacktop,"Register import: Yeouch. not here",nil,NONCONTINUABLE);

  return nil;
}
EUFUN_CLOSE

int module_binding_exists_p(LispObject *stacktop,
			    LispObject mod,
			    LispObject name)
{
  LispObject bind;
  
  bind=GET_BINDING(mod,name);
  
  return (bind!=nil);
}	


/* *************************************************************** */
/* Initialisation of this section                                  */
/* *************************************************************** */

void initialise_modules(LispObject *stacktop)
{
  extern MODULE *current_open_module;

  sym_include_forms = get_symbol(stacktop,"include-forms");
  add_root(&sym_include_forms);
  SYSTEM_INITIALISE_GLOBAL(LispObject,current_interactive_module,NULL);
  ADD_SYSTEM_GLOBAL_ROOT(current_interactive_module);
  global_module_table = (LispObject) EUCALL_1(make_table,NULL);
  add_root(&global_module_table);
  add_root((LispObject*)&current_open_module);
  backtrace_handle = get_symbol(stacktop,"****backtrace-handle****");
  add_root(&backtrace_handle);
  sym_only   = get_symbol(stacktop,"only");
  add_root(&sym_only);
  sym_except = get_symbol(stacktop,"except");
  add_root(&sym_except);
  Cb_no_function_fn=EUCALL_2(Fn_cons,nil,nil);
  add_root(&Cb_no_function_fn);
}

