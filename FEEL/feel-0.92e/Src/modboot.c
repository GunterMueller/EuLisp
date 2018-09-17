/* ******************************************************************** */
/*  modboot.c        Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/*  Wild thing II                                                       */
/* ******************************************************************** */

/*
 * $Id: modboot.c,v 1.1 1994/01/25 13:45:08 djb Exp $
 *
 * $Log: modboot.c,v $
 * Revision 1.1  1994/01/25  13:45:08  djb
 * Initial revision
 *
 * Revision 1.1  1994/01/25  13:29:33  djb
 * Initial revision
 *
 * Revision 2.1  93/01/17  17:25:21  pab
 * 17 Jan 1993 The next generation...
 * 
 * Revision 1.14  1992/11/26  15:56:44  pab
 * Lost Envs, descrim_depth
 *
 * Revision 1.13  1992/06/09  14:04:24  pab
 * fixed includes
 *
 * Revision 1.12  1992/05/26  11:28:03  pab
 * map option added
 *
 * Revision 1.11  1992/04/27  21:57:29  pab
 * correctied some casts
 *
 * Revision 1.10  1992/04/26  21:02:52  pab
 * Added support for static_vectors, plus call to
 * add_boot_module
 * (Stardent bug gone now !)
 *
 * Revision 1.9  1992/03/13  18:12:02  pab
 * sysV fix: move value vectors into shared space
 * so GC can get to them.
 *
 * Revision 1.8  1992/01/29  13:42:45  pab
 * binding fixes
 *
 * Revision 1.7  1992/01/09  22:28:55  pab
 * Fixed for low tag ints
 *
 * Revision 1.6  1992/01/07  22:15:44  pab
 * ncc compatable, plus backtrace
 *
 * Revision 1.5  1992/01/07  17:12:29  pab
 * Added a cast. No sign of the stardent bug
 *
 * Revision 1.4  1992/01/05  22:48:09  pab
 * Minor bug fixes, plus BSD version
 *
 * Revision 1.3  1991/12/22  15:14:19  pab
 * Xmas revision
 *
 * Revision 1.2  1991/09/11  12:07:25  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:49:47  pab
 * Initial revision
 *
 * Revision 1.4  1991/06/04  17:17:21  kjp
 * No acceptable change.
 *
 * Revision 1.3  1991/02/13  18:23:36  kjp
 * Pass.
 *
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "funcalls.h"
#include "defs.h"
#include "structs.h"
#include "global.h"

#include "allocate.h"
#include "symboot.h"

#include "ngenerics.h"
#include "modules.h"

#include "table.h"
#include "error.h"

#include "modboot.h"
#include "bvf.h"

/* Current module information */

MODULE*  current_open_module; /* The thing itself */
static int      entries;     /* No of entries it claims to have */
static int      entry_count; /* The no of entries thus far */

/* Are we generating .i files... */

extern int command_line_interface_flag;
#define CREATE_INTERFACE (command_line_interface_flag)

/* Interface generators... */

static FILE *i_file;

static void open_module_interface(char *name)
{
  char i_name[500];
  
  i_name[0]='\0';
  strcat(i_name,name);
  strcat(i_name,".i");

  i_file = fopen(i_name,"w");

  fprintf(i_file,"((dependencies)\n (exported-ids ");
  fflush(i_file);
  printf("Open %s - ",name); fflush(stdout);
}

static void update_interface(char *name,int index,char *class,int argtype)
{
  fprintf(i_file,"\n   ((name . |%s|) (address |%s| |%s|) (class . %s) (argtype . %d) (position %d))",
	  name,stringof(current_open_module->name->SYMBOL.pname),name,class,argtype,index);
  fflush(i_file);
}

static void close_module_interface()
{
  printf("closing - "); fflush(stdout);
  fprintf(i_file,"))\n");
  fflush(i_file);
  fclose(i_file);
  printf("closed\n"); fflush(stdout);
}

void open_module(LispObject *stacktop, MODULE* mod,LispObject *vals,char* name,int ents)
{
  LispObject Fn_make_module(LispObject *);

  LispObject sym_name,lisp_ents;
  if (current_open_module != NULL) {
    fprintf(stderr,"\nINITERROR: tried to open '%s' while in '%s'\n",
	           name,current_open_module->name);
    system_lisp_exit(1);
  }

  sym_name = get_symbol(stacktop,name);
  lisp_ents = allocate_integer(stacktop,ents);

  mod=(MODULE *)EUCALL_2(Fn_make_module,sym_name,lisp_ents);
  /* Set up the fresh module */
  
  /* Set up tracking info */

  current_open_module = mod;
  entries = ents;
  entry_count = 0;
  
  /* Interface... */

  if (CREATE_INTERFACE) open_module_interface(name);
}

LispObject make_module_function(LispObject *stacktop,char* lispname,
				LispObject (*fun)(LispObject*),int argcode)
{
  LispObject lfunc;
  LispObject symbol,number;

  if (entry_count == entries) {
    fprintf(stderr,
	    "\nINITERROR: more module functions that declared in '%s'\n",
	    stringof(current_open_module->name->SYMBOL.pname));
    exit(1);
  }

  symbol = get_symbol(stacktop,lispname); /* May or may not allocate anew */
  STACK_TMP(symbol);

  vref((current_open_module->values),entry_count) = 
    allocate_module_function(stacktop,(LispObject)current_open_module,
			     symbol,fun,argcode);
  number=allocate_integer(stacktop,entry_count);
  UNSTACK_TMP(symbol);
  /* GC Safe */
  
  SYM_CACHE_INIT(symbol);
  ADD_BINDING(current_open_module,symbol,number,nil);

  current_open_module->exported_names = 
    EUCALL_2(Fn_cons,symbol,current_open_module->exported_names);

  if (CREATE_INTERFACE) update_interface(lispname,entry_count,"function",argcode);
  ++entry_count;

  return(vref(current_open_module->values,entry_count-1));
}

LispObject make_unexported_module_function(LispObject *stacktop,char* lispname,
					   LispObject (*fun)(),int argcode)
{
  LispObject lfunc;
  LispObject symbol,number;

  if (entry_count == entries) {
    fprintf(stderr,
	    "\nINITERROR: more module functions that declared in '%s'\n",
	    stringof(current_open_module->name->SYMBOL.pname));
    exit(1);
  }

  symbol = get_symbol(stacktop,lispname); /* May or may not allocate anew */

  STACK_TMP(symbol);
  vref((current_open_module->values),entry_count) = 
    allocate_module_function(stacktop,(LispObject)current_open_module,
			     symbol,fun,argcode);
  number=allocate_integer(stacktop,entry_count);
  UNSTACK_TMP(symbol);
  
  SYM_CACHE_INIT(symbol);
  ADD_BINDING(current_open_module,symbol,number,nil);


  /* Symbols can't be GC'd and modules are safe anyway!! */

  ++entry_count;

/*  fprintf(stderr,"%d OK\n",entry_count); fflush(stderr); */

  return(vref((current_open_module->values),entry_count-1));
}

LispObject make_module_macro(LispObject *stacktop,char *name,LispObject (*func)(),int args)
{
  LispObject ret;

  ret = make_module_function(stacktop,name,func,args);
  lval_typeof(ret) = TYPE_C_MACRO;

  return(ret);
}

void close_module()
{
  if (current_open_module == NULL) {
    fprintf(stderr,"\nINITERROR: tried to close NULL module\n");
    exit(1);
  }

  if (entries != entry_count) {
    fprintf(stderr,
	    "\nINITERROR: tried to close '%s' with %d entries, %d needed\n",
	    stringof(current_open_module->name->SYMBOL.pname),entry_count,entries);
    exit(1);
  }

#ifdef BCI
  add_boot_module((LispObject)current_open_module);
#endif

  current_open_module = NULL;
  if (CREATE_INTERFACE) close_module_interface();
}


LispObject make_unexported_module_special(LispObject *stacktop,char* lispname,LispObject (*fun)())
{
  LispObject number;
  LispObject symbol;

  if (entry_count == entries) {
    fprintf(stderr,
	    "\nINITERROR: more module functions that declared in '%s'\n",
	    stringof(current_open_module->name->SYMBOL.pname));
    exit(1);
  }

  symbol = get_symbol(stacktop,lispname); /* May or may not allocate anew */

  STACK_TMP(symbol);
  vref((current_open_module->values),entry_count) = allocate_special(stacktop,symbol,fun);
  number=allocate_integer(stacktop,entry_count);
  UNSTACK_TMP(symbol);
  
  SYM_CACHE_INIT(symbol);
  ADD_BINDING(current_open_module,symbol,number,nil);

  /* Symbols can't be GC'd and modules are safe anyway!! */

  ++entry_count;

  return(vref((current_open_module->values),entry_count-1));
}

LispObject make_module_entry(LispObject *stacktop,char *name,LispObject value)
{
  LispObject symbol,number;

  if (entry_count == entries) {
    fprintf(stderr,
	    "\nINITERROR: more module entries that declared in '%s'\n",
	    stringof(current_open_module->name->SYMBOL.pname));
    exit(1);
  }
  vref((current_open_module->values),entry_count) = value; 

  STACK_TMP(value);
  symbol = get_symbol(stacktop,name); /* May or may not allocate anew */
  STACK_TMP(symbol);
  number = allocate_integer(stacktop,entry_count);
  UNSTACK_TMP(symbol); STACK_TMP(symbol);
  
  SYM_CACHE_INIT(symbol);
  ADD_BINDING(current_open_module,symbol,number,nil);

  
  UNSTACK_TMP(symbol);

  number =
    EUCALL_2(Fn_cons,symbol,current_open_module->exported_names);
  current_open_module->exported_names = number;

  if (CREATE_INTERFACE) update_interface(name,entry_count,"unknown",-1);
  ++entry_count;

  UNSTACK_TMP(value);
  return(value);
}


LispObject make_unexported_module_entry(LispObject *stacktop,char *name,
					LispObject value)
{
  LispObject symbol,number;

  if (entry_count == entries) {
    fprintf(stderr,
	    "\nINITERROR: more module entries that declared in '%s'\n",
	    stringof(current_open_module->name->SYMBOL.pname));
    exit(1);
  }
  vref((current_open_module->values),entry_count) = value; 

  STACK_TMP(value);
  symbol = get_symbol(stacktop,name); /* May or may not allocate anew */
  STACK_TMP(symbol);
  number = allocate_integer(stacktop,entry_count);
  UNSTACK_TMP(symbol); STACK_TMP(symbol);
  
  SYM_CACHE_INIT(symbol);
  ADD_BINDING(current_open_module,symbol,number,nil);

  
  UNSTACK_TMP(symbol);

  ++entry_count;

  UNSTACK_TMP(value);
  return(value);
}


LispObject make_module_entry_using_symbol(LispObject *stacktop,
					  LispObject symbol,LispObject value)
{
  LispObject number;
  if (entry_count == entries) {
    fprintf(stderr,
	    "\nINITERROR: more module entries that declared in '%s'\n",
	    stringof(current_open_module->name->SYMBOL.pname));
    exit(1);
  }
  

  vref((current_open_module->values),entry_count) = value; 

  STACK_TMP(value); STACK_TMP(symbol);
  number = allocate_integer(stacktop,entry_count);
  SYM_CACHE_INIT(symbol);
  ADD_BINDING(current_open_module,symbol,number,nil);

  UNSTACK_TMP(symbol); STACK_TMP(symbol);
  current_open_module->exported_names = 
    EUCALL_2(Fn_cons,symbol,current_open_module->exported_names);
  UNSTACK_TMP(symbol);

  if (CREATE_INTERFACE) update_interface(stringof(symbol->SYMBOL.pname),entry_count,"unknown",-1);
  ++entry_count;
  UNSTACK_TMP(value);
  return(value);
}

LispObject make_module_special(LispObject *stacktop,
			       char* lispname,LispObject (*fun)())
{
  LispObject lfunc;
  LispObject symbol,number;

  if (entry_count == entries) {
    fprintf(stderr,
	    "\nINITERROR: more module functions that declared in '%s'\n",
	    stringof(current_open_module->name->SYMBOL.pname));
    exit(1);
  }

  symbol = get_symbol(stacktop,lispname); /* May or may not allocate anew */
  STACK_TMP(symbol);
  vref((current_open_module->values),entry_count) = 
    (LispObject) allocate_special(stacktop,symbol,fun);
  number = allocate_integer(stacktop,entry_count);
  UNSTACK_TMP(symbol);
  STACK_TMP(symbol);

  UNSTACK_TMP(symbol);
  /* Symbols can't be GC'd and modules are safe anyway!! */
  SYM_CACHE_INIT(symbol);
  ADD_BINDING(current_open_module,symbol,number,nil);

  current_open_module->exported_names = 
    EUCALL_2(Fn_cons,symbol,current_open_module->exported_names);

  ++entry_count;

  return(vref((current_open_module->values),entry_count-1));
}

LispObject make_module_generic(LispObject *stackbase,char *name,int code)
{
  LispObject sym,number,tmp;
  LispObject *stacktop=stackbase+1,*gf=stackbase;
  if (entry_count == entries) {
    fprintf(stderr,
	    "\nINITERROR: more module functions that declared in '%s'\n",
	    stringof(current_open_module->name->SYMBOL.pname));
    exit(1);
  }

  *gf=nil;
  vref(current_open_module->values,entry_count) =
    allocate_instance(stacktop,Generic);

  *gf=vref(current_open_module->values,entry_count);
  generic_home(*gf) = (LispObject)current_open_module;
  lval_typeof(*gf)=TYPE_GENERIC;

  sym = get_symbol(stacktop,name);
  STACK_TMP(sym);
  tmp = allocate_integer(stacktop,code);
  generic_argtype(*gf)=tmp;
  generic_discrimination_depth(*gf)=allocate_integer(stacktop,0);
  number=allocate_integer(stacktop,entry_count);
  UNSTACK_TMP(sym);

  STACK_TMP(number); STACK_TMP(sym);
  generic_name(*gf) = sym;

  generic_discriminator(*gf) = nil;
  generic_slow_method_cache(*gf) = nil;
  generic_fast_method_cache(*gf) = nil;
  generic_method_table(*gf) = nil;
  
  generic_method_class(*gf) = Method;
  UNSTACK_TMP(sym); UNSTACK_TMP(number);
  STACK_TMP(sym);
  SYM_CACHE_INIT(sym);
  ADD_BINDING(current_open_module,sym,number,nil);
  UNSTACK_TMP(sym);
  /* Symbols can't be GC'd and modules are safe anyway!! */

  current_open_module->exported_names = 
    EUCALL_2(Fn_cons,sym,current_open_module->exported_names);
  
  /** Ought to have code "generic" */
  if (CREATE_INTERFACE) update_interface(name,entry_count,"unknown",code);
  ++entry_count;

  return(*gf);
}

LispObject make_wrapped_module_generic(LispObject *stacktop,char *name,int code,
				       LispObject (*fun)())
{
  LispObject number;
  LispObject sym,gf,tmp;
  LispObject *stackbase=stacktop;

  ARG_0(stackbase) = nil; /*gf*/
  ARG_1(stackbase)=nil; /* number*/
  ARG_2(stackbase)=nil; /*sym*/

  stacktop+=3;
  if (entry_count == entries) {
    fprintf(stderr,
	    "\nINITERROR: more module functions that declared in '%s'\n",
	    stringof(current_open_module->name->SYMBOL.pname));
    exit(1);
  }

  sym = get_symbol(stacktop,name);
  ARG_2(stackbase)=sym;
  ARG_0(stackbase) = vref(current_open_module->values,entry_count) =
    allocate_instance(stacktop,Generic);

  
  lval_typeof(ARG_0(stackbase))=TYPE_GENERIC;
  generic_home(ARG_0(stackbase)) = (LispObject)current_open_module;
  tmp = allocate_integer(stacktop,code);
  generic_argtype(ARG_0(stackbase)) =tmp;
  generic_discrimination_depth(ARG_0(stackbase)) =allocate_integer(stacktop,0);
  generic_name(ARG_0(stackbase)) = ARG_2(stackbase);
  
  generic_fast_method_cache(ARG_0(stackbase)) = nil;
  generic_slow_method_cache(ARG_0(stackbase)) = nil;
  ARG_1(stackbase)=allocate_integer(stacktop,entry_count);

  generic_method_table(ARG_0(stackbase)) = nil;
  generic_method_class(ARG_0(stackbase)) = Method;

  generic_discriminator(ARG_0(stackbase)) = nil;
  
  SYM_CACHE_INIT(ARG_2(stackbase));
  ADD_BINDING(current_open_module,ARG_2(stackbase),ARG_1(stackbase),nil);

  /* Symbols can't be GC'd and modules are safe anyway!! */

  tmp =
    EUCALL_2(Fn_cons,ARG_2(stackbase),current_open_module->exported_names);
  current_open_module->exported_names = tmp;

  if (CREATE_INTERFACE) update_interface(name,entry_count,"unknown",code);
  ++entry_count;

  return(ARG_0(stackbase));
}


/*

 * Environment functions...

 */

LispObject make_anonymous_module_env_function_1(LispObject *stacktop,
						LispObject mod,
						LispObject (*fun)(LispObject*),
						int argtype,
						LispObject sym,
						LispObject val)
{
  LispObject lfunc;
  LispObject env;

  STACK_TMP(sym); STACK_TMP(val);
  lfunc = allocate_module_function(stacktop,mod,nil,fun,argtype); /* GC Safe */
  UNSTACK_TMP(val); UNSTACK_TMP(sym);
  STACK_TMP(lfunc);
  /* Rig the environment... */

  env = allocate_env(stacktop,sym,val,NULL);
  UNSTACK_TMP(lfunc);
  lfunc->C_FUNCTION.env = env;

  return(lfunc);
}

LispObject make_anonymous_module_env_function_2(LispObject *stacktop,
						LispObject mod,
						LispObject (*fun)(LispObject*),
						int argtype,
						LispObject sym1,
						LispObject val1,
						LispObject sym2,
						LispObject val2)
{
  LispObject lfunc;
  LispObject env;
  STACK_TMP(sym2); STACK_TMP(val2);
  STACK_TMP(sym1); STACK_TMP(val1);
  lfunc = allocate_module_function(stacktop,mod,nil,fun,argtype); /* GC Safe */
  
  /* Rig the environment... */
  UNSTACK_TMP(val1); UNSTACK_TMP(sym1); STACK_TMP(lfunc);
  env = allocate_env(stacktop,sym1,val1,NULL);
  UNSTACK_TMP(lfunc);
  UNSTACK_TMP(val2); UNSTACK_TMP(sym2); STACK_TMP(lfunc);
  env = allocate_env(stacktop,sym2,val2,env);
  UNSTACK_TMP(lfunc);
  lfunc->C_FUNCTION.env = env;

  return(lfunc);
}

