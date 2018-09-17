/* ******************************************************************** */
/*  symbols.c        Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/*  General symbol hacking and global oblist                            */
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, March 1990 (During compiler rationalisation)
 */

#include <stdio.h>
#include "funcalls.h"
#include "defs.h"
#include "structs.h"
#include "global.h"
#include "error.h"
#include <string.h>
#include "table.h"
#include "symboot.h"
#include "allocate.h"
#include "copy.h"

/* Changed 'cos of a KSR-compiler bug! */
#define strings_equal_p(a,b) ((a)[0] == (b)[0] && (strcmp(a,b)==0))

LispObject ObList;


typedef enum { LHere, LLeft, LRight, LFirst } LookupDirection;
static LispObject find_name_in_oblist(LispObject ,char *,LookupDirection *);
static void add_sym_to_oblist(LispObject where,LispObject sym, LookupDirection dir);
  
LispObject get_symbol(LispObject* stackbase, char *name)
{
  LookupDirection dir;
  LispObject newloc,sym;
  LispObject *stacktop=stackbase;

  ATOMIC(stackbase,
    newloc=find_name_in_oblist(ObList,name,&dir);
    if (dir==LHere)
	 sym=newloc;
    else
      { /* NOT GC SAFE */
	STACK_TMP(newloc);
        sym=allocate_symbol(stacktop,name);
	UNSTACK_TMP(newloc);
        add_sym_to_oblist(newloc,sym,dir);
      }
	 );
  return sym;
}

/* Provided for compatibility */

LispObject get_symbol_by_copying(LispObject *stackbase,char *name)
{
  return(get_symbol(stackbase,name));
}

static void add_sym_to_oblist(LispObject where,LispObject sym, LookupDirection dir)
{
  switch(dir)
    {
    case LLeft:
      where->SYMBOL.left=sym;
      break;

    case LRight:
      where->SYMBOL.right=sym;
      break;
	  
    case LFirst:
      ObList=sym;
    }
}


static LispObject find_name_in_oblist(LispObject tree,char *str,LookupDirection *dir)
{
  LookupDirection mydir=LFirst;
  LispObject prev=NULL;
  int newhash=hash(str);
  
  while(TRUE)
    {
      if (tree==NULL)
	{
	  *dir=mydir;
	  return prev;
	}

      if (newhash==tree->SYMBOL.hash)
	{
	  if (strings_equal_p(stringof(tree->SYMBOL.pname),str))
	    {	
	      *dir=LHere;
	      return tree;
	    }
	  else
	    {
	      prev=tree; mydir=LLeft;
	      tree=tree->SYMBOL.left;
	    }
	}
      else 
	{
	  if (tree->SYMBOL.hash<newhash)
	    {
	      prev=tree; mydir=LLeft;
	      tree=tree->SYMBOL.left;
	    }
	  else
	    {
	      prev=tree; mydir=LRight;
	      tree=tree->SYMBOL.right;
	    }
	}
    }
}	

int reserved_symbol_p(LispObject sym)
{
  return((sym == sym_dynamic ||
	  sym == sym_dynamic_let ||
	  sym == sym_dynamic_setq ||
	  sym == sym_dynamic_set ||
/*
	  sym == sym_defclass ||
	  sym == sym_defcondition ||
*/
	  sym == sym_defconstant ||
/*
	  sym == sym_defgeneric ||
*/
	  sym == sym_deflocal ||
	  sym == sym_defmacro ||
/*
	  sym == sym_defmethod ||
	  sym == sym_defstruct ||
*/
	  sym == sym_defun || 
	  sym == sym_defvar ||
	  sym == sym_if ||
	  sym == sym_lambda ||
/*
	  sym == sym_letcc ||
          sym == sym_with_handler ||
*/
	  sym == sym_nil || 
	  sym == sym_quote ||
	  sym == lisptrue ||
	  sym == sym_setq));
}

/* Useful symbols to have... */

LispObject sym_nil;

LispObject sym_define;
LispObject sym_function,sym_macro,sym_constant;

LispObject sym_defclass,sym_defcondition,sym_defconstant,sym_defgeneric,
           sym_deflocal,sym_defmacro,sym_defmethod,sym_defstruct,sym_defun;

LispObject sym_defmodule,sym_load_module,sym_start_module,sym_enter_module;

LispObject sym_root;

LispObject sym_loaded_modules;

LispObject sym_lambda,sym_macro_lambda,sym_setq,sym_if,sym_progn;
LispObject sym_import,sym_expose,sym_expose_except,sym_rename,sym_export;
LispObject sym_root;
LispObject sym_letcc,sym_unwind_protect;

LispObject sym_methods;

LispObject sym_defvar,sym_dynamic_setq,
           sym_dynamic_set,sym_dynamic,sym_dynamic_let;

LispObject sym_with_handler;

LispObject sym_rest;

LispObject sym_cons;

/* defstruct symbols... */

LispObject sym_initarg,sym_initargs,sym_initform,sym_reader,sym_writer,
           sym_accessor,sym_class,sym_mutable;

LispObject sym_constructor,sym_metaclass,sym_metaclass_initargs;

LispObject sym_position;

LispObject sym_message,sym_error_value;

LispObject sym_anonymous_class;

LispObject sym_name,sym_superclass,sym_slot_descriptions;

LispObject sym_exit;

LispObject sym_evalcm;

LispObject sym_tagbody;

LispObject sym_quote, sym_unquote, sym_unquote_splicing;

void initialise_symbols(LispObject *stacktop)
{
  /* Garbage proofed by virtue of being on the object list */
  /* Better do gensyms differently... */
  add_root(&ObList);
  

  sym_nil = get_symbol(stacktop,"nil");
  add_root(&sym_nil);
  sym_define   = get_symbol(stacktop,"define");
  add_root(&sym_define);
  sym_function = get_symbol(stacktop,"function");
  add_root(&sym_function);
  sym_macro    = get_symbol(stacktop,"macro");
  add_root(&sym_macro);
  sym_constant = get_symbol(stacktop,"constant");
  add_root(&sym_constant);
  
  sym_defclass     = get_symbol(stacktop,"defclass");
  add_root(&sym_defclass);
  sym_defcondition = get_symbol(stacktop,"defcondition");
  add_root(&sym_defcondition);
  sym_defconstant  = get_symbol(stacktop,"defconstant");
  add_root(&sym_defconstant);
  sym_defgeneric   = get_symbol(stacktop,"defgeneric");
  add_root(&sym_defgeneric);
  sym_deflocal     = get_symbol(stacktop,"deflocal");
  add_root(&sym_deflocal);
  sym_defmacro     = get_symbol(stacktop,"defmacro");
  add_root(&sym_defmacro);
  sym_defmethod    = get_symbol(stacktop,"defmethod");
  add_root(&sym_defmethod);
  sym_defstruct    = get_symbol(stacktop,"defstruct");
  add_root(&sym_defstruct);
  sym_defun        = get_symbol(stacktop,"defun");
  add_root(&sym_defun);
  
  sym_defmodule  = get_symbol(stacktop,"defmodule");
  add_root(&sym_defmodule);
  sym_load_module = get_symbol(stacktop,"load-module");
  add_root(&sym_load_module);
  sym_start_module = get_symbol(stacktop,"start-module");
  add_root(&sym_start_module);
  sym_enter_module = get_symbol(stacktop,"enter-module");
  add_root(&sym_enter_module);
  sym_loaded_modules = get_symbol(stacktop,"loaded-modules");
  add_root(&sym_loaded_modules);
  
  sym_root = get_symbol(stacktop,"root");
  add_root(&sym_root);
  
  sym_lambda  = get_symbol(stacktop,"lambda");
  add_root(&sym_lambda);
  sym_macro_lambda = get_symbol(stacktop,"macro-lambda");
  add_root(&sym_macro);
  sym_setq    = get_symbol(stacktop,"setq");
  add_root(&sym_setq);
  sym_if      = get_symbol(stacktop,"if");
  add_root(&sym_if);
  sym_progn   = get_symbol(stacktop,"progn");
  add_root(&sym_progn);
  sym_quote   = get_symbol(stacktop,"quote");
  add_root(&sym_quote);
  
  sym_import = get_symbol(stacktop,"import");
  add_root(&sym_import);
  sym_expose = get_symbol(stacktop,"expose");
  add_root(&sym_expose);
  sym_expose_except = get_symbol(stacktop,"expose-except");
  add_root(&sym_expose_except);
  sym_rename = get_symbol(stacktop,"rename");
  add_root(&sym_rename);
  
  sym_export = get_symbol(stacktop,"export");
  add_root(&sym_export);
  
  sym_root = get_symbol(stacktop,"root");
  add_root(&sym_root);
  
  sym_letcc          = get_symbol(stacktop,"let/cc");
  add_root(&sym_letcc);
  sym_unwind_protect = get_symbol(stacktop,"unwind-protect");
  add_root(&sym_unwind_protect);
  
  sym_with_handler   = get_symbol(stacktop,"with-handler");
  add_root(&sym_with_handler);
  
  sym_methods = get_symbol(stacktop,"methods");
  add_root(&sym_methods);
  
  sym_defvar       = get_symbol(stacktop,"defvar");
  add_root(&sym_defvar);
  sym_dynamic_setq = get_symbol(stacktop,"dynamic-setq");
  add_root(&sym_dynamic_setq);
  sym_dynamic_set  = get_symbol(stacktop,"dynamic-set");
  add_root(&sym_dynamic_set);
  sym_dynamic_let  = get_symbol(stacktop,"dynamic-let");
  add_root(&sym_dynamic_let);
  sym_dynamic      = get_symbol(stacktop,"dynamic");
  add_root(&sym_dynamic);
  
  sym_rest = get_symbol(stacktop,"rest");
  add_root(&sym_rest);
  
  sym_cons = get_symbol(stacktop,"cons");
  add_root(&sym_cons);
  
  sym_initarg  = get_symbol(stacktop,"initarg");
  add_root(&sym_initarg);
  sym_initargs = get_symbol(stacktop,"initargs");
  add_root(&sym_initargs);
  sym_initform = get_symbol(stacktop,"initform");
  add_root(&sym_initform);
  sym_reader   = get_symbol(stacktop,"reader");
  add_root(&sym_reader);
  sym_writer   = get_symbol(stacktop,"writer");
  add_root(&sym_writer);
  sym_accessor = get_symbol(stacktop,"accessor");
  add_root(&sym_accessor);
  sym_class    = get_symbol(stacktop,"class");
  add_root(&sym_class);
  sym_mutable  = get_symbol(stacktop,"mutable");
  add_root(&sym_mutable);
  
  sym_constructor = get_symbol(stacktop,"constructor");
  add_root(&sym_constructor);
  sym_metaclass   = get_symbol(stacktop,"metaclass");
  add_root(&sym_metaclass);
  sym_metaclass_initargs = get_symbol(stacktop,"metaclass-initargs");
  add_root(&sym_metaclass_initargs);
  
  sym_position = get_symbol(stacktop,"position");
  add_root(&sym_position);
  sym_message = get_symbol(stacktop,"message");
  add_root(&sym_message);
  sym_error_value = get_symbol(stacktop,"error-value");
  add_root(&sym_error_value);
  
  sym_anonymous_class = get_symbol(stacktop,"anonymous-class");
  add_root(&sym_anonymous_class);
  
  sym_name = get_symbol(stacktop,"name");
  add_root(&sym_name);
  sym_superclass = get_symbol(stacktop,"superclass");
  add_root(&sym_superclass);
  sym_slot_descriptions = get_symbol(stacktop,"slot-descriptions");
  add_root(&sym_slot_descriptions);
  
  sym_exit = get_symbol(stacktop,"exit");
  add_root(&sym_exit);
  
  sym_evalcm = get_symbol(stacktop,"eval/cm");
  add_root(&sym_evalcm);
  
  sym_tagbody = get_symbol(stacktop,"tagbody");
  add_root(&sym_tagbody);

  sym_quote = get_symbol(stacktop,"quote");
  add_root(&sym_quote);

  sym_unquote = get_symbol(stacktop,"unquote");
  add_root(&sym_unquote);

  sym_unquote_splicing = get_symbol(stacktop,"unquote-splicing");
  add_root(&sym_unquote_splicing);

}
