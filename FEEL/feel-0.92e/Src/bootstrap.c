/* ******************************************************************** */
/*  bootstrap.c      Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Rig up the basic Metaclasses/Classes                                 */
/* ******************************************************************** */

/*
 * $Id: bootstrap.c,v 1.1 1994/02/08 11:24:06 djb Exp $
 *
 * $Log: bootstrap.c,v $
 * Revision 1.1  1994/02/08  11:24:06  djb
 * Initial revision
 *
 * Revision 2.1  93/01/17  17:25:21  pab
 * 17 Jan 1993 The next generation...
 * 
 * Revision 1.9  1992/11/25  16:59:58  pab
 * Version .8
 *
 * Revision 1.7  1992/08/06  18:08:10  pab
 * I/C-function support
 *
 * Revision 1.6  1992/01/17  22:26:18  pab
 * deleted redundant function
 *
 * Revision 1.5  1992/01/09  22:28:43  pab
 * Fixed for low tag ints
 *
 * Revision 1.4  1991/12/22  15:13:50  pab
 * Xmas revision
 *
 * Revision 1.3  1991/11/15  13:44:21  pab
 * copyalloc rev 0.01
 *
 * Revision 1.2  1991/09/11  12:07:00  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:49:27  pab
 * Initial revision
 *
 * Revision 1.2  1991/02/13  18:16:46  kjp
 * Weak wrapper class + RCS log headers.
 *
 */

#define KJPDBG(x) 

/*
 * Change Log:
 *   Version 1, June 1989
 */

#include <stdio.h>
#include "funcalls.h"
#include "defs.h"
#include "structs.h"
#include "global.h"

#include "bootstrap.h"
#include "symboot.h"
#include "allocate.h"
#include "copy.h"

#include "slots.h"
#include "ngenerics.h"
#include "table.h"
/*

 * Should maybe turn all the symbol and class structure mallocs
 * into statics...

 */

extern LispObject Primitive_Class;
extern LispObject Thread_Class;
extern LispObject Method_Class;

/*

 * Special symbol initialisation...

 */

/* 
 * 'Place marker' class initialisation.
 * NB. Count must include superclasses...
 */
void gen_class_with_slots(LispObject *stacktop,
			  LispObject *obj,
			  int local_count)
{
  gen_class(stacktop,obj);
  (*obj)->CLASS.local_count = allocate_integer(stacktop, local_count);

}

/* Also registers a new root */

void gen_class(LispObject *stackbase,
	       LispObject *obj)
{
  LispObject sym, xx;
  LispObject *stacktop=stackbase+2;

  *obj = (LispObject) allocate_class(stacktop,NULL);
}

void set_class_size(LispObject *stacktop, LispObject class, LispObject super, int size)
{
  LispObject sz;

  STACK_TMP(class);
  sz=allocate_integer(stacktop, (super==NULL) ? size : intval(super->CLASS.local_count)+size);
  UNSTACK_TMP(class);

  class->CLASS.local_count=sz;
  
}

/*

 * Set up all the provided classes + special symbols.

 */

void bootstrap(LispObject *stacktop)
{
  /* Reserve space for the classes... 
     ... non garbage and easy for self reference */

  /* Root object and root class - self referential... */

  Object          = (LispObject) allocate_class(stacktop,NULL);
  Standard_Class  = (LispObject) allocate_class(stacktop,NULL);

  add_root(&Object); add_root(&Standard_Class); 
    
  Symbol = (LispObject) allocate_class(stacktop,NULL);

  Null = (LispObject) allocate_class(stacktop,NULL);

  Cons = (LispObject) allocate_class(stacktop,NULL);

  Integer = (LispObject) allocate_class(stacktop,NULL);

  Vector = (LispObject) allocate_class(stacktop,NULL);
  add_root(&Symbol);
  add_root(&Null);
  add_root(&Cons);
  add_root(&Integer);
  add_root(&Vector);
  /* Get nil... */

  EUCALLSET_2(nil, Fn_cons, NULL,NULL);
  lval_typeof(nil) = TYPE_NULL;
  add_root(&nil);
  /* Fill it later... */
  
  /* Symbols and objects needed during class gen */
/**
  lisptrue 
    = (LispObject) system_static_malloc(sizeof(struct symbol_structure));
**/
  /* Self evaluating symbols and nil */

  (void) make_special_symbol(stacktop,&lisptrue,"t");
  (void) make_special_symbol(stacktop,&unbound,"%_*unbound*_%");
  add_root(&lisptrue);	
  add_root(&unbound);

  /* Begin initialising... */
  /* We need integers for this... */
  allocate_static_integers(stacktop);
    
  gen_class(stacktop,&Primitive_Class);
  add_root(&Primitive_Class);

  gen_class(stacktop,&Thread_Class);
  add_root(&Thread_Class);

  /* The "place marker" classes */

  /* Metas */

  gen_class(stacktop,&Funcallable_Object_Class);
  add_root(&Funcallable_Object_Class);
  gen_class(stacktop,&Method_Class);
  add_root(&Method_Class);
  gen_class(stacktop,&Generic_Class);
  add_root(&Generic_Class);
  gen_class(stacktop,&Number);
  add_root(&Number);
  gen_class(stacktop,&Real);
  add_root(&Real);
  gen_class(stacktop,&Character);
  add_root(&Character);
  gen_class(stacktop,&String);
  add_root(&String);
  gen_class(stacktop,&Thread);
  add_root(&Thread);
  gen_class(stacktop,&Function);
  add_root(&Function);

  gen_class(stacktop,&CFunction);
  add_root(&CFunction);

  gen_class(stacktop,&IFunction);
  add_root(&IFunction);

  gen_class(stacktop,&Continue);
  add_root(&Continue);
  gen_class(stacktop,&Generic);
  add_root(&Generic);
  gen_class(stacktop,&Method);
  add_root(&Method);
  gen_class(stacktop,&Table);
  add_root(&Table);

  gen_class(stacktop,&Weak_Wrapper);
  add_root(&Weak_Wrapper);
  /* Do nil... */

#ifdef WITH_SMALL_CONSES
  nil->CONS.car = nil;
  nil->CONS.cdr = nil;
#else
  lval_classof(nil) = Null;
  nil->CONS.car = nil;
  nil->CONS.cdr = nil;
#endif
  { 
    extern LispObject boot_thread;
    lval_classof(boot_thread)=Thread;
  }

  /* Set up class size hierarchy. 
     Size _is_ important in this game.
     It is poss. for extra classes to be 
     inserted by init, but _no_ additional slots !*/
  
  set_class_size(stacktop,Object,NULL,0);
  /* metaclasses...*/
  set_class_size(stacktop,Standard_Class,Object,N_SLOTS_IN_CLASS);

  set_class_size(stacktop,Table,Object,N_SLOTS_IN_TABLE);
  set_class_size(stacktop,Thread,Object,N_SLOTS_IN_THREAD);
  set_class_size(stacktop,Generic,Object,N_SLOTS_IN_GENERIC);
  set_class_size(stacktop,Method,Object,N_SLOTS_IN_METHOD);
  
  /* Characters */
  allocate_static_chars(stacktop);


}

/* see #define CLASS_NAMES_ENTRIES in class.c */
void initialize_boot_classes(LispObject *stacktop)
{
  
  make_module_entry(stacktop,"<object>",Object);
  make_module_entry(stacktop,"<class>",Standard_Class);
  make_module_entry(stacktop,"<primitive-class>",Primitive_Class);
  make_module_entry(stacktop,"<thread-class>",Thread_Class);
  make_module_entry(stacktop,"<funcallable-object-class>",Funcallable_Object_Class);
  make_module_entry(stacktop,"<method-class>",Method_Class);
  make_module_entry(stacktop,"<generic-class>",Generic_Class);
  make_module_entry(stacktop,"<number>",Number);
  make_module_entry(stacktop,"<double-float>",Real);
  make_module_entry(stacktop,"<fixint>",Integer);
  make_module_entry(stacktop,"<symbol>",Symbol);
  make_module_entry(stacktop,"<null>",Null);
/*  make_module_entry(stacktop,"<pair>",Cons);*/
  make_module_entry(stacktop,"<cons>",Cons);
  make_module_entry(stacktop,"<character>",Character);
  make_module_entry(stacktop,"<string>",String);
  make_module_entry(stacktop,"<thread>",Thread);
  make_module_entry(stacktop,"<function>",Function);
  make_module_entry(stacktop,"<c-function>",CFunction);
  make_module_entry(stacktop,"<i-function>",IFunction);
  make_module_entry(stacktop,"<continuation>",Continue);
  make_module_entry(stacktop,"<generic-function>",Generic);
  make_module_entry(stacktop,"<method>",Method);
  make_module_entry(stacktop,"<vector>",Vector);
  make_module_entry(stacktop,"<table>",Table);
  make_module_entry(stacktop,"<weak-wrapper>",Weak_Wrapper);

}
