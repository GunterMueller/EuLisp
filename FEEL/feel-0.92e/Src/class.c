/* ******************************************************************** */
/*  class.c          Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* classes			                                        */
/* ******************************************************************** */

/*
 * $Id: class.c,v 1.2 1994/06/30 16:30:17 djb Exp $
 *
 * $Log: class.c,v $
 * Revision 1.2  1994/06/30  16:30:17  djb
 * Wolfgang's E7
 *
 * Revision 1.1  1994/01/25  13:45:08  djb
 * Initial revision
 *
 * Revision 1.1  1994/01/25  13:29:33  djb
 * Initial revision
 *
 * Revision 2.1  1993/01/17  17:25:21  pab
 * 17 Jan 1993 The next generation...
 *
 * Revision 1.19  1992/11/25  17:07:01  pab
 * Table changes, local_count->lispobject
 *
 * Revision 1.17  1992/08/06  18:09:48  pab
 * more reflective operations
 * q
 *
 * Revision 1.16  1992/06/12  00:03:02  pab
 * added more reflective-type hacks
 *
 * Revision 1.15  1992/06/09  13:58:35  pab
 * added set class , etc
 *
 * Revision 1.14  1992/05/26  12:28:40  pab
 * fixed for moving modules (xxx_template)
 *
 * Revision 1.13  1992/05/19  11:15:58  pab
 * exported alloc class, instance
 *
 * Revision 1.12  1992/04/26  21:00:15  pab
 * alloc_int fixes
 *
 * Revision 1.11  1992/03/14  14:33:48  pab
 * side efects return values
 *
 * Revision 1.10  1992/02/27  15:46:57  pab
 * bytecode + error changes
 *
 * Revision 1.9  1992/01/29  13:39:10  pab
 * Fixed gc bug
 *
 * Revision 1.8  1992/01/22  13:29:49  pab
 * Fixed GC bug
 *
 * Revision 1.7  1992/01/17  22:28:06  pab
 * Removed defstruct + defclass 'cos
 * no one used them
 *
 * Revision 1.6  1992/01/09  22:28:46  pab
 * Fixed for low tag ints
 *
 * Revision 1.5  1992/01/05  22:47:57  pab
 * Minor bug fixes, plus BSD version
 *
 * Revision 1.4  1991/12/22  15:13:56  pab
 * Xmas revision
 *
 * Revision 1.3  1991/11/15  13:44:31  pab
 * copyalloc rev 0.01
 *
 * Revision 1.2  1991/09/11  12:07:05  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:49:30  pab
 * Initial revision
 *
 * Revision 1.10  1991/06/17  19:05:23  pab
 * altered set_assoc to eval properly.
 *
 * Revision 1.8  1991/02/13  18:18:53  kjp
 * Pass.
 *
 */

#define KJPDBG(x)
#define INOUT(x)
#define CLASSBUG(x) /* fprintf(stderr,"CLASSBUG:");x;fflush(stderr) */

/*
 * Change Log:
 *   Version 1, June 1989
 *   Version N ( N >> 1 ), November 1989
 *   Mostly killed. Pab. Dec 92.
 */

#include <stdio.h>
#include "defs.h"
#include "structs.h"

#include "funcalls.h"

#include "global.h"
#include "error.h"

#include "class.h"
#include "vectors.h"
#include "table.h"
#include "bootstrap.h"
#include "slots.h"
#include "ngenerics.h"
#include "modules.h"
#include "modboot.h"
#include "garbage.h"
#include "calls.h"

static LispObject reader_template(LispObject *);
static LispObject writer_template(LispObject *);

#define CLASSES_ENTRIES 34
MODULE Module_classes;
static LispObject classes_module; /* Utter hack, Module_x no longer useful */
LispObject Module_classes_values[CLASSES_ENTRIES];

static LispObject generic_initialize;
static LispObject generic_allocate;

EUFUN_1(Fn_class_of,x)
{
  return classof(x);
}
EUFUN_CLOSE

EUFUN_2(Fn_set_class,x,cl)
{
  return lval_classof(x)=cl;
}
EUFUN_CLOSE

EUFUN_1(Fn_classp,x)
{
  return (typeof(x)==TYPE_CLASS ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_2(Gf_make_instance,x,lst)
{
  LispObject proto;
  
  proto=generic_apply_2(stacktop,generic_allocate,x,lst);
  return generic_apply_2(stacktop,generic_initialize,proto,ARG_1(stackbase));
}
EUFUN_CLOSE

EUFUN_2(Fn_subclassp,c1,c2)
{
  LispObject prec;
  
  if (c1 == c2) return(c1); /* Used to say lisptrue which is wrong */
  
  prec=c1->CLASS.precedence;
  
  while (prec!=nil)
    {
      if (CAR(prec)==c2)
	return c1;
      else
	prec=CDR(prec);
    }
  
  return(nil);
}
EUFUN_CLOSE

LispObject search_keylist(LispObject *stacktop,LispObject list,LispObject key)
{
  int i=0;
  LispObject ptr;

  if (list != nil && !is_cons(list))
    CallError(stacktop,"invalid key list",list,NONCONTINUABLE);
  
  ptr=list;
  while (ptr!=nil)
    { i++;
      ptr=CDR(ptr);
    }

  if (i%2 != 0)
    CallError(stacktop,"unbalanced initlist",list,NONCONTINUABLE);


  while(list != nil) {
    LispObject lkey = CAR(list);
    LispObject lval = CAR(CDR(list));
    
    if (key == lkey) return(lval);

    list = CDR(CDR(list));
  }

  return(unbound);
}


EUFUN_1(Fn_allocate_object,class)
{
  LispObject ans;

  ans=allocate_instance(stacktop,class);

  return ans;
}
EUFUN_CLOSE

EUFUN_2(Fn_set_type,x,n)
{
    /* Keep static types, callable types and watch for tagged integers */
    int t = typeof(x) & (STATIC_TYPE|CALLABLE_TYPE);

    t |= intval(n);
#ifndef NOLOWTAGINTS
    if(typeof(x) != TYPE_INT)
#endif
    lval_typeof(x) = t;
    /*printf("Null=%p, type of %p set to %x\n", Null,x,t);*/
    return x;
}
EUFUN_CLOSE

EUFUN_2(Fn_slot_ref,o,n)
{
  return slotref(o,intval(n));
}
EUFUN_CLOSE

EUFUN_3(Fn_set_slot_ref,o,n,v)
{
  return slotrefupdate(o,intval(n),v);
}
EUFUN_CLOSE


static EUFUN_1(Fn_slot_0_ref,o) { return slotref(o,0); }EUFUN_CLOSE
static EUFUN_1(Fn_slot_1_ref,o) { return slotref(o,1); }EUFUN_CLOSE
static EUFUN_1(Fn_slot_2_ref,o) { return slotref(o,2); }EUFUN_CLOSE
static EUFUN_1(Fn_slot_3_ref,o) { return slotref(o,3); }EUFUN_CLOSE
static EUFUN_1(Fn_slot_4_ref,o) { return slotref(o,4); }EUFUN_CLOSE
static EUFUN_1(Fn_slot_5_ref,o) { return slotref(o,5); }EUFUN_CLOSE
static EUFUN_1(Fn_slot_6_ref,o) { return slotref(o,6); }EUFUN_CLOSE
static EUFUN_1(Fn_slot_7_ref,o) { return slotref(o,7); }EUFUN_CLOSE
static EUFUN_1(Fn_slot_8_ref,o) { return slotref(o,8); }EUFUN_CLOSE
static EUFUN_1(Fn_slot_9_ref,o) { return slotref(o,9); }EUFUN_CLOSE

static EUFUN_2(Fn_set_slot_0,o,v) { return slotrefupdate(o,0,v); } EUFUN_CLOSE
static EUFUN_2(Fn_set_slot_1,o,v) { return slotrefupdate(o,1,v); } EUFUN_CLOSE
static EUFUN_2(Fn_set_slot_2,o,v) { return slotrefupdate(o,2,v); } EUFUN_CLOSE
static EUFUN_2(Fn_set_slot_3,o,v) { return slotrefupdate(o,3,v); } EUFUN_CLOSE
static EUFUN_2(Fn_set_slot_4,o,v) { return slotrefupdate(o,4,v); } EUFUN_CLOSE
static EUFUN_2(Fn_set_slot_5,o,v) { return slotrefupdate(o,5,v); } EUFUN_CLOSE
static EUFUN_2(Fn_set_slot_6,o,v) { return slotrefupdate(o,6,v); } EUFUN_CLOSE
static EUFUN_2(Fn_set_slot_7,o,v) { return slotrefupdate(o,7,v); } EUFUN_CLOSE
static EUFUN_2(Fn_set_slot_8,o,v) { return slotrefupdate(o,8,v); } EUFUN_CLOSE
static EUFUN_2(Fn_set_slot_9,o,v) { return slotrefupdate(o,9,v); } EUFUN_CLOSE

/* Slot accessors with a type check */
EUFUN_2(Fn_make_structure_reader,class,n)
{
  LispObject reader;

  reader=make_anonymous_module_env_function_2(stacktop, classes_module,
					      reader_template,1,
					      lisptrue, class,
					      lisptrue,n);
  
  return reader;
}
EUFUN_CLOSE

EUFUN_2(Fn_make_structure_writer,class,n)
{
  LispObject writer;

  writer=make_anonymous_module_env_function_2(stacktop, classes_module,
					      writer_template,2,
					      lisptrue, class,
					      lisptrue, n);
  
  return writer;
}
EUFUN_CLOSE

static EUFUN_2(reader_template, env, obj)
{
  LispObject class, n;
  /* Envs get switched by make_env_fn */
  n=env->ENV.value;
  class=env->ENV.next->ENV.value;

  if (EUCALL_2(Fn_subclassp,classof(obj),class)==nil)
    {
      LispObject xx;
      STACK_TMP(class); STACK_TMP(n);
      xx=EUCALL_2(Fn_cons,obj,nil);
      UNSTACK_TMP(n);
      xx=EUCALL_2(Fn_cons,n,xx);
      UNSTACK_TMP(class);
      xx=EUCALL_2(Fn_cons,class,xx);
      CallError(stacktop,"wrong class of object for reader",xx,NONCONTINUABLE);
    }

  return(slotref(obj,intval(n)));
}
EUFUN_CLOSE

static EUFUN_3(writer_template, env, obj, val)
{
  LispObject class, n;
  n=env->ENV.value;
  class=env->ENV.next->ENV.value;

  if (EUCALL_2(Fn_subclassp,classof(obj),class)==nil)
    {
      LispObject xx;
      STACK_TMP(class); STACK_TMP(n); 
      xx=EUCALL_2(Fn_cons,obj,nil);
      UNSTACK_TMP(n);
      xx=EUCALL_2(Fn_cons,n,xx);
      UNSTACK_TMP(class);
      xx=EUCALL_2(Fn_cons,class,xx);
      CallError(stacktop,"wrong class of object for writer",xx,NONCONTINUABLE);
    }

  return(slotrefupdate(obj,intval(n),val));
}
EUFUN_CLOSE

static EUFUN_2(Fn_initialize_local_slots,obj,lst)
{
  LispObject slots,inits,initarg,initfunc,val;
  int n=0;

  slots=classof(obj)->CLASS.local_slot_list;
  
  while (slots!=nil)
    {	
      if (CAR(slots)!=nil)
	{
	  val=unbound;
	  initarg=init_slot_initarg(CAR(slots));
	  if (initarg!=unbound)
	    {
	      inits=lst;
	      while (inits!=nil)
		{
		  if (CAR(inits)==initarg)
		    {
		      val=CAR(CDR(inits));
		      break;
		    } 	
		  inits=CDR(CDR(inits));
		}
	      /* initarg not found --- continue */
	    }
	  
	  if (val==unbound && (initfunc=init_slot_initfunction(CAR(slots)))!=unbound)
	    {
	      STACK_TMP(slots);
	      val=EUCALL_2(Fn_apply,initfunc,nil);
	      UNSTACK_TMP(slots);
	      obj=ARG_0(stackbase);
	      lst=ARG_1(stackbase);
	    }
	  slotref(obj,n)=val;
	}
      slots=CDR(slots);
      n++;
    }
  return obj;
}
EUFUN_CLOSE

/* *************************************************************** */
/* Initialisation of this module (should be separate...)           */
/* *************************************************************** */

/* Class name module stuff... */

#define CLASS_NAMES_ENTRIES 25 /* From bootstrap.c */  /* Too many */
MODULE Module_class_names;
LispObject Module_class_names_values[CLASS_NAMES_ENTRIES];

void register_class_names(LispObject *stacktop,LispObject c)
{
  LispObject sub;

  make_module_entry_using_symbol(stacktop,c->CLASS.name,c);

  sub = c->CLASS.subclasses;

  while (sub != nil) {
    STACK_TMP(CDR(sub));
    register_class_names(stacktop,CAR(sub));
    UNSTACK_TMP(sub);
  }
}

/* *************************************************************** */
/* Initialisation of this module                                   */
/* *************************************************************** */

#define SET_ASSOC(a,b) \
  { LispObject tmp,tmp2; \
    STACK_TMP(a); \
    tmp2=b; \
    UNSTACK_TMP(tmp); \
    set_anon_associate(stacktop,tmp,tmp2); \
  }

void initialise_classes(LispObject *stacktop)
{


  open_module(stacktop,
	      &Module_class_names,Module_class_names_values,
	      "class-names",CLASS_NAMES_ENTRIES);
  initialize_boot_classes(stacktop);
  close_module();

  /* Class operations */


  open_module(stacktop,
	      &Module_classes,Module_classes_values,
	      "classes",CLASSES_ENTRIES);

  generic_initialize
    = make_module_generic(stacktop,"initialize",2);
  
  generic_allocate
    = make_module_generic(stacktop,"allocate",2);
  
  add_root(&generic_initialize);
  add_root(&generic_allocate);

  /* Class object accessors... */

  (void) make_module_function(stacktop,"classp",Fn_classp,1);
  (void) make_module_function(stacktop,"subclassp",Fn_subclassp,2);
  (void) make_module_function(stacktop,"class-of",Fn_class_of,1);

  make_module_function(stacktop,"set-class-of",Fn_set_class,2);
  make_module_function(stacktop,"set-type",Fn_set_type,2);
  make_module_function(stacktop,"allocate-object",Fn_allocate_object,1);
  make_module_function(stacktop,"primitive-slot-ref",Fn_slot_ref,2);
  make_module_function(stacktop,"primitive-set-slot-ref",Fn_set_slot_ref,3);

  make_module_function(stacktop,"make",Gf_make_instance,-2);

  make_module_function(stacktop,"primitive-slot-ref-0",Fn_slot_0_ref,1);
  make_module_function(stacktop,"primitive-slot-ref-1",Fn_slot_1_ref,1);
  make_module_function(stacktop,"primitive-slot-ref-2",Fn_slot_2_ref,1);
  make_module_function(stacktop,"primitive-slot-ref-3",Fn_slot_3_ref,1);
  make_module_function(stacktop,"primitive-slot-ref-4",Fn_slot_4_ref,1);
  make_module_function(stacktop,"primitive-slot-ref-5",Fn_slot_5_ref,1);
  make_module_function(stacktop,"primitive-slot-ref-6",Fn_slot_6_ref,1);
  make_module_function(stacktop,"primitive-slot-ref-7",Fn_slot_7_ref,1);
  make_module_function(stacktop,"primitive-slot-ref-8",Fn_slot_8_ref,1);
  make_module_function(stacktop,"primitive-slot-ref-9",Fn_slot_9_ref,1);

  make_module_function(stacktop,"primitive-set-slot-ref-0",Fn_set_slot_0,2);
  make_module_function(stacktop,"primitive-set-slot-ref-1",Fn_set_slot_1,2);
  make_module_function(stacktop,"primitive-set-slot-ref-2",Fn_set_slot_2,2);
  make_module_function(stacktop,"primitive-set-slot-ref-3",Fn_set_slot_3,2);
  make_module_function(stacktop,"primitive-set-slot-ref-4",Fn_set_slot_4,2);
  make_module_function(stacktop,"primitive-set-slot-ref-5",Fn_set_slot_5,2);
  make_module_function(stacktop,"primitive-set-slot-ref-6",Fn_set_slot_6,2);
  make_module_function(stacktop,"primitive-set-slot-ref-7",Fn_set_slot_7,2);
  make_module_function(stacktop,"primitive-set-slot-ref-8",Fn_set_slot_8,2);
  make_module_function(stacktop,"primitive-set-slot-ref-9",Fn_set_slot_9,2);

  make_module_function(stacktop,"make-structure-reader", Fn_make_structure_reader,2);
  make_module_function(stacktop,"make-structure-writer", Fn_make_structure_writer,2);

  make_module_function(stacktop,"initialize-local-slots", Fn_initialize_local_slots,2);
  close_module();

  {
    LispObject xx;
    xx=get_symbol(stacktop,"classes");

    classes_module=get_module(stacktop,xx);
    add_root(&classes_module);
  }
}

