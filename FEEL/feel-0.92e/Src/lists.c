/* ******************************************************************** */
/* lists.c           Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* basic list operations		                                */
/* ******************************************************************** */

#define JMPDBG(x)

/*
 * Change Log:
 *   Version 1, March 1990 (Compiler rationalisation)
 *     Verified GC proof.
 */

#include <string.h>
#include "funcalls.h"
#include "defs.h"
#include "structs.h"
#include "error.h"
#include "global.h"

#include "allocate.h"
#include "modboot.h"
#include "calls.h"
#include "modules.h"

EUFUN_1( Fn_consp, form)
{
  return (is_cons(form) ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_1( Fn_car, x)
{

  while (TRUE) {
    if (is_cons(x)) return (x->CONS).car;
				/* Illegal car; needs to act on signals */
				/* Until that is fixed just stop        */
    x = CallError(stacktop,"car: not a list",x,CONTINUABLE);
  }

  return(nil); /* dummy */
}
EUFUN_CLOSE
 
EUFUN_2( car_updator,  x, y)
{
  while (!is_cons(x))
    x = CallError(stacktop,"car_updator: attempt to rplaca into atom", x,
		  CONTINUABLE);
  (x->CONS).car = y;
  return y;
}
EUFUN_CLOSE

EUFUN_1( Fn_cdr, x)
{

  while (TRUE) {
    if (is_cons(x)) return (x->CONS).cdr;
				/* Illegal car; needs to act on signals */
				/* Until that is fixed just stop        */
    x = CallError(stacktop,"cdr: not a list",x,CONTINUABLE);
  }

  return(nil); /* dummy */
}
EUFUN_CLOSE
 
EUFUN_2( cdr_updator,  x, y)
{
  while (!is_cons(x))
    x = CallError(stacktop,"cdr_updator: attempt to rplacd into atom", x,
		  CONTINUABLE);
  (x->CONS).cdr = y;
  return y;
}
EUFUN_CLOSE

				/* Length of a list; does not check */
EUFUN_1( Fn_length, form)
{
  int i = 0;

  while (is_cons(form)) {
    i++;
    form = CDR(form);
  }
  return allocate_integer(stacktop,i);
}
EUFUN_CLOSE

EUFUN_1( Fn_list, ll)
{
  /* Say, wow!! Declaring this n-ary gives us it for free... */

  return(ll);
}
EUFUN_CLOSE

/*     
  * Other list functions
  *
 */
LispObject flat_list_copy(LispObject *);

EUFUN_1( Fn_null, form)
{
  return (form==nil?lisptrue:nil);
}
EUFUN_CLOSE

				/* Destructive append */
EUFUN_2( Fn_nconc,  form1, form2)
{
  LispObject p = form1;
  if (!is_cons(form1)) return(form2);
  while (CDR(p)!=nil) p = CDR(p);
  CDR(p) = form2;
  return form1;
}
EUFUN_CLOSE

EUFUN_2( Fn_append,  l1, l2)
{
  LispObject endptr,walker,val;

  if (!is_cons(l1)) return(l2);

  /* reasonable append */
  
  val = EUCALL_2(Fn_cons,CAR(l1),nil);
  STACK_TMP(val);
  endptr = val;
  walker = CDR(ARG_0(stackbase)/*l1*/);
  while (is_cons(walker))
    {
      LispObject xx;
      STACK_TMP(endptr);
      STACK_TMP(CDR(walker));
      xx = EUCALL_2(Fn_cons, CAR(walker), nil);
      UNSTACK_TMP(walker);
      UNSTACK_TMP(endptr);
      CDR(endptr)=xx;
      endptr=CDR(endptr);
    }
  CDR(endptr) = ARG_1(stackbase)/*l2*/;
  UNSTACK_TMP(val);
  return(val);
}
EUFUN_CLOSE

				/* Simple predicate for NULL */
EUFUN_1( Fn_lastpair, form)
{
  while (!is_cons(form))
    form = CallError(stacktop,"Not a list in last-pair",form,CONTINUABLE);
  while (is_cons(form) && CDR(form)!=nil)
    form = CDR(form);
  return form;
}
EUFUN_CLOSE

EUFUN_1( Fn_nreverse, form)
{
  LispObject x=nil;
  while (form!=nil) {
    LispObject y = CDR(form);
    CDR(form) = x;
    x = form;
    form = y;
  }
  return x;
}
EUFUN_CLOSE

EUFUN_3( Fn_assoc, obj, list, fn)
{
  while (list!=nil) {
    LispObject xx;
    EUCALLSET_3(xx,apply2,ARG_2(stackbase),ARG_0(stackbase),CAR(CAR(list)));
    if (xx != nil)  {
      list=ARG_1(stackbase);
      return CAR(list);
    }
    list = ARG_1(stackbase);
    list = CDR(list);
    ARG_1(stackbase) = list;
  }
  return nil;
}
EUFUN_CLOSE

EUFUN_3( Fn_member, obj, list, fn)
{
  while (list!=nil) {
    if (EUCALL_3(apply2,ARG_2(stackbase),ARG_0(stackbase),CAR(list)) != nil) {
      return ARG_1(stackbase);
    }
    list = ARG_1(stackbase);
    list = CDR(list);
    ARG_1(stackbase) = list;
  }
  return nil;
}
EUFUN_CLOSE

EUFUN_2( Fn_memq,  obj, list)
{
  if (!is_cons(list) && list != nil)
    CallError(stacktop,"memq: non-lists passed",list,NONCONTINUABLE);

  while (is_cons(list)) {
    if (obj == CAR(list))
      return(lisptrue);
    else
      list = CDR(list);
  }
  
  return(nil);
}
EUFUN_CLOSE

/* ******************************************************************** */
/*                            Lisp Mappers                              */
/* ******************************************************************** */

static LispObject mapcar_apply_args(LispObject *stackbase, LispObject set)
{
  LispObject walker,res,ptr;
  LispObject *stacktop=stackbase+1;

  ARG_0(stackbase)=nil;
  res = nil; ptr = nil;

  walker = set;
  while (is_cons(walker)) 
    {
      if (!is_cons(CAR(walker))) 
	return(nil);

      STACK_TMP(CDR(walker));
      if (ptr == nil)
	{
	  EUCALLSET_2(res, Fn_cons,CAR(CAR(walker)),nil);
	  ARG_0(stackbase)=res;
	  ptr = res;
	}
      else
	{
	  LispObject xx;
	  STACK_TMP(ptr);
	  EUCALLSET_2(xx, Fn_cons, CAR(CAR(walker)),nil);
	  UNSTACK_TMP(ptr);
	  CDR(ptr) = xx;
	  ptr = CDR(ptr);
	}
      UNSTACK_TMP(walker);
    }
  res=ARG_0(stackbase);
  return(res);
}

static LispObject mapcar_advance_lists(LispObject set)
{ 
  LispObject walker = set;

  while (is_cons(walker)) {
    CAR(walker) = CDR(CAR(walker));
    walker = CDR(walker);
  }
  
  return(set);
}

EUFUN_3( Fn_mapcar, fn, l1, lists)
{
  LispObject flat_list_copy(LispObject *);
  
  if (!is_cons(l1) && l1 != nil)
    CallError(stacktop,"mapcar: not a list",l1,NONCONTINUABLE);

  ARG_3(stackbase)=nil;
  stacktop++;

  {
    LispObject set,args;
    LispObject res,ptr,val;
    
    /* More general... */

    EUCALLSET_1(set, flat_list_copy, lists);
    EUCALLSET_2(set, Fn_cons,ARG_1(stackbase),set);

    res = nil; ptr = nil;
      
    while (TRUE) 
      {

	/* Construct args to apply... */
      
	STACK_TMP(set);	
	STACK_TMP(ptr);
	if ((args = mapcar_apply_args(stacktop,set)) == nil) 
	  {	
	    res=ARG_3(stackbase);
	    return(res);
	  }
	UNSTACK_TMP(ptr);
	STACK_TMP(ptr);
	EUCALLSET_2(val,module_mv_apply_1,ARG_0(stackbase),args);
	UNSTACK_TMP(ptr);
      
	if (ptr == nil)
	  {
	    EUCALLSET_2(res, Fn_cons,val,nil);
	    ARG_3(stackbase)=res;
	    ptr = res;
	  }
	else 
	  {
	    LispObject xx;
	    STACK_TMP(ptr);
	    EUCALLSET_2(xx, Fn_cons, val,nil);
	    UNSTACK_TMP(ptr);
	    CDR(ptr) = xx;
	    ptr = CDR(ptr);
	  }
	UNSTACK_TMP(set);
	mapcar_advance_lists(set);
      }
  }

  return(nil);
}
EUFUN_CLOSE

EUFUN_3( Fn_mapc, fn, l1, lists)
{

  if (!is_cons(l1) && l1 != nil)
    CallError(stacktop,"mapc: not a list",l1,NONCONTINUABLE);

  if (FALSE) {
    ;
  }
  else {
    LispObject set,args;
    
    /* More general... */

    EUCALLSET_1(set,flat_list_copy,lists);
    EUCALLSET_2(set, Fn_cons,ARG_1(stackbase),set);

    while (TRUE) {
      LispObject dummy;

      /* Construct args to apply... */

      STACK_TMP(set);
      if ((args = mapcar_apply_args(stacktop,set)) == nil) {
	return(nil);
      }
      UNSTACK_TMP(set);

      STACK_TMP(set);
      EUCALL_2(module_mv_apply_1,ARG_0(stackbase),args);
      UNSTACK_TMP(set);
      mapcar_advance_lists(set);
    }
  }

  return(nil);
}
EUFUN_CLOSE

EUFUN_1( flat_list_copy, list)
{
  LispObject xx;
  if (!is_cons(list)) return(nil);
  EUCALLSET_1(xx, flat_list_copy, CDR(list));
  return(EUCALL_2(Fn_cons, CAR(ARG_0(stackbase)),xx));
}
EUFUN_CLOSE
  

EUFUN_1( Fn_atom, form)
{
  return (is_cons(form) ? nil : lisptrue);
}
EUFUN_CLOSE

EUFUN_1( Fn_consn, n)
{
  int i;
  LispObject l = nil;

  for (i = intval(n); i > 0; --i) {
    ARG_1(stacktop) = l;
    ARG_0(stacktop) = nil;
    l = Fn_cons(stacktop);
  }

  return(l);
}
EUFUN_CLOSE


/*
 * Module initialisation...
 */

#define LISTS_ENTRIES 21
MODULE Module_lists;
LispObject Module_lists_values[LISTS_ENTRIES];

void initialise_lists(LispObject* stacktop)
{
  extern LispObject generic_generic_convert;
  LispObject get,set;

  open_module(stacktop,
	      &Module_lists,
	      Module_lists_values,
	      "lists",
	      LISTS_ENTRIES);

  (void) make_module_function(stacktop,"consp",Fn_consp,1);
  (void) make_module_function(stacktop,"cons",Fn_cons,2); /* In allocate.c */
  
  get = make_module_function(stacktop,"car",Fn_car,1);
  STACK_TMP(get);
  set = make_unexported_module_function(stacktop,"car-updator",car_updator,2);
  UNSTACK_TMP(get);
  set_anon_associate(stacktop,get,set);

  get = make_module_function(stacktop,"cdr",Fn_cdr,1);
  STACK_TMP(get);
  set = make_unexported_module_function(stacktop,"cdr-updator",cdr_updator,2);
  UNSTACK_TMP(get);
  set_anon_associate(stacktop,get,set);

  (void) make_module_function(stacktop,"list-length",Fn_length,1);
  (void) make_module_function(stacktop,"list",Fn_list,-1);

  (void) make_module_function(stacktop,"memq",Fn_memq,2);
  (void) make_module_function(stacktop,"append",Fn_append,2);
  (void) make_module_function(stacktop,"copy-list",flat_list_copy,1);
  (void) make_module_function(stacktop,"null",Fn_null,1);
  (void) make_module_function(stacktop,"nconc",Fn_nconc,2);
  (void) make_module_function(stacktop,"last-pair",Fn_lastpair,1);
  (void) make_module_function(stacktop,"nreverse",Fn_nreverse,1);
  (void) make_module_function(stacktop,"assoc",Fn_assoc,3);
  (void) make_module_function(stacktop,"member-list",Fn_member,3);
  (void) make_module_function(stacktop,"mapcar",Fn_mapcar,-3);
  (void) make_module_function(stacktop,"mapc",Fn_mapc,-3);
  (void) make_module_function(stacktop,"atom",Fn_atom,1);
  (void) make_module_function(stacktop,"consn", Fn_consn, 1);
  close_module();
}
