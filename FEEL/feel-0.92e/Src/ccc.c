/* ******************************************************************** */
/* ccc.c             Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Comparing, copying and conversion.                                   */
/* ******************************************************************** */

/*
 * $Id: ccc.c,v 1.1 1994/01/25 13:45:08 djb Exp $
 *
 * $Log: ccc.c,v $
 * Revision 1.1  1994/01/25  13:45:08  djb
 * Initial revision
 *
 * Revision 1.1  1994/01/25  13:29:33  djb
 * Initial revision
 *
 * Revision 2.1  93/01/17  17:25:21  pab
 * 17 Jan 1993 The next generation...
 * 
 * Revision 1.10  1992/11/25  17:06:02  pab
 * Removed some obsolete functions
 *
 * Revision 1.9  1992/05/19  11:15:24  pab
 * fixed equal
 *
 * Revision 1.8  1992/02/27  15:49:10  pab
 * lose type_condition
 *
 * Revision 1.7  1992/01/21  22:38:31  pab
 * Fixed equal on structs
 *
 * Revision 1.6  1992/01/17  22:25:49  pab
 * Added conversion+copy methods
 *
 * Revision 1.5  1992/01/09  22:28:44  pab
 * Fixed for low tag ints
 *
 * Revision 1.4  1991/12/22  15:13:53  pab
 * Xmas revision
 *
 * Revision 1.3  1991/11/15  13:44:25  pab
 * copyalloc rev 0.01
 *
 * Revision 1.2  1991/09/11  12:07:03  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:49:29  pab
 * Initial revision
 *
 * Revision 1.4  1991/02/14  10:07:28  kjp
 * Added an eq lisp function handle for table optimisation.
 *
 * Revision 1.3  1991/02/14  05:59:24  kjp
 * Fixed Fn_equal in the symbol case.
 *
 */

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

#include "calls.h"
#include "modboot.h"
#include "ngenerics.h"

LispObject function_eq;

EUFUN_2( Fn_eq, x, y)
{
  if (x == y) 
    return(lisptrue);
  else
    return(nil);
}
EUFUN_CLOSE

/* Non-generic, hacked equal */

LispObject equal_lookup_table;

EUFUN_2( Fn_equal, x, y)
{
  while (TRUE) {
    if (x == y) return lisptrue;
    if (typeof(x) != typeof(y)) return nil;
    switch (typeof(x)) {
    case TYPE_CONS:
      if (EUCALL_2(Fn_equal, CAR(x), CAR(y))) {
	ARG_0(stackbase) = x = CDR(ARG_0(stackbase));
	ARG_1(stackbase) = y = CDR(ARG_1(stackbase));
	continue;
      }
      else return nil;
    case TYPE_CHAR:
      if ((((x->CHAR).code) == ((y->CHAR).code)) &&
	  (((x->CHAR).font) == ((y->CHAR).font))) return lisptrue;
      else return nil;
    case TYPE_STRING:
      if (strcmp(stringof(x),stringof(y)) == 0) return lisptrue;
      else return nil;
    case TYPE_SYMBOL:
      return nil;
    case TYPE_THREAD:
    case TYPE_STREAM:
      CallError(stacktop,"Unimplemented facility in equal",nil,NONCONTINUABLE);
    case TYPE_INT:
      if (intval(x) == intval(y)) return lisptrue;
      else return nil;
    case TYPE_FLOAT:
      if ((x->FLOAT).fvalue == (y->FLOAT).fvalue) return lisptrue;
      else return nil;

    default:
      {
#ifdef veryverytacky /* Thu Sep 24 17:16:35 1992 */
/**/
/**/	LispObject foo = allocate_integer(stacktop,(int) typeof(x));
/**/	LispObject ans;
/**/	EUCALLSET_2(ans, Fn_table_ref, equal_lookup_table, foo);
/**/	x = ARG_0(stackbase); y = ARG_1(stackbase);
/**/	if (null(ans))
#endif /* veryverytacky Thu Sep 24 17:16:35 1992 */
  (void) CallError(stacktop,
		   "equal: No method for ~a", x, NONCONTINUABLE);
	return nil; /* not reached */
      }
    }
  }

  return(nil); /* dummy */

}
EUFUN_CLOSE

/* Non-generic hacked copy */

EUFUN_1( Fn_copy, form)
{
  switch (typeof(form)) 
    {
    case TYPE_NULL:
      return(nil);
    case TYPE_INT:
      return allocate_integer(stacktop,intval(form));
    case TYPE_SYMBOL:
      return form;

    case TYPE_CONS:
      {
	LispObject xx, yy;
	EUCALLSET_1(xx, Fn_copy, CAR(form));
	EUCALLSET_1(yy, Fn_copy, CDR(ARG_0(stackbase)));
	return EUCALL_2(Fn_cons,xx, yy);
      }
    default:
      (void) CallError(stacktop,
		       "copy: No method for ~a", form, NONCONTINUABLE);
    }

  return(nil); /* dummy */

}
EUFUN_CLOSE

/* ******************************************************************** */
/*                          Generic Copying                             */
/* ******************************************************************** */

EUFUN_1( Md_copy_Object, obj)
{
  return(Fn_copy(stackbase));
}
EUFUN_CLOSE

#ifndef NO_COMPACT
#define myvref(v,i) vref(v,i)
#else
#define vrefupdate(v,i,obj) (*(&(v->VECTOR.base)+i)=obj)
#define myvref(v,i) (*(&(v->VECTOR.base)+i))
#endif

EUFUN_1( Md_copy_Vector, v)
{
  LispObject new;
  int i;

  new = (LispObject) allocate_vector(stacktop,v->VECTOR.length);
  v = ARG_0(stackbase);
  for (i=0; i<v->VECTOR.length; ++i) {
    vrefupdate(new,i,myvref(v,i));
  }

  return(new);
}
EUFUN_CLOSE

EUFUN_1( Md_copy_Structure, str)
{
  LispObject new;

 
#ifdef dunno /* Tue Jul 23 12:06:58 1991 */
/**/  STACK(str);
/**/  if (typeof(str) != TYPE_INSTANCE) return(Fn_copy(/*+:NULL:+*/str));
/**/  new = allocate_instance(classof(str));
/**/  STACK(new);
/**/  new->INSTANCE.slots = Gf_copy(str->INSTANCE.slots);
/**/  UNSTACK(2);
#endif /* dunno Tue Jul 23 12:06:58 1991 */
  
  return(str);
}
EUFUN_CLOSE

/* ******************************************************************** */
/*                          Generic Equality                            */
/* ******************************************************************** */
LispObject generic_equal;

EUFUN_2( Gf_equal, o1, o2)
{
  return(generic_apply_2(stacktop,generic_equal,o1,o2));
}
EUFUN_CLOSE

/* Basic methods... */

EUFUN_2( Md_equal_Object_Object, o1, o2)
{
  /* Same class? */

  if (classof(o1) != classof(o2)) return(nil);

  /* Same type? */

  if (typeof(o1) != typeof(o2)) return(nil);

  /* Instance? */

/**
  if (typeof(o1) == TYPE_INSTANCE) {
    if (Gf_equal(o1->INSTANCE.slots,o2->INSTANCE.slots) == nil) {
      UNSTACK(2);
      return(nil);
    }
    else {
      UNSTACK(2);
      return(lisptrue);
    }
  }
  **/

  return(Fn_equal(stackbase));
}
EUFUN_CLOSE

EUFUN_2( Md_equal_Pair_Pair, p1, p2)
{
  LispObject xx;
  if (p1 == p2) return(lisptrue);
  if (p1 == nil) return(nil);
  if (p2 == nil) return(nil);

  if (EUCALL_2(Gf_equal,CAR(p1),CAR(p2)) == nil)
    return nil;
  p1 = ARG_0(stackbase); p2 = ARG_1(stackbase);
  if (EUCALL_2(Gf_equal,CDR(p1),CDR(p2)) == nil)
    return(nil);
  else
    return(lisptrue);
}
EUFUN_CLOSE

EUFUN_2( Md_equal_Vector_Vector, v1, v2)
{
  int i;

  if (v1->VECTOR.length != v2->VECTOR.length) return(nil);

  for (i=0; i<v1->VECTOR.length; ++i) {
    if (EUCALL_2(Gf_equal,myvref(v1,i),myvref(v2,i)) == nil) return(nil);
    v1 = ARG_0(stackbase); v2 = ARG_1(stackbase);
  }
  
  return(lisptrue);
}
EUFUN_CLOSE

EUFUN_2( Md_equal_Structure_Structure, s1, s2)
{
  int i;
  LispObject res;

  if (classof(s1)==classof(s2)) 
    return  nil;
  
  for (i=0; i<intval(classof(s1)->CLASS.local_count) ; i++)
    {
      if (slotref(s1,i)!=slotref(s2,i))
	return nil;
      i++;
    }

  return lisptrue;
  
}
EUFUN_CLOSE

EUFUN_2( Md_equal_Class_Class, c1, c2)
{
  return((c1 == c2 ? lisptrue : nil));
}
EUFUN_CLOSE


/* ******************************************************************** */
/*                          Generic Conversion                          */
/* ******************************************************************** */

EUFUN_1( Md_generic_convert_Pair_Vector, l1)
{
  LispObject walker;
  LispObject new;
  int i;

  if (l1 == nil) return(nil);
  new = (LispObject)
          allocate_vector(stacktop,intval(EUCALL_1(Fn_length,l1)));

  l1 = ARG_0(stackbase);
  for (i=0,walker = l1; is_cons(walker); ++i,walker = CDR(walker)) 
    vrefupdate(new,i,CAR(walker));

  return(new);
}
EUFUN_CLOSE

EUFUN_1( Md_generic_convert_Vector_Pair, v1)
{
  extern LispObject Fn_convert_vector_list(LispObject*);
  
  return(Fn_convert_vector_list(stackbase));
}
EUFUN_CLOSE

#define CCC_ENTRIES 12
MODULE Module_ccc;
LispObject Module_ccc_values[CCC_ENTRIES];

void initialise_ccc(LispObject *stacktop)
{
  extern LispObject Basic_Structure;

  open_module(stacktop,
	      &Module_ccc,
	      Module_ccc_values,
	      "ccc",
	      CCC_ENTRIES);

  function_eq = make_module_function(stacktop,"eq",Fn_eq,2);
  add_root(&function_eq);

  EUCALLSET_1(equal_lookup_table, make_table,NULL);
  add_root(&equal_lookup_table);

  (void) make_module_function(stacktop,"generic_equal,Cons,Cons",
			      Md_equal_Pair_Pair,2
			      );
  (void) make_module_function(stacktop,"generic_equal,Object,Object",
			      Md_equal_Object_Object,2
			      );
  (void) make_module_function(stacktop,"generic_equal,Vector,Vector",
			      Md_equal_Vector_Vector,2
			      );
  (void) make_module_function(stacktop,"generic_equal,Basic_Structure,Basic_Structure",
			      Md_equal_Structure_Structure,2
			      );
  (void) make_module_function(stacktop,"generic_equal,Standard_Class,Standard_Class",
			      Md_equal_Class_Class,2
			      );

  generic_equal = make_wrapped_module_generic(stacktop,"equal",2,Gf_equal);
  add_root(&generic_equal);
  (void) make_module_function(stacktop,"generic_copy,Object",Md_copy_Object,1);
  (void) make_module_function(stacktop,"generic_copy,Vector",Md_copy_Vector,1);
  (void) make_module_function(stacktop,
			      "generic_copy,Basic_Structure",Md_copy_Structure,1);

  /* conversion methods */
  (void) make_module_function(stacktop,"generic_generic_convert,Cons,Vector",
			      Md_generic_convert_Pair_Vector,1
			      );
  (void) make_module_function(stacktop,"generic_generic_convert,Vector,Cons",
			      Md_generic_convert_Vector_Pair,1
			      );

  close_module();
}

