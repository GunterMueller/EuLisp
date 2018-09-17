/* ******************************************************************** */
/*  vector.c         Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/*  Wild thing                                                          */
/* ******************************************************************** */

/*
 * $Id: vectors.c,v 1.1 1994/01/25 13:45:08 djb Exp $
 *
 * $Log: vectors.c,v $
 * Revision 1.1  1994/01/25  13:45:08  djb
 * Initial revision
 *
 * Revision 1.6  1992/11/26  16:13:51  pab
 * Correct type check
 *
 * Revision 1.5  1992/06/16  19:31:54  pab
 * added primitive operations
 *
 * Revision 1.4  1992/01/09  22:29:12  pab
 * Fixed for low tag ints
 *
 * Revision 1.3  1991/12/22  15:14:46  pab
 * Xmas revision
 *
 * Revision 1.2  1991/09/11  12:07:52  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:50:13  pab
 * Initial revision
 *
 * Revision 1.3  1991/02/13  18:27:11  kjp
 * Pass.
 *
 */

#define KJPDBG(x) 

/*
 * Change Log:
 *   Version 1, October 1989
 *   Hacked everything - not robust ( even slightly ) (24/10/89) KJP
 *   Properly + GC protect (hopefully) 
 *
 *   Garbage checked - OK.
 */

#include <stdio.h>
#include "funcalls.h"
#include "defs.h"
#include "structs.h"
#include "global.h"
#include "error.h"
#include "bootstrap.h"

/* Modulise: allocation */

#include "modboot.h"

#define VECTORS_ENTRIES 11

MODULE     Module_vectors;
LispObject Module_vectors_values[VECTORS_ENTRIES];

static LispObject maximum_vector_index;

/* End Modulise: allocation*/


EUFUN_1( Fn_vectorp, obj)
{
  return(is_vector(obj) ? lisptrue : nil);
}
EUFUN_CLOSE

EUFUN_2( Fn_make_vector, n, obj)
{
  LispObject vector;
  int i;

  while (!is_fixnum(n)) 
    n = CallError(stacktop,
		  "Non-integer vector length in 'make-vector'",n,CONTINUABLE);

  if (intval(n) < 0)
    CallError(stacktop,
	      "Non-positive vector length in 'make-vector'",n,NONCONTINUABLE);

/*
  if (intval(n) == 0) return(nil);
*/

  if (intval(n) > intval(maximum_vector_index))
    CallError(stacktop,
	      "Vector length in 'make-vector' too large",n,NONCONTINUABLE);

  /* For the moment using object as an initialisation argument */

  vector = (LispObject) allocate_vector(stacktop,intval(n));

  obj = ARG_1(stackbase);
  for (i = 0; i < intval(n); ++i) vrefupdate(vector,i,obj);

  return(vector);
}
EUFUN_CLOSE

EUFUN_2( Fn_make_vector_optional, n, args)
{
  return(EUCALL_2(Fn_make_vector,n,(args == nil ? nil : CAR(args))));
}
EUFUN_CLOSE

EUFUN_1( Fn_vector_length, vect)
{
  LispObject len;

  while (!is_vector(vect))
    vect = CallError(stacktop,
		     "Non-vector in 'vector-length'",vect,CONTINUABLE);

  len = (LispObject) allocate_integer(stacktop,vect->VECTOR.length);
  
  return(len);
}
EUFUN_CLOSE

EUFUN_2( Fn_vector_ref, vect, n)
{
  while (!is_vector(vect))
    vect = CallError(stacktop,
		     "Non-vector in 'vector-ref'", vect, CONTINUABLE);

  while (!is_fixnum(n))
    vect = CallError(stacktop,
		     "Non-integer in 'vector-ref'",
		     ARG_1(stackbase), CONTINUABLE );

  n = ARG_1(stackbase);
  if (intval(n) < 0 || intval(n) >= vect->VECTOR.length)
    CallError(stacktop,"Index out of range in 'vector-ref'",n,NONCONTINUABLE);
  
  return(vref(vect,intval(n)));
}
EUFUN_CLOSE

EUFUN_3( Fn_vector_ref_updator, vect, n, obj)
{
  while (!is_vector(vect))
    vect = CallError(stacktop,
		     "Non-vector in 'vector-ref-updator'", vect, CONTINUABLE);

  while (!is_fixnum(n))
    vect = CallError(stacktop,
		     "Non-integer in 'vector-ref-updator'",
		     ARG_1(stackbase), CONTINUABLE );

  n = ARG_1(stackbase);
  if (intval(n) < 0 || intval(n) >= vect->VECTOR.length)
    CallError(stacktop,
	      "Index out of range in 'vector-ref-updator'",n,NONCONTINUABLE);

  vect = ARG_0(stackbase);
  obj = ARG_2(stackbase);
  vrefupdate(vect,intval(n),obj);

  return(obj);
}
EUFUN_CLOSE

EUFUN_1( Fn_vector, forms)
{
  LispObject vect;
  int i, vlen;

/*
  if (forms == nil)
    CallError("Can't make zero length vector in 'vector'",nil,NONCONTINUABLE);
*/

  EUCALLSET_1(vect, Fn_length, forms);
  vlen = intval(vect);
  vect = (LispObject) allocate_vector(stacktop,vlen);

  forms = ARG_0(stackbase);
  for (i = 0; i < vlen; ++i) {
    vrefupdate(vect,i,CAR(forms));
    forms = CDR( forms );
  }

  return(vect);
}
EUFUN_CLOSE

/* This should just be a method to 'convert' when it exists */

EUFUN_1( Fn_convert_vector_list, vect )
{
  LispObject newlist;
  int i;

  while (!is_vector(vect)) {
    vect = CallError(stacktop,
		     "Non-vector in vector coercion", vect, CONTINUABLE );
  }

  newlist = nil;
  for ( i = vect->VECTOR.length; i > 0; --i ) {
    ARG_0(stackbase) = vect;
    EUCALLSET_2(newlist, Fn_cons, vref( vect, i-1 ), newlist );
    vect = ARG_0(stackbase);
  }

  return( newlist );
}
EUFUN_CLOSE

EUFUN_2(Fn_make_primitive_object, class, size)
{
  LispObject tmp;

  tmp=allocate_vector(stacktop,intval(size));
  lval_classof(tmp)=class;
  
  return tmp;

}
EUFUN_CLOSE

EUFUN_2(Fn_primitive_ref, o, n)
{
  return vref(o,intval(n));
}
EUFUN_CLOSE

EUFUN_3(Fn_primitive_ref_setter, o, n ,v)
{
  vref(o,intval(n))=v;
  
  return v;

}
EUFUN_CLOSE


void initialise_vectors(LispObject* stacktop)
{
  LispObject getter, setter;

  /* Modulise: initialisation */

  open_module(stacktop,
	      &Module_vectors,Module_vectors_values,"vectors",VECTORS_ENTRIES);

  (void) make_module_function(stacktop,"vectorp",Fn_vectorp,1);
  (void) make_module_function(stacktop,
			      "make-vector",Fn_make_vector_optional,-2);
  (void) make_module_function(stacktop,"vector-length",Fn_vector_length,1);
  getter = make_module_function(stacktop,"vector-ref",Fn_vector_ref,2);
  STACK_TMP(getter);
  setter = make_module_function(stacktop,
				"vector-ref-updator",Fn_vector_ref_updator,3);
  UNSTACK_TMP(getter);
  set_anon_associate(stacktop,getter,setter);
  (void) make_module_function(stacktop,"make-initialized-vector",Fn_vector,-1);
  (void) make_module_function(stacktop,
			      "convert-vector-list",Fn_convert_vector_list,1);
  maximum_vector_index = allocate_integer(stacktop,0xfffff);
  add_root(&maximum_vector_index);

  (void) make_module_entry(stacktop,"maximum-vector-index",maximum_vector_index);

  (void) make_module_function(stacktop,"make-primitive-object",Fn_make_primitive_object,3);
  (void) make_module_function(stacktop,"primitive-ref",Fn_primitive_ref,2);
  (void) make_module_function(stacktop,"set-primitive-ref",Fn_primitive_ref_setter,3);
  close_module();
}
