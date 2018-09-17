/* $Id: bvecs2.c,v 1.1 1994/01/25 13:45:08 djb Exp $
 * 
 * $Log: bvecs2.c,v $
 * Revision 1.1  1994/01/25  13:45:08  djb
 * Initial revision
 *
 * Revision 1.1  1994/01/25  13:42:04  djb
 * Initial revision
 *
 * Revision 1.1  1994/01/25  13:29:33  djb
 * Initial revision
 *
 * Revision 1.1  1992/04/29  12:32:43  pab
 * Initial revision
 *
 * Revision 1.4  1992/01/09  22:28:42  pab
 * Fixed for low tag ints
 *
 * Revision 1.3  1991/12/22  15:13:49  pab
 * Xmas revision
 *
 * Revision 1.2  1991/09/11  12:07:00  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:49:26  pab
 * Initial revision
 *
 * Revision 1.4  1991/02/11  21:24:13  pab
 * tidied up...
 *
 * Revision 1.3  1991/02/04  17:33:39  kjp
 * classof() standardisation.
 *
 * Revision 1.2  1990/11/29  22:45:19  pab
 * Got vector arithmetic right. added integer->bit-vector
 * NB: vectors indexed from 0. always have been. Always will be.
 *
 *   (CB) rewritten 4/24/92
 *	Modified by pab
 */
/* ******************************************************************** */
/* bit-vectors.c     Copyright (C) Codemist and University of Bath 1990 */
/*                                                                      */
/* Just so			                                        */
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, September 1990
 *    28/11/90 added bit-vector->integer
 *  
 */

#include <stdio.h>
#include "funcalls.h"
#include "defs.h"
#include "structs.h"
#include "global.h"
#include "error.h"
#include "allocate.h"
#include "class.h"
#include "modboot.h"
#include "bootstrap.h"

static LispObject Bit_Vector;

#define BV_BUG(x) 

static EUFUN_1(Fn_make_bit_vector, lisplen)
{
  LispObject new;
  int bytes,len;

  if (!is_fixnum(lisplen))
    CallError(stacktop,"make-bit-vector: bad size",lisplen,NONCONTINUABLE);

  len = intval(lisplen);
  
  if (len <= 0)
    CallError(stacktop,"make-bit-vector: bad size",lisplen,NONCONTINUABLE);

  bytes = sizeof(int)+len/8 + 1;
#if 0
  str = (char *)feel_malloc(bytes + 1);
  str[bytes - 1] = '\0';
  str[0] = len;
  for (len = 1 ; len < bytes ; len++)
    str[len] = 0;
#endif
  new = allocate_string(stacktop, "", bytes);  
  *(int *)stringof(new)=len;
  BV_BUG(fprintf(stderr,"alloc: %x %d\n", new,bytes));
 return(new);
}
EUFUN_CLOSE

EUFUN_1( Fn_bit_vector_length, v)
{
  if (!is_string(v))
    CallError(stacktop,"bit-vector-length: bad bit vector",v,NONCONTINUABLE);

  return(allocate_integer(stacktop, *((int *) stringof(v))));
}
EUFUN_CLOSE
  
EUFUN_2( Fn_bit_vector_ref,  v, i)
{
  int index,byte,bit;
  int size;
  char *str;

  if (!is_string(v))
    CallError(stacktop,"bit-vector-ref: non bit-vector",v,NONCONTINUABLE);

  str = stringof(v);
  size = *((int *) &str[0]);

  if (!is_fixnum(i))
    CallError(stacktop,"bit-vector-ref: bad index",i,NONCONTINUABLE);

  index = intval(i);
  if (index < 0 || index >= size)
    CallError(stacktop,"bit-vector-ref: bad index",i,NONCONTINUABLE);

  byte = index/8;
  bit = index%8;
  str+=sizeof(int);

  if ((1 << bit) & str[byte])
    return(allocate_integer(stacktop,1));
  else
    return(allocate_integer(stacktop,0));
}
EUFUN_CLOSE

EUFUN_3( Fn_bit_vector_ref_setter, v, i, val)
{
  int index,byte,bit;
  int size,state;
  char *str;
  
  if (!is_string(v))
    CallError(stacktop,"bit-vector-ref: non bit-vector",v,NONCONTINUABLE);

  str = stringof(v);
  
  size = *((int *) &str[0]);

  if (!is_fixnum(i))
    CallError(stacktop,"bit-vector-ref: bad index",i,NONCONTINUABLE);

  index = intval(i);
  if (index < 0 || index >= size)
    CallError(stacktop,"bit-vector-ref: bad index",i,NONCONTINUABLE);

  if (!is_fixnum(val))
    CallError(stacktop,
	      "(setter bit-vector-ref): bad bit value",val,NONCONTINUABLE);

  if ((state = intval(val)) != 0 && state != 1)
    CallError(stacktop,
	      "(setter bit-vector-ref): bad bit value",val,NONCONTINUABLE);

  byte = index/8;
  bit = index%8;

  if (state == 1)
    str[byte+sizeof(int)] |= (char) (1 << bit);
  else
    str[byte + sizeof(int)]  &= (char) ~(1 << bit);    

  return(v);
}
EUFUN_CLOSE

static EUFUN_1(Fn_convert_int,num)
{	
  LispObject new;
  unsigned char *ptr,*dest_ptr;
  int tmp,i;
  
  tmp=intval(num);
  ptr=(unsigned char *)&tmp;
  
  new=allocate_string(stacktop,"",sizeof(int));
  dest_ptr=(unsigned char *)stringof(new);
  /* Hmm, let's assume that this is big-endian */
#if 1  
  for (i=0; i < sizeof(int) ; i++)
    {
      dest_ptr[i] = ptr[sizeof(int) - (i+1)];
    }
#else
  for (i=0; i < sizeof(int) ; i++)
    v_ptr[i] = v_buf[i];
#endif

}
EUFUN_CLOSE


#define BIT_VECTORS_ENTRIES (5)
MODULE Module_bit_vectors;
LispObject Module_bit_vectors_values[BIT_VECTORS_ENTRIES];

void initialise_bit_vectors(LispObject *stacktop)
{
  extern void set_anon_associate(LispObject *,LispObject,LispObject);
  LispObject get,set;

  open_module(stacktop,&Module_bit_vectors,Module_bit_vectors_values,
	      "bit-vectors",BIT_VECTORS_ENTRIES);

  (void) make_module_function(stacktop,"primitive-make-bit-vector",
			      Fn_make_bit_vector,1);
  (void) make_module_function(stacktop,"integer-to-bitvector",
			      Fn_make_bit_vector,1);
  (void) make_module_function(stacktop,
			      "bit-vector-length",Fn_bit_vector_length,1);
  get = make_module_function(stacktop,"primitive-bit-vector-ref",Fn_bit_vector_ref,2);
  STACK_TMP(get);
  set = make_unexported_module_function(stacktop,"primitive-bit-vector-ref-setter",
					Fn_bit_vector_ref_setter,3);
  UNSTACK_TMP(get);
  set_anon_associate(stacktop,get,set);

  close_module();
}

			 
