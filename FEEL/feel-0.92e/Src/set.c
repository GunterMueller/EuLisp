/* ******************************************************************** */
/*  set.c            Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/*  support for "set"                                                   */
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, May 1989
 * 
 *   Had to add a new function to get it to work on anoymous functions
 *                                                      (16/11/89) KJP
 *   Mostly killed. Pab (11/11/92)
 */

#include "defs.h"
#include "structs.h"
#include "funcalls.h"

#include "error.h"
#include "global.h"
#include "class.h"
#include "ngenerics.h"

/* associate the updator with the function func: both are ids */

/* as above for function objects */

void set_anon_associate(LispObject *stacktop, LispObject get,LispObject set)
{
  if (is_c_function(get))
    get->C_FUNCTION.setter=set;
  else
    {
      if (is_generic(get))
	generic_setter(get)=set;
      else 
	CallError(stacktop,"Daft setter",nil,NONCONTINUABLE);
    }
}	

static EUFUN_1(Fn_c_setter,x)
{
  return(x->C_FUNCTION.setter);
}
EUFUN_CLOSE

static EUFUN_2(Fn_c_setter_setter,x,y)
{
  return (x->C_FUNCTION.setter=y);
}
EUFUN_CLOSE

void initialise_set(LispObject *stacktop)
{
  make_module_function(stacktop,"c-setter",Fn_c_setter,1);
  make_module_function(stacktop,"c-setter-setter",Fn_c_setter_setter,2);
}
