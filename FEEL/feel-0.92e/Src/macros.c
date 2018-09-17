/* ******************************************************************** */
/* macros.c          Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Misc compiled macros                                                 */
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, May 1990 
 */

#include "defs.h"
#include "structs.h"
#include "error.h"
#include "funcalls.h"

#include "global.h"
#include "slots.h"
#include "garbage.h"


#include "symboot.h"
#include "modules.h"
#include "toplevel.h"
#include "root.h"
#include "allocate.h"
#include "specials.h"

#include "modboot.h"

extern LispObject sym_unquote_splicing,sym_unquote,sym_cons;
static LispObject sym_append;
LispObject unquote_constructor(LispObject *stacktop, LispObject x)
{
  LispObject retval;

  /* Atoms... */

  if (!is_cons(x)) {
    if (x == nil || !is_symbol(x)) {
      return(x);
    }
    else {
      EUCALLSET_2(x,Fn_cons,x,nil);
      return(EUCALL_2(Fn_cons,sym_quote,x));
    }
  }

  if (CAR(x) == sym_unquote) return((CAR(CDR(x))));

  if (CAR(x) == sym_unquote_splicing)
    CallError(stacktop,"`: illegal ,@ use",x,NONCONTINUABLE);

  STACK_TMP(x);
  if (CAR(CAR(x)) == sym_unquote_splicing) {
    LispObject xx;
    xx = unquote_constructor(stacktop,CDR(x));
    EUCALLSET_2(xx, Fn_cons, xx,nil);
    UNSTACK_TMP(x);
    EUCALLSET_2(xx, Fn_cons, CAR(CDR(CAR(x))), xx);
    return (EUCALL_2(Fn_cons, sym_append, xx));
  }
  
  retval = unquote_constructor(stacktop,CDR(x));
  EUCALLSET_2(retval, Fn_cons, retval,nil);
  UNSTACK_TMP(x);
  STACK_TMP(retval);
  x = unquote_constructor(stacktop,CAR(x));
  UNSTACK_TMP(retval);
  EUCALLSET_2(retval, Fn_cons, x,retval);
  return (EUCALL_2(Fn_cons, sym_cons, retval));

}

EUFUN_1( Mo_quasiquote, forms)
{
  return(unquote_constructor(stacktop,forms));
}
EUFUN_CLOSE

void initialise_macros(LispObject *stacktop)
{

  make_module_macro(stacktop,"quasiquote",Mo_quasiquote,1);
  
  sym_append=get_symbol(stacktop,"append");	
  add_root(&sym_append);
}
