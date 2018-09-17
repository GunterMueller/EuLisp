/* ******************************************************************** */
/*  ngenerics.c      Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* New Generic function interface for feel                              */
/* ******************************************************************** */

/*
 * $Id: ngenerics.c,v 1.2 1994/02/08 11:31:46 djb Exp $
 *
 * $Log: ngenerics.c,v $
 * Revision 1.2  1994/02/08  11:31:46  djb
 * stack paranoia
 *
 */

 /*
   functions:
   
   generic_apply (gf, arglist)
   call_method(meth, sig, args)
   set_compute_function(lisp function)
   sundry accessors
   This approach has lots of advantages....

   if generic_apply fails, we call the function
   'compute_and_apply_method' which should 
   1) calculate method to apply
   2) stash the method in a cache
   3) call it via call_method
   
   */
/*
  Data structures:
  A table is a cons structure for accessing via a list	
  format: 

  fast cache: (last-method-call-sig result)
  slow cache: table of methods, keying (sig+methods)
              --- keep the sig as we don't want to recontruct it.
*/

#include "defs.h"
#include "structs.h"
#include "funcalls.h"

#include "global.h"
#include "error.h"
#include "allocate.h"
#include "ngenerics.h"
#include "bootstrap.h"
#include "class.h"
#include "bvf.h"
#include "modules.h"
#include "symboot.h"
#include "specials.h"
#include "modboot.h"
#include "calls.h"
#include "streams.h"

static LispObject sym_signature;
static LispObject sym_qualifiers;

static LispObject sym_lambda_list;
static LispObject sym_method_class;

static LispObject method_status_handle;
static LispObject method_args_handle;

static EUFUN_1( Fn_generic_function_p, obj)
{
  return((is_generic(obj) ? lisptrue : nil));
}
EUFUN_CLOSE

static EUFUN_1( Fn_methodp, obj)
{
  return((is_method(obj) ? lisptrue : nil));
}
EUFUN_CLOSE

/* Time waster functions */

LispObject generic_apply_4(LispObject *stacktop, LispObject gf,
			   LispObject a1, LispObject a2,
			   LispObject a3, LispObject a4)
{
  LispObject *stackbase=stacktop;
  STACK_TMP(a1); STACK_TMP(a2); STACK_TMP(a3); STACK_TMP(a4);
  
  return(generic_apply(stackbase,gf));
}

LispObject generic_apply_3(LispObject *stacktop,LispObject gf,
			   LispObject a1, LispObject a2, LispObject a3)
{
  LispObject *stackbase=stacktop;
  STACK_TMP(a1); STACK_TMP(a2); STACK_TMP(a3);
  return(generic_apply(stackbase,gf));
}

LispObject generic_apply_2(LispObject *stacktop,LispObject gf,LispObject a1, LispObject a2)
{
  LispObject *stackbase=stacktop;
  STACK_TMP(a1); STACK_TMP(a2); 
  return(generic_apply(stackbase,gf));
}

LispObject generic_apply_1(LispObject *stacktop, LispObject gf,
			   LispObject a1)
{
  LispObject *stackbase=stacktop;
  STACK_TMP(a1); 
  return(generic_apply(stackbase,gf));
}

#define TRACE_GCALLS
#ifdef TRACE_GCALLS
#define ON_TRACE(x) x
int fasthits=0;
int slowhits=0;
int gcalls=0;
#else
#define ON_TRACE(x)
#endif

LispObject generic_apply(LispObject *stackbase,LispObject gf)
{
  LispObject compute_and_apply_method(LispObject *);
  LispObject call_method(LispObject *,int,LispObject);
  LispObject *stacktop, *walker;
  LispObject ptr,args,fastcache,slowcache;
  int count, nargs,explicit,extras,depth;

  ON_TRACE(gcalls++);
  if (intval(generic_argtype(gf)) >= 0) {
    explicit = intval(generic_argtype(gf));
    extras = FALSE;
  }
  else {
    explicit = -intval(generic_argtype(gf))-1;
    extras = TRUE;
  }
  nargs=explicit+(extras ? 1 : 0);
  
  stacktop=stackbase+nargs;

  /* fast cache first */
  fastcache=(generic_fast_method_cache(gf));
  slowcache=(generic_slow_method_cache(gf));
  /* is there a cache ? */
  if (fastcache!=nil)
    {
      /** Method lookup **/
      ptr=CAR(fastcache); /* nb car(nil)==nil */
      walker=stackbase;
      
      while (ptr!=nil && CAR(ptr)==classof(*(walker)))
	{
	  walker++;
	  ptr=CDR(ptr);
	}
      if (ptr==nil)
	{ ON_TRACE(fasthits++);
	  return(call_method(stackbase,nargs,
			     CDR(fastcache)));
	}

      /* then the slow cache */

      ptr=slowcache;
      walker=stackbase;
      count=0;
      depth=intval(generic_discrimination_depth(gf));

      while(ptr!=nil && count<depth)
	{
	  if (CAR(CAR(ptr))==classof(*(walker)))
	    {			/* move down 1 */
	      ptr=CDR(CAR(ptr));
	      walker++;
	      count++;
	    }
	  else
	    ptr=CDR(ptr);
	}
      
      if (count==depth)
	{
	  generic_fast_method_cache(gf)=ptr;
	  ON_TRACE(slowhits++);
	  return(call_method(stackbase,nargs,CDR(ptr)));
	}
      /* not in slow cache */
    }

  STACK_TMP(gf);
  /** find Args **/
  args=allocate_n_conses(stacktop,nargs);
  ptr=args;

  walker=stackbase;
  count=0;
  while (count<nargs)
    {
      CAR(ptr)= *walker;
      ptr=CDR(ptr);
      ++walker;
      ++count;
    }
  UNSTACK_TMP(gf);

  return(EUCALL_2(compute_and_apply_method,gf, args));
  
}	

LispObject call_method(LispObject *stackbase, int nargs, LispObject ml)
{
  LispObject mf;

  if (!is_method(CAR(ml)))
    CallError(stackbase,"call-method: Not a method\n",nil,NONCONTINUABLE);
  
  mf = method_function(CAR(ml));

#ifdef BCI
  if (is_b_function(mf))
    return(apply_nary_bytefunction(stackbase,nargs,ml));
  
#ifdef WITH_SPECIAL_METHODS
  if (is_special_method(mf))
    return(apply_special_method(stackbase,nargs,mf));
#endif
#endif

  if (is_c_function(mf)) {
    return((mf->C_FUNCTION.func)(stackbase));
  }

  /* Should we check the arity of the function --- no add method should. */
  if (is_i_function(mf) || is_e_function(mf)) 
    { /* Should I make the env and apply here ? */
      LispObject *walker,*stacktop;
      LispObject args,ret,ptr;
      int count;
      
      stacktop=stackbase+nargs;

      STACK_TMP(mf);
      STACK_TMP(CDR(ml));

      /* one method list, one arg list */
      args=allocate_n_conses(stacktop,nargs+2); 
      UNSTACK_TMP(ml);  
      CAR(args)=ml;     /* Arg 1: arg list */
      ptr=CDR(args);
      CAR(ptr)=CDR(ptr);  /* Arg 2: Arguments */
      
      ptr=CDR(ptr);
      walker=stackbase;
      count=0;
      
      while (count<nargs)
	{
	  CAR(ptr)= *walker;
	  ptr=CDR(ptr);
	  ++walker;
	  ++count;
	}
      
      UNSTACK_TMP(mf);
      count=0;
      ptr=args;
      while (count<nargs+2)
	{
	  *(stackbase+count)=CAR(ptr);
	  ptr=CDR(ptr);	
	  count++;
	}

      stacktop=stackbase;
      ret=module_apply_args(stacktop,count,mf);
      return ret;
    }


  CallError(stackbase,
	    "call method: unknown method function class",mf,NONCONTINUABLE);

  return(nil);
}

/* repeat of last, but with args passed in a list this time... */
static EUFUN_2(call_method_by_list,ml , args)
{
  LispObject mf;

  if (!is_method(CAR(ml)))
    CallError(stacktop,"Not a method\n",nil,NONCONTINUABLE);


  mf = method_function(CAR(ml));

  if (is_i_function(mf) || is_e_function(mf)) {
    LispObject allargs,ret;

    STACK_TMP(mf);
    STACK_TMP(ml);
    EUCALLSET_2(allargs, Fn_cons,args,args);
    UNSTACK_TMP(ml);
    EUCALLSET_2(allargs, Fn_cons,CDR(ml),allargs);
    UNSTACK_TMP(mf);

    EUCALLSET_2(ret,module_mv_apply_1,mf,allargs);
    return ret;
  }

  if (is_c_function(mf)) 
    {
      LispObject ret;

      EUCALLSET_2(ret,module_mv_apply_1,mf,args);
      return ret;
    }

#ifdef BCI
  if (is_b_function(mf))
    {	
      LispObject *ptr=stackbase;
      int i=0;

      while (is_cons(args))
	{
	  *ptr=CAR(args);
	  args=CDR(args);
	  ptr++;
	  i++;
	}
      return(apply_nary_bytefunction(stackbase,i,ml));
    }

#ifdef WITH_SPECIAL_METHODS
  if (is_special_method(mf))
    {
      LispObject lst=args;
      LispObject *ptr=stackbase;
      int nargs=0;

      while (is_cons(lst))
	{
	  *ptr=CAR(lst);
	  lst=CDR(lst);
	  ptr++;
	  nargs++;
	}
      return (apply_special_method(stackbase,nargs,mf));
    }
#endif 
#endif /* BCI */
  CallError(stacktop,
            "call method: unknown method function class",mf,NONCONTINUABLE);

  return(nil);
}
EUFUN_CLOSE

/** accessors and dull stuff **/

static EUFUN_1(Fn_generic_slow_method_cache,gf)
{
  return generic_slow_method_cache(gf);
}
EUFUN_CLOSE

static EUFUN_1(Fn_generic_fast_method_cache,gf)
{
  return generic_fast_method_cache(gf);
}
EUFUN_CLOSE

static EUFUN_2(Fn_generic_slow_method_cache_setter,gf, value)
{
  return generic_slow_method_cache(gf)=value;
}
EUFUN_CLOSE

static EUFUN_2(Fn_generic_fast_method_cache_setter,gf, value)
{
  generic_fast_method_cache(gf)=value;
  return nil;
}
EUFUN_CLOSE

static EUFUN_1(Fn_generic_name,gf)
{
  if (!is_generic(gf))
    CallError(stacktop,"generic-method-name: Not a generic",gf,NONCONTINUABLE);

  return generic_name(gf);
}
EUFUN_CLOSE

static EUFUN_1(Fn_generic_method_class,gf)
{
  if (!is_generic(gf))
    CallError(stacktop,"generic-method-class: Not a generic",gf,NONCONTINUABLE);

  return generic_method_class(gf);
}
EUFUN_CLOSE

static EUFUN_1(Fn_generic_method_table,gf)
{
  if (!is_generic(gf))
    CallError(stacktop,"generic-method-table: Not a generic",gf,NONCONTINUABLE);

  return generic_method_table(gf);
}
EUFUN_CLOSE

static EUFUN_2(Fn_generic_method_table_setter,gf, value)
{
  return generic_method_table(gf)=value;
}
EUFUN_CLOSE

static EUFUN_1(Fn_generic_discriminator,gf)
{
  return generic_discriminator(gf);
}
EUFUN_CLOSE

static EUFUN_2(Fn_generic_discriminator_setter,gf, value)
{
  return generic_discriminator(gf)=value;
}
EUFUN_CLOSE

static EUFUN_1(Fn_generic_discrimination_depth,gf)
{
  return generic_discrimination_depth(gf);
}
EUFUN_CLOSE

static EUFUN_2(Fn_generic_discrimination_depth_setter,gf, value)
{
  return generic_discrimination_depth(gf)=value;
}
EUFUN_CLOSE


static EUFUN_1(Fn_generic_setter,gf)
{
  return generic_setter(gf);
}
EUFUN_CLOSE

static EUFUN_2(Fn_generic_setter_setter,gf, value)
{
  return generic_setter(gf)=value;
}
EUFUN_CLOSE

/* Method accessors */

static EUFUN_1(Fn_method_signature, meth)
{
  return method_signature(meth);
}
EUFUN_CLOSE

/***
  ** Callback definition... 
  **/

static LispObject Cb_compute_and_apply_method;

EUFUN_2(compute_and_apply_method, gf, args)
{
  LispObject xx;
  EUCALLSET_2(xx,Fn_cons,args,nil);
  EUCALLSET_2(xx,Fn_cons,ARG_0(stackbase),xx);
  
  stacktop=stackbase;
  return EUCALL_2(module_mv_apply_1,CAR(Cb_compute_and_apply_method),xx);
}
EUFUN_CLOSE

EUFUN_1(Fn_set_compute_fn,val)
{
  CAR(Cb_compute_and_apply_method)=val;
  return nil;
}
EUFUN_CLOSE

/***
  ** Initialising objects 
  **
 ***/
ON_TRACE(
	 EUFUN_0(print_hits)
	 {
	   char buf[256];
	   sprintf(buf,"Calls: %d fast: %d (%d%%) slow: %d (%d%%)",
		   gcalls,fasthits,(fasthits*100)/gcalls,slowhits,(slowhits*100)/gcalls);
	   print_string(stacktop,StdOut(),buf);
	   return nil;
	 }	
	 EUFUN_CLOSE
	 )

extern MODULE Module_generics;

/* Initialisation of the module */
#ifdef TRACE_GCALLS
#define GENERICS_ENTRIES 5
#else
#define GENERICS_ENTRIES 4
#endif

MODULE Module_generics;
LispObject Module_generics_values[GENERICS_ENTRIES];

void initialise_generics(LispObject *stacktop)
{
  Cb_compute_and_apply_method=EUCALL_2(Fn_cons,nil,nil);
  add_root(&Cb_compute_and_apply_method);

  method_args_handle = get_symbol(stacktop,"***method-args-handle***");
  add_root(&method_args_handle);
  method_status_handle = get_symbol(stacktop,"***method-status-handle***");
  add_root(&method_status_handle);

  sym_signature = get_symbol(stacktop,"signature");
  add_root(&sym_signature);
  sym_qualifiers = get_symbol(stacktop,"qualifiers");
  add_root(&sym_qualifiers);

  open_module(stacktop,
	      &Module_generics,
	      Module_generics_values,
	      "generics",
	      GENERICS_ENTRIES);

  (void) make_module_function(stacktop,"generic-function-p",Fn_generic_function_p,1);
  (void) make_module_function(stacktop,"methodp",Fn_methodp,1);

  (void) make_module_function(stacktop,"set-compute-and-apply-fn",Fn_set_compute_fn,1);
  (void) make_module_function(stacktop,"call-method-by-list",call_method_by_list,2);
  ON_TRACE((void) make_module_function(stacktop,"print-generic-hits",print_hits,0));

  close_module();
}



#if 0 /* GENERIC LOOKUP WITH 1st ARG SWITCHING --- case not proven */
      /* then the slow cache */

{      tmp=generic_slow_method_cache(gf);
      ptr=tmp;

      while(ptr!=nil && CAR(CAR(ptr))!=classof(*stackbase))
	ptr=CDR(ptr);
      
      if (ptr!=nil)
	{
	  LispObject tmp2;

	  tmp2=CAR(tmp);
	  CAR(tmp)=CAR(ptr);
	  CAR(ptr)=tmp2;
	  ptr=CDR(CAR(tmp));

	  walker=stackbase+1;
	  count=1;
	  while(ptr!=nil && count<explicit)
	    {
	      if (CAR(CAR(ptr))==classof(*(walker)))
		{		/* move down 1 */
		  ptr=CDR(CAR(ptr));
		  walker++;
		  count++;
		}
	      else
		ptr=CDR(ptr);
	    }
      
	  if (count==explicit)
	    {
	      generic_fast_method_cache(gf)=ptr;

	      return(call_method(stackbase,nargs,CDR(ptr)));
	    }
	} 
      /* not in slow cache */
    }
#endif
