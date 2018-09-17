/*
  *
  * New Number code
  */

/* History:
 * Created: 2/12/92
 */


#include "defs.h"
#include "structs.h"
#include "error.h"
#include "funcalls.h"
#include "modboot.h"
#include "global.h"
#include "ngenerics.h"

#include <math.h>
#include <limits.h>

/* half-hearted ANSI C */
#ifndef DBL_MAX
/* #include <values.h> */
#include <float.h>
/* #define DBL_MAX MAXDOUBLE */
/* #define DBL_MIN MINDOUBLE */
#endif

#define floatval(x) ((x)->FLOAT.fvalue)
#define integerval(x) intval(x)

/* generics for n-ary functions */
static LispObject generic_add;
static LispObject generic_subtract;
static LispObject generic_multiply;
static LispObject generic_divide;
static LispObject generic_lcm;
static LispObject generic_gcd;
static LispObject generic_lt;
static LispObject generic_negate;

/* Integer Operations */
EUFUN_2(Md_binary_add_Integer_Integer, a1,  a2)
{
  return (allocate_integer(stacktop,integerval(a1)+integerval(a2)));
}
EUFUN_CLOSE

EUFUN_2(Md_binary_subtract_Integer_Integer, a1,  a2)
{
  return (allocate_integer(stacktop,integerval(a1)-integerval(a2)));
}
EUFUN_CLOSE

EUFUN_2(Md_binary_multiply_Integer_Integer, a1,  a2)
{
  return (allocate_integer(stacktop,integerval(a1)*integerval(a2)));
}
EUFUN_CLOSE

EUFUN_2(Md_binary_divide_Integer_Integer, a1,  a2)
{
  if (integerval(a2))
    return (allocate_integer(stacktop,integerval(a1)/integerval(a2)));
  else
    CallError(stacktop,"division by zero",a1,NONCONTINUABLE);
}
EUFUN_CLOSE

EUFUN_1(Md_negate_Integer, a1)
{
  return (allocate_integer(stacktop,-integerval(a1)));
}
EUFUN_CLOSE

EUFUN_2(Md_binary_lcm_Integer_Integer, n1,  n2)
{
  extern int abs(int);
  int a,b,r,origa,origb;

  a = abs(intval(n1)); b = abs(intval(n2));
  origa = a; origb = b;
  do {
    r = a%b;
    a = b; b = r;
  } while(b != 0);

  a = (origa/a)*origb;
  return allocate_integer(stackbase, a);
}
EUFUN_CLOSE

EUFUN_2(Md_binary_gcd_Integer_Integer, n1,  n2)
{
  int a,b,r;
  LispObject ans;

  a = abs(intval(n1)); b = abs(intval(n2));

  if (b!=0)
  {
      do {
      
      r = a%b;
      a = b; b = r;
      
      } while(b != 0);
  }
  
  return (LispObject) allocate_integer(stackbase, a);
}
EUFUN_CLOSE

EUFUN_2(Md_binary_lt_Integer_Integer, a1,  a2)
{
  if (integerval(a1)<integerval(a2))
    return lisptrue;
  else
    return nil;
}
EUFUN_CLOSE

EUFUN_2(Md_binary_eqn_Integer_Integer, a1,  a2)
{
  if (integerval(a1)==integerval(a2))
    return lisptrue;
  else
    return nil;
}
EUFUN_CLOSE


/* Float Operations */
EUFUN_2(Md_binary_add_Float_Float, a1,  a2)
{
  return (allocate_float(stacktop,floatval(a1)+floatval(a2)));
}
EUFUN_CLOSE

EUFUN_2(Md_binary_subtract_Float_Float, a1,  a2)
{
  return (allocate_float(stacktop,floatval(a1)-floatval(a2)));
}
EUFUN_CLOSE

EUFUN_2(Md_binary_multiply_Float_Float, a1,  a2)
{
  return (allocate_float(stacktop,floatval(a1)*floatval(a2)));
}
EUFUN_CLOSE

EUFUN_2(Md_binary_divide_Float_Float, a1,  a2)
{
  return (allocate_float(stacktop,floatval(a1)/floatval(a2)));
}
EUFUN_CLOSE

EUFUN_2(Md_binary_lt_Float_Float, a1,  a2)
{
  if (floatval(a1)<floatval(a2))
    return lisptrue;
  else
    return nil;
}
EUFUN_CLOSE

EUFUN_2(Md_binary_eqn_Float_Float, a1,  a2)
{
  if (floatval(a1)==floatval(a2))
    return lisptrue;
  else
    return nil;
}
EUFUN_CLOSE

EUFUN_1(Md_negate_Float, a1)
{
  return (allocate_float(stacktop,-floatval(a1)));
}
EUFUN_CLOSE

/* Primitive operations */
/* Additional Ops */
#define acosh my_acosh
#define asinh my_asinh
#define atanh my_atanh


static double acosh(double x)
{
  return log(x+sqrt(x*x-1));
}

static double asinh(double x)
{
  return log(x+sqrt(x*x+1));
}

static double atanh(double x)
{
  return 0.5*(log((x+1.0)/(x-1.0)));
}

#ifdef __STDC__
#define PrimOp(op)			\
EUFUN_1(Md_## op ##_Float,x) 		\
{					\
  return allocate_float(stacktop,op(floatval(x)));\
}					\
EUFUN_CLOSE				\
/*Hack to allow semis */ extern LispObject nil
#else
#define PrimOp(op)			\
EUFUN_1(Md_/**/op/**/_Float,x)		\
{					\
  return allocate_float(stacktop,op(floatval(x)));\
}					\
EUFUN_CLOSE				\
/*Hack to allow semis */ extern LispObject nil
#endif

PrimOp(sin);
PrimOp(cos);
PrimOp(tan);
PrimOp(asin);
PrimOp(acos);
PrimOp(atan);
PrimOp(log);
PrimOp(log10);
PrimOp(sqrt);
PrimOp(exp);
PrimOp(sinh);
PrimOp(cosh);
PrimOp(tanh);
PrimOp(asinh);
PrimOp(acosh);
PrimOp(atanh);

EUFUN_2(Md_pow_float_float,x,y)
{
  return allocate_float(stacktop,pow(floatval(x),floatval(y)));
}
EUFUN_CLOSE

EUFUN_2(Md_pow_integer_float,x,y)
{
  return allocate_float(stacktop,pow((double)intval(x),floatval(y)));
}
EUFUN_CLOSE

EUFUN_2(Md_pow_float_integer,x,y)
{
  return allocate_float(stacktop,pow(floatval(x),(double)intval(y)));
}
EUFUN_CLOSE

EUFUN_2(Md_pow_integer_integer,xx,nn)
{
  int x, n, y, z;
  x = intval(xx);
  n = intval(nn);
  if (n == 0)
    return allocate_integer(stacktop, 1);
  if (x == 1)
    return xx;
  if (x == -1)
    return n%2==0 ? allocate_integer(stacktop, 1) : xx;
  if (n < 0)
    return allocate_float(stacktop,pow((double)x,(double)n));

  z = x;
  y = 1;
  do {
    if (n%2 != 0) y = y*z;
    z = z*z;
    n /= 2;
  } while (n > 0);

  return allocate_integer(stacktop, y);
}
EUFUN_CLOSE

/* XX: 
   Ceiling, Floor, Round, Truncate 
 */

EUFUN_1(Md_convert_integer,n)
{
  return (allocate_float(stacktop, (double) intval(n)));
}
EUFUN_CLOSE

EUFUN_1(Md_round_float,fl)
{	
  double x=floatval(fl),diff;
  int result;

  diff= abs(x - floor(x));
  
  if ( diff== 0.5)
    {
      result=(((int)floor(x)) & 1)==0 ? (int)(floor(x)) : (int) floor(x)+1;
      return allocate_integer(stackbase,result);
    }
  else
    return allocate_integer(stackbase, (int)floor(x + (double) 0.5));
}
EUFUN_CLOSE

EUFUN_1(Md_ceiling_float,fl)
{
  return allocate_integer(stacktop, (int) ceil(floatval(fl)));
}
EUFUN_CLOSE

EUFUN_1(Md_floor_float,fl)
{
  return allocate_integer(stacktop, (int) floor(floatval(fl)));
}
EUFUN_CLOSE


/* n-ary operations */
EUFUN_3(Fn_nary_add,n1,n2,lst)
{
  LispObject acc;

  acc=generic_apply_2(stacktop,generic_add,n1,n2);
  lst=ARG_2(stackbase);
  while (lst!=nil)
    {
      STACK_TMP(CDR(lst));
      acc=generic_apply_2(stacktop,generic_add,acc,CAR(lst));
      UNSTACK_TMP(lst);
    }

  return acc;
}
EUFUN_CLOSE

EUFUN_2(Fn_nary_subtract,n1,lst)
{
  LispObject acc;

  if (lst==nil)
    return (generic_apply_1(stacktop,generic_negate,n1));

  STACK_TMP(CDR(lst));
  acc=generic_apply_2(stacktop,generic_subtract,n1,(CAR(lst)));
  UNSTACK_TMP(lst);

  while (lst!=nil)
    {
      STACK_TMP(CDR(lst));
      acc=generic_apply_2(stacktop,generic_subtract,acc,CAR(lst));
      UNSTACK_TMP(lst);
    }

  return acc;
}
EUFUN_CLOSE

EUFUN_3(Fn_nary_multiply,n1,n2,lst)
{
  LispObject acc;

  acc=generic_apply_2(stacktop,generic_multiply,n1,n2);
  lst=ARG_2(stackbase);
  while (lst!=nil)
    {
      STACK_TMP(CDR(lst));
      acc=generic_apply_2(stacktop,generic_multiply,acc,CAR(lst));
      UNSTACK_TMP(lst);
    }
  return acc;
}
EUFUN_CLOSE

EUFUN_3(Fn_nary_divide,n1,n2,lst)
{
  LispObject acc;

  acc=generic_apply_2(stacktop,generic_divide,n1,n2);
  lst=ARG_2(stackbase);
  while (lst!=nil)
    {
      STACK_TMP(CDR(lst));
      acc=generic_apply_2(stacktop,generic_divide,acc,CAR(lst));
      UNSTACK_TMP(lst);
    }
  return acc;
}
EUFUN_CLOSE

EUFUN_3(Fn_nary_gcd,n1,n2,lst)
{
  LispObject acc;
  
  acc=generic_apply_2(stacktop,generic_gcd,n1,n2);
  lst=ARG_2(stackbase);
  while (lst!=nil)
    {
      STACK_TMP(CDR(lst));
      acc=generic_apply_2(stacktop,generic_gcd,acc,CAR(lst));
      UNSTACK_TMP(lst);
    }
  return acc;
}
EUFUN_CLOSE

EUFUN_3(Fn_nary_lcm,n1,n2,lst)
{
  LispObject acc;

  acc=generic_apply_2(stacktop,generic_lcm,n1,n2);
  lst=ARG_2(stackbase);
  while (lst!=nil)
    {
      STACK_TMP(CDR(lst));
      acc=generic_apply_2(stacktop,generic_lcm,acc,CAR(lst));
      UNSTACK_TMP(lst);
    }
  return acc;
}
EUFUN_CLOSE

EUFUN_2(Fn_nary_lt,n1,lst)
{
  while (lst!=nil)
    {
      STACK_TMP(lst);
      if (generic_apply_2(stacktop,generic_lt,n1,CAR(lst))==nil)
	return nil;

      UNSTACK_TMP(lst);
      n1=CAR(lst);
      lst=CDR(lst);
    }
  return lisptrue;
}
EUFUN_CLOSE

EUFUN_2(Fn_nary_ge,n1,lst)
{
  while (lst!=nil)
    {
      STACK_TMP(lst);
      if (generic_apply_2(stacktop,generic_lt,n1,CAR(lst))!=nil)
	return nil;

      UNSTACK_TMP(lst);
      n1=CAR(lst);
      lst=CDR(lst);
    }
  return lisptrue;
}
EUFUN_CLOSE

EUFUN_2(Md_remainder_Integer,a, b)
{
  return allocate_integer(stackbase,intval(a)%intval(b));
}
EUFUN_CLOSE

EUFUN_2(Fn_nary_le,n1,lst)
{

  while (lst!=nil)
    {
      STACK_TMP(lst);

      if (generic_apply_2(stacktop,generic_lt,CAR(lst),n1)==lisptrue)
	return nil;

      UNSTACK_TMP(lst);
      n1=CAR(lst);
      lst=CDR(lst);
    }

  return lisptrue;
}
EUFUN_CLOSE

EUFUN_2(Fn_nary_gt,n1,lst)
{
  while (lst!=nil)
    {
      STACK_TMP(lst);

      if (generic_apply_2(stacktop,generic_lt,CAR(lst),n1)==nil)
	return nil;

      UNSTACK_TMP(lst);
      n1=CAR(lst);
      lst=CDR(lst);
    }
  return lisptrue;
}
EUFUN_CLOSE


EUFUN_0( Fn_rand)
{
  extern int rand(void);
  int n;
  n=rand();

  return(real_allocate_integer(stackbase, n));
}
EUFUN_CLOSE

EUFUN_1( Fn_srand, s)
{
  extern void srand(unsigned int);

  srand((unsigned int) intval(s));

  return(nil);
}
EUFUN_CLOSE

static LispObject pi_val, mpdf, lpdf, lndf, mndf, mpfpi, mnfpi;

#define ARITH_ENTRIES 70
MODULE Module_arith;
LispObject Module_arith_values[ARITH_ENTRIES];

void initialise_arith(LispObject *stacktop)
{
  open_module(stacktop,
	      &Module_arith,
	      Module_arith_values,
	      "arith",
	      ARITH_ENTRIES);

  generic_add
    = make_module_generic(stacktop,"binary+",2);
  generic_subtract
    = make_module_generic(stacktop,"binary-",2);
  generic_multiply
    = make_module_generic(stacktop,"binary*",2);
  generic_divide
    = make_module_generic(stacktop,"binary/",2);
  generic_lcm
    = make_module_generic(stacktop,"binary-lcm",2);
  generic_gcd
    = make_module_generic(stacktop,"binary-gcd",2);
  generic_lt
    = make_module_generic(stacktop,"binary<",2);
  generic_negate
    = make_module_generic(stacktop,"negate",1);

  add_root(&generic_add);
  add_root(&generic_subtract);
  add_root(&generic_multiply);
  add_root(&generic_divide);
  add_root(&generic_lt);
  add_root(&generic_lcm);
  add_root(&generic_gcd);
  add_root(&generic_negate);

  (void) make_module_function(stacktop,"binary+_Integer",Md_binary_add_Integer_Integer,2);
  (void) make_module_function(stacktop,"binary-_Integer",Md_binary_subtract_Integer_Integer,2);
  (void) make_module_function(stacktop,"binary*_Integer",Md_binary_multiply_Integer_Integer,2);
  (void) make_module_function(stacktop,"binary/_Integer",Md_binary_divide_Integer_Integer,2);
  (void) make_module_function(stacktop,"binary=_Integer",Md_binary_eqn_Integer_Integer,2);
  (void) make_module_function(stacktop,"binary<_Integer",Md_binary_lt_Integer_Integer,2);
  (void) make_module_function(stacktop,"negate-integer",Md_negate_Integer,1);

  (void) make_module_function(stacktop,"binary+_Float",Md_binary_add_Float_Float,2);
  (void) make_module_function(stacktop,"binary-_Float",Md_binary_subtract_Float_Float,2);
  (void) make_module_function(stacktop,"binary*_Float",Md_binary_multiply_Float_Float,2);
  (void) make_module_function(stacktop,"binary/_Float",Md_binary_divide_Float_Float,2);
  (void) make_module_function(stacktop,"binary=_Float",Md_binary_eqn_Float_Float,2);
  (void) make_module_function(stacktop,"binary<_Float",Md_binary_lt_Float_Float,2);
  (void) make_module_function(stacktop,"negate-float",Md_negate_Float,1);

  /* Integer Methods */
  (void) make_module_function(stacktop,"binary-lcm-integer",Md_binary_lcm_Integer_Integer,2);
  (void) make_module_function(stacktop,"binary-gcd-integer",Md_binary_gcd_Integer_Integer,2);
  (void) make_module_function(stacktop,"quotient-integer",Md_binary_divide_Integer_Integer,2);
  (void) make_module_function(stacktop,"remainder-integer",Md_remainder_Integer,2);
  (void) make_module_function(stacktop,"modulo-integer",Md_remainder_Integer,2); /* XXX */
  
  /* Float Methods */
  (void) make_module_function(stacktop,"sin-float",Md_sin_Float,1);
  (void) make_module_function(stacktop,"cos-float",Md_cos_Float,1);
  (void) make_module_function(stacktop,"tan-float",Md_tan_Float,1);
  (void) make_module_function(stacktop,"asin-float",Md_asin_Float,1);
  (void) make_module_function(stacktop,"acos-float",Md_acos_Float,1);
  (void) make_module_function(stacktop,"atan-float",Md_atan_Float,1);
  (void) make_module_function(stacktop,"log-float",Md_log_Float,1);
  (void) make_module_function(stacktop,"log10-float",Md_log10_Float,1);
  (void) make_module_function(stacktop,"sqrt-float",Md_sqrt_Float,1);
  (void) make_module_function(stacktop,"exp-float",Md_exp_Float,1);
  (void) make_module_function(stacktop,"sinh-float",Md_sinh_Float,1);
  (void) make_module_function(stacktop,"cosh-float",Md_cosh_Float,1);
  (void) make_module_function(stacktop,"tanh-float",Md_tanh_Float,1);
  (void) make_module_function(stacktop,"asinh-float",Md_asinh_Float,1);
  (void) make_module_function(stacktop,"acosh-float",Md_acosh_Float,1);
  (void) make_module_function(stacktop,"atanh-float",Md_atanh_Float,1);
  (void) make_module_function(stacktop,"pow-float-float",
			      Md_pow_float_float,2);
  (void) make_module_function(stacktop,"pow-integer-float",
			      Md_pow_integer_float, 2);
  (void) make_module_function(stacktop,"pow-float-integer",
			      Md_pow_float_integer, 2);
  (void) make_module_function(stacktop,"pow-integer-integer",
			      Md_pow_integer_integer, 2);

  (void) make_module_function(stacktop,"convert-integer-float",Md_convert_integer,1);
  (void) make_module_function(stacktop,"round-float",Md_round_float,1);
  (void) make_module_function(stacktop,"ceiling-float",Md_ceiling_float,1);
  (void) make_module_function(stacktop,"floor-float",Md_floor_float,1);

  (void) make_module_function(stacktop,"+",Fn_nary_add,-3);
  (void) make_module_function(stacktop,"-",Fn_nary_subtract,-2);
  (void) make_module_function(stacktop,"*",Fn_nary_multiply,-3);
  (void) make_module_function(stacktop,"/",Fn_nary_divide,-3);
  (void) make_module_function(stacktop,"gcd",Fn_nary_gcd,-3);
  (void) make_module_function(stacktop,"lcm",Fn_nary_lcm,-3);

  (void) make_module_function(stacktop,"<",Fn_nary_lt,-2);
  (void) make_module_function(stacktop,"<=",Fn_nary_le,-2);
  (void) make_module_function(stacktop,">",Fn_nary_gt,-2);
  (void) make_module_function(stacktop,">=",Fn_nary_ge,-2);

  (void) make_module_function(stacktop,"c-rand",Fn_rand,0);
  (void) make_module_function(stacktop,"c-srand",Fn_srand,1);
  
  pi_val = allocate_float(stacktop, M_PI);
  add_root(&pi_val);
  (void) make_module_entry(stacktop,"pi",pi_val);

  mpdf = allocate_float(stacktop, DBL_MAX);
  add_root(&mpdf);
  (void) make_module_entry(stacktop,"most-positive-double-float", mpdf);

  lpdf = allocate_float(stacktop,DBL_MIN);
  add_root(&lpdf);
  (void) make_module_entry(stacktop,"least-positive-double-float", lpdf);

  lndf = allocate_float(stacktop,-DBL_MIN);
  add_root(&lndf);
  (void) make_module_entry(stacktop,"least-negative-double-float", lndf);

  mndf = allocate_float(stacktop,-DBL_MAX);
  add_root(&mndf);
  (void) make_module_entry(stacktop,"most-negative-double-float", mndf);

  mpfpi = allocate_integer(stacktop,INT_MAX);
  add_root(&mpfpi);
  (void) make_module_entry(stacktop,"most-positive-fixed-precision-integer",
			   mpfpi);

  mnfpi = allocate_integer(stacktop,INT_MIN);
  add_root(&mnfpi);
  (void) make_module_entry(stacktop,"most-negative-fixed-precision-integer",
			   mnfpi);

  close_module();
}
