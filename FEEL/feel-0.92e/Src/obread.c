/*
 * generic object reading and writing
 *
 * Assumptions
 *   Floating point is the same on all interesting machines...
 *   Overflow never happens...
 *   Also ints are the same everywhere: This must be changed soon!
 * Lisp Functions:
 *   make-obj-reader
 *     make a new instance of the default reader
 *
 *   add-reader (reader id function)
 *     function should take a reader function and a position as arguments
 *
 *   add-writer (reader id-num class function)   
 *     function should take a writer function and a position as arguments 
 *   
 * C Functions:
 *   LispObject read_obj(buf,reader)
 */

/*
 * $Id: obread.c,v 1.3 1994/06/30 16:26:15 djb Exp $
 *
 * $Log: obread.c,v $
 * Revision 1.3  1994/06/30  16:26:15  djb
 * Wolfgang's E5
 *
 * Revision 1.2  1994/04/11  16:01:53  djb
 * ANSI changes -- took static function declarations out of function
 *
 * Revision 1.1  1994/04/06  14:40:26  djb
 * Initial revision
 *
 *
 */

#include <stdio.h>
#include "defs.h"
#include "structs.h"
#include "funcalls.h"
#include "global.h"
#include "error.h"
#include "allocate.h"
#include "class.h"
#include "modboot.h"
#include "bootstrap.h"
#include "allocate.h"
#include "ngenerics.h"
#include "calls.h"

#include "obread.h"
#include "obread_p.h"

/* lose this sometime... */
#define OBJECTIFY(ptr) (allocate_integer(stacktop,(int) ptr))

/* Need this to find writers in a pseudo-generic way */

LispObject lookup_by_cpl(LispObject *stacktop,LispObject tbl, LispObject class)
{
  LispObject res;

  res=generic_apply_1(stacktop,tbl,class);
 	   
  return res;
}

EUFUN_1( Fn_make_obj_reader,gf)
{
  LispObject ans,tmp;
  
  ans = (LispObject) allocate_vector(stacktop,2);
  STACK_TMP(ans);
  tmp = allocate_vector(stacktop,MAX_ID + 1);
  UNSTACK_TMP(ans);
  vref(ans,0)=tmp;
  vref(ans,1) = ARG_0(stackbase);

  return ans;
}
EUFUN_CLOSE

static EUFUN_3( Fn_add_reader, reader, id, fn)
{
  GET_READER(reader,intval(id)) = fn;
  return lisptrue;
}
EUFUN_CLOSE

static EUFUN_4( Fn_add_writer, reader, class, 
	       id,  fn)
{
  LispObject xx;
  if (!is_function(fn))
    CallError(stacktop,"add-writer: type error",fn,NONCONTINUABLE);

  if(!is_fixnum(id))
    CallError(stacktop,"add-writer: type error",id,NONCONTINUABLE);
      
  xx=MAKE_WRITER(id,fn);
  reader=ARG_0(stackbase);
  class=ARG_1(stackbase);
  SET_WRITER(reader,class,xx);
  return nil;
}
EUFUN_CLOSE

static EUFUN_2( Fn_read_next, obj, reader)
{
  LispObject ans;

  ans = read_obj(stacktop,(unsigned char **)(intval(obj)),reader);
  return ans;
}
EUFUN_CLOSE

static EUFUN_3( Fn_write_next, 
	       thing, 
	       posn,
	       reader)
{
  write_obj(stacktop,thing, (unsigned char **) (intval(posn)),reader);
  return nil;
}
EUFUN_CLOSE

static LispObject read_cons_object(LispObject *,unsigned char **p_ptr, LispObject reader);
static LispObject read_vector_object(LispObject *,unsigned char **p_ptr, LispObject reader);
extern LispObject get_symbol_by_copying(LispObject *,char *);

LispObject read_obj(LispObject *stacktop,unsigned char **p_ptr, LispObject reader)
{
  double fl;
  int n;
  unsigned char *tmp;
  
  EUBUG(printf("read: looking at: %d\n",**p_ptr));
  if (**p_ptr < FIRST_USER_ID)
    {
      switch(**p_ptr)
	{
	case READ_FLOAT:
	  ++ (*p_ptr);
	  bcopy(*p_ptr, (char *) &fl, sizeof(double));
	  *p_ptr = *p_ptr + sizeof(double);
	  return(allocate_float(stacktop,fl));
	  break;

	case READ_INT:
	  ++(*p_ptr);
	  bcopy(*p_ptr, (char *) &n, sizeof(int));
	  *p_ptr = *p_ptr + sizeof(int);
	  return (allocate_integer(stacktop,n));
	  break;

	case READ_STRING:
	  ++(*p_ptr); 
	  n = strlen((*p_ptr));
	  tmp = *p_ptr;
	  /* add 1 for the 0 character */
	  (*p_ptr) += n + 1;
	  return(allocate_string(stacktop,(char *)tmp,n));
	  break;

	case READ_SYMBOL:
	  ++(*p_ptr); 
	  n = strlen(*p_ptr);	/*  Need copy, o/w it gets stomped */
	  tmp = *p_ptr;
	  (*p_ptr) += n + 1;
	  return get_symbol_by_copying(stacktop,(char *) tmp);
	  break;
      
	case READ_NULL:
	  ++(*p_ptr) ; 
	  return nil;
	  break;

	case READ_CONS:
	  ++ (*p_ptr);
	  return(read_cons_object(stacktop,p_ptr,reader));
	  break;

	case READ_VECTOR:
	  ++ (*p_ptr);
	  return(read_vector_object(stacktop,p_ptr, reader));
	  break;

	default:
	  CallError(stacktop,"Attempted to read impossible datatype",nil,NONCONTINUABLE);
	  break;
	}    
    }
  else
    { LispObject xx;
      if (reader == nil)
	CallError(stacktop,"No reader specified in socket-read",nil,NONCONTINUABLE);
      tmp = *p_ptr; ++(*p_ptr);
      STACK_TMP(reader);
      xx=OBJECTIFY(p_ptr);
      UNSTACK_TMP(reader);
      return(EUCALL_3(apply2,GET_READER(CAR(reader),*tmp),xx,reader));
    }

  CallError(stacktop,"Recieved a shock",nil,NONCONTINUABLE);
  return nil; /* not ever */
}

static LispObject read_cons_object(LispObject *stackbase,
				   unsigned char **p_ptr,
				   LispObject reader)
{
  LispObject *stacktop=stackbase+3;
  int end=FALSE;
  LispObject first_cons, this_cons;
  
  ARG_0(stackbase)=reader; ARG_1(stackbase)=nil;ARG_2(stackbase)=nil;
  first_cons = EUCALL_2(Fn_cons,nil,nil);
  ARG_1(stackbase)=first_cons;
  this_cons = first_cons;
  ARG_2(stackbase)=this_cons;
  while (!end)
    { LispObject xx;

      xx = read_obj(stacktop,p_ptr,ARG_0(stackbase));
      this_cons=ARG_2(stackbase);
      CAR(this_cons)=xx;
      
      switch(**p_ptr)
	{
	  /* move along 1 */
	case READ_CONS:
	  ++(*p_ptr);
	  xx = EUCALL_2(Fn_cons,nil,nil);
	  CDR(ARG_2(stackbase))=xx;
	  this_cons = xx;
	  ARG_2(stackbase)=this_cons;
	  break;
      
	case READ_NULL:
	  end = TRUE;
	  ++ (*p_ptr);
	  break;

	default:
	  end = TRUE;
	  xx = read_obj(stacktop,p_ptr,ARG_0(stackbase));
	  CDR(ARG_2(stackbase))=xx;
	  break;
	}
    }

  return ARG_1(stackbase);
}

static LispObject read_vector_object(LispObject *stackbase,unsigned char **p_ptr, LispObject reader)
{
  LispObject read_obj(LispObject *,unsigned char **p_ptr, LispObject reader);
  extern LispObject allocate_vector(LispObject *,int);

  LispObject *stacktop=stackbase+1;
  LispObject vect;
  int vlen;
  int i;
  
  bcopy((char *)*p_ptr, (char *) &vlen, sizeof(int));
  *p_ptr += sizeof(int);
  
  vect = allocate_vector(stacktop,vlen);
  ARG_0(stackbase)=vect;

  for (i=0; i<vlen; i++)
    {
      STACK_TMP(reader);
      vref(ARG_0(stackbase),i) = read_obj(stacktop,p_ptr,reader);
      UNSTACK_TMP(reader);
    }
  
  return(ARG_0(stackbase));

}

/* We assume that *stackbase is the object to be written here */
void write_obj(LispObject *stackbase, LispObject ob,unsigned char **p_buf, LispObject reader)
{
  void write_cons_obj(LispObject *,LispObject ob,unsigned char **p_buf,LispObject reader);
  
  LispObject *stacktop=stackbase+1;
  char *p_str;
  int i;
  
  ARG_0(stackbase)=ob;
  switch(typeof(ob))
    {
    case TYPE_INT:
      **p_buf = READ_INT;
      ++(*p_buf);
      i=intval(ob);
      bcopy((char *) &i, *p_buf,sizeof(int));
      *p_buf += sizeof(int);
      break;

    case TYPE_FLOAT:
       **p_buf = READ_FLOAT;
      ++(*p_buf);
       bcopy((char *) &(ob->FLOAT.fvalue), *p_buf, sizeof(double));
       *p_buf += sizeof(double);
      break;

    case TYPE_STRING:
      **p_buf = READ_STRING;
      ++ (*p_buf);
      p_str = stringof(ob);
      while(*p_str != '\0')
	{
	  **p_buf = *p_str;
	  ++ (*p_buf);
	  ++ p_str;
	}
      **p_buf = *p_str;
      ++ (*p_buf);
      break;

    case TYPE_SYMBOL:  /* There are more cunning ways to do this... */
      **p_buf = READ_SYMBOL;
      ++ (*p_buf);
      p_str = stringof(ob->SYMBOL.pname);
      
      while (*p_str != '\0')
	{
	  **p_buf = *p_str;
	  ++ (*p_buf);
	  ++ p_str;
	}
      /* and copy the '\0' */
      **p_buf = *p_str;
      ++ (*p_buf);
      break;

    case TYPE_NULL:
      **p_buf = READ_NULL;
      ++ (*p_buf);
      break;
      
    case TYPE_CONS:
      write_cons_obj(stacktop,ob,p_buf,reader);
      break;

    case TYPE_VECTOR:
      **p_buf = READ_VECTOR;
      ++ (*p_buf);
      bcopy((char *) &(ob->VECTOR.length), *p_buf,sizeof(int));
      *p_buf += sizeof(int);
      for (i=0; i< ob->VECTOR.length ; i++)
	{
	  STACK_TMP(ob);
	  write_obj(stacktop,vref(ob,i),p_buf,reader);
	  UNSTACK_TMP(ob);
	}
      break;

    default:
      /* reader is either nil or a 1-elt list contaning a reader */
      if (reader == nil) 
	CallError(stacktop,"No reader specified",ob,NONCONTINUABLE);
      else
	{
	  LispObject lst,tmp,tmp2;
	  char tmp3;
	  STACK_TMP(ob);
	  STACK_TMP(reader);
	  STACK_TMP(reader);
	  tmp=OBJECTIFY(p_buf);
	  UNSTACK_TMP(reader);
	  STACK_TMP(tmp);
	  EUCALLSET_2(lst,Fn_cons,reader,nil);
	  UNSTACK_TMP(tmp);
	  EUCALLSET_2(lst,Fn_cons,tmp,lst);
	  EUCALLSET_2(lst,Fn_cons,ARG_0(stackbase),lst);
	  UNSTACK_TMP(reader);
	  UNSTACK_TMP(ob);
	  STACK_TMP(lst);
	  STACK_TMP(ob);
	  STACK_TMP(reader);
	  tmp3=WRITER_ID(GET_WRITER(CAR(reader),(ob)));
	  UNSTACK_TMP(reader);
	  UNSTACK_TMP(ob);
	  **p_buf = tmp3;
	  ++ (*p_buf);
	  tmp=WRITER_FN(GET_WRITER(CAR(reader),(ob))); /* can gc */
	  UNSTACK_TMP(lst);
	  EUCALL_2(Fn_apply,tmp,lst);
	}
      break;
    }
  return ;
}


void write_cons_obj(LispObject *stackbase,LispObject ob,unsigned char **p_buf,LispObject reader)
{
  LispObject aob;
  LispObject *stacktop=stackbase;
  aob=ob;
  while(is_cons(aob))
    {
      **p_buf = (unsigned char) READ_CONS;
      ++ (*p_buf); 
      STACK_TMP(reader);
      STACK_TMP(CDR(aob));
      write_obj(stacktop,CAR(aob),p_buf,reader);
      UNSTACK_TMP(aob);
      UNSTACK_TMP(reader);

    }
  /* And the final CDR */
  write_obj(stacktop,aob,p_buf,reader);
}


#define READER_ENTRIES (4)

MODULE Module_reader;
LispObject Module_reader_values[READER_ENTRIES];

void initialise_lreader(LispObject *stacktop)
{
  open_module(stacktop,&Module_reader,Module_reader_values,"lreader",READER_ENTRIES);

  (void) make_module_function(stacktop,"make-obj-reader",Fn_make_obj_reader,1);
  (void) make_module_function(stacktop,"add-reader",Fn_add_reader,3);
  (void) make_module_function(stacktop,"read-next",Fn_read_next,2);
  (void) make_module_function(stacktop,"write-next",Fn_write_next,3);
  close_module();
}
