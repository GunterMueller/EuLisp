/**
  * Reader for feel. 
  * Just reads tokens(via lex), and does some mangling...
  ***/

#include <string.h>
#include <ctype.h>
#include "defs.h"
#include "structs.h"
#include "funcalls.h"
#include "global.h"
#include "symboot.h"
#include "error.h"

#include "lex_global.h"

#define READBUG(x) 

static LispObject start_list(LispObject *stacktop, FILE *stream, int *len);
static void end_list(LispObject *stacktop, FILE *stream, LispObject first, int *len);
static LispObject this_object(LispObject *stacktop, int token);
static LispObject read2(LispObject *stacktop,FILE *stream,int *len);
static LispObject list2vector(LispObject *stacktop,int len, LispObject lst);

LispObject q_eof;
LispObject current_input;

LispObject sys_read(LispObject *stackbase, FILE *stream)
{
  int dummy;
  LispObject obj;

  yy_set_stream(stream);
  obj = read2(stackbase,stream,&dummy);
  
  return obj;
}

int reader_fclose(LispObject *stackbase, FILE *stream)
{
#ifdef WITH_FUDGE
  {
    extern int yy_close_stream(FILE *);

    return yy_close_stream(stream);
  }
#else
  return system_fclose((stream->STREAM).handle);
#endif
}

static LispObject read2(LispObject *stacktop,FILE *stream,int *len)
{
  LispObject result, first,tmp;
  int mylen, token;
  
  while (1) {
  token=yylex(stacktop);
  switch(token)
    {
    case OPEN_PAIR:
      return(start_list(stacktop,stream,len));
      break;

    case EXTENSION:		
      first = read2(stacktop,stream,&mylen);
      if (first==nil || is_cons(first))
	return(list2vector(stacktop,mylen,first));
      else
	{
	  CallError(stacktop,"Bad extension syntax",nil,NONCONTINUABLE);
	  return nil;
	}
      break;

    case CLOSE_PAIR:
/*      fprintf(stderr,"Spurious closing parenthisis ignored");
      return nil;*/
      break;

    case WRAPPER:
      STACK_TMP(pptok.lispval);
      first=read2(stacktop,stream,&mylen);
      first=EUCALL_2(Fn_cons,first,nil);
      UNSTACK_TMP(tmp);
      first=EUCALL_2(Fn_cons,tmp,first);
      *len=2;
      return first;
      break;
      
    case DOT:
      CallError(stacktop,"Bad list..",nil,NONCONTINUABLE);
      return nil; /* Never */
      break;

    default:
      return(this_object(stacktop,token));
    }
  }
  
}

static LispObject start_list(LispObject *stacktop, FILE *stream, int *len)
{
  LispObject next;
  int mylen, next_token;

  *len=0;
  next_token=yylex(stacktop);

  switch (next_token)
    {
    case OPEN_PAIR:
      next=start_list(stacktop,stream,&mylen);
      next=EUCALL_2(Fn_cons,next,nil);
      *len=1;
      STACK_TMP(next);
      end_list(stacktop,stream,next,len);
      UNSTACK_TMP(next);
      return next;

    case EXTENSION:
      next=read2(stacktop,stream,&mylen);
      if  (next!=nil && !is_cons(next))
	CallError(stacktop,"Bad extension syntax",nil,NONCONTINUABLE);
      else
	{
	  next=list2vector(stacktop,mylen,next);
	  next=EUCALL_2(Fn_cons,next,nil);
	  STACK_TMP(next);
	  *len=1;
	  end_list(stacktop,stream,next,len);
	  UNSTACK_TMP(next);
	  return next;
	}
      break;

    case CLOSE_PAIR:
      return nil;

    case WRAPPER:
      STACK_TMP(pptok.lispval);
      next=read2(stacktop,stream,&mylen);
      next=EUCALL_2(Fn_cons,next,nil);
      UNSTACK_TMP(pptok.lispval);
      next=EUCALL_2(Fn_cons,pptok.lispval,next);
      next=EUCALL_2(Fn_cons,next,nil);
      STACK_TMP(next);
      *len=1;
      end_list(stacktop,stream,next,len);
      UNSTACK_TMP(next);
      return next;
      break;


    case DOT:
      CallError(stacktop,"Misplaced dot",nil,NONCONTINUABLE);
      break;

    case END_OF_STREAM:
      CallError(stacktop,"Unexpected end of file",nil,NONCONTINUABLE);
      break;
 
   default:
      next = this_object(stacktop,next_token);
      next= EUCALL_2(Fn_cons,next,nil);
      ++*len;
      STACK_TMP(next);
      end_list(stacktop,stream,next,len); 
      UNSTACK_TMP(next);
      return next;
    }
}

static void end_list(LispObject *stacktop, FILE *stream, LispObject first, int *len)
{
  LispObject *stackbase;
  int token;
  LispObject next,tmp;
  int mylen;
  
  stackbase=stacktop;

  ARG_0(stackbase)=first; stacktop++;
  STACK_TMP(first);
  while ( (token=yylex(stacktop))!=CLOSE_PAIR)
    {
      switch (token)
	{
	case OPEN_PAIR:
	  next=start_list(stacktop,stream,&mylen);
	  next=EUCALL_2(Fn_cons,next,nil);	
	  UNSTACK_TMP(first);
	  CDR(first)=next;
	  first=next;
	  ++*len;
	  break;

	case EXTENSION:
	  next=read2(stacktop,stream,&mylen);
	  if  (next!=nil && !is_cons(next))
	    CallError(stacktop,"Bad extension syntax",next,NONCONTINUABLE);
	  else
	    {
	      next=list2vector(stacktop,mylen,next);
	      next=EUCALL_2(Fn_cons,next,nil);
	      UNSTACK_TMP(first);
	      CDR(first)=next;
	      first=next;
	      ++*len;
	    }
	  break;

	case WRAPPER:
	  STACK_TMP(pptok.lispval);
	  next=read2(stacktop,stream,&mylen);
	  next=EUCALL_2(Fn_cons,next,nil);
	  UNSTACK_TMP(tmp);
	  next=EUCALL_2(Fn_cons,tmp,next);
	  next=EUCALL_2(Fn_cons,next,nil);
	  UNSTACK_TMP(first);
	  CDR(first)=next;
	  first=next;
	  ++*len;
	  break;

	case DOT:
	  next=read2(stacktop,stream,&mylen);
	  UNSTACK_TMP(first);
	  CDR(first)=next;
	  if (is_cons(next))
	    {
	      if (mylen<0) 
		*len=mylen-*len;
	      else
		*len+=mylen;
	    }
	  else
	    *len= -*len;
	  break;

	case END_OF_STREAM:
	  CallError(stacktop,"Unexpected end of file",nil,NONCONTINUABLE);
	  break;

	default:
	  next=this_object(stacktop,token);
	  next=EUCALL_2(Fn_cons,next,nil);
	  UNSTACK_TMP(first);
	  CDR(first)=next;
	  first=next;
	  ++*len;
	  break;
	}	
      STACK_TMP(first);
    }
}

static LispObject this_object(LispObject *stacktop, int token)
{
  switch (token)
    {
      /** Literal objects **/
    case CHARACTER:	
    case INTEGER:
    case FLOAT:
    case RATIONAL: /* XXX: Still need to fix this */
    case STRING:
    case IDENTIFIER:
    case LISPNIL:
      return pptok.lispval;
      break;
      
    case END_OF_STREAM:
      return q_eof;
      break;

    default:
      CallError(stacktop,"Bad token type",allocate_integer(stacktop,token),NONCONTINUABLE);
    }
}

static LispObject list2vector(LispObject *stacktop,int len, LispObject lst)
{
  LispObject new;
  int i;

  if (len<0)
    CallError(stacktop,"bad vector syntax",lst,NONCONTINUABLE);
  
  new=allocate_vector(stacktop,len);

  for (i=0 ; i<len ; i++)
    {	
      vref(new,i)=CAR(lst);
      lst=CDR(lst);
    }	
  return new;
}
