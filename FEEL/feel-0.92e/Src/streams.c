/* 
 * streams.c
 * Revised printing routines for feel
 * Idea is that a stream should only handle 
 * its raw type (eg strings, or bytesequences)
 */

/*
 * $Id: streams.c,v 1.8 1994/08/29 11:48:00 jap Exp $
 *
 * $Log: streams.c,v $
 * Revision 1.8  1994/08/29  11:48:00  jap
 * added entries for calls to POSIX fns: open, close, fcntl and
 * access to the value of errno.  Lisp fns are c_open, c_close
 * c_fcntl and c_errno.
 *
 * Revision 1.7  1994/08/12  10:02:55  djb
 * made some more of the file functions accessible from C by
 * removing the 'static' declaration
 *
 * Revision 1.6  1994/06/14  12:58:44  djb
 * changes to handle flex-2.4.6
 *
 * Revision 1.5  1994/04/25  12:36:04  djb
 * Dave Hall fix -- calls to ioctl need fileno around first arg
 *
 * Revision 1.4  1994/03/15  10:34:00  djb
 * #def'd xpipe code out for WITH_SYSV_SOCKETS or WITH_BSD_SOCKETS
 * but haven't got replacement code.
 *
 * Revision 1.3  1994/03/09  14:50:57  djb
 * c==0 -> c==EOF in Fn_read_char (but is still won't work
 * when mixed with yylex reads because (f)lex buffers input
 *
 * Revision 1.2  1994/02/08  12:03:33  djb
 * added jap's changes to pass stacktop down to low level reader code
 *
 * Revision 1.1  1994/02/08  11:56:32  djb
 * Initial revision
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/types.h>
#include <errno.h>

#ifndef SEEK_SET /* For non-conforming defs of seek */
#define SEEK_SET 0
#define SEEK_END 2
#endif 

#include "funcalls.h"
#include "defs.h"
#include "structs.h"
#include "error.h"
#include "global.h"
#include "allocate.h"
#include "modboot.h"
#include "calls.h"

#include "streams.h"
#include "reader.h"
#include "ngenerics.h"

#if 0 /* debugging ..*/
(fprintf(stderr, "File: %x",*((FILE **) stringof(fobj))))
#endif

#define fpof(fobj)   (*((FILE **) stringof(fobj)))

#define fpof1(fobj)   (*((FILE **) stringof(fobj)))

/* Standard Streams */

LispObject std_streams;

EUFUN_0(Fn_std_streams)
{
  return std_streams;
}
EUFUN_CLOSE

/* File operations */

EUFUN_2(Fn_fopen,name,mode)
{ 
  LispObject ans;
  FILE *fp;
  
  fp=fopen(stringof(name),stringof(mode));
  if (fp==NULL)
    CallError(stacktop,"Could not open file",name,NONCONTINUABLE);
  ans=allocate_string(stacktop,"",sizeof(FILE*));
  fpof1(ans)=fp;
  return ans;
}
EUFUN_CLOSE 

EUFUN_2(Fn_popen,name,mode)
{
#ifdef HAS_POPEN
  LispObject ans;
  FILE *fp;
  
  fp=popen(stringof(name),stringof(mode));
  if (fp==NULL)
    CallError(stacktop,"Could not open pipe",name,NONCONTINUABLE);
  ans=allocate_string(stacktop,"",sizeof(FILE*));
  fpof1(ans)=fp;
  return ans;
#else
  CallError(stacktop,"popen: no pipes here",nil,NONCONTINUABLE);
  return nil;
#endif
}
EUFUN_CLOSE
  
EUFUN_1(Fn_reopen,what)
{
  LispObject ans;
  FILE *fp;

  switch (intval(what))
    {
    case 0:
      fp=stdin;
      break;
    case 1:
      fp=stdout;
      break;
    case 2:
      fp=stderr;
      break;
    default:
      return nil;
    }

  ans=allocate_string(stacktop,"",sizeof(FILE*));
  fpof1(ans)=fp;
  
  return ans;
}
EUFUN_CLOSE

EUFUN_2(Fn_seek,stream,offset)
{
  int ret;
  
  if (!is_fixnum(offset))
    CallError(stacktop,"seek[file]: Invalid offset",offset,NONCONTINUABLE);

  if (intval(offset) == -1)
    ret=fseek(fpof(stream),0,SEEK_END);
  else
    ret=fseek(fpof(stream),intval(offset),SEEK_SET);
  
  if (ret)
    CallError(stacktop,"seek[file]: Seek failed",offset,NONCONTINUABLE);
  
#ifdef WITH_FUDGE
  {
    extern void yy_reset_stream(LispObject *stacktop, FILE *);
    yy_reset_stream(stacktop, fpof(stream));		    
  }
#endif

  return lisptrue;
}
EUFUN_CLOSE
  
EUFUN_1(Fn_tell,stream)
{
  int ret;

  ret=ftell(fpof(stream));
  
  if (ret<0)
    CallError(stacktop,"tell[file]: Tell failed",stream,NONCONTINUABLE);

  return(allocate_integer(stacktop,ret));
}
EUFUN_CLOSE
  
EUFUN_1(Fn_flush,stream)
{
  int ret;

  ret=fflush(fpof(stream));
  if (ret!=0)
    {	
#if 0 /* Stardents sometimes return non-zero on flush! */
      print_string(stacktop,StdOut(),"Ouch--flush failure\n");
      EUCALL_3(generic_apply_2,generic_prin,stream,StdOut());
#endif
      return nil;

    }	
/*CallError(stacktop,"flush[file]: flush failed",stream,NONCONTINUABLE);*/
  
  return lisptrue;
}
EUFUN_CLOSE
  
/* Bug: Pipes should call pclose, not fclose! */

EUFUN_1(Fn_close,stream)
{ 
  int ret;
  
  ret=reader_fclose(stacktop,fpof(stream));
  if (ret!=0)	
    return nil;

/*
    {
      perror("close");
      CallError(stacktop,"close[file]: close failed",stream,NONCONTINUABLE);
    }
*/
  
  return lisptrue;
}
EUFUN_CLOSE
  
/* Output to a stream */

/* This can handle both strings and characters */
  
EUFUN_2(Fn_put,stream,ob)
{
  if (is_string(ob))
    {
      fputs(stringof(ob),fpof(stream));
      return ob;
    }
  
  if (is_char(ob))
    {
      fputc(ob->CHAR.code,fpof(stream));
      return ob;
    }
  
  CallError(stacktop,"put[file]: Invalid object type",classof(ob),NONCONTINUABLE);
  return nil; /* Not ever */
}
EUFUN_CLOSE

  
static EUFUN_2(Fn_prin_fixnum,n,stream)
{	
  char buf[32];
  
  sprintf(buf,"%d",intval(n));
  return(print_string(stacktop,stream,buf));
}	
EUFUN_CLOSE
  
/* Callbacks */
LispObject generic_prin,generic_write, generic_flush;
static LispObject generic_output,generic_read;
LispObject format_specifiers;
static LispObject Fn_prin_list(LispObject *);

EUFUN_2(Fn_print,ob,stream)
{
  if (stream==nil)
    stream=StdOut();
  else
    stream=stream;
  
  STACK_TMP(stream);
  generic_apply_2(stacktop,generic_prin,ob,stream);
  UNSTACK_TMP(stream);
  print_string(stacktop,stream,"\n");
  
  return ARG_0(stackbase);
}
EUFUN_CLOSE
/* Ops coded in 'C' for efficiency */
/* Only handles a few cases --- needed to bootstrap */
static EUFUN_2(Fn_prin_object,ob,stream)
{
  switch(typeof(ob))
    {
    case TYPE_CONS:	
      EUCALL_2(Fn_prin_list,ob,stream);
      break;

    case TYPE_INT:
      EUCALL_2(Fn_prin_fixnum,ob,stream);
      break;

    case TYPE_STRING:
      print_string(stacktop,stream,stringof(ob));
      break;
      
    case TYPE_SYMBOL:
      print_string(stacktop,stream,stringof(ob->SYMBOL.pname));
      break;

    default:
      {
	char buf[32];
	print_string(stacktop,stream,"#<");
	print_string(stacktop,ARG_1(stackbase),stringof(classof(ARG_0(stackbase))->CLASS.name->SYMBOL.pname));
	sprintf(buf,": %x>",(unsigned long) ob);
	print_string(stacktop,ARG_1(stackbase),buf);
      }
      break;

    }	

  return ARG_0(stackbase);
}
EUFUN_CLOSE

static EUFUN_2(Fn_prin_list,form,stream)
{
  stacktop++;
  ARG_2(stackbase)=form;
  if (typeof(stream)==TYPE_STREAM)
    {
      putc('(',fpof(stream));
      generic_apply_2(stacktop,generic_prin, CAR(form), ARG_1(stackbase));
      form = ARG_0(stackbase);

      form=CDR(form);
      while (is_cons(form))
	{
	  putc(' ',fpof(ARG_1(stackbase)));
	  ARG_0(stackbase) = form;
	  generic_apply_2(stacktop,generic_prin, CAR(form), ARG_1(stackbase));
	  form = ARG_0(stackbase);
	  form=CDR(form);
	}
      if (form!=nil)
	{
	  fputs(" . ",fpof(ARG_1(stackbase)));
	  generic_apply_2(stacktop,generic_prin, form, ARG_1(stackbase));
	}
      putc('(',fpof(ARG_1(stackbase)));
    }
  else
    {
      LispObject s; /* Temporary for holding bits of string */
      
      s=allocate_string(stacktop,"(",3);
      STACK_TMP(s);
      generic_apply_2(stacktop,generic_prin,s,ARG_1(stackbase));
      form=ARG_0(stackbase);
      generic_apply_2(stacktop,generic_prin,CAR(form),ARG_1(stackbase));
      
      UNSTACK_TMP(s);
      strcpy(stringof(s)," ");
      form=CDR(ARG_0(stackbase));
      STACK_TMP(s);
      while (is_cons(form))
	{
	  UNSTACK_TMP(s);
	  STACK_TMP(s);
	  ARG_0(stackbase)=form;
	  generic_apply_2(stacktop,generic_prin,s,ARG_1(stackbase));	  
	  form=ARG_0(stackbase);
	  generic_apply_2(stacktop,generic_prin,CAR(form),ARG_1(stackbase));
	  form=CDR(ARG_0(stackbase));
	}
      UNSTACK_TMP(s);
      STACK_TMP(s);
      if (form!=nil)
	{
	  strcpy(stringof(s)," . ");
	  ARG_0(stackbase)=form;
	  generic_apply_2(stacktop,generic_prin,s,ARG_1(stackbase));
	  form=ARG_0(stackbase);
	  generic_apply_2(stacktop,generic_prin,form,ARG_1(stackbase));
	}
      UNSTACK_TMP(s);
      strcpy(stringof(s),")");
      generic_apply_2(stacktop,generic_prin,s,ARG_1(stackbase));
    }
  return ARG_0(stackbase);
}	
EUFUN_CLOSE

/* HACK: if stream is nil, use stdout. if t, use stderr */
LispObject print_string(LispObject *stacktop,LispObject stream, char *ptr)
{
  if (typeof(stream)==TYPE_STRING)
    fputs(ptr,fpof(stream));
  else if (stream==nil)
    fputs(ptr,stdout);
  else if (stream==lisptrue)
    fputs(ptr,stderr);
  else
    {
      LispObject s;
      STACK_TMP(stream);
      s=allocate_string(stacktop,ptr,strlen(ptr));
      UNSTACK_TMP(stream);
      generic_apply_2(stacktop,generic_prin,s,stream);
    }
  return nil;
}

/* Format operations */

/* Getting at callbacks */

static EUFUN_0(Fn_std_formatters)
{
  return format_specifiers;
}
EUFUN_CLOSE

#define FORMATSIZE 200
/* Internal format --- can't handle format nil, t, etc */
static EUFUN_3(Fn_iformat,stream,fmt,args)
{
  char buf[FORMATSIZE];
  char *next,*fmtptr,c;
  int i,j,done=FALSE;
  LispObject add_arg=nil;

  /* Check arguments */
  if (typeof(stream) != TYPE_INSTANCE) /* as close as we can get, for now */
    CallError(stacktop,"format: not a stream",stream,NONCONTINUABLE);
  if (!is_string(fmt))
    CallError(stacktop,"format: not a string",fmt,NONCONTINUABLE);
  
  /* wait until we get a tilde */
  
  j=0;
  
  while (!done)
    {
      i=0;
      while ( (c=stringof(fmt)[j])!='\0'
	     && c!='~')
	{
	  buf[i++]=c;
	  j++;
	  if (i==FORMATSIZE-1)
	    {	
	      print_string(stacktop,stream,buf);
	      fmt=ARG_1(stackbase);
	      stream=ARG_0(stackbase);
	      i=0;
	    }
	}
  
      buf[i]='\0';
      if (i!=0)
	print_string(stacktop,stream,buf);
      fmt=ARG_1(stackbase);
      /* We have to be careful here as fmt may move */

      if (c=='\0')
	done=TRUE;
      else
	{
	  int n1,n2;
	  LispObject tmp=nil;

	  j++;
	  fmtptr=&stringof(fmt)[j];
	  n1=strtol(fmtptr,&next,10);
	  if (next!=fmtptr)
	    {
	      if (*next=='.')
		{
		  fmtptr=next+1;
		  n2=strtol(fmtptr,&next,10);
		  if (next==fmtptr)
		    CallError(stacktop,"format: No number after dot",fmt,NONCONTINUABLE);
		  
		  tmp=allocate_integer(stacktop,n2);
		}
	      j=next-stringof(fmt);
	      add_arg=allocate_integer(stacktop,n1);
	      add_arg=EUCALL_2(Fn_cons,add_arg,tmp);
	    }
	  fmt=ARG_1(stackbase);
	  if (stringof(fmt)[j]=='\0')
	    done=TRUE;
	  else
	    {
	      LispObject fn;

	      fn=vref(format_specifiers,stringof(fmt)[j]);
	      if (fn==nil)
		CallError(stacktop,"Format: unknown format specifier",fmt,NONCONTINUABLE);
      
	      args=EUCALL_4(apply3,fn,ARG_0(stackbase),ARG_2(stackbase),add_arg);
	      ARG_2(stackbase)=args;
	      j++;
	      fmt=ARG_1(stackbase);
	      stream=ARG_0(stackbase);
	    }
	}
    } /* end while(1) */
  return nil;
}
EUFUN_CLOSE

/* Format functions */
#ifndef N_BITS_IN_CHAR
#define N_BITS_IN_CHAR 8
#endif

#define BIGBIN sizeof(int)*N_BITS_IN_CHAR
static EUFUN_3(Fn_format_b,stream,args,add_args)
{
  char buf[BIGBIN+1], *ptr;
  int n;
  
  if (args==nil)
    CallError(stacktop,"format ~b: insufficient args",args,NONCONTINUABLE);
  if (!is_fixnum(CAR(args)))
    CallError(stacktop,"format ~b: not an integer",CAR(args),NONCONTINUABLE);
	    
  n=intval(CAR(args));
  if (n < 0) {
    print_string(stacktop,stream,"-");
    n = -n;
  }

  ptr=buf+BIGBIN;
  *ptr-- = 0;

  do {
    *ptr-- = (n&1)+'0';
    n>>=1;
  } while (n > 0);
	    
  print_string(stacktop,stream,ptr+1);
  
  return CDR(args);
}
EUFUN_CLOSE

static EUFUN_3(Fn_format_r,stream,args,add_args)
{
  char buf[BIGBIN+1], *ptr;
  int n, base, digit;

  if (args==nil)
    CallError(stacktop,"format ~r: insufficient args",args,NONCONTINUABLE);
  if (!is_fixnum(CAR(args)))
    CallError(stacktop,"format ~r: not an integer",CAR(args),NONCONTINUABLE);
  if (add_args==nil)
    CallError(stacktop,"format ~r: missing base",nil,NONCONTINUABLE);

  base = intval(CAR(add_args));
  if (base > 36)
    CallError(stacktop,"format ~r: base too large",CAR(add_args),
	      NONCONTINUABLE);
  if (base < 2)
    CallError(stacktop,"format ~r: base too small",CAR(add_args),
              NONCONTINUABLE);

  n = intval(CAR(args));
  if (n < 0) {
    print_string(stacktop,stream,"-");
    n = -n;
  }

  ptr = buf+BIGBIN;
  *ptr-- = 0;

  do {
    digit = n % base;
    *ptr-- = digit < 10 ? digit + '0' : digit - 10 + 'a';
    n /= base;
  } while (n > 0);

  print_string(stacktop,stream,ptr+1);

  return CDR(args);
}
EUFUN_CLOSE

static EUFUN_3(Fn_format_o,stream,args,add_args)
{
  char buf[512];
  int n;

  if (args==nil)
    CallError(stacktop,"format ~o: insufficient args",args,NONCONTINUABLE);
  if (!is_fixnum(CAR(args)))
    CallError(stacktop,"format ~o: not an integer",CAR(args),NONCONTINUABLE);

  n = intval(CAR(args));
  if (n < 0)
    sprintf(buf, "-#o%o", -n);
  else
    sprintf(buf, "#o%o", n);
  print_string(stacktop,stream,buf);

  return CDR(args);
}
EUFUN_CLOSE

static EUFUN_3(Fn_format_x,stream,args,add_args)
{
  char buf[512];
  int n;

  if (args==nil)
    CallError(stacktop,"format ~x: insufficient args",args,NONCONTINUABLE);
  if (!is_fixnum(CAR(args)))
    CallError(stacktop,"format ~x: not an integer",CAR(args),NONCONTINUABLE);

  n = intval(CAR(args));
  if (n < 0)
    sprintf(buf, "-#x%x", -n);
  else
    sprintf(buf, "#x%x", n);
  print_string(stacktop,stream,buf);

  return CDR(args);
}
EUFUN_CLOSE

/* non-standard */
static EUFUN_3(Fn_format_u,stream,args,add_args)
{
  char buf[64];
  
  if (args==nil)
    CallError(stacktop,"format ~u: insufficient args",args,NONCONTINUABLE);

  sprintf(buf,"0x%x",CAR(args));
  print_string(stacktop,stream,buf);
  
  return CDR(args);
}
EUFUN_CLOSE


static EUFUN_3(Fn_format_e,stream,args,add_args)
{
  char buf[512],fmtbuf[40];
  double val;

  if (args==nil)
    CallError(stacktop,"format ~e: insufficient args",args,NONCONTINUABLE);

  if (is_float(CAR(args)))
    val=CAR(args)->FLOAT.fvalue;
  else if (is_fixnum(CAR(args)))
    val=(double)intval(CAR(args));
  else
    CallError(stacktop,"format ~e: not a number",CAR(args),NONCONTINUABLE);

  if (add_args==nil)
    strcpy(fmtbuf,"%#e");
  else if (CDR(add_args)==nil)
    sprintf(fmtbuf,"%%%de",intval(CAR(add_args)));
  else
    sprintf(fmtbuf,"%%%d.%de",intval(CAR(add_args)),
	    intval(CDR(add_args)));
  
  sprintf(buf,fmtbuf,val);
  print_string(stacktop,stream,buf);
  
  return CDR(args);
}
EUFUN_CLOSE

static EUFUN_3(Fn_format_f,stream,args,add_args)
{
  char buf[512],fmtbuf[40]; /* can't have floats with more than ~300 chars */
  double val;

  if (args==nil)
    CallError(stacktop,"format ~f: insufficient args",args,NONCONTINUABLE);

  if (is_float(CAR(args)))
    val=CAR(args)->FLOAT.fvalue;
  else if (is_fixnum(CAR(args)))
    val=(double)intval(CAR(args));
  else
    CallError(stacktop,"format ~f: not a number",CAR(args),NONCONTINUABLE);

  if (add_args==nil)
    strcpy(fmtbuf,"%#f");
  else if (CDR(add_args)==nil)
    sprintf(fmtbuf,"%%%df",intval(CAR(add_args)));
  else
    sprintf(fmtbuf,"%%%d.%df",intval(CAR(add_args)),
	    intval(CDR(add_args)));
  
  sprintf(buf,fmtbuf,val);
  print_string(stacktop,stream,buf);
  
  return CDR(args);
}
EUFUN_CLOSE

/* non-standard */
static EUFUN_3(Fn_format_g,stream,args,add_args)
{
  char buf[512],fmtbuf[40];
  double val;

  if (args==nil)
    CallError(stacktop,"format ~g: insufficient args",args,NONCONTINUABLE);

  if (is_float(CAR(args)))
    val=CAR(args)->FLOAT.fvalue;
  else if (is_fixnum(CAR(args)))
    val=(double)intval(CAR(args));
  else
    CallError(stacktop,"format ~g: not a number",CAR(args),NONCONTINUABLE);

  if (add_args==nil)
    strcpy(fmtbuf,"%#g");
  else if (CDR(add_args)==nil)
    sprintf(fmtbuf,"%%%dg",intval(CAR(add_args)));
  else
    sprintf(fmtbuf,"%%%d.%dg",intval(CAR(add_args)),
	    intval(CDR(add_args)));
  
  sprintf(buf,fmtbuf,val);
  print_string(stacktop,stream,buf);
  
  return CDR(args);
}
EUFUN_CLOSE


/* Input operations */

EUFUN_1(Fn_read_char,stream)
{
  int c;

  c=fgetc(fpof(stream));
  
  if (c==EOF)
    return q_eof;
  else
    {
#ifdef WITH_FUDGE
      {
	extern void yy_reset_stream(LispObject *,FILE *);
	yy_reset_stream(stacktop, fpof(stream));
      }
#endif
      return allocate_char(stacktop,c);
    }
}
EUFUN_CLOSE

EUFUN_2(Fn_ungetc,stream,c)
{
  ungetc(c->CHAR.code,fpof(stream));
#ifdef WITH_FUDGE
      {
	extern void yy_reset_stream(LispObject *,FILE *);
	yy_reset_stream(stacktop, fpof(stream));
      }
#endif
  return lisptrue;
}
EUFUN_CLOSE
/* Read chars until we hit whitespace */
#define READBUFSZ 10
EUFUN_1(Fn_read_line,stream)
{		
  LispObject tmp=nil,oldtmp;
  char buf[READBUFSZ];
  int len=0,i=0,c;
  
  while ((c=getc(fpof(stream)))!=EOF)
    {	
      buf[i]=c;
      if (c == '\n') break;
      i++;
      if (i==READBUFSZ)
	{	/* Grab more... */
	  if (tmp==nil)
	    {
	      tmp=allocate_string(stacktop,buf,READBUFSZ);
	    }
	  else
	    {	
	      oldtmp=tmp;
	      tmp=allocate_string(stacktop,stringof(oldtmp),len+READBUFSZ);
	      strncpy(stringof(tmp)+len,buf,READBUFSZ);
	    }
	  len+=READBUFSZ;
	  i=0;
	  stream=ARG_0(stackbase);
	}
    }	
  
  if (len+i==0)
    return q_eof;

  buf[i]='\0';

  if (tmp==nil)
    return allocate_string(stacktop,buf,i);
  else
    {
      oldtmp=tmp;
      tmp=allocate_string(stacktop,stringof(oldtmp),len+i);
      strcpy(stringof(tmp)+len,buf);
      return tmp;
    }
}
EUFUN_CLOSE

EUFUN_1(Fn_read,stream)
{
  if (stream==nil) 
    return(sys_read(stacktop,stdin));
  else
    return generic_apply_1(stacktop,generic_read,stream);
}
EUFUN_CLOSE

EUFUN_1(Fn_fread,stream)
{
 return(sys_read(stacktop,fpof(stream)));
}
EUFUN_CLOSE

EUFUN_1(Fn_escape_id_p,s)
{
  extern int escaped_id(char *);

  return (escaped_id(stringof(s))
	  ? lisptrue : nil);
}
EUFUN_CLOSE

#if (defined(WITH_BSD_SOCKETS) || defined(WITH_SYSTEMV_SOCKETS))

EUFUN_0(Fn_xpipe_select)
{
  return nil;
}
EUFUN_CLOSE

#else

 /* BSD */

EUFUN_0(Fn_xpipe_select)
{
  fd_set xfds;
  FD_ZERO( &xfds );
  FD_SET( 6, &xfds );
  select(7,&xfds,NULL,NULL,0);
}
EUFUN_CLOSE

#endif

EUFUN_3(Fn_c_write,fd,buffer,nbytes)
{
  int ret;

  ret=write(intval(fd),stringof(buffer),intval(nbytes));
  
/*
  if (ret<0)
    CallError(stacktop,"write[fd]: write failed",fd,NONCONTINUABLE);
*/

  return(allocate_integer(stacktop,ret));
}
EUFUN_CLOSE

EUFUN_3(Fn_c_read,fd,buffer,nbytes)
{
  long int len;
  int ret;

  ret=system_read(stacktop,intval(fd),stringof(buffer),intval(nbytes));
  
/*
  if (ret<1)
    CallError(stacktop,"read[fd]: read failed",fd,NONCONTINUABLE);
*/

  return(allocate_integer(stacktop,ret));
}
EUFUN_CLOSE

EUFUN_1(Fn_input_available,stream)
{
  long int len;

  if (stream==nil) 
    ioctl(fileno(stdin), FIONREAD, &len);
  else
    ioctl(fileno(fpof(stream)), FIONREAD, &len);

  if (len == 0) return nil; 

  return lisptrue;
}
EUFUN_CLOSE

/* JAP additions: 940825 */

EUFUN_2(Fn_c_open,path,oflag)
{
  int ret;
  ret = open(stringof(path),intval(oflag));
  return(allocate_integer(stacktop,ret));
}
EUFUN_CLOSE

EUFUN_0(Fn_c_errno)
{
  return(allocate_integer(stacktop,errno));
}
EUFUN_CLOSE

EUFUN_1(Fn_c_close,fildes)
{
  int ret;
  ret = close(intval(fildes));
  return(allocate_integer(stacktop,ret));
}
EUFUN_CLOSE

EUFUN_3(Fn_c_fcntl,fildes,cmd,values)
{
  int ret;
  ret = fcntl(intval(fildes),intval(cmd),intval(values));
  return(allocate_integer(stacktop,ret));
}
EUFUN_CLOSE

#define NSTREAMS_ENTRIES 42

MODULE Module_nstreams;
LispObject Module_nstreams_values[NSTREAMS_ENTRIES];

void initialise_streams(LispObject *stacktop)
{
#ifdef WITH_FUDGE
  initialise_fudge();
#endif

  open_module(stacktop,
	      &Module_nstreams,	
	      Module_nstreams_values,
	      "streams",
	      NSTREAMS_ENTRIES);
  
  /* For bootstrapping, we use nil, nil and t for stdin, etc.
     These are re-hacked later */
  MakeStdStreams();
  StdIn()=nil;
  StdOut()=nil;
  StdErr()=lisptrue;
  
  q_eof=allocate_char(stacktop,256);
  add_root(&q_eof);

  format_specifiers=allocate_vector(stacktop,256);
  add_root(&format_specifiers);

  make_module_entry(stacktop,"*eof*",q_eof);

  generic_prin
    = make_module_generic(stacktop,"generic-prin",2);
  add_root(&generic_prin);

  generic_write
    = make_module_generic(stacktop,"generic-write",2);
  add_root(&generic_write);

  generic_output
    = make_module_generic(stacktop,"output",2);
  add_root(&generic_output);

  generic_flush
    = make_module_generic(stacktop,"flush",1);
  add_root(&generic_flush);

  generic_read
    = make_module_generic(stacktop,"generic-read",1);
  add_root(&generic_read);

  (void) make_module_function(stacktop,"std-streams",Fn_std_streams,0);
  (void) make_module_function(stacktop,"fopen",Fn_fopen,2);
  (void) make_module_function(stacktop,"fpopen",Fn_popen,2);
  (void) make_module_function(stacktop,"freopen",Fn_reopen,1);
  (void) make_module_function(stacktop,"fseek",Fn_seek,2);
  (void) make_module_function(stacktop,"ftell",Fn_tell,1);
  (void) make_module_function(stacktop,"fflush",Fn_flush,1);
  (void) make_module_function(stacktop,"fclose",Fn_close,1);
  (void) make_module_function(stacktop,"fput",Fn_put,2);

  (void) make_module_function(stacktop,"print-fixnum",Fn_prin_fixnum,2);
  (void) make_module_function(stacktop,"print-list",Fn_prin_list,2);
  (void) make_module_function(stacktop,"prin-object",Fn_prin_object,2);

  (void) make_module_function(stacktop,"std-formatters",Fn_std_formatters,0);
  (void) make_module_function(stacktop,"internal-format",Fn_iformat,3);
  (void) make_module_function(stacktop,"b-formatter",Fn_format_b,3);
  (void) make_module_function(stacktop,"o-formatter",Fn_format_o,3);
  (void) make_module_function(stacktop,"x-formatter",Fn_format_x,3);
  (void) make_module_function(stacktop,"r-formatter",Fn_format_r,3);
  (void) make_module_function(stacktop,"e-formatter",Fn_format_e,3);
  (void) make_module_function(stacktop,"f-formatter",Fn_format_f,3);
  (void) make_module_function(stacktop,"g-formatter",Fn_format_g,3);
  (void) make_module_function(stacktop,"u-formatter",Fn_format_u,3);

  (void) make_module_function(stacktop,"read",Fn_read,1);
  (void) make_module_function(stacktop,"fread",Fn_fread,1);
  (void) make_module_function(stacktop,"fread-line",Fn_read_line,1);
  (void) make_module_function(stacktop,"fread-char",Fn_read_char,1);
  (void) make_module_function(stacktop,"fungetc",Fn_ungetc,2);

  (void) make_module_function(stacktop,"escaped-id-p",Fn_escape_id_p,1);

  (void) make_module_function(stacktop,"c_write",Fn_c_write,3);
  (void) make_module_function(stacktop,"c_read",Fn_c_read,3);
  (void) make_module_function(stacktop,"input_available",Fn_input_available,1);
  (void) make_module_function(stacktop,"xpipe_select",Fn_xpipe_select,0);

  (void) make_module_function(stacktop,"c_open",Fn_c_open,2);
  (void) make_module_function(stacktop,"c_errno",Fn_c_errno,0);
  (void) make_module_function(stacktop,"c_close",Fn_c_close,1);
  (void) make_module_function(stacktop,"c_fcntl",Fn_c_fcntl,3);

  close_module();
}

