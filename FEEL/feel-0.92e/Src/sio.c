/* ******************************************************************** */
/* sio.c             Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* String IO (mainly for sockets)	                                */
/* ******************************************************************** */

#define PAUSE() 

/*
 * Change Log:
 *   Version 1, June 1990
 */

#include <string.h>

#include "funcalls.h"
#include "defs.h"
#include "structs.h"
#include "error.h"
#include "global.h"

#include "allocate.h"

#include "symboot.h"
#include "syssockets.h"
#include "sio.h"

/*

 * Socket reader/writer... 

 */

SYSTEM_THREAD_SPECIFIC_DECLARATION(LispObject,socket_buffer_form);
SYSTEM_THREAD_SPECIFIC_DECLARATION(char *,socket_buffer);
SYSTEM_THREAD_SPECIFIC_DECLARATION(int,socket_buffer_ptr);

#define BUFFER_LEFT() (SOCKET_BUFFER_SIZE - \
		       SYSTEM_THREAD_SPECIFIC_VALUE(socket_buffer_ptr))
#define BUFFER_PTR() (SYSTEM_THREAD_SPECIFIC_VALUE(socket_buffer_ptr))
#define BUFFER() (SYSTEM_THREAD_SPECIFIC_VALUE(socket_buffer) +\
		  SYSTEM_THREAD_SPECIFIC_VALUE(socket_buffer_ptr))
#define BUFFER_START() (SYSTEM_THREAD_SPECIFIC_VALUE(socket_buffer))
#define BUFFER_FORM() (SYSTEM_THREAD_SPECIFIC_VALUE(socket_buffer_form))

/*

 * Writing... 

 */

void write_symbol(LispObject *stacktop,LispObject sym)
{
  int len = strlen(stringof(sym->SYMBOL.pname));

  if (BUFFER_LEFT() <= len)
    CallError(stacktop,"socket-write: form too big",BUFFER_FORM(),NONCONTINUABLE);

  sprintf(BUFFER(),"%s",stringof(sym->SYMBOL.pname));

  BUFFER_PTR() += len;
}

void write_integer(LispObject *stacktop,LispObject i)
{
  char buf[50];
  int len;

  /* Bugger length... */

  sprintf(buf,"%d\0",intval(i));

  len = strlen(buf);

  if (BUFFER_LEFT() <= len)
    CallError(stacktop,
	      "socket-write: form too big",BUFFER_FORM(),NONCONTINUABLE);

  strcpy(BUFFER(),buf);

  BUFFER_PTR() += len;
}

void write_float(LispObject *stacktop,LispObject f)
{
  char buf[50];
  int len;

  sprintf(buf,"%lf\0",f->FLOAT.fvalue);
  len = strlen(buf);

  if (BUFFER_LEFT() <= len)
    CallError(stacktop,
	      "socket-write: form too long",BUFFER_FORM(),NONCONTINUABLE);

  strcpy(BUFFER(),buf);
  BUFFER_PTR() += len;
}

void write_string(LispObject s)
{
  sprintf(BUFFER(),"\"%s\"",stringof(s));
  BUFFER_PTR() += strlen(stringof(s)) + 2;
}

void write_cons(LispObject *stacktop,LispObject l)
{
  if (BUFFER_LEFT() < 3)
    CallError(stacktop,
	      "socket-write: form too big",BUFFER_FORM(),NONCONTINUABLE);

  if (l == nil) {
    sprintf(BUFFER(),"()");
    BUFFER_PTR() += 2;
    return;
  }

  sprintf(BUFFER(),"(");
  
  BUFFER_PTR() += 1;

  while (is_cons(CDR(l))) {
    
    write_object(stacktop,CAR(l));

    sprintf(BUFFER()," ");

    BUFFER_PTR() += 1;

    l = CDR(l);

  }

  if (CDR(l) != nil) {

    write_object(stacktop,CAR(l));

    sprintf(BUFFER()," . ");

    BUFFER_PTR() += 3;

    write_object(stacktop,CDR(l));

  }
  else {

    write_object(stacktop,CAR(l));

  }

  sprintf(BUFFER(),")");

  ++BUFFER_PTR();
}

void write_object(LispObject *stacktop,LispObject obj)
{
  switch (typeof(obj)) {

  case TYPE_SYMBOL: 
    write_symbol(stacktop,obj);
    return;
  case TYPE_INT:
    write_integer(stacktop,obj);
    return;
  case TYPE_FLOAT:
    write_float(stacktop,obj);
    return;
  case TYPE_NULL:
  case TYPE_CONS:
    write_cons(stacktop,obj);
    return;
  case TYPE_STRING:
    write_string(obj);
    return;
  default:
    CallError(stacktop,"socket-write: unwriteable object",obj,NONCONTINUABLE);

  }
}

/* 

 * Reading... 

 */

#define iswhitespace(c) (c == ' ' || c == '\t' || c == '\n')

#define BUFFER_PEEK() (*((SYSTEM_THREAD_SPECIFIC_VALUE(socket_buffer) \
			  + SYSTEM_THREAD_SPECIFIC_VALUE(socket_buffer_ptr))))

LispObject read_number(LispObject *stacktop)
{
  int floating;
  char *ptr;

  floating=FALSE;
  ptr=BUFFER();
  
  ++BUFFER_PTR();

  while(isdigit(BUFFER_PEEK()) ||
	(BUFFER_PEEK() == '.') || (BUFFER_PEEK() == 'e') ||
	(BUFFER_PEEK() == 'E') || (BUFFER_PEEK() == '-') ||
	(BUFFER_PEEK() == '+'))
  {
      if (!(isdigit(BUFFER_PEEK()))) floating = TRUE;
      ++BUFFER_PTR();
  }

  while(iswhitespace(BUFFER_PEEK())) ++BUFFER_PTR();

  if (floating)
  {
      double num;
      sscanf(ptr,"%lf",&num);
      return((LispObject) allocate_float(stacktop,num));
  }
  else
  {
      int num;
      sscanf(ptr,"%d",&num);
      return((LispObject) allocate_integer(stacktop,num));
  }
}

LispObject read_symbol(LispObject *stacktop)
{
  char name[100];
  int i = 0;

  while (!iswhitespace(BUFFER_PEEK()) 
	 && BUFFER_PEEK() != ')'
	 && BUFFER_PEEK() != '.'
	 && BUFFER_PEEK() != '\0') {

    name[i] = BUFFER_PEEK();
    ++BUFFER_PTR();
    ++i;

  }

  name[i] = '\0';

  while(iswhitespace(BUFFER_PEEK())) ++BUFFER_PTR();

  return(get_symbol_by_copying(stacktop,name));
}

LispObject read_string(LispObject *stacktop)
{
  char string[160];
  int i = 0;

  ++BUFFER_PTR();

  while (BUFFER_PEEK() != '"') {

    string[i] = BUFFER_PEEK();
    ++BUFFER_PTR(); ++i;

  }

  string[i] = '\0';

  ++BUFFER_PTR();

  while(iswhitespace(BUFFER_PEEK())) ++BUFFER_PTR();

  return((LispObject) allocate_string(stacktop,string,i));

}
    
LispObject read_list(LispObject *stacktop)
{
  extern LispObject Fn_nconc(LispObject*);
  LispObject read_object(LispObject*);
  LispObject kludge = nil;

  PAUSE();

  ++BUFFER_PTR();

  while (BUFFER_PEEK() != ')' && BUFFER_PEEK() != '.') {
    LispObject xx;
    STACK_TMP(kludge);
    EUCALLSET_2(xx, Fn_cons, read_object(stacktop), nil);
    UNSTACK_TMP(kludge);
    EUCALLSET_2(kludge, Fn_nconc, kludge, xx);

    while (iswhitespace(BUFFER_PEEK())) ++BUFFER_PTR();

    PAUSE();

  }

  if (BUFFER_PEEK() == '.') {

    ++BUFFER_PTR();
    EUCALLSET_2(kludge, Fn_nconc,kludge,read_object(stacktop));
    while (iswhitespace(BUFFER_PEEK())) ++BUFFER_PTR();

    if (BUFFER_PEEK() != ')')
      CallError(stacktop,"socket-read: invalid list (. a b)",BUFFER_FORM(),
		NONCONTINUABLE);

  }

  ++BUFFER_PTR();

  while(iswhitespace(BUFFER_PEEK())) ++BUFFER_PTR();

  return(kludge);
}

LispObject read_quote(LispObject *stacktop)
{
  LispObject read_object(LispObject*);

  LispObject kludge = nil;

  ++BUFFER_PTR();

  STACK(kludge);

  EUCALLSET_2(kludge, Fn_cons,read_object(stacktop),nil);
  EUCALLSET_2(kludge, Fn_cons,sym_quote,kludge);

  UNSTACK(1);

  return(kludge);
}
  
LispObject read_object(LispObject *stacktop)
{
  char c = BUFFER_PEEK();

  while (iswhitespace(c)) {

    ++BUFFER_PTR();
    c = BUFFER_PEEK();

  }

  PAUSE();

  if (c == '(') return(read_list(stacktop));
  if (c == '\'') return(read_quote(stacktop));
  if (c == '"') return(read_string(stacktop));
  if (isdigit(c)) return(read_number(stacktop));
  if (c == '-' || c == '+')
  {
      ++BUFFER_PTR();
      c = BUFFER_PEEK();
      --BUFFER_PTR();
      if (isdigit(c)) return(read_number(stacktop));
  }

  return(read_symbol(stacktop));
}

