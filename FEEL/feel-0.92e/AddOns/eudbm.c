/*
  Project: Databases
  File: eudbm.c
  Date: 18/10/90
*/

/*
  Database module for eulisp
  Provides base-level routines
  as per NDBM manual.
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

#include "obread.h"
#include "feel_malloc.h"

/* This may not be on some machines -- but gdbm sure is */
#include "ndbm.h"
#define dbm_hash _gdbm_hash
#define MAXOBJLEN 16384
#define EUDEBUG(x) x
extern int errno;


#include <fcntl.h>

#define ALLOCATE_PAIR(car,cdr)   EUCALL_2(Fn_cons,(car),(cdr))
#define ARG_4(stack) (*(stack+4))

/* need to define new class --- dbm-file */
/* We assume that keys are strings and 
   records are numbers, symbols, strings, lists and vectors
   Hopfully this is fairly general, and quite efficient.
*/

#define DBMOF(x)  *((DBM **)(stringof(x))) 
#define SETDBMOF(x,y)  *((DBM**)(stringof(x)))=y;

/* an initarg or two*/
static LispObject sym_name;
static LispObject sym_mode;
static LispObject sym_readonly;
static LispObject sym_readwrite;
static LispObject sym_create;

extern LispObject search_keylist(LispObject *,LispObject,LispObject);

EUFUN_1(Fn_open_dbase,lst)
{
  LispObject name,tmp;
  int flags,create=FALSE;
  DBM* dbase;
  name=search_keylist(stacktop,lst,sym_name);
  
  if (name==nil)
    CallError(stacktop,"Open dbase: No name supplied",nil,NONCONTINUABLE);
  
  if (is_symbol(name))
    name=name->SYMBOL.pname;

  tmp=search_keylist(stacktop,lst,sym_mode);
  
  flags= (tmp==sym_readonly ? O_RDONLY 
	  : tmp==sym_readwrite ? O_RDWR : O_RDONLY);
  if (search_keylist(stacktop,lst,sym_create)==lisptrue)
    flags |= O_CREAT;
  
  fprintf(stderr,"Opening: %s %x",stringof(name),flags);
  dbase=dbm_open(stringof(name),flags,0740);
  
  if (dbase==NULL)
    {
      perror("open");
      CallError(stacktop,"Couldn't open dbase",name,NONCONTINUABLE);
    }

  tmp=allocate_string(stacktop,"",sizeof(DBM*));
  SETDBMOF(tmp,dbase);
  return tmp;
}
EUFUN_CLOSE

/* 
 * For extendability, if we don't have a database, we call these...
 */

LispObject generic_dbase_fetch, generic_dbase_store;

EUFUN_3( Gf_generic_dbase_fetch,dbase, key,reader)
{
  return(generic_apply_3(stacktop,generic_dbase_fetch,dbase,key,reader));
}

EUFUN_CLOSE

EUFUN_4( Gf_generic_dbase_store, dbase,
				 key,
				 value,
				reader)
{
  return(generic_apply_4(stacktop,generic_dbase_store,dbase,key,value,reader));
}
EUFUN_CLOSE

/* all objects stored as: type, value pairs
   Note that a limit of 4K is imposed on the length of a record + key.
   */
EUFUN_3( Fn_dbase_fetch,dbase, key,reader_maybe)
{
  LispObject ret;
  datum dkey;
  datum result;
  unsigned char *ptr; /* need a copy of the datapointer... */

  dbm_clearerr( DBMOF(dbase));
  if (!is_string(key))
    CallError(stacktop,"dbase_fetch: Key must be a string",key,NONCONTINUABLE);
  
  dkey.dptr = stringof(key);
  dkey.dsize = strlen(stringof(key)) + 1;
  
  result = dbm_fetch( DBMOF(dbase), dkey);
  
  if (result.dptr == NULL)
    { 	
      extern int gdbm_errno;

      fprintf(stderr,"dbm_fetch dbm_err: %d\n",gdbm_errno);
      dbm_clearerr( DBMOF(dbase));
      return nil;
    } 

  /* It would be nice to allocate this locally */
  ptr=(unsigned char *)result.dptr;
  ret=read_obj(stacktop,&ptr,reader_maybe);
  return ret;
}
EUFUN_CLOSE

EUFUN_4( Fn_dbase_store, dbase, key, record,reader)
{
  int dbase_store(DBM *, datum, datum, int);
  datum dkey;
  datum drecord;
  static unsigned char *buf=NULL;
  unsigned char *ptr;
  int len=0;
  
  if (buf == NULL)
    {
      buf = (unsigned char *) feel_malloc(MAXOBJLEN);
    }
  dbm_clearerr(DBMOF( dbase));

  if (!is_string(key))
    CallError(stacktop,"dbase_store: Key must be a string",key,NONCONTINUABLE);

  dkey.dsize = strlen(stringof(key)) + 1;
  dkey.dptr = stringof(key);

  ptr = &buf[0];
  /* XXX */
  write_obj(stacktop,record,&ptr,reader);
  /* YYY */
  len=ptr-buf;

  if (len >= MAXOBJLEN)
    CallError(stacktop,"dbase-store: Overflowed buffer",nil,NONCONTINUABLE);

  drecord.dptr = (char *) &buf[0];	
  if (len + dkey.dsize & 1) len ++;

  drecord.dsize= len;

  if (dbm_store(DBMOF(dbase),
		dkey, drecord, DBM_REPLACE) < 0)
    {	
      perror("store");
      dbm_clearerr( DBMOF( dbase));
      CallError(stacktop,"dbm_store error",nil,NONCONTINUABLE);
    }

  return lisptrue;
}
EUFUN_CLOSE

EUFUN_2( Fn_dbase_delete, dbase, key)
{
  datum dkey;

  if (!is_string(key))
    CallError(stacktop,"dbase_delete: Key must be a string",key,NONCONTINUABLE);

  dkey.dptr = stringof(key);
  dkey.dsize = strlen(stringof(key));
  
  if (dbm_delete( DBMOF(dbase),dkey)<0)
    return nil;
  else return lisptrue;
}
EUFUN_CLOSE

EUFUN_1( Fn_dbase_close, dbase)
{
  
  dbm_close( DBMOF(dbase));
	    
  return lisptrue;
}
EUFUN_CLOSE

EUFUN_1( Fn_dbase_firstkey, dbase)
{
  datum key;

  key = dbm_firstkey( DBMOF( dbase));
	    
  if (key.dptr == NULL) 
    return nil;
  
  *(key.dptr+key.dsize) = '\0';
  return (allocate_string(stacktop,key.dptr,key.dsize));
}
EUFUN_CLOSE


EUFUN_1( Fn_dbase_nextkey, dbase)
{
  datum key;

  key = dbm_nextkey( DBMOF(dbase));
	    
  if (key.dptr == NULL) 
    return nil;
  
  *(key.dptr+key.dsize) = '\0';
  return (allocate_string(stacktop,key.dptr,key.dsize));

}
EUFUN_CLOSE

EUFUN_1( Fn_string_hash, string)
{
  extern unsigned long dbm_hash(char *,int);

  unsigned long i;

  if (!is_string(string))
    CallError(stacktop,"string_hash: not a string",string,NONCONTINUABLE);

  i = dbm_hash(stringof(string),strlen(stringof(string)));
  
#ifdef PARANOID
   printf("string: (%d)[%s] hash: %d\n",
	 strlen(string->STRING.value),
	 string->STRING.value,
	 i); 
#endif
								   
  return allocate_integer(stacktop,abs(i));
}
EUFUN_CLOSE

EUFUN_2( Md_db_generic_prin, db,  str)
{
  char buf[50];
  if (!is_stream(str))
    CallError(stacktop,"generic-prin: bad stream",str,NONCONTINUABLE);
  
  sprintf(buf,"#<Database %x>",(int)DBMOF(db));
  print_string(stacktop,str,buf);
  
  return(db);
}
EUFUN_CLOSE

static EUFUN_0( Fn_dbase_info)
{
#ifdef __FILE__
  printf("%s compiled: %s\n", __FILE__,MAKE_DATE);
#else
  syntax error
#endif
  return nil;
}
EUFUN_CLOSE

#define DBM_ENTRIES (12)
MODULE Module_dbm;
LispObject Module_dbm_values[DBM_ENTRIES];

void INIT_database(LispObject *stacktop)
{
  extern LispObject generic_generic_prin;
  extern LispObject get_symbol(LispObject *,char *);


  EUDEBUG(fprintf(stderr,"Version Date: %s\n", MAKE_DATE));

  sym_name = get_symbol(stacktop,"name");
  add_root(&sym_name);
  sym_mode = get_symbol(stacktop,"mode");
  add_root(&sym_mode);
  sym_readonly = get_symbol(stacktop,"readonly");
  add_root(&sym_readonly);
  sym_readwrite = get_symbol(stacktop,"read-write");
  add_root(&sym_readwrite);
  sym_create = get_symbol(stacktop,"create");
  add_root(&sym_create);

  open_module(stacktop,&Module_dbm,Module_dbm_values,
	      "dbm",DBM_ENTRIES);

  (void) make_module_function(stacktop,"dbase-open",Fn_open_dbase,-1);
  (void) make_module_function(stacktop,"dbase-fetch",Fn_dbase_fetch,-3);
  (void) make_module_function(stacktop,"dbase-store",Fn_dbase_store,-4);
  (void) make_module_function(stacktop,"dbase-delete",Fn_dbase_delete,2);
  (void) make_module_function(stacktop,"dbase-close",Fn_dbase_close,1);
  (void) make_module_function(stacktop,"dbase-firstkey",Fn_dbase_firstkey,1);
  (void) make_module_function(stacktop,"dbase-nextkey",Fn_dbase_nextkey,1);
  (void) make_module_function(stacktop,"dbm-info",Fn_dbase_info,0);
  (void) make_module_function(stacktop,"string-hash",Fn_string_hash,1);
  /* We don't install methods on generic-dbase-store, etc */
  generic_dbase_store
    = make_wrapped_module_generic(stacktop,"generic-dbase-store",3,Gf_generic_dbase_store);

  generic_dbase_fetch
    = make_wrapped_module_generic(stacktop,"generic-dbase-fetch",2,Gf_generic_dbase_fetch);

  make_module_function(stacktop,"Md_generic_prin_Database",
		       Md_db_generic_prin,2);

  close_module();
}


