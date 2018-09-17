/*
 * Bytecode Interpreter for Feel
 */
char *interpret_c="$id$";

#include <stdio.h>

#include "defs.h"
#include "structs.h"
#include "funcalls.h"
#include "global.h"
#include "ngenerics.h"
#include "modules.h"
#include "bvf.h"
#include "allocate.h"
#include "modboot.h"
#include "bootstrap.h"
#include "error.h"
#include "reader.h"
/* Definition of the bytecodes */  
#define COUNT_BYTES /* ---- I want to see what goes on... */
#include "iset.h"
#include "interpret.h"
#include "bytecodes.h"
#include "reader.h"
#include "threads.h"

/* classes */
static LispObject ByteFunction_Class;
static LispObject ByteFunction,ExtByteFunction;

#ifdef BCI

/* Boot Modules */
#define MAX_BOOT_MODULES 50

BC_GLOBALS()

/* search path for boot file */
static SYSTEM_GLOBAL(LispObject,list_boot_path);

/* Function that returns to 'c' */
static LispObject Cb_generic_lookup;

/* Interface from the world */
LispObject compute_and_apply_method();
LispObject call_method();
LispObject module_apply_args();


/* The biggie */
LispObject interpret_bytes(LispObject *stacktop, bytecode *start_pc, LispObject context)
{
  /* locals for a few specials */
  LispObject BCtrue=lisptrue;
  LispObject BCnil=nil;
  LispObject BC_globals;
  bytecode *pc;
  LispObject *sp;
  LispObject this_context;

  BC_INITIALISE_GLOBALS();

  while (TRUE)
    {
      BC_PRESWITCH();
      switch(*(pc++))
	{
	  
	  BC_CASE(BC_NOP);
	  
	  /* Globals, etc */
	  BC_CASE(BC_PUSH_GLOBAL);
	  BC_CASE(BC_SET_GLOBAL);
	  BC_CASE(BC_PUSH_SPECIAL);
	  BC_CASE(BC_PUSH_STATIC);
	  BC_CASE(BC_PUSH_FIXNUM);
	  BC_CASE(BC_PUSH_SMALL_FIXNUM);
	  BC_CASE(BC_SET_STATIC);

	  /* stack refs */
	  BC_CASE(BC_PUSH_NTH);
	  BC_CASE(BC_PUSH_NTH_0);
	  BC_CASE(BC_PUSH_NTH_1);
	  BC_CASE(BC_PUSH_NTH_2);
	  BC_CASE(BC_PUSH_NTH_3);
	  BC_CASE(BC_SET_NTH);
	  
	  
	  /* env reference */
	  BC_CASE(BC_ENV_REF);
	  BC_CASE(BC_SET_ENV);
	  BC_CASE(BC_POP_ENV);
	  BC_CASE(BC_MAKE_ENV);

	  /* object reference */
	  BC_CASE(BC_VREF);
	  BC_CASE(BC_SET_VREF);
	  BC_CASE(BC_SLOT_REF);
	  BC_CASE(BC_SLOT_REF_0);
	  BC_CASE(BC_SLOT_REF_1);
	  BC_CASE(BC_SET_SLOT);
	  BC_CASE(BC_SET_SLOT_1);
	  BC_CASE(BC_SET_TYPE);
	  
	  /* Stack abuse */
	  BC_CASE(BC_SLIDE_STACK);
	  BC_CASE(BC_SLIDE_1);
	  BC_CASE(BC_SWAP);
	  BC_CASE(BC_DROP);
	  BC_CASE(BC_DROP_1);

	  /* Leaping merrily */
	  BC_CASE(BC_BRANCH);
	  BC_CASE(BC_BRANCH_NIL);

	  /* Calling things */
	  BC_CASE(BC_APPLY_ARGS);
	  BC_CASE(BC_APPLY_ANY);
	  BC_CASE(BC_APPLY_BVF);
	  BC_CASE(BC_APPLY_CFN);
	  BC_CASE(BC_APPLY_CFN2);
	  BC_CASE(BC_APPLY_METHODS);
	  BC_CASE(BC_APPLY_METHOD_LIST);

	  BC_CASE(BC_PUSH_LABEL);
	  
	  /* and return */
	  BC_CASE(BC_RETURN);
	  /* real return */
	  BC_CASE(BC_EXIT);

	  /* allocation */	
	  BC_CASE(BC_CONS);
	  BC_CASE(BC_ALLOC_CLOSURE);
	  BC_CASE(BC_ALLOC_EXT_CLOSURE);

	  /* Tests */
	  BC_CASE(BC_NULLP);
	  BC_CASE(BC_EQP);
	  BC_CASE(BC_CONSP);
	  
	  BC_CASE(BC_CONTEXT); 
	  
	  /* Inlined functions */
	  BC_CASE(BC_ASSQ);
	  BC_CASE(BC_MEMQ);
	  BC_CASE(BC_SCANQ);
	  /* darn, no such bytecode...*/
	  BC_NOINSTRUCT(*(pc-1));
	}
      Cb_generic_lookup=BCnil;
    }
  /* not ever */
  return nil; 
}


/* Returns a closure which will execute from 0 */
/* It is vital that the vector is not GC'd */
EUFUN_4(Fn_add_codevector,bytes,nbytes, statics, nstatics)
{
  LispObject new_closure;
  LispObject ptr,codevector, slotvector;
  int i,lim=intval(nbytes);
  bytecode *space;

  codevector=allocate_static_string(stacktop,lim);
  slotvector = allocate_static_vector(stacktop, intval(nstatics));
  vref(static_vectors,SYSTEM_GLOBAL_VALUE(static_count))=slotvector;
  
  space=(bytecode *)stringof(codevector);

  ptr=bytes;

  for (i=0; i<lim ; i++)
    {
      if (!is_fixnum(CAR(ptr)))
	CallError(stacktop,"add codevector: bad byte",CAR(ptr),NONCONTINUABLE);
      
      if (intval(CAR(ptr))>255)
	CallError(stacktop,"add codevector: bad byte number",CAR(ptr),NONCONTINUABLE);

      space[i]=(bytecode)intval(CAR(ptr));
      ptr=CDR(ptr);
    }
  
  ptr=statics;
  for (i=1 ; i<intval(nstatics) ; i++)
    {
      vref(slotvector,i)=CAR(ptr);
      ptr=CDR(ptr);
    }

  vref(slotvector,0)=codevector;
  /* Allocate a new closure and interpret it. */
  new_closure=allocate_instance(stacktop,ByteFunction);
  lval_typeof(new_closure)=TYPE_B_FUNCTION;

  bytefunction_offset(new_closure)=allocate_integer(stacktop,0);
  bytefunction_nargs(new_closure)=allocate_integer(stacktop,0);
  bytefunction_env(new_closure)=nil;
  bytefunction_globals(new_closure)=slotvector; /* XXX: GC proof */
  SYSTEM_GLOBAL_VALUE(static_count)++;
  
  return(apply_nary_bytefunction(stacktop,0,new_closure));
}
EUFUN_CLOSE

#ifdef WITH_SPECIAL_METHODS
/* Nary methods --- needed here 'cos the interpreter needs them */
LispObject apply_special_method(LispObject *stackbase, int nargs, LispObject meth)
{
  LispObject *stacktop=stackbase+nargs-1;

  BC_METHOD_SWITCH(stacktop,intval(special_method_id(meth)));
  
  return (*stackbase);
}
#endif

#define BUFSIZE 1024
EUFUN_1(Fn_load_bytecodes,name)
{
  LispObject slotvector,*slots,codevector,stream, walker;
  char buf[BUFSIZE], path[200];
  int nslots,nbytes,i;
  FILE *file=NULL;
  bytecode *code;

  if (!is_string(name))
    CallError(stacktop,
	      "load-bytecodes: not a string",name,NONCONTINUABLE);

  walker = SYSTEM_GLOBAL_VALUE(list_boot_path);
  while (is_cons(walker) && file==NULL) 
  {
      LispObject dir;

      if (!is_string((dir = CAR(walker))))
	  CallError(stacktop,
		    "load-bytecodes: bad search directory",dir,NONCONTINUABLE);

      (void) strcpy(path,stringof(dir));
      (void) strcat(path,DIR_SEP);
      (void) strcat(path,stringof(name));
      (void) sprintf(buf,"%s.ebc",path);

      if ((file = fopen(buf,"r")) == NULL)
	  walker = CDR(walker);
  }

  if (file==NULL)
  {	
      fprintf(stderr,
	      "cannot open file %s.ebc\n",stringof(name));
      system_lisp_exit(1);
  }
  else
  {
      fprintf(stderr,"%s\n",buf);

      fgets(buf,BUFSIZE,file);
  
      if (strcmp(buf,"ASCIIBYTES\n")==0)
      {
	  fgets(buf,BUFSIZE,file);
	  nslots=atoi(buf);
	  fgets(buf,BUFSIZE,file);
	  nbytes=atoi(buf);

	  codevector= allocate_static_string(stacktop,nbytes);      
	  code=(bytecode *) stringof(codevector);
	  slotvector=allocate_static_vector(stacktop,nslots);
	  vref(static_vectors,SYSTEM_GLOBAL_VALUE(static_count))=slotvector;

/*      fprintf(stderr,"code: %d bytes, %d slots\n",nbytes,nslots);*/
	  STACK_TMP(slotvector);
	  
	  for (i=0 ; i<nbytes ; i++)
	  {	
	      if (fgets(buf,BUFSIZE,file)==NULL)
		  perror("fgets");
	      
	      code[i]=(bytecode) (atoi(buf));
	  }
	  fclose(file);
      }
      else
      {	 
	  fprintf(stderr,"%s\n",buf);
	  CallError(stacktop,"Unknown format: %s\n",nil,NONCONTINUABLE);
      }
  }
  
  /* Load the statics --- should be done in lisp but what the hell... */

  sprintf(buf,"%s.est",path);
  file=fopen(buf,"r");

  if (file==NULL)
  {
      fprintf(stderr,
	      "cannot open file %s.est\n",stringof(name));
      system_lisp_exit(1);
  }
  else
  {
      LispObject new;

      fprintf(stderr,"%s\n",buf);
      
      new=sys_read(stacktop,file);
      nslots=intval(new);
      for (i=0; i<nslots ; i++)
      {
	  new=sys_read(stacktop,file);
	  vref(vref(static_vectors,SYSTEM_GLOBAL_VALUE(static_count)),i)=new;
      }
#ifdef WITH_FUDGE
      {
	  extern int yy_close_stream(FILE*);
	  yy_close_stream(file);
      }
#else
      system_fclose((file->STREAM).handle);
#endif
  }
  vref(slotvector,0)=codevector;
  /* Allocate a new closure and interpret it. */
  {
      LispObject apply_nary_bytefunction(LispObject *, int, LispObject);
      LispObject new_closure;
  
      new_closure=allocate_instance(stacktop,ByteFunction);
      lval_typeof(new_closure)=TYPE_B_FUNCTION;

      bytefunction_offset(new_closure)=allocate_integer(stacktop,0);
      bytefunction_nargs(new_closure)=allocate_integer(stacktop,0);
      bytefunction_env(new_closure)=nil;
      bytefunction_globals(new_closure)=slotvector; /* XXX: GC proof */
      SYSTEM_GLOBAL_VALUE(static_count)++;
      GLOBAL_REF(BC_Debug)=lisptrue;
      return(apply_nary_bytefunction(stacktop,0,new_closure));
  }
}
EUFUN_CLOSE


EUFUN_2(Fn_set_module_statics,module,vect)
{
  int i;
  
  module->C_MODULE.values=vect;

  return nil;
}
EUFUN_CLOSE

LispObject apply_nary_bytefunction(LispObject *stackbase, int nargs, LispObject fn)
{
  bytecode *start;
  LispObject this_vector,this_context,this_code; /* to make reify do the business */
  LispObject rfn,*fake_sp;
  int i;
  
  if (is_cons(fn))
    rfn=method_function(CAR(fn));
  else 
    rfn=fn;
  /* move the arguments up a little --- top first */
  
  for (i=nargs-1; i>=0 ; i--)
    *(stackbase+i+3)= *(stackbase+i);

  /* Place the exit function in the return address */	
  fake_sp=stackbase-1;
  start=(bytecode *)stringof(vref(return_context,0));
  this_context=return_context;
  STASH_PC(fake_sp,start);

  /* hack fn slot */
  *(stackbase+2)=fn;

  /* Hack env slot */
  *(stackbase+nargs+3)=bytefunction_env(rfn);
  this_context=bytefunction_globals(rfn);
  /* Work out where to start (also updates this_vector)*/
  start=BF2PC(rfn);  
  
  return(interpret_bytes(stackbase+nargs+4,start,this_context));
}

EUFUN_0(Fn_print_counts)
{
  PRINT_COUNTS;

  return nil;
}
EUFUN_CLOSE

void add_boot_module(LispObject mod)
{
  if (static_vectors==NULL)
    {
      static_vectors=allocate_static_vector(NULL,MAX_MODS); /* NULL is a hack */
      add_root(&static_vectors);
      boot_modules=allocate_static_vector(NULL,MAX_BOOT_MODULES);
      add_root(&boot_modules);
    }

  vref(static_vectors,boot_module_count)=mod->C_MODULE.values;
  vref(boot_modules,boot_module_count)=mod;
  boot_module_count++;
}

EUFUN_0(Fn_boot_module_list)
{
  LispObject lst,end;
  int i;
  
  lst=EUCALL_2(Fn_cons,nil,nil);
  end=lst; /* not gc safe */
  for (i=1; i<boot_module_count; i++)
    { 
      LispObject tmp;

      tmp=EUCALL_2(Fn_cons,vref(boot_modules,i),nil);
      CDR(end)=tmp;
      end=tmp;
    }
  return(lst);
}
EUFUN_CLOSE

static EUFUN_2(Fn_set_global,n,val)
{
  GLOBAL_REF(intval(n))=val;

  return val;
}
EUFUN_CLOSE

static EUFUN_0(Fn_get_codepos)
{
  return (allocate_integer(stacktop,SYSTEM_GLOBAL_VALUE(static_count)-1));
}
EUFUN_CLOSE


#define BCI_ENTRIES 10
#define FIRST_USER_CODE 32
MODULE Module_bci;
LispObject Module_bci_values[BCI_ENTRIES];

void initialise_bci(LispObject *stacktop)
{
  extern char *getenv(char *);
  extern LispObject Fn_nconc(LispObject*);
  char *path_list;
  int i;

  SYSTEM_INITIALISE_GLOBAL(LispObject,list_boot_path,nil);
  ADD_SYSTEM_GLOBAL_ROOT(list_boot_path);

  /* Initialise the boot path... */
  
  path_list = getenv(BOOT_PATH_NAME);

  if (path_list == NULL)
  {
      SYSTEM_GLOBAL_VALUE(list_boot_path)
	  = EUCALL_2(Fn_cons,
		     allocate_string(stacktop,".",1),
		     SYSTEM_GLOBAL_VALUE(list_boot_path));
  }
  else
  {
    char *nxt, buf[1024];

    path_list = (char *)strcpy(buf,path_list); /* avoid trampling the env */
    
    nxt = (char *)strtok(path_list,":");
    while (nxt != NULL)
    {
      LispObject xx;
      xx = allocate_string(stacktop,nxt,strlen(nxt));
      EUCALLSET_2(xx, Fn_cons, xx,nil);
      EUCALLSET_2(SYSTEM_GLOBAL_VALUE(list_boot_path), 
		  Fn_nconc,SYSTEM_GLOBAL_VALUE(list_boot_path), xx);
      nxt = (char *)strtok(NULL,":");
    }
  }
  
/*  fprintf(stderr,"Bytecodes compiled on: %s\n", MAKE_DATE);*/
  
  SYSTEM_INITIALISE_GLOBAL(int,static_count,FIRST_USER_CODE);
  global_vector=allocate_vector(stacktop,N_GLOBALS);
  add_root(&global_vector);

  ByteFunction_Class = (LispObject) allocate_class(stacktop,Standard_Class);  
  set_class_size(stacktop,ByteFunction_Class,Object, N_SLOTS_IN_CLASS);
  add_root(&ByteFunction_Class);

  ByteFunction = (LispObject) allocate_class(stacktop,Standard_Class);
  ExtByteFunction = (LispObject) allocate_class(stacktop,Standard_Class);
  set_class_size(stacktop,ByteFunction,Object, N_SLOTS_IN_BYTEFUNCTION);
  set_class_size(stacktop,ExtByteFunction,Object,N_SLOTS_IN_BYTEFUNCTION+1);
  add_root(&ByteFunction);
  add_root(&ExtByteFunction);


  open_module(stacktop,
	      &Module_bci,Module_bci_values,"bci",BCI_ENTRIES);
  
  (void) make_module_entry(stacktop,"<bytefunction-class>",ByteFunction_Class);
  (void) make_module_entry(stacktop,"<bytefunction>",ByteFunction);
  (void) make_module_entry(stacktop,"<extended-bytefunction>",ExtByteFunction);
  (void) make_module_function(stacktop,"add-code-vector",Fn_add_codevector,4);
  (void) make_module_function(stacktop,"load-bytecodes",Fn_load_bytecodes,1);
  (void) make_module_function(stacktop,"set-module-statics",Fn_set_module_statics,2);
  (void) make_module_function(stacktop,"boot-module-list",Fn_boot_module_list,0);
  (void) make_module_function(stacktop,"byte-counts",Fn_print_counts,0);
  (void) make_module_function(stacktop,"get-bci-codepos",Fn_get_codepos,0);
  (void) make_module_function(stacktop,"set-bc-global",Fn_set_global,2);
  close_module();
  
  
  {
    LispObject tmp=allocate_static_string(stacktop,4);
    stringof(tmp)[0]=(char)BC_EXIT;
    return_context=allocate_static_vector(stacktop,1);
    vref(return_context,0)=tmp;
    
    add_root(&return_context);
  }
}

/* Debugger helper functions... */

int debug_off()
{
  GLOBAL_REF(BC_Debug)=nil;
  return 0;
}

int debug_on()
{
  GLOBAL_REF(BC_Debug)=lisptrue;
  return 1;
}

#else /* BCI */

/* just enough stubs to get going */

static EUFUN_2(Fn_set_global,n,val)
{
  return val;
}

EUFUN_CLOSE
#define N_SLOTS_IN_BYTEFUNCTION 5
#define BCI_ENTRIES 4
MODULE Module_bci;
LispObject Module_bci_values[BCI_ENTRIES];

void initialise_bci(LispObject *stacktop)
{
  ByteFunction_Class = (LispObject) allocate_class(stacktop,Standard_Class);  
  set_class_size(stacktop,ByteFunction_Class,Object, N_SLOTS_IN_CLASS);
  add_root(&ByteFunction_Class);

  ByteFunction = (LispObject) allocate_class(stacktop,Standard_Class);
  ExtByteFunction = (LispObject) allocate_class(stacktop,Standard_Class);
  set_class_size(stacktop,ByteFunction,Object, N_SLOTS_IN_BYTEFUNCTION);
  set_class_size(stacktop,ExtByteFunction,Object,N_SLOTS_IN_BYTEFUNCTION+1);
  add_root(&ByteFunction);
  add_root(&ExtByteFunction);


  open_module(stacktop,
	      &Module_bci,Module_bci_values,"bci",BCI_ENTRIES);
  
  (void) make_module_entry(stacktop,"<bytefunction-class>",ByteFunction_Class);
  (void) make_module_entry(stacktop,"<bytefunction>",ByteFunction);
  (void) make_module_entry(stacktop,"<extended-bytefunction>",ExtByteFunction);
  (void) make_module_function(stacktop,"set-bc-global",Fn_set_global,2);

  close_module();
}

#endif
