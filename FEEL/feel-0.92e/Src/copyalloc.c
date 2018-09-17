 /*	
  * Allocation routines for feel
  *
  */

/* what we need to stay ahead*/
#include "defs.h"
#include "structs.h"
#include "funcalls.h"
#include "global.h"
#include "allocate.h" 
#include "error.h"
#include "table.h"
#include "threads.h" 

/* other junk */
#include "copy.h"

#ifndef DEFAULT_HEAP_SIZE
#define DEFAULT_HEAP_SIZE (4*1024*1024)
#endif

#ifndef DEFAULT_STACK_SPACE_SIZE
#define DEFAULT_STACK_SPACE_SIZE (1*1024*1024)
#endif

#define N_SLOTS_IN_CLASS N_SLOTS_IN_STRUCT(struct class_structure)
#define N_SLOTS_IN_THREAD N_SLOTS_IN_STRUCT(struct thread_structure)

#ifdef ALIGN8
#define ROUNDTO 8
#else
#define ROUNDTO 8
#endif

#define ROUND_ADDR(x) ((((int)x)&(ROUNDTO-1))==0 ? x : x+(ROUNDTO-((int)x&(ROUNDTO-1))))

#ifdef NODEBUG
#define FPRINTF_GC_BUG(x) 
#define GC_BUG(x)
#else
#define GC_BUG(x) x
#define FPRINTF_GC_BUG(x) fprintf x
#endif

LispObject static_ints;
LispObject static_chars;

static void initialise_stack_space(int);

void runtime_initialise_allocator(LispObject *stacktop)
{
  extern void init_allocator(int);
  extern int command_line_heap_size;
  extern int command_line_stack_space_size;
  extern int command_line_cons_percentage;
  extern int command_line_cons_cut_off;

  int heap,stack_space;
  
  heap = (command_line_heap_size == 0
            ? DEFAULT_HEAP_SIZE 
            : command_line_heap_size);

  if (heap < 50)
    heap=heap*1024*1024;


  {
    extern int command_line_cons_percentage;
    extern int command_line_cons_cut_off;
    
    if (command_line_stack_space_size < 50)
      command_line_stack_space_size = command_line_stack_space_size*1024*1024;

    stack_space = (command_line_stack_space_size == 0
		   ? DEFAULT_STACK_SPACE_SIZE
		   : command_line_stack_space_size);
  }

  init_allocator(heap); 
  initialise_stack_space(stack_space); 

  /* Really need a smarter way of doing these... --- like do them last */
  add_root(&state_dynamic_env);
  add_root(&state_last_continue);
  add_root(&state_handler_stack);
  add_root(&state_current_thread);
}

char *allocate_space(LispObject *stacktop,int n)
{
  char* allocate_stack(LispObject *stacktop, int nbytes);  

  return allocate_stack(stacktop,n);
}

void deallocate_space(LispObject*stacktop,char *addr,int siz)
{
  void deallocate_stack(LispObject *, char *, int);

  deallocate_stack(stacktop,addr,siz);
}
void runtime_initialise_collector(LispObject *stacktop)
{

}

#define NOT_YET_DONE(name) \
{ fprintf(stderr,"%s: cannot alloc\n",name) ; return nil;}
  
EUFUN_2(Fn_cons, a1, a2)
{
  LispObject ans;

  ans = allocate_nbytes(stacktop,sizeof(struct cons_structure),TYPE_CONS); 
  
  lval_classof(ans)=Cons;
  ans->CONS.car= ARG_0(stackbase);
  ans->CONS.cdr= ARG_1(stackbase);
  
  return ans;
}
EUFUN_CLOSE

/* Optimised to allow easier code in a lot of places... */
LispObject allocate_n_conses(LispObject *stacktop, int n)
{	
  LispObject xx;
  int i;
#ifndef DGC
  LispObject tmp,prev;
  struct cons_structure *ptr;

  xx=allocate_nbytes(stacktop,n*sizeof(struct cons_structure),TYPE_CONS);
  ptr=&(xx->CONS);
  prev=nil;

  for (i=0; i< n; i++)
    {
      tmp=(LispObject) ptr;
      lval_typeof(tmp)=TYPE_CONS;
      gcof(tmp)=current_space();
#ifdef GENERATIONAL
      ageof(tmp)=0;
#endif
      lval_classof(tmp)=Cons;
      CAR(tmp)=nil;
      CDR(tmp)=prev;
      
      prev=tmp;
      ptr++;
    }
  
  return (LispObject) tmp;
#else
  xx=nil;
  for (i=0; i<n; i++)
    {
      xx=EUCALL_2(Fn_cons,nil,xx);
    }
      
  return xx;

#endif
}

LispObject allocate_n_envs(LispObject *stacktop, int n, LispObject last)
{	
  LispObject xx;
  int i;
  xx=last;
  for (i=0; i< n; i++)
    {
      xx=allocate_env(stacktop,nil,nil,xx);
    }

  return xx;
}

LispObject allocate_class(LispObject *stacktop,LispObject class)
{	/* Really the same as allocate_integer, but we have to be a 
	 * little careful at bootstrap */
  LispObject ans;
  int i;

  STACK_TMP(class);
  if (class==NULL)
    ans = allocate_nbytes(stacktop,sizeof(struct class_structure),TYPE_CLASS);
  else 
    ans = allocate_nbytes(stacktop,
			  sizeof(Object_t)+sizeof(LispObject)*intval(class->CLASS.local_count),
			  TYPE_CLASS);
  UNSTACK_TMP(class);
  lval_classof(ans) = (class==NULL) ? nil : class;

  (ans->CLASS).local_count = (class==NULL) ? nil : allocate_integer(stacktop,0);
  (ans->CLASS).name = unbound;
  (ans->CLASS).superclasses = nil;
  (ans->CLASS).subclasses = nil;
  (ans->CLASS).local_slot_list = nil;
  (ans->CLASS).slot_list = nil;
  (ans->CLASS).nonlocal_slot_list = nil;
  (ans->CLASS).precedence = nil;
  (ans->CLASS).spare1 = nil;
  (ans->CLASS).spare2 = nil;
  
  if (class!=NULL)
    {
      for (i=N_SLOTS_IN_CLASS ; i<intval(class->CLASS.local_count) ; i++)
	slotref(ans,i) = nil;
    }
  return ans;
}

LispObject allocate_instance(LispObject *stacktop,LispObject class)
{
  LispObject ans;
  int i;

  STACK_TMP(class);

  ans=allocate_nbytes(stacktop,sizeof(Object_t)
		      +sizeof(LispObject)*intval(class->CLASS.local_count),TYPE_INSTANCE);

  UNSTACK_TMP(class);
  lval_classof(ans)=class;

  for (i=0; i<intval(class->CLASS.local_count); i++)
    slotref(ans,i)=unbound;

  return ans;
}

LispObject allocate_thread(LispObject *stacktop,int stack_size, 
			   int gc_stack_size, int nslots)
{ 
  char* allocate_stack(LispObject *stacktop, int nbytes);
  /* xxx: need extra slots hack */
  LispObject ans,cont,data;
  int extra;

  extra=nslots>0? nslots-N_SLOTS_IN_THREAD: 0;
  cont = allocate_continue(stacktop);
  STACK_TMP(cont);
  data = allocate_string(stacktop,"",sizeof(struct thread_data));
  memset(stringof(data),sizeof(struct thread_data), 0);

  STACK_TMP(data);

  ans=allocate_nbytes(stacktop,
		      sizeof(struct thread_structure)+extra*sizeof(LispObject),
		      TYPE_THREAD);

  lval_classof(ans) = Thread;
  UNSTACK_TMP(data);
  ans->THREAD.sysdata = data;
  thread_stack_size(ans)=stack_size;
  thread_gc_stack_size(ans) = gc_stack_size;

  ans->THREAD.fun = nil;
  ans->THREAD.args = nil;
  ans->THREAD.value = nil;

  thread_status(ans) = 0;

  ans->THREAD.thd_queue = nil;

  UNSTACK_TMP(cont);
  ans->THREAD.state = cont;
  thread_stack_base(ans) = NULL;
  thread_gc_stack_base(ans) = NULL;

  ans->THREAD.state->CONTINUE.thread=ans;

#ifdef MACHINE_ANY

  thread_stack_base(ans) = (int *) allocate_stack(stacktop+1,stack_size);
  (ans->THREAD.state)->CONTINUE.gc_stack_pointer =
    thread_gc_stack_base(ans) =
      (LispObject *) allocate_stack(stacktop+1,gc_stack_size*sizeof(LispObject));
  
  STACK_TMP(ans);
  cont=EUCALL_2(Fn_cons,function_default_handler,nil);
  UNSTACK_TMP(ans);
  ans->THREAD.state->CONTINUE.handler_stack = cont;
    
#else

  thread_stack_base(ans) = NULL;
  thread_gc_stack_base(ans) = NULL;
  ans->THREAD.state->CONTINUE.gc_stack_pointer = NULL;
  STACK_TMP(ans);
  cont =  EUCALL_2(Fn_cons,function_default_handler,nil);
  UNSTACK_TMP(ans);
  ans->THREAD.state->CONTINUE.handler_stack = cont;

  ans->THREAD.sig_queue=nil;
#endif
  { /* ugh */
    int i;
    if (extra>0)
      for(i=N_SLOTS_IN_THREAD; i<nslots; i++)
	slotref(ans,i) = unbound;
  }
  return ans;
}

LispObject allocate_vector(LispObject *stacktop,int size)
{
  LispObject ans;
  int i;

  ans = allocate_nbytes(stacktop,sizeof(Object_t)+sizeof(int)+size*sizeof(LispObject),
			TYPE_VECTOR);
  
  lval_classof(ans)= Vector;
  
  ans->VECTOR.length=size;

  for(i=0; i<size ; i++)
    vref(ans,i)=nil;

  return ans;
}

LispObject allocate_string(LispObject *stacktop, char *string, int len)
{
  LispObject ans;

  len++;
  len=ROUND_ADDR(len);
  ans = allocate_nbytes(stacktop,sizeof(Object_t)+sizeof(int)+len,
			TYPE_STRING); 
  
  lval_classof(ans)=String;
  ans->STRING.length= len;
  stringof(ans)[len-1]=0;
  strncpy(stringof(ans),string,len);

  return ans;
}

LispObject allocate_symbol(LispObject *stacktop, char *str)
{
  int hash(char *); /* from tables.c */
  extern int command_line_processors; /* yuck! */

  int hv;
  LispObject ans;
  LispObject tmp,tmp2;
  
  tmp=allocate_string(stacktop,str,strlen(str));
  STACK_TMP(tmp);
  hv=hash(str);
  ans=allocate_nbytes(stacktop,sizeof(struct symbol_structure),TYPE_SYMBOL);
  UNSTACK_TMP(tmp);

  lval_classof(ans)=Symbol;
  ans->SYMBOL.pname=tmp;
#ifdef MACHINE_SYSTEMV
  STACK_TMP(ans);
  tmp=allocate_vector(stacktop,command_line_processors);
  STACK_TMP(tmp);
  tmp2=allocate_vector(stacktop,command_line_processors);
  UNSTACK_TMP(tmp);
  UNSTACK_TMP(ans);
  ans->SYMBOL.lmodule=tmp;
  ans->SYMBOL.lvalue=tmp2;
#else
  ans->SYMBOL.lmodule=nil;
  ans->SYMBOL.lvalue=nil;
#endif
  ans->SYMBOL.gvalue = NULL;
  ans->SYMBOL.left = NULL;
  ans->SYMBOL.right = NULL;
  ans->SYMBOL.hash = hv;

  return ans;
}

LispObject allocate_module_function(LispObject *stacktop,
				    LispObject mod,LispObject name,
				    LispObject (*fun)(LispObject*),
				    int code)
{
  LispObject ans;

  STACK_TMP(name); STACK_TMP(mod);
  ans=allocate_nbytes(stacktop,sizeof(struct c_function_structure),TYPE_C_FUNCTION);
  UNSTACK_TMP(mod); UNSTACK_TMP(name);
  lval_classof(ans) = CFunction;

  ans->C_FUNCTION.name = name;
  ans->C_FUNCTION.home = mod;
  ans->C_FUNCTION.argtype = code;
  ans->C_FUNCTION.env = NULL;
  ans->C_FUNCTION.setter = nil;

  ans->C_FUNCTION.func = fun;
  
  return ans;
}

#ifdef NOLOWTAGINTS
LispObject real_allocate_integer(LispObject *stacktop, int n)
{
  LispObject ans;

  if (n>=0 && n<STATIC_INTEGERS)
    return vref(static_ints,n);

  ans=allocate_nbytes(stacktop,sizeof(struct integer_structure),TYPE_INT);

  lval_classof(ans)=Integer;
  intval(ans)=n;

  return ans;
}
#endif

LispObject allocate_float(LispObject *stacktop,double x )
{
  LispObject ans;

  ans=allocate_nbytes(stacktop,sizeof(struct float_structure),TYPE_FLOAT);

  lval_classof(ans)=Real;
  ans->FLOAT.fvalue=x;
  
  return ans;
  
}

static LispObject real_allocate_char(LispObject *stacktop, int x)
{
  LispObject ans;

  ans=allocate_nbytes(stacktop,sizeof(struct character_structure),
		      TYPE_CHAR);
  lval_classof(ans)=Character;
  ans->CHAR.font=0;
  ans->CHAR.code=x;
  return ans;
  
}

LispObject allocate_char(LispObject *stacktop,int x)
{
  if (x<0 || x>MAX_CHAR)
    return (real_allocate_char(stacktop,x));
  else
    return vref(static_chars,x);
}

LispObject allocate_continue(LispObject *stacktop)
{

  LispObject ans;

  ans=allocate_nbytes(stacktop,sizeof(struct continue_structure),TYPE_CONTINUE);

  lval_classof(ans) = Continue;

  (ans->CONTINUE).thread = nil;

  (ans->CONTINUE).value = nil;
  (ans->CONTINUE).target = nil;

  /*  (ans->CONTINUE).machine_state; */
  (ans->CONTINUE).gc_stack_pointer = NULL;
  (ans->CONTINUE).dynamic_env = NULL;
  (ans->CONTINUE).last_continue = nil;
  (ans->CONTINUE).handler_stack = nil;

  (ans->CONTINUE).dp = nil;

  (ans->CONTINUE).live = FALSE;
  (ans->CONTINUE).unwind = FALSE;  
  
  return ans;
}

LispObject allocate_env(LispObject *stacktop, LispObject name, 
			LispObject value, LispObject prev)
{
  LispObject ans;

  STACK_TMP(prev); STACK_TMP(name); STACK_TMP(value);
  ans=allocate_nbytes(stacktop,sizeof(struct env_structure),TYPE_ENV);
  UNSTACK_TMP(value); UNSTACK_TMP(name); UNSTACK_TMP(prev);
  lval_classof(ans) = nil; /* ? */

  ans->ENV.variable = name;
  ans->ENV.value = value;
  ans->ENV.next = prev;

  return ans;
}

LispObject allocate_envimut(LispObject *stacktop, LispObject name, LispObject value, LispObject prev)
{
  LispObject ans;
  
  ans=allocate_env(stacktop,name,value,prev);
  
  lval_typeof(ans)=TYPE_FIXENV;
  return ans;
}

LispObject allocate_special(LispObject *stacktop, 
			    LispObject name, 
			    LispObject (*fn)(LispObject *))
{
  LispObject ans;

  STACK_TMP(name);
  ans=allocate_nbytes(stacktop,sizeof(struct special_structure),TYPE_SPECIAL);
  UNSTACK_TMP(name);

  lval_classof(ans) = Object;

  ans->SPECIAL.name  = name;
  ans->SPECIAL.env   = NULL;
  ans->SPECIAL.func  = fn;

  return(ans);

}


LispObject allocate_i_function(LispObject *stacktop, LispObject mod, 
			       LispObject env, int argcode)
{
  LispObject ans;

  STACK_TMP(mod); STACK_TMP(env);
  ans=allocate_nbytes(stacktop,sizeof(struct i_function_structure),TYPE_I_FUNCTION);

  UNSTACK_TMP(env); UNSTACK_TMP(mod);
  lval_classof(ans)=IFunction;
  ans->I_FUNCTION.name=nil;
  ans->I_FUNCTION.home = mod;
  ans->I_FUNCTION.env = env;
  ans->I_FUNCTION.argtype = argcode;
  
  ans->I_FUNCTION.bvl = nil;
  ans->I_FUNCTION.body = nil;

  return ans;
}


LispObject allocate_i_module(LispObject *stacktop, LispObject name)
{
  LispObject ans;

  STACK_TMP(name);
  ans=allocate_nbytes(stacktop,sizeof(struct i_module_structure), TYPE_I_MODULE);
  UNSTACK_TMP(name);

  lval_classof(ans)=Object;
  ans->I_MODULE.name = name;
  ans->I_MODULE.home = nil;
  ans->I_MODULE.exported_names = nil;
  ans->I_MODULE.bounce_flag = nil;
  ans->I_MODULE.imported_modules = nil; /* HACK !!! GC */
  ans->I_MODULE.bindings = nil;
  
  return ans;
}

#if (defined(WITH_BSD_SOCKETS) || defined(WITH_SYSTEMV_SOCKETS))

LispObject allocate_listener(LispObject *stacktop)
{
  LispObject ans;

  ans=allocate_nbytes(stacktop,sizeof(struct listener_structure), TYPE_LISTENER);
  lval_classof(ans)=nil; /* will be set later */
  return ans;
}


LispObject allocate_socket(LispObject *stacktop)
{
  LispObject ans;
  
  ans=allocate_nbytes(stacktop,sizeof(struct socket_structure), TYPE_SOCKET);
  lval_classof(ans)=nil; /* will be set later */
  return ans;
}
#endif

LispObject allocate_semaphore(LispObject *stacktop)
{
  LispObject ans;
  
  ans=allocate_nbytes(stacktop,sizeof(struct semaphore_structure), TYPE_SEMAPHORE);

  lval_classof(ans)=Object; /* Ugh */
  system_allocate_semaphore(&(ans->SEMAPHORE.semaphore));

  return ans;
}

LispObject allocate_static_vector(LispObject *stacktop,int nslots)
{
  LispObject space;
  int i;

  space=(LispObject) allocate_space(stacktop,sizeof(Object_t)+sizeof(int)+nslots*sizeof(LispObject));
  
  for (i=0; i<nslots; i++)
    vref(space,i)=NULL;

  lval_typeof(space)=TYPE_VECTOR|STATIC_TYPE;
  lval_classof(space)=Vector;
  gcof(space)=current_space();
  ageof(space)=0;
  space->VECTOR.length=nslots;

  return(space);
}

LispObject allocate_static_string(LispObject *stacktop, int size)
{
  LispObject space;
  size++;
  size=ROUND_ADDR(size);
  space=(LispObject) allocate_space(stacktop,sizeof(Object_t)+sizeof(int)+size);
  
  lval_typeof(space)=TYPE_STRING|STATIC_TYPE;
  lval_classof(space)=String;
  gcof(space)=current_space();
  ageof(space)=0;
  space->STRING.length=size;
  return (space);
}


void allocate_static_integers(LispObject *stacktop)
{
#ifdef NOLOWTAGINTS
  int i;

  static_ints=allocate_static_vector(stacktop,STATIC_INTEGERS);
  for (i=0; i<STATIC_INTEGERS; i++)
    {		/* alloc a big integer, then 'fix' it */
      LispObject xx=real_allocate_integer(stacktop,STATIC_INTEGERS);
      intval(xx)=i;
      vref(static_ints,i)=xx;
    }

  add_root(&static_ints);
#endif
}

void allocate_static_chars(LispObject *stacktop)
{
  int i;
  static_chars=allocate_static_vector(stacktop,MAX_CHAR+1);
  
  for (i=0; i<MAX_CHAR+1 ; i++)
    {
      LispObject c=real_allocate_char(stacktop,i);
      vref(static_chars,i)=c;
    }
  add_root(&static_chars);
}

typedef struct free_list_struct
{
  int size;
  struct free_list_struct *next;
} *FreeList;

static SYSTEM_GLOBAL(FreeList, stack_chain);

static int free_count;
static int nfrags;

static void initialise_stack_space(int stackspace)
{
  char *space;
  int allocated=0;
  FreeList *chain_end;
  
  SYSTEM_INITIALISE_GLOBAL(FreeList,stack_chain,NULL);

  chain_end=(FreeList*)&SYSTEM_GLOBAL_VALUE(stack_chain);
  nfrags=0;
  while (allocated < stackspace)
    {
      int sz;
      
      sz=stackspace-allocated;
      if (sz>SYSTEM_MAX_SHARED_SIZE)
	sz=SYSTEM_MAX_SHARED_SIZE;

      space=system_malloc(sz);

      *chain_end=(FreeList)space;

      (*chain_end)->size= sz - sizeof(*chain_end);
      (*chain_end)->next= NULL;
      chain_end=&((*chain_end)->next);
      free_count+=SYSTEM_GLOBAL_VALUE(stack_chain)->size;
      
      allocated+=sz;
      nfrags++;
    }
}

void show_stack_space()
{
  fprintf(stderr,"Stack space: %d remaining, %d fragments\n",free_count, nfrags);
}

/* use header as pointer to prevously allocated stack */

char* allocate_stack(LispObject *stacktop, int nbytes)
{
  FreeList oldstack;
  FreeList *walker;
  char *ret;

  if (nbytes==0)
    return NULL;

  system_open_semaphore(stacktop,&S_G_V(GC_sem)); 
  walker= (FreeList*)&SYSTEM_GLOBAL_VALUE(stack_chain);
  nbytes=ROUND_ADDR(nbytes);

  free_count -= nbytes;

  while ( (*walker)!=NULL)
    {
      if ((*walker)->size+sizeof(*walker)==nbytes)
	{ 
	  ret= (char*) (*walker);
	  *walker=(*walker)->next;
	  nfrags--;
	  FPRINTF_GC_BUG((stderr,"{Cool stack: %x->%x}",ret,ret+nbytes));
	  GC_BUG(memset(ret,'S',nbytes));
	  system_close_semaphore(&S_G_V(GC_sem)); 
	  return ret;
	}
      if ((*walker)->size<nbytes)
	{
	  FPRINTF_GC_BUG((stderr,"[Looking at: %x->%x (%d)]",*walker,(*walker)+(*walker)->size,
			  (*walker)->size));	  
	  walker = &((*walker)->next);
	}
      else
	{
	  ret= ((char *)((*walker)+1))+((*walker)->size-nbytes);
	  (*walker)->size=(*walker)->size-nbytes;
	  GC_BUG(memset(ret,'S',nbytes));
	  FPRINTF_GC_BUG((stderr,"{Alloc stack: %x->%x}",ret,ret+nbytes));
	  system_close_semaphore(&S_G_V(GC_sem)); 
	  return ret;
	}
    }
  fprintf(stderr,"alloc stack: stack wimped out (%d remaining --- probably)\n",free_count);
  system_close_semaphore(&S_G_V(GC_sem)); 
  return NULL;
}

void deallocate_stack(LispObject *stacktop, char *addr,int nbytes)
{
  FreeList old, walker;
  /* Too damm lazy */
  nbytes=ROUND_ADDR(nbytes);

  
  system_open_semaphore(stacktop,&S_G_V(GC_sem)); 
  walker=SYSTEM_GLOBAL_VALUE(stack_chain);
  FPRINTF_GC_BUG((stderr,"{dealloc: %x->%x [%d]",addr,addr+nbytes,nbytes));
  while (   ((char *)walker->next) < addr
	 && walker->next!=NULL)
    {
      /* sanity check */
#if 0	/* sun allocates shared memory in the wrong order */
      if (walker >= walker->next)
	{ 
	  FPRINTF_GC_BUG((stderr,"Rats--- strange chain\n"));
	  system_lisp_exit(1);
	}
#endif
      walker=walker->next;
    }
  /* 3 cases --- at the start */
  if ( ((char *)(walker+1)) + walker->size == addr)
    {
      /* side check for end */

      if (walker->next!=NULL && addr+nbytes == (char *) walker->next)
	{
	  walker->size = walker->size+nbytes
	    +sizeof(*walker)
	      +walker->next->size;
	  walker->next=walker->next->next;
	  free_count+=nbytes+sizeof(*walker);
	  nfrags--;
	  FPRINTF_GC_BUG((stderr,"Filler}"));
	}
      else	
	{
	  walker->size=walker->size+nbytes;
	  free_count+=nbytes;
	  FPRINTF_GC_BUG((stderr,"Start}"));
	}
      system_close_semaphore(&S_G_V(GC_sem)); 
      return;
    }
  /* at the end */
  if ( walker->next!=NULL && addr+nbytes == (char *) walker->next)
    {
      old=walker->next;
      walker->next=(FreeList) addr;
      walker->next->size=nbytes+old->size;
      walker->next->next=old->next;
      free_count+=nbytes;
      FPRINTF_GC_BUG((stderr,"End}"));
      system_close_semaphore(&S_G_V(GC_sem)); 
      return;
    }
  /* in the middle */
  old=walker->next;      
  walker->next=(FreeList) addr;
  walker->next->next=old;
  walker->next->size=nbytes-sizeof(*walker);
  nfrags++;
  free_count+=nbytes-sizeof(*walker);
  FPRINTF_GC_BUG((stderr,"Middle}"));
  system_close_semaphore(&S_G_V(GC_sem)); 
}
  
int dump_obj(unsigned int *x,int s)
{
  int i;
  
  if (s>200) s=16;

  for (i=0; i<s ; i+=4)
    fprintf(stderr,"0x%x: %x %x %x %x\n",
	    x+i,
	    (int)*(x+i),(int)*(x+i+1),(int)*(x+i+2),(int)*(x+i+3));
  return s;
}
  
