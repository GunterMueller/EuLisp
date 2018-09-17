/* ******************************************************************** */
/*  main.c           Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* User top level			                                */
/* ******************************************************************** */

/*
 * $Id: main.c,v 1.5 1994/08/12 10:06:32 djb Exp $
 *
 * $Log: main.c,v $
 * Revision 1.5  1994/08/12  10:06:32  djb
 * increased number of entries in 'others' module to compensate
 * for post-gc-callback function added in basic.c
 *
 * Revision 1.4  1994/04/07  09:42:25  djb
 * added initialise_pvm3 call (only if PVM3 defined)
 *
 * Revision 1.3  1994/04/06  17:16:41  djb
 * added pvm initialisation (only if -DPVM)
 * and lreader initialisation
 *
 * Revision 1.2  1994/03/31  11:26:11  jap
 * added entry to OTHERS_ENTRIES (see also basic.c)
 *
 * Revision 1.1  1994/01/25  13:45:08  djb
 * Initial revision
 *
 * Revision 1.1  1994/01/25  13:29:33  djb
 * Initial revision
 *
 * Revision 2.1  1993/01/19  15:02:02  pab
 * New Version
 *
 * Revision 1.24.1.1  1993/01/17  17:54:41  pab
 * New Version
 * changed name of init module, added newline after load_bytecodes.
 *
 * Revision 1.24  1992/11/26  15:55:40  pab
 * Removed kjp code, added WITH_SYS stuff,.
 *
 * Revision 1.22.1.1  1992/08/06  18:12:29  pab
 * i-function support
 *
 * Revision 1.22  1992/06/18  10:01:24  pab
 * improved includes, decls
 *
 * Revision 1.21  1992/06/16  19:38:08  pab
 * feelrc mods
 *
 * Revision 1.20  1992/06/09  14:03:59  pab
 * more BCI paranoia
 *
 * Revision 1.19  1992/05/28  17:04:32  rjb
 * a NULL -> 0
 *
 * Revision 1.18  1992/05/26  11:28:03  pab
 * map option added
 *
 * Revision 1.17  1992/05/19  11:19:22  pab
 * -boot option
 *
 * Revision 1.16  1992/04/26  21:02:27  pab
 * symbol fixes
 *
 * Revision 1.15  1992/03/13  18:08:06  pab
 * SysV fixes (interpreter thread sort out)
 *
 * Revision 1.14  1992/02/18  11:16:06  pab
 * added handler
 *
 * Revision 1.13  1992/02/11  13:38:32  pab
 * fixed generic version
 *
 * Revision 1.12  1992/02/11  12:06:05  pab
 * handler around load of initcode
 *
 * Revision 1.11  1992/02/10  12:07:02  pab
 * Bytecode support
 *
 * Revision 1.10  1992/01/29  13:42:12  pab
 * sysV fixes
 *
 * Revision 1.9  1992/01/17  22:31:19  pab
 * fixed to load initcode at startup
 *
 * Revision 1.7  1992/01/09  22:28:53  pab
 * Fixed for low tag ints
 *
 * Revision 1.6  1991/12/22  15:14:18  pab
 * Xmas revision
 *
 * Revision 1.5  1991/11/15  13:45:08  pab
 * copyalloc rev 0.01
 *
 * Revision 1.4  1991/10/08  19:27:42  pab
 * arg to init_elvira changed
 *
 * Revision 1.3  1991/09/22  19:14:37  pab
 * Fixed obvious bugs
 *
 * Revision 1.2  1991/09/11  12:07:24  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:49:47  pab
 * Initial revision
 *
 * Revision 1.18  1991/04/03  21:06:36  kjp
 * -cons-cut-off option
 *
 * Revision 1.17  1991/04/03  16:28:06  kjp
 * History modifications - incomplete
 *
 * Revision 1.16  1991/04/02  16:41:32  kjp
 * Conses command line option.
 *
 * Revision 1.15  1991/02/28  14:00:52  kjp
 * Command line stack-space option.
 *
 * Revision 1.14  1991/02/13  18:23:09  kjp
 * Pass.
 *
 */

#define JMPDBG(x)
#define CODBG(x) /* fprintf(stderr,"CODBG:");x;fflush(stderr) */

/*
 * Change Log:
 *   Version 1, April 1989
 *     Read a .feelrc file if it exists - JPff
 *	Various changes for streams
 *	Remove Env argument from make_module_function and make_special 
 *        as always NULL
 *	Initialise threads.
 *      Added a one result history and fiddled with some object definitions.
 */

#include "version.h"

#include "defs.h"
#include "structs.h"
#include "funcalls.h"

#include "error.h"
#include "global.h"
#include "slots.h"
/*#include "compact.h" */
#include "garbage.h" /* What do I need this for */

#include "symboot.h"
#include "modules.h"
#include "toplevel.h"
#include "root.h"
#include "specials.h"
#include "lists.h"
#include "listops.h"
#include "calls.h"
#include "ccc.h"
#include "allocate.h"

#include "modboot.h"

#include "state.h"
#include "macros.h"
#include "semaphores.h"
#include "format.h"
#include "modops.h"
#include "threads.h"
#include "streams.h"
#include "reader.h"
#include "sio.h"
#include "sockets.h"

#ifdef BCI
#include "bvf.h"
#endif

/*
 * Hack number 1A - push everything as yet unmodulised into OTHER
 */

#ifdef WITH_SYS_TIMES
#define OTHER_ENTRIES 23
#else
#define OTHER_ENTRIES 22
#endif

MODULE Module_others;
LispObject Module_others_values[OTHER_ENTRIES];

/*
 * The provided classes / constants / symbols
 */

/* Built in constants */

LispObject nil;
LispObject lisptrue;
LispObject unbound;

/* Root class */

LispObject Object;

/* Meta classes */

LispObject  Standard_Class;
LispObject   Slot_Description_Class;

LispObject Abstract_Class;

LispObject Slot_Description;
LispObject  Local_Slot_Description;

LispObject Basic_Structure;

/* Allocation specifying metaclasses */

LispObject Structure_Class;                /* Analogous to C structs */
LispObject Funcallable_Object_Class;       /* Function forms */
LispObject Generic_Class;
LispObject Pair_Class;
LispObject Unpredictable_Fixed_Size_Class; /* Vector-type things */
LispObject Variable_Size_Keyed_Class;      /* Tabular instances */
LispObject Thread_Class;
LispObject Method_Class;

/* Built in stuff */

LispObject Primitive_Class; 

/* The core building blocks */

LispObject Abstract_Class; /* Meta */
LispObject Number, Complex, Real, Rational, Integer;
LispObject Symbol, Character, String;
LispObject Thread, Continue;
LispObject Function, Generic, Method, IFunction,CFunction;

/* Composites */

LispObject Cons, Vector, Table, Null; /* Empty list... */

/* Special pointer */

LispObject Weak_Wrapper;

/* Flag thing */

LispObject top_level(LispObject*);
extern FILE* current_output;

static char *command_line_boot_file;

/* Quick way of making self evaluating sybols */

void make_special_symbol(LispObject *stacktop, LispObject *objptr, char *name )
{
  *objptr = (LispObject) get_symbol(stacktop, name );
  lval_typeof(*objptr) = TYPE_SYMBOL;
#ifndef DGC
  gcof((*objptr))   = 0;
#endif
  ((*objptr)->SYMBOL).right = NULL;
}

/* Top level thread holder... */

LispObject interpreter_thread;

/* Temporary-ish jump buffer... */

LispObject tl_thread;

jmp_buf temp_buffer;

extern LispObject read_eval_print_continue;
LispObject boot_thread;

int main(int argc, char ** argv)
{
  void load_and_boot(LispObject *);
  extern void runtime_initialise_allocator(LispObject*);
  void configure(int,char **);
  void start_interpreter(LispObject*);

  LispObject *gc_local_stack;

  configure(argc,argv);

  /*

   * System initialisation...

   */

  runtime_initialise_system();     /* Rig system spec stuff */
  runtime_initialise_allocator(NULL);  
  runtime_initialise_garbage_collector(NULL);

#ifdef WITH_BYTECODE
/* Initialize bytecode interpreter stack */

  init_stack();
#endif

  OFF_collect();

  /*

   * We gotta rig up something so that we can use a few basic system
   * functions during the main bootstrap sequence - this implies
   * just setting up what will become the interpreter thread enough
   * to get us moving...

   */

  /*

   * Set up preliminary thread stuff...

   */

  /* Interpreter GC stack (nominal, for bootstrapping)... */

  gc_local_stack = (LispObject*) malloc(4096*sizeof(LispObject*));
  if (gc_local_stack ==  NULL) {
    fprintf(stderr,"Really nasty error: unable to malloc gc_local_stack\n");
    exit(1);
  }

/*  fprintf(stderr,"stack: 0x%x Lim: 0x%x\n",
	  gc_local_stack,
	  gc_local_stack + 4096);*/
  /* Allocate the top level thread... */

  nil = NULL;
  Thread = NULL;

  boot_thread 
    = allocate_thread(gc_local_stack,0,0,0);

  /* Fill in as best we can... */

  thread_stack_base(boot_thread) = NULL;
  thread_gc_stack_base(boot_thread) = gc_local_stack;
  boot_thread->THREAD.state->CONTINUE.gc_stack_pointer = gc_local_stack;

  thread_stack_base(boot_thread) = NULL;
  thread_gc_stack_base(boot_thread) = gc_local_stack;

  thread_stack_size(boot_thread) = 0xffffffff; /* lots'n'lots */
  thread_gc_stack_size(boot_thread) = 100*HUNK_PAGE_SIZE()*sizeof(LispObject*);

  boot_thread->THREAD.fun = nil;
  boot_thread->THREAD.args = nil;
  boot_thread->THREAD.value = nil;
  
  thread_status(boot_thread) = 0;

  boot_thread->THREAD.thd_queue = nil;

  /* Thread continuation... */

  boot_thread->THREAD.state->CONTINUE.thread = boot_thread;

  boot_thread->THREAD.state->CONTINUE.value = nil;
  boot_thread->THREAD.state->CONTINUE.target = nil;

/*  boot_thread->THREAD.state.machine_state; */
  boot_thread->THREAD.state->CONTINUE.gc_stack_pointer = gc_local_stack;
  boot_thread->THREAD.state->CONTINUE.dynamic_env = NULL;
  boot_thread->THREAD.state->CONTINUE.last_continue = nil;
  boot_thread->THREAD.state->CONTINUE.handler_stack = nil;

  boot_thread->THREAD.state->CONTINUE.live = FALSE;
  boot_thread->THREAD.state->CONTINUE.unwind = FALSE;

  /*

   * We have a 'serviceable' thread - initialise the system specific
   * bits for serial initialisation...

   */
  { 
    LispObject *stacktop;
    
    stacktop = load_thread(boot_thread); /* Context to this thread... */
    add_root(&boot_thread);
    load_and_boot(stacktop);          /* Do module boot sequence... */
    
    interpreter_thread=EUCALL_2(Fn_cons,nil,nil);
    read_eval_print_continue=EUCALL_2(Fn_cons,nil,nil);
    tl_thread=EUCALL_2(Fn_cons,nil,nil);

    add_root(&interpreter_thread);
    add_root(&read_eval_print_continue);
    add_root(&tl_thread);

    start_interpreter(stacktop);      /* Start the interpreter... */
  }
}

#define INTERPRETER_THREAD_STACK_SIZE  (96*1024*1)
#define INTERPRETER_THREAD_GC_STACK_SIZE  (48*1024*1)

int command_line_stack_size;

#ifndef MACHINE_ANY

void start_interpreter(LispObject *stacktop)
{
  extern LispObject Fn_thread_start(LispObject*);
  void start_history(void);

  LispObject function_read_eval_print;

  if (command_line_stack_size<1000)
    command_line_stack_size*=1024;

  CAR(interpreter_thread) 
    = allocate_thread(stacktop, command_line_stack_size,
		      command_line_stack_size/2,0);

  function_read_eval_print =
    allocate_module_function(stacktop, nil,nil,top_level,0);

  CAR(interpreter_thread)->THREAD.fun = function_read_eval_print;
  thread_status(CAR(interpreter_thread)) = THREAD_LIMBO;
  system_thread_rig(stacktop,CAR(interpreter_thread));

  /* Install as ready... */

  EUCALL_2(Fn_thread_start,CAR(interpreter_thread),nil);

  CAR(read_eval_print_continue) = allocate_continue(stacktop);
#ifndef KJP
  start_history();
#endif

  /* Store as the top level thread... */
  
  tl_thread = CAR(interpreter_thread);

  /* Name and configuration... */

  printf("EuLISP FEEL: Version (%d.%.02d ",MAJOR_VERSION,MINOR_VERSION);

#ifdef MACHINE_SYSTEMV
  printf("SystemV)");
#endif
#ifdef MACHINE_BSD
  printf("BSD)");
#endif
#ifdef MACHINE_ANY
  printf("Generic)");
#endif
#ifdef FIX_LEVEL
  printf(" (fix %d)",FIX_LEVEL);
#endif

  printf(" %s\n",MAKE_DATE);
  printf("\n");

#ifdef VERSION_MESSAGE
  printf("                    Version Message\n\n");
  printf(VERSION_MESSAGE);
  printf("\n");
#endif

  fflush(stdout);
  ON_collect();
  
  {LispObject xx;

   xx=boot_thread;
   boot_thread=nil;
   runtime_begin_processes(xx->THREAD.state->CONTINUE.gc_stack_pointer);
 }
}

#else

void start_interpreter(LispObject *stacktop)
{
  void start_history(void);

  /* Generate the interpreter thread... */

  CAR(interpreter_thread )
    = allocate_thread(stacktop, 0,INTERPRETER_THREAD_GC_STACK_SIZE,0);
  CAR(interpreter_thread)->THREAD.fun = nil;
  thread_status(CAR(interpreter_thread)) = THREAD_RUNNING;

  CAR(read_eval_print_continue) = allocate_continue(stacktop);

#ifndef KJP
  start_history();
#endif

  /* Store as the top level thread... */

  CAR(tl_thread) = CAR(interpreter_thread);
  /* Name and configuration... */
  ON_collect();

  printf("EuLISP FEEL: Version (%d.%.02d ",MAJOR_VERSION,MINOR_VERSION);

#ifdef MACHINE_SYSTEMV
  printf("SystemV)");
#endif
#ifdef MACHINE_BSD
  printf("BSD)");
#endif
#ifdef MACHINE_ANY
  printf("Generic)");
#endif
#ifdef FIX_LEVEL
  printf(" (fix %d)",FIX_LEVEL);
#endif

  printf(" %s\n",MAKE_DATE);
  printf("\n");

#ifdef VERSION_MESSAGE
  printf("                    Version Message\n\n");
  printf(VERSION_MESSAGE);
  printf("\n");
#endif

  fflush(stdout);

  stacktop = load_thread(CAR(tl_thread)); /* So repl continue has the right thread base */
  ON_collect();
  (void) top_level(stacktop);
}

#endif

void load_and_boot(LispObject *stacktop)
{
  extern MODULE Module_generics;
  extern int gc_enabled;
  extern void initialise_elvira_modules(LispObject *);

  bootstrap(stacktop); /* Bootstrap classes and some special symbols */
  initialise_modules(stacktop);
  initialise_symbols(stacktop); /* Rig up the others */
  initialise_specials(stacktop);
  initialise_root(stacktop);

  /* Open up the other module and do the rest */

  open_module(stacktop,
	      &Module_others,Module_others_values,"others",OTHER_ENTRIES);

  initialise_set(stacktop);
  initialise_basic(stacktop);
  initialise_garbage(stacktop);
  initialise_macros(stacktop);

  close_module();	
  
  /* Initialise the modular sections */

  initialise_error(stacktop);
  initialise_classes(stacktop);
  initialise_streams(stacktop);
  initialise_generics(stacktop);
  initialise_ccc(stacktop);
  initialise_lists(stacktop);
  initialise_tables(stacktop);
  initialise_vectors(stacktop);
  initialise_chars(stacktop);
  initialise_calls(stacktop);
  initialise_arith(stacktop);
  initialise_threads(stacktop);
  initialise_semaphores(stacktop);
  initialise_module_operators(stacktop);
  initialise_sockets(stacktop);
  initialise_bit_vectors(stacktop);

#ifdef WITH_BIGNUMS
  initialise_bignums(stacktop);
#endif

  initialise_bci(stacktop);
  initialise_lreader(stacktop);

#ifdef PVM
  initialise_pvm(stacktop);
#endif

#ifdef PVM3
  initialise_pvm3(stacktop);
#endif

  /* Set up Elvira modules... */

  /* Note: because these may contain init-errors, we provide a handler */

  {
    extern LispObject function_bootstrap_handler;
    LispObject xx;

    EUCALLSET_2(xx,Fn_cons,function_bootstrap_handler,nil);
    HANDLER_STACK() =
      CURRENT_THREAD()->THREAD.state->CONTINUE.handler_stack 
	= xx;
  }

  initialise_elvira_modules(stacktop);
}

LispObject read_eval_print_continue;

/* This top-level is the function which is run on the interpreter thread... */

int command_line_do_done_flag;
int feelrc_read_flag;

LispObject top_level(LispObject *stacktop)
{
  extern char *getenv(char *);
  LispObject get_history_form(LispObject);
  void put_history_form(LispObject *,LispObject);
  int get_history_count(void);
  void initialise_input_processing(void);
  LispObject process_input_form(LispObject);
  LispObject process_result_form(LispObject);
  void make_description_file(LispObject *);

  extern char *command_line_do_string;
  extern int command_line_map_flag;

  LispObject start_module;
  char *start_mod_name;

  if (command_line_map_flag) make_description_file(stacktop);

  CODBG(fprintf(stderr,"Entering toplevel on thread %d\n",THIS_PROCESS));

  SYSTEM_GLOBAL_VALUE(current_interactive_module) =
    get_module(stacktop,sym_root);

  command_line_do_done_flag = FALSE;
  feelrc_read_flag = FALSE;


  /* Load the initialisation module/bootfile */
#ifdef BCI
  if (command_line_boot_file!=NULL)
    {
      LispObject str;
      str=allocate_string(stacktop,command_line_boot_file,strlen(command_line_boot_file)+1);
      EUCALL_1(Fn_load_bytecodes,str);
      print_string(stacktop,StdErr(),"\n");
    }
  else
#endif
    {
      LispObject tmp_sym;
      extern LispObject function_bootstrap_handler;
      extern LispObject function_default_handler;
      LispObject xx,oldstack;

      tmp_sym=get_symbol(stacktop,"init");
      EUCALL_1(load_module,tmp_sym);

    }
  
  if ( (start_mod_name=getenv("FEEL_START_MODULE"))==NULL) 
    start_mod_name="user";

  start_module=get_symbol(stacktop,start_mod_name);
  EUCALL_1(load_module,start_module);

  /* Move to the right module */
  SYSTEM_GLOBAL_VALUE(current_interactive_module)=
    get_module(stacktop,get_symbol(stacktop,start_mod_name));

 reset:

  if (set_continue(stacktop,CAR(read_eval_print_continue))) {

    if (CAR(read_eval_print_continue)->CONTINUE.value == lisptrue) {
      (void) garbage_collect(stacktop);
      print_string(stacktop,StdOut(),"\n");
      fflush(stdout);
    }

    /* Doc Frankenstein would be proud... */

    goto reset;

  }

  /* If do was configured, fix it... */

  if (command_line_do_string != NULL && command_line_do_done_flag == FALSE) {
    LispObject command,ans;
    
    command_line_do_done_flag = TRUE;

    BUFFER_PTR() = 0;
    strcpy(BUFFER_START(),command_line_do_string);

    command = read_object(stacktop);

    EUCALLSET_2(ans,process_top_level_form,
		 SYSTEM_GLOBAL_VALUE(current_interactive_module),
		 command);

  }

  /* Load the configuration file... */

  if (!feelrc_read_flag) {
    char path[1000];
    FILE *inits;
    char *home;

    feelrc_read_flag = TRUE;

    home = getenv("HOME");
    if (home == NULL)	
      path[0]='\0';
    else
      strcpy(path,home);

    strcat(path, FEEL_RC_FILE );
    inits = fopen(path,"r");
    if (inits != NULL) {

      while (TRUE) {
	LispObject form;

	form=sys_read(stacktop,inits);

	if (form == q_eof) break;
	EUCALL_2(process_top_level_form,
		     SYSTEM_GLOBAL_VALUE(current_interactive_module),
		     form);
      }
    }
  }

  while (TRUE) {
    extern char current_prompt_string[];
    char promptbuf[64];
    extern LispObject sym_pling_root;
    extern LispObject sym_pling_exit;
    extern int system_scheduler_number;
    LispObject form, ans;

    sprintf(promptbuf,"%s!%d> ",
	    stringof(SYSTEM_GLOBAL_VALUE(current_interactive_module)
		     ->I_MODULE.name->SYMBOL.pname),
	    get_history_count());
    print_string(stacktop,StdOut(),promptbuf);
    generic_apply_1(stacktop,generic_flush,StdOut());

    EUCALLSET_1(form, Fn_read, StdIn());

    form = get_history_form(form); /* never allocs */
    STACK_TMP(form);
    put_history_form(stacktop, form);
    UNSTACK_TMP(form);
    if (form == q_eof || form == sym_pling_exit) break;
    if (form == sym_pling_root) {
      SYSTEM_GLOBAL_VALUE(current_interactive_module) =
	get_module(stacktop,sym_root);
      ans = nil;
    }
    else {
      EUCALLSET_2(ans,process_top_level_form,
		  SYSTEM_GLOBAL_VALUE(current_interactive_module),
		  form);

    }

    STACK_TMP(ans);
    sprintf(promptbuf,"%s!%d< ",
	    stringof(SYSTEM_GLOBAL_VALUE(current_interactive_module)
		     ->I_MODULE.name->SYMBOL.pname),
	    get_history_count()-1);
    
    print_string(stacktop,StdOut(),promptbuf);
    UNSTACK_TMP(ans);
    generic_apply_2(stacktop,generic_write,ans,StdOut());

    print_string(stacktop,StdOut(),"\n\n");
  }

  fprintf(stderr,"\nEuLISP finishing\n\n");

  system_lisp_exit(1);

  return nil;

}

/* 

 * Configuration... 

 */

char *command_line_do_string;
int command_line_window_flag;
int command_line_heap_size;
int command_line_stack_space_size;
int command_line_map_flag;
int command_line_processors;
int command_line_interface_flag;
void configure(int argc,char **argv)
{
  extern int command_line_x_debug;
  int i = 1;

  /* Nullify options... */

  command_line_do_string = NULL;
  command_line_window_flag = FALSE;
  command_line_heap_size = 0;
  command_line_stack_space_size = 0;
  command_line_map_flag = FALSE;
  command_line_x_debug = FALSE;
  command_line_interface_flag = FALSE;
  command_line_processors = 1;
  command_line_boot_file = NULL;
  command_line_stack_size = INTERPRETER_THREAD_STACK_SIZE;
  while (i < argc) {

    if (strcmp(argv[i],"-do") == 0) {
      if (argc - i < 2) {
	fprintf(stderr,"eulisp: bad -do option\n");
	exit(1);
      }
      command_line_do_string = argv[i+1];
      i+=2;
      continue;
    }

    if (strcmp(argv[i],"-win") == 0) {
      command_line_window_flag = TRUE;
      ++i;
      continue;
    }

    if (strcmp(argv[i],"-xdebug") == 0 
	|| strcmp(argv[i],"-Xdebug") == 0) {
      command_line_x_debug = TRUE;
      ++i;
      continue;
    }

    if (strcmp(argv[i],"-boot") == 0 
	|| strcmp(argv[i],"-Xdebug") == 0) {
      command_line_boot_file = argv[i+1];
      i+=2;
      continue;
    }

    if (strcmp(argv[i],"-heap") == 0) {
      if (argc - i < 2) {
	fprintf(stderr,"eulisp: bad -heap option\n");
	exit(1);
      }
      sscanf(argv[i+1],"%d",&command_line_heap_size);
      i+=2;
      continue;
    }

    if (strcmp(argv[i],"-stack-space") == 0) {
      if (argc - i < 2) {
	fprintf(stderr,"eulisp: bad -stack-space option\n");
	exit(1);
      }
      sscanf(argv[i+1],"%d",&command_line_stack_space_size);
      i+=2;
      continue;
    }

    if (strcmp(argv[i],"-stack-size") == 0) {
      if (argc - i < 2) {
	fprintf(stderr,"eulisp: bad -stack-size option\n");
	exit(1);
      }
      sscanf(argv[i+1],"%d",&command_line_stack_size);
      i+=2;
      continue;
    }

    if (strcmp(argv[i],"-procs") == 0) {
      if (argc - i < 2) {
	fprintf(stderr,"eulisp: bad -procs option\n");
	exit(1);
      }
      sscanf(argv[i+1],"%d",&command_line_processors);
      if (command_line_processors < 1) {
	fprintf(stderr,"eulisp: bad -procs value\n");
	exit(1);
      }
      if (command_line_processors > MAX_PROCESSORS) {
	fprintf(stderr,"eulisp: -procs value higher than %d maximum\n",
		MAX_PROCESSORS);
	exit(1);
      }
      i+=2;
      continue;
    }

    if (strcmp(argv[i],"-map") == 0) {
      command_line_map_flag = TRUE;
      ++i;
      continue;
    }

    if (strcmp(argv[i],"-gen-interfaces") == 0) {
      command_line_interface_flag = TRUE;
      ++i;
      continue;
    }

    fprintf(stderr,"eulisp: unknown option '%s'\n",argv[i]);
    exit(1);

  }

  /* From environment */
}


/* Old hacked histories */

static SYSTEM_GLOBAL(LispObject,history_list);
static SYSTEM_GLOBAL(int,history_list_length);
static SYSTEM_GLOBAL(int,history_count);

int get_history_count()
{
  return(SYSTEM_GLOBAL_VALUE(history_count));
}

LispObject get_history_form(LispObject obj)
{
  LispObject walker;
  int i,n,pos;

  if (!is_symbol(obj)) return(obj);
  if (stringof(obj->SYMBOL.pname)[0] != '!') return(obj);

  i = 1;
  while(stringof(obj->SYMBOL.pname)[i] != '\0') {
    if (!isdigit(stringof(obj->SYMBOL.pname)[i])) return(obj);
    ++i;
  }

  sscanf(&(stringof(obj->SYMBOL.pname)[1]),"%d",&n);

  if (n > SYSTEM_GLOBAL_VALUE(history_count)) return(nil);

  pos = SYSTEM_GLOBAL_VALUE(history_list_length) - n - 1;

  for (walker = SYSTEM_GLOBAL_VALUE(history_list),i = 0; 
       i < pos;
       ++i, walker = CDR(walker));

  return(CAR(walker));
}

void put_history_form(LispObject *stacktop, LispObject form)
{
  LispObject tmp;
  ++SYSTEM_GLOBAL_VALUE(history_count);
  ++SYSTEM_GLOBAL_VALUE(history_list_length);
  EUCALLSET_2(tmp, Fn_cons,
	      form,SYSTEM_GLOBAL_VALUE(history_list));
  SYSTEM_GLOBAL_VALUE(history_list)=tmp;
}

void start_history()
{
  SYSTEM_INITIALISE_GLOBAL(LispObject,history_list,nil);
  SYSTEM_INITIALISE_GLOBAL(int,history_list_length,0);
  SYSTEM_INITIALISE_GLOBAL(int,history_count,0);

  ADD_SYSTEM_GLOBAL_ROOT(history_list);
}

