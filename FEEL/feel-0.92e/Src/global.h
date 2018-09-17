/* ******************************************************************** */
/*  global.h         Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Global variables			                                */
/* ******************************************************************** */

/*
 * $Id: global.h,v 1.1 1994/01/25 13:45:36 djb Exp $
 *
 * $Log: global.h,v $
 * Revision 1.1  1994/01/25  13:45:36  djb
 * Initial revision
 *
 * Revision 2.1  1993/01/17  17:25:21  pab
 * 17 Jan 1993 The next generation...
 *
 * Revision 1.14  1992/11/26  15:47:18  pab
 * Env Support, IFunctions, Etc
 *
 * Revision 1.12  1992/08/06  18:11:24  pab
 * i function support
 *
 * Revision 1.11  1992/06/18  19:36:19  pab
 * Tidied up relics
 *
 * Revision 1.10  1992/04/27  21:56:27  pab
 * changed malloc decl
 *
 * Revision 1.9  1992/04/26  21:01:22  pab
 * definition of add_root and allocate_static_vector
 *
 * Revision 1.8  1992/02/27  15:48:17  pab
 * lose alloc_condition
 *
 * Revision 1.7  1992/02/10  12:08:45  pab
 * macroised allocate_integer
 *
 * Revision 1.6  1992/01/29  13:41:50  pab
 * sysV fixes
 *
 * Revision 1.5  1992/01/10  15:17:56  pab
 * changed allocate_integer for fixnums
 *
 * Revision 1.4  1991/12/22  15:14:11  pab
 * Xmas revision
 *
 * Revision 1.3  1991/11/15  13:44:52  pab
 * copyalloc rev 0.01
 *
 * Revision 1.2  1991/09/11  12:07:16  pab
 * 11/9/91 First Alpha release of modified system
 *
 * Revision 1.1  1991/08/12  16:49:39  pab
 * Initial revision
 *
 * Revision 1.7  1991/05/16  11:23:16  pab
 * 'C' garbage collector support
 *
 * Revision 1.5  1991/02/13  18:21:16  kjp
 * New class declarations.
 *
 */

/*
 * Change Log:
 *   Version 1, April 1989
 *     Reserved slot declarations and make_special_symbol prototype
 */

#ifndef GLOBAL_H
#define GLOBAL_H

#include <stdio.h>
#ifndef SETJMP_H
#define SETJMP_H
#include <setjmp.h>
#endif

#ifdef WITH_BIGNUMS
extern void initialise_bignums(void);
extern LispObject Big_Integer;
#endif

extern LispObject ObList;

extern LispObject Object;
extern LispObject nil;
extern LispObject lisptrue;
extern LispObject unbound;

/* Meta classes */


extern LispObject Object;
extern LispObject  Standard_Class;
extern LispObject   Slot_Description_Class;
extern LispObject     Slot_Description;
extern LispObject      Local_Slot_Description;

extern LispObject Abstract_Class;

/* Allocation specifying metaclasses */

extern LispObject Structure_Class;                /* Like to C structs */
extern LispObject Funcallable_Object_Class;       /* Function forms */
extern LispObject Generic_Class;
extern LispObject Pair_Class;
extern LispObject Unpredictable_Fixed_Size_Class; /* Vector-type things */
extern LispObject Variable_Size_Keyed_Class;      /* Tabular instances */
extern LispObject Thread_Class;
extern LispObject Method_Class;

/* The core building blocks */

extern LispObject Abstract_Class; /* Meta */
extern LispObject Number, Real, Integer;
extern LispObject Symbol, Character, String;
extern LispObject Thread, Continue;
extern LispObject Function, Generic, Method,CFunction,IFunction;

/* Composites */

extern LispObject Cons, Vector, Table, Null;

/* Pointer */

extern LispObject Weak_Wrapper;

/* KJP prototypes */

extern LispObject Slot_Initarg;
extern LispObject Default_Initargs;

extern LispObject Object_Class;     /* Reserved slot symbols */
extern LispObject Class_Name;
extern LispObject Class_Parent;
extern LispObject Class_Children;
extern LispObject Class_Instance_Description;

extern LispObject Slot_Class; /* A slot option */

extern LispObject Slot_Name;                    /* Slot descriptor fields */
extern LispObject Slot_Position;
extern LispObject Slot_Reader;
extern LispObject Slot_Writer;
extern LispObject Slot_Initform;  /* Local */
extern LispObject Slot_Value;     /* Shared */

extern void make_special_symbol(LispObject*, LispObject *, char * );

extern void put_table( LispObject, LispObject );

extern void bootstrap(LispObject*);
extern LispObject Fn_plus(LispObject*);
extern LispObject Fn_difference(LispObject*);

/* Vectors */

extern void initialise_vectors(LispObject* );

/* Others I needed prototypes for */

EUDECL(Fn_symbolvalue);

/* KJP def end */

extern LispObject q_eof;

int add_root(volatile LispObject *);
EUDECL( Fn_cons);
extern LispObject allocate_symbol(LispObject*,char *);
extern LispObject get_symbol(LispObject*,char *); /* Use this one */
LispObject allocate_vector(LispObject *,int);
LispObject allocate_static_vector(LispObject *,int);
LispObject allocate_static_string(LispObject *, int);
extern LispObject allocate_function(int,LispObject(*)(), int, LispObject);

extern LispObject Fn_nreverse(LispObject*);
extern LispObject make_table(LispObject*);
extern LispObject Fn_table_ref(LispObject*);
extern LispObject Fn_table_ref_setter(LispObject*);
extern LispObject Fn_length(LispObject*);
extern LispObject Fn_nconc(LispObject *);

extern LispObject allocate_char(LispObject*,int);
extern LispObject allocate_string(LispObject*,char *,int);


#ifdef NOLOWTAGINTS
#define STATIC_INTEGERS 1024
extern LispObject static_ints;

extern LispObject real_allocate_integer(LispObject*, int);
#define allocate_integer(stacktop,x) \
  ((x>=0 && x<STATIC_INTEGERS) ? vref(static_ints,x) : real_allocate_integer(stacktop,x))
    
#else
#define allocate_integer(waste,x) (mk_fixnum(x))
#endif

#define MAX_CHAR 256 /* 0-255 + EOF */

extern LispObject allocate_float(LispObject*,double);
extern LispObject allocate_continue(LispObject*);
extern LispObject allocate_thread(LispObject*,int, int, int);
extern LispObject allocate_module(LispObject*,LispObject, LispObject, LispObject);
extern LispObject make_module_function(LispObject*,
				       char *, LispObject(*)(), int);
extern LispObject make_special(char *, LispObject(*)());
extern LispObject allocate_env(LispObject*,LispObject, LispObject, LispObject);
extern LispObject allocate_envimut(LispObject*,
				   LispObject, LispObject, LispObject);

extern LispObject allocate_condition_class(LispObject*,int, LispObject, 
					   LispObject, LispObject);
extern LispObject allocate_class(LispObject*,LispObject);
extern LispObject allocate_instance(LispObject*,LispObject);

extern void initialise_input(LispObject*);
extern void re_initialise_input(void);
extern LispObject sym_quote;
extern LispObject sym_quasiquote;
extern LispObject sym_unquote;
extern LispObject sym_progn;
extern LispObject sym_defun;
extern LispObject sym_defglobal;
extern LispObject sym_setq;

extern void initialise_output(LispObject*);
extern void initialise_eval(void);
extern LispObject sym_lambda;
extern void initialise_basic(LispObject*);
extern void initialise_generics(LispObject*);
extern void initialise_chars(LispObject*);
extern void initialise_streams(LispObject*);
extern void initialise_tables(LispObject*);
extern void initialise_set(LispObject*);
extern void initialise_error(LispObject*);
extern void initialise_arith(LispObject*);
extern void initialise_threads(LispObject*);
extern void initialise_modules(LispObject*);
extern void initialise_classes(LispObject*);
extern void initialise_bit_vectors(LispObject *);
extern LispObject sym_handler, sym_accept, sym_decline;
extern LispObject sym_dynamic;
extern LispObject sym_dynamic_let;

#ifdef CGC
#define malloc gc_malloc
#endif

#ifdef NEEDS_MALLOC
#ifdef __STDC__
extern void *malloc(int); /* not size_t yet */
#else
extern char* malloc(int);
#endif
#endif

#ifndef HAS_BCOPY
#define bcopy(x,y,z) memcpy(y,x,z)
#endif
/*
 * Globally used thread information... 
 */

#include "state.h"

#define STACK(x) 0
#define UNSTACK(n) 0

#define N_SLOTS_IN_STRUCT(x) \
  (((sizeof(x))-sizeof(Object_t))/sizeof(LispObject))


#endif /* GLOBAL_H */
/* End of global.h */
