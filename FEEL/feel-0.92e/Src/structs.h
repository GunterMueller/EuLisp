/* ******************************************************************** */
/*  structs.h        Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Basic definitions of tags and structures                             */
/* ******************************************************************** */

/*
 * $Id: structs.h,v 1.3 1994/02/08 11:35:12 djb Exp $
 *
 * $Log: structs.h,v $
 * Revision 1.3  1994/02/08  11:35:12  djb
 * removed Env typedef
 *
 *
 * Change Log:
 *   Version 1, April 1989
 *   added a little support for classes RJB
 *   hacked it about a bit KJP
 *   added semaphores KJP
 */

#ifndef STRUCTS_H
#define STRUCTS_H

#include <stdio.h>

#ifdef WITH_BIGNUMS
#include "BigZ.h"
#endif
#undef BIGNUM

#ifndef SETJMP_H
#define SETJMP_H
#include <setjmp.h>
#endif

/* Load system types... */

#include "system_t.h"

/*#include "compact.h"*/
/* Primitive types... */

/* indiacte that ob can be swept */

#define CALLABLE_TYPE 0x80
#define STATIC_TYPE   0x40

#define TYPE_UNUSED 	-1

#define TYPE_ENV	0x30
#define TYPE_FIXENV     0x31
#define TYPE_MBIND	0x32 /* Not really envs, but close enough */
#define TYPE_FIXMBIND  0x33
#define TYPE_EXP_MBIND 0x34
#define TYPE_EXP_FIXMBIND 0x35


#define TYPE_CONS	0x1
#define TYPE_CHAR	(0x2)
#define TYPE_STRING	(0x3)

#define TYPE_SYMBOL     (0x6)
#define TYPE_THREAD	(0xb)
#define TYPE_STREAM	(0xc)
#define TYPE_CLASS	(0xd)
#define TYPE_INSTANCE	(0xe)
#define TYPE_SPECIAL	(0xf)
#define TYPE_VECTOR	0x10

#define TYPE_INT	(0x11)
#define TYPE_FLOAT	(0x12)

#define TYPE_SEMAPHORE  (0x13)
#define TYPE_LISTENER   (0x14)
#define TYPE_SOCKET     (0x15)
#define TYPE_NULL       (0x16)
#define TYPE_WEAK_WRAPPER 0x17

#define TYPE_CONTINUE	(0x18)

#define TYPE_C_MODULE   (0x20)
#define TYPE_I_MODULE   (0x21)
#define TYPE_C_FUNCTION (0x22 | CALLABLE_TYPE)
#define TYPE_I_FUNCTION (0x23 | CALLABLE_TYPE)
#define TYPE_GENERIC    (0x24 | CALLABLE_TYPE)
#define TYPE_METHOD     0x25
#define TYPE_SPECIAL_METHOD 0x26

#define TYPE_C_MACRO    (0x22 )
#define TYPE_I_MACRO    (0x23 )

#define TYPE_B_FUNCTION (0x27 | CALLABLE_TYPE)
#define TYPE_B_MACRO	(0x27)

/* Primitive accessors... */
#ifdef NOLOWTAGINTS
#define typeof(p)  	((p)->OBJECT.header.type)
#define classof(p)      ((p)->OBJECT.header.class)
#else
#define typeof(p)       (((int)p) & 1 ? TYPE_INT: ((p)->OBJECT.header.type))
#define classof(p) 	(((int)p) & 1 ? Integer: ((p)->OBJECT.header.class))
#endif
#define type_of(p)      typeof(p)
#define gcof(p)         (((p)->OBJECT).header.gc)
#define gc_of(p)        gcof(p)
#define ageof(p)	((p)->OBJECT.header.age)
#define lval_classof(p)  ((p)->OBJECT.header.class)
#define lval_typeof(p)   ((p)->OBJECT.header.type)

#define class_of(p)     classof(p)

/* Primitive type testers... */

#define is_cons(p)      (typeof(p) == TYPE_CONS)
#define is_char(p)      (typeof(p) == TYPE_CHAR)
#define is_string(p)    ((typeof(p) &0xbf) == TYPE_STRING)
#define is_table(p)     (classof(p) == Table)
#define is_symbol(p)    (typeof(p) == TYPE_SYMBOL)
#define is_function(p)  (typeof(p) & CALLABLE_TYPE)
#define is_macro(p)     (typeof(p) == TYPE_C_MACRO || typeof(p) == TYPE_I_MACRO || typeof(p) == TYPE_B_MACRO)
#define is_static(p)	(typeof(p) & STATIC_TYPE)
#define is_module(p)    ((typeof(p) == TYPE_I_MODULE)  | \
			 (typeof(p) == TYPE_C_MODULE))
#define is_special(p)   (typeof(p) == TYPE_SPECIAL)
#define is_thread(p)    (typeof(p) == TYPE_THREAD)
#define is_stream(p)    (typeof(p) == TYPE_STREAM)
#ifdef NOLOWTAGINTS
#define is_fixnum(p)    (typeof(p) == TYPE_INT)
#else
#define is_fixnum(p)	(((int) (p)) &1)
#define mk_fixnum(x) 	((LispObject) (((x)<<1) | 1))
#endif

#define is_float(p)     (typeof(p) == TYPE_FLOAT)
#define is_vector(p)    ((typeof(p)&(STATIC_TYPE-1)) == TYPE_VECTOR)
#define is_continue(p)	(typeof(p) == TYPE_CONTINUE)
#define is_env(p)	(typeof(p) == TYPE_ENV || typeof(p)==TYPE_FIXENV)
#define is_bind(x) ( (typeof(x)&0xf0) == TYPE_ENV)


#define is_c_function(p) (typeof(p) == TYPE_C_FUNCTION)
#define is_c_module(p)  (typeof(p) == TYPE_C_MODULE)
#define is_i_function(p) (typeof(p) == TYPE_I_FUNCTION)
#define is_i_module(p)  (typeof(p) == TYPE_I_MODULE)
#define is_c_macro(p)   (typeof(p) == TYPE_C_MACRO)
#define is_i_macro(p)   (typeof(p) == TYPE_I_MACRO)
#define is_b_function(p) (typeof(p)==TYPE_B_FUNCTION)
#define is_b_macro(p)	(typeof(p) == TYPE_B_MACRO)

#define is_semaphore(p) (typeof(p) == TYPE_SEMAPHORE)
#define is_listener(p)  (typeof(p) == TYPE_LISTENER)
#define is_socket(p)    (typeof(p) == TYPE_SOCKET)
#define is_weak_wrapper(p) (typeof(p) == TYPE_WEAK_WRAPPER)

#define is_e_function(p) (0)
#define is_e_macro(p) (0)

/* Other macros... */

#define null(p)      ((LispObject)(p) == nil)
#define consp(p)     (is_cons(p) && (p) != nil)
#define symbolp(p)   (is_symbol(p) || (p) == nil)
#define CAR(p)	     (((p)->CONS).car)
#define CDR(p)	     (((p)->CONS).cdr)


typedef union lispunion *LispObject;

typedef struct Object_struct
{
  short type;
  char gc;
  unsigned char age;
  LispObject class;
} Object_t;

struct env_structure {
  Object_t	header;
  LispObject    variable;
  LispObject    value;
  LispObject 	next;
};

/* the top most class object */

struct object_structure {
  Object_t	header;
  LispObject    slots[1];	/* the other slots */
};


struct integer_structure {
  Object_t 	header;
  int		value_part;
};
#ifdef NOLOWTAGINTS
#define intval(x) ((x)->INT.value_part)
#else
#define intval(x) (((int)x)>>1)
#endif

/* low tag ints */



struct float_structure {
  Object_t 	header;
  double	fvalue;
};


struct character_structure {
  Object_t header; 
  short font;
  short code;
};

struct symbol_structure {
  Object_t	header;
  int 		hash;	  /* hash value cache */
  LispObject    lmodule;  /* Module lookup cache for the interpreter */
  LispObject	lvalue;   /* Part II */
  LispObject	gvalue;   /* Dynamic global value */
  LispObject    pname;

  LispObject left;
  LispObject right;
};

#define vref(v,n) (&((v)->VECTOR.base))[n]
#define vrefupdate(v,n,obj) (vref(v,n)=(obj))
#define vector_length(x) ((x)->VECTOR.length)

struct vector_structure {
  Object_t header;
  int length;			/* for now */
  LispObject base;   		
};

#ifdef WITH_SMALL_CONSES
struct cons_structure {
  short		type;
  short		gc;
  LispObject	car;
  LispObject	cdr;
};
#else
struct cons_structure {
  Object_t header;
  LispObject	car;
  LispObject	cdr;
};
#endif

struct string_structure {
  Object_t header;
  int length;
  char value; /* really a c-string --- Should these be CHARs ?? */
};

#define stringof(x)\
  (&((x)->STRING.value))

struct continue_structure {
  Object_t header;

  LispObject  *gc_stack_pointer; /* Interpreter state */
  /* Bytecode state? */

  jmp_buf       machine_state;

  int           live;
  int           unwind;
  LispObject    value;     /* Returned with... */
  LispObject    target;    /* When bouncing unwind protects... */

  LispObject    thread;

  LispObject    dynamic_env;
  LispObject    last_continue;
  LispObject    handler_stack;

  LispObject    dp;  /* Elvira state */
};

typedef struct thread_data {
  LispObject *gc_stack_base;
  short signalled;
  short status;
  int stack_size;
  int gc_stack_size;
  int *stack_base;
} *ThreadData;

struct thread_structure {
  Object_t header;
  
  LispObject 	sysdata; /* pointer to thread data */
  LispObject 	state;

  LispObject    fun;
  LispObject    args;
  LispObject    value;

  LispObject    thd_queue;
  LispObject 	sig_queue;
};

struct semaphore_structure {
  Object_t header;
  SystemSemaphore semaphore; /* Just a hacked wrapper */
};

struct class_structure {
  Object_t header;
  LispObject	local_count;   /* Number of local slots */
  LispObject	name;	       /* Name of the class (NOT binding name) */
  LispObject	superclasses;  /* Direct parents */
  LispObject    subclasses;    /* Direct subclasses */
  LispObject    local_slot_list;    /* local slot descriptions (fed to init-structure) */
  LispObject    slot_list;     /* Slot list */
  LispObject    nonlocal_slot_list; /* non-local sds slot list */
  LispObject    precedence;    /* Class precedence list */
  LispObject    spare1;
  LispObject 	spare2;
};

#define slotref(v,n)  ((&((v)->INSTANCE.slots))[n])
#define slotrefupdate(v,n,obj) (slotref(v,n)=obj)

struct instance_structure {
  Object_t	header;
  LispObject	slots;		/* Some structure of data */
};


/* Functions... */

/* Special forms are compiler only and don't have homes (?) */

struct special_structure {
  Object_t header;
  LispObject	(*func)();
  LispObject    name;
  LispObject    env;
};

/* Basic function template to which all conform */

struct function_structure {
  Object_t	 header;
  int		argtype;   /* Argument type code - unique for args */
  LispObject	env;       /* Defining parameter environment */
  LispObject    gunk;	   /* either func or bvl */
  LispObject    name;      /* Original name in their module of origin */
  LispObject    home;      /* Module of origin */

};

struct c_function_structure {
  Object_t      header;
  int         argtype;
  LispObject  (*func)();   /* Compiled functions just need fun pointer */
  LispObject  env;
  LispObject  name;
  LispObject  home;

  LispObject setter;
};

struct i_function_structure {
  Object_t	header;
  int		argtype;	
  LispObject	env;
  LispObject	bvl;		/* Parameter list */
  LispObject    name;
  LispObject    home;  

  LispObject	body;           /* Body forms */
};

/* Macros are a logical entity - being just specially interpretted functions */

/* Module structures */

/* Template for all types - an abstract class like function */

struct module_structure {
  Object_t      header;
  LispObject  name;              /* Symbol */
  LispObject  home;              /* In ? */
  LispObject  imported_modules;  /* Module dependecies - name list */
  LispObject  exported_names;    /* Name list too */
  LispObject  bindings;
};

struct c_module_structure {
  Object_t    header;
  LispObject  name;
  LispObject  home;
  LispObject  imported_modules;
  LispObject  exported_names;
  LispObject  bindings;
  
  LispObject  values;            /* Value vector of static module */
  LispObject  entry_count;
};

typedef struct c_module_structure MODULE;

struct i_module_structure {
  Object_t     header;
  LispObject   name;
  LispObject   home;
  LispObject   imported_modules;      
  LispObject   exported_names;        
  LispObject   bindings;

  LispObject   bounce_flag;
};

/* Sockets support... */

#if (defined(WITH_BSD_SOCKETS) || defined(WITH_SYSTEMV_SOCKETS))

#include "syssockets.h"

struct listener_structure {
  Object_t header;
  
  SocketHandle   socket;
  SocketInName   name;

  int            state;
};

struct socket_structure {
  Object_t 	 header;

  SocketHandle   socket;
  SocketInName   name;

  char           buffer[SOCKET_BUFFER_SIZE]; /* Input buffer */

  int            state;
  int 		 lastchar;
};

#endif

/* Weak wrappers... */

struct weak_wrapper_structure {
  Object_t header;
  LispObject  object;
};

union lispunion {
  struct object_structure	OBJECT;
  struct integer_structure	INT;
  struct float_structure	FLOAT;
  struct character_structure	CHAR;
  struct symbol_structure	SYMBOL;
  struct cons_structure		CONS;
  struct string_structure       STRING;
  struct thread_structure       THREAD;
  struct semaphore_structure    SEMAPHORE;
  struct class_structure	CLASS;
  struct instance_structure	INSTANCE;
  struct vector_structure       VECTOR;
  struct continue_structure	CONTINUE;
  struct env_structure		ENV;
  struct special_structure      SPECIAL;
  struct function_structure     FUNCTION;
  struct c_function_structure   C_FUNCTION;
  struct i_function_structure   I_FUNCTION;
/**  struct generic_structure      GENERIC; */
  struct function_structure     MACRO;
  struct c_function_structure   C_MACRO;
  struct i_function_structure   I_MACRO;
/**   struct method_structure       METHOD; */
  struct module_structure       MODULE;
  struct c_module_structure     C_MODULE;
  struct i_module_structure     I_MODULE;
#if (defined(WITH_BSD_SOCKETS) || defined(WITH_SYSTEMV_SOCKETS))
  struct listener_structure     LISTENER;
  struct socket_structure       SOCKET;
#endif 
  struct weak_wrapper_structure WEAK_WRAPPER;
};

#include "system_p.h"

#endif /* STRUCTS_H */

/* End of structs.h */
