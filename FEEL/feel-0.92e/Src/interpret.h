/*
 * Defines of bytecode junk
 */

#ifndef _INTERPRET_H
#define _INTERPRET_H
/*****************************************/
/* For debugging */
#ifndef NODEBUG
#define BC_BUG(x)	x
#define BC_BUG_EXP(x)   x
#define BC_CHECK(pred,val)	(pred(val)?nil:CallError(sp,"Bad Type value",val,NONCONTINUABLE))
#else
#define BC_BUG(x)
#define BC_BUG_EXP(x)   0
#define BC_CHECK(pred,val) 
#endif

#ifndef NODEBUG
#define VCHECK(x) (( (x)!=NULL && (((int) (x))&1==1)) ? CallError(sp,"Dumb value",nil,NONCONTINUABLE) : nil)
#else 
#define VCHECK(x) 0
#endif


#ifdef COUNT_BYTES
#define BC_COUNTER(x) x
#else 
#define BC_COUNTER(x)
#endif

#define BC_PRESWITCH() 	\
 int nargs;		\
 toplabel:		\
  BC_BUG({ if (GLOBAL_REF(BC_Debug)==BCtrue)			\
	     { fprintf(stderr,"{Doing: [%x, %x, %d] %d}\n",pc,sp,(int) (sp-oldsp),*pc); \
		 DUMP_STACK(sp);		\
		 oldsp=sp;	\
	      }		\
	   }) \
   BC_COUNTER(exec_counts[*pc]++);

/* Global reference */
#define GLOB_REF(n,m) 	\
  vref(vref(static_vectors,n),m)


/* Stack hacking */

#define NTH_REF(sp,n)   (*((sp)-(n)))

#define SET_NTH_REF(sp,n,v) (*((sp)-(n))=v)

#define PUSH_VAL(sp,val)    (*(++sp)=val)

#define POP_VALS(sp,n)	    ((sp) -= (n))

#define PEEK_VAL(sp)	    (*(sp))

#define TOP_VAL(sp)	    (*(sp--))

#define SHOVE_VAL(sp,val) ((*(sp))=val)

#define SET_STACK(sp,val)    (sp)=(val);

/* Environment hacking */

#define ENV_NTH(e,depth)		\
counter=depth;				\
while (counter)				\
{					\
  e=vref(e,0);				\
  counter--;				\
  VCHECK(e); \
}

#define ENV_REF(e,into,depth,dist)	\
ENV_NTH(e,depth)			\
into=vref(e,dist+1);

#define SET_ENV_REF(e,depth,dist,val)	\
ENV_NTH(e,depth)			\
vref(e,dist+1)=val;

#define MAKE_ENV(sp,size)		\
{					\
  LispObject tmp;			\
/**/					\
  tmp=allocate_vector(sp+1, size+1);	\
  vref(tmp,0)= PEEK_VAL(sp);		\
  SHOVE_VAL(sp,tmp);			\
}

/******************************/
/* instruction stream hacking */

typedef unsigned char bytecode;

/* shoves arg into 'into' and updates pc */
/* Should be a bit (read lots) cleverer  */
#define read_int_arg(into,stream)	\
  into= (int)(*(stream++));		\
  into=(into<<8)+((int)(*(stream++)));		\
  into=(into<<8)+((int)(*(stream++)));		\
  into= *(stream++) ? -into: into;		\
  BC_BUG(if (GLOBAL_REF(BC_Debug)==BCtrue) fprintf(stderr,"Read int: got: %d [%x]\n", into,into))

#define read_short_arg(into,stream) /* NOT YET */	\
  into=1; stream+=2;

#ifdef CHAR_IS_UNSIGNED
# ifdef SIGNED_CHAR
#   define read_sign_arg(into,stream)	\
      into=(int)((SIGNED_CHAR) *(stream++))
# else
    This is a problem - No signed char
# endif
#else
# define read_sign_arg(into,stream)	\
    into=(int)((char) *(stream++))
#endif

#define read_byte_arg(into,stream) \
  into = *(stream++)

#define skip_int_arg(pc)	pc+=sizeof(int)

#define next(stream)	stream++;

#define INC_PC(pc)	(pc++)

#define LISPIFY(x) ((LispObject) (((x)<<1)|1))
/* representation of BC on stack */
#define STASH_PC(sp,pc)						\
  PUSH_VAL(sp,LISPIFY((int)((pc)-bytevector_start(this_context))));	\
  PUSH_VAL(sp,this_context)

#define bytevector_start(context) ((bytecode *)stringof(vref(context,0)))

#define SET_PC(sp) \
  (this_context=TOP_VAL(sp),	\
   GLOBAL_REF(BC_context)=this_context,	\
   BC_BUG_EXP( (((int)PEEK_VAL(sp))&1) ? 0 : CallError(sp+1,"Youch",nil,NONCONTINUABLE)), \
   bytevector_start(this_context)+((int)TOP_VAL(sp)>>1))


#define UNSTASH_PC(sp,ctxt,offset)		\
  ctxt=(TOP_VAL(sp));				\
  offset=((int) TOP_VAL(sp)>>1);		

/* modifies pc by x bytes */
#define ADJUST_PC(pc,x)	((pc)+((x)-1))

#define BF2PC(x) \
  (this_context=(bytefunction_globals(x)),\
   GLOBAL_REF(BC_context)=this_context,	   \
   bytevector_start(this_context)+intval(bytefunction_offset(x)))

/* Move sp to the start of a new nary list */


/**********************/
/* Garbage protection */

#define GC_RESTORE_GLOBALS		\
    BCnil=nil;		\
    BCtrue=lisptrue;		\


#ifdef nope /* Wed Nov  4 20:13:56 1992 */
/**/    this_context=GLOBAL_REF(BC_context);  
#endif /* nope Wed Nov  4 20:13:56 1992 */

/* Printing counts ... */
#ifdef COUNT_BYTES
#define PRINT_COUNTS	\
{				\
  int i,j;			\
  for (i=0, j=0; i<256; i++)	\
    {				\
      if (exec_counts[i]!=0)	\
	{ 				\
	  fprintf(stderr,"%3d: %7d ",i,exec_counts[i]); 	\
	  j++;			\
	  if ( (j%6) == 0)	\
	    fputc('\n',stderr);	\
	}			\
    }				\
  if (j%6!=0) fputc('\n',stderr); \
}      
#else
#define PRINT_COUNTS  fprintf(stderr,"Count-bytes: Couldn't tell you\n");
#endif

#define DUMP_STACK(sp)					\
{							\
  LispObject *ptr= (LispObject*) (((int) (sp-8)) & ~15);	\
  int i, j;						\
    /**/				\
  for (i=0 ; i<5 ; i++)			\
    {					\
      fprintf(stderr,"%x:",ptr);	\
      for (j=0 ; j<4 ; j++)		\
	{				\
	  fprintf(stderr, " 0x%x",*ptr);	\
	  ptr++;			\
	}				\
      fprintf(stderr,"\n");		\
    }					\
}	
/*****************************************/
/* Interpreter macros */
#define MAX_MODS 256

#if 0
#define GOTO_TOP goto toplabel; break
#else 
#define GOTO_TOP continue
#endif

#ifdef __STDC__
#  ifndef NODEBUG
#  define BC_CASE(name) \
     case name: if (GLOBAL_REF(BC_Debug)==BCtrue) \
             fprintf(stderr,"{Exec: "#name" [%x]}",(int)name,(int)pc); name##_CODE continue;
#  else
#  define  BC_CASE(name)\
     case name: name##_CODE continue;
#  endif
#else /* stdc */
#  ifndef NODEBUG
#  define BC_CASE(name) \
case name: if (GLOBAL_REF(BC_Debug)==BCtrue) \
       fprintf(stderr,"{Exec: name [%x %x %x]}",(int)name,*(pc),*(pc+1)); name/**/_CODE continue;
#  else
#  define BC_CASE(name) \
 case name: name/**/_CODE  continue;
#  endif
#endif

#define BC_DROP_THROUGH_CASE(n)  case n:
     

#define N_GLOBALS 10
#define GLOBAL_REF(n) vref(global_vector,(n))
#define Generic_Lookup_Fn 0
#define Generic_Apply_Fn 1
#define BC_context 2
#define BC_Debug 3
#define Interpreter_Thread 4
#define Signal_Thread_Fn 5

#define BC_GLOBALS()	\
  static LispObject boot_modules; \
  static int boot_module_count=1;		       \
  static SYSTEM_GLOBAL(int,static_count);	\
  static LispObject static_vectors;	\
  static LispObject global_vector;	\
  static LispObject return_context;	\
  /**/					\
  static bytecode **bytevectors;	\
  BC_BUG(static LispObject *oldsp;)	\
  BC_COUNTER(static int exec_counts[256];)     


#define BC_INITIALISE_GLOBALS()		\
  BCnil=nil;				\
  BCtrue=lisptrue;			\
  BC_BUG(oldsp=sp);			\
  sp=stacktop-1; 	/* stackpointer[0]= top elt */ \
  pc=start_pc;			\
  GLOBAL_REF(BC_context)=context;	\
  GLOBAL_REF(Interpreter_Thread)=CAR(interpreter_thread); \
  this_context=context;		\

#define BC_NOINSTRUCT(pc)	\
 default:			\
  fprintf(stderr,"No such instruction: %d\n",pc);

/* GC Protection */
#define SAVE_REGISTERS(sp)

#define RESTORE_REGISTERS(sp)

#endif /**_INTERPRET_H**/
