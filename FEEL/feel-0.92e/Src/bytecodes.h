/* Contains definitions of all the bytecodes I'll use */
/* Warning: This version not unbder RCS! */
#define BC_NOP_CODE		\
/* easy */

/* Arg 0: Module, Arg 1: offset */
#define BC_PUSH_GLOBAL_CODE \
{				\
  int i,j;			\
  LispObject tmp;		\
				\
  read_int_arg(i,pc);		\
  read_int_arg(j,pc);		\
  PUSH_VAL(sp,GLOB_REF(i,j));	\
}

#define BC_PUSH_STATIC_CODE \
{				\
  int j;			\
  LispObject tmp;		\
  read_int_arg(j,pc);		\
  PUSH_VAL(sp,vref(this_context,j));	\
  VCHECK(PEEK_VAL(sp));		\
}

#define BC_SET_STATIC_CODE 	\
{				\
  int j;			\
/**/				\
  read_int_arg(j,pc);		\
  vref(this_context,j)=TOP_VAL(sp);	\
}

/* Arg 0: module, Arg 1: offset */
#define BC_SET_GLOBAL_CODE \
{				\
  int i,j;			\
  				\
  read_int_arg(i,pc);		\
  read_int_arg(j,pc);		\
  GLOB_REF(i,j) = TOP_VAL(sp);		\
}

#define BC_PUSH_FIXNUM_CODE \
{		\
  int i;	\
  LispObject tmp;	\
  read_int_arg(i,pc);	\
  tmp=allocate_integer(sp+1,i);	\
  PUSH_VAL(sp,tmp);	\
}

#define BC_PUSH_SMALL_FIXNUM_CODE \
{			\
  int i;		\
  LispObject tmp;	\
/**/			\
  i=read_byte_arg(i,pc);	\
  tmp=allocate_integer(sp+1,i);	\
  PUSH_VAL(sp,tmp);		\
}

#define BC_PUSH_SPECIAL_CODE		\
{					\
   switch (*(pc++))				\
     {					\
     case 0:				\
       PUSH_VAL(sp,BCnil);		\
       break;				\
       					\
     case 1:			       \
       PUSH_VAL(sp,BCtrue);		\
       break;				\
    \
     default:				\
       fprintf(stderr,"odd special");   \
       PUSH_VAL(sp,BCnil);		\
       break;					\
     }					\
}					\

/* args: n */
#define BC_PUSH_NTH_CODE 	\
{				\
  int i;			\
  LispObject tmp;		\
  read_byte_arg(i,pc);		\
/**/  				\
  tmp=NTH_REF(sp,i);		\
  PUSH_VAL(sp,tmp);		\
}

#define BC_PUSH_NTH_0_CODE	\
{			\
  LispObject tmp;	\
  /**/			\
  tmp=PEEK_VAL(sp);	\
  PUSH_VAL(sp,tmp);	\
}

#define BC_PUSH_NTH_1_CODE	\
{				\
  LispObject tmp;		\
  tmp=NTH_REF(sp,1);		\
  PUSH_VAL(sp,tmp);		\
}


#define BC_PUSH_NTH_2_CODE	\
{				\
  LispObject tmp;		\
  tmp=NTH_REF(sp,2);		\
  PUSH_VAL(sp,tmp);		\
}


#define BC_PUSH_NTH_3_CODE	\
{				\
  LispObject tmp;		\
  tmp=NTH_REF(sp,3);		\
  PUSH_VAL(sp,tmp);		\
}


/* Arg 1: n */
#define BC_SET_NTH_CODE		\
{				\
  int i;			\
				\
  read_byte_arg(i,pc);		\
				\
  NTH_REF(sp,i)=PEEK_VAL(sp);	\
  POP_VALS(sp,1);		\
  VCHECK(PEEK_VAL(sp));		\
}


/* Arg1: dist arg2: keep */

#define BC_SLIDE_STACK_CODE	\
{				\
  int depth,keep,n,counter;	\
  				\
  read_byte_arg(depth,pc);		\
  read_byte_arg(keep,pc);		\
  sp-= depth;				\
  n=depth-keep;				\
  for (counter=0; counter<keep;		\
       counter++)			\
   {					\
     sp++;				\
     *sp= *(sp+n);			\
    }					\
}

#define BC_SLIDE_1_CODE	\
{				\
  int depth;			\
  LispObject tmp;		\
/**/				\
  read_byte_arg(depth,pc);	\
  tmp=PEEK_VAL(sp);		\
  POP_VALS(sp,depth);		\
  PUSH_VAL(sp,tmp);		\
}

#define BC_SWAP_CODE		\
{				\
  LispObject tmp;		\
				\
  tmp= *sp;			\
  *sp = *(sp-1);		\
  *(sp-1) = tmp;		\
}


#define BC_DROP_CODE		\
{				\
  int i;			\
				\
  read_byte_arg(i,pc);		\
  POP_VALS(sp,i);		\
}

#define BC_DROP_1_CODE	\
{			\
  POP_VALS(sp,1);	\
}

/* arg1: depth arg2: dist */
#define BC_ENV_REF_CODE		\
{				\
  int i,j,counter;		\
  LispObject env=PEEK_VAL(sp);	\
				\
  BC_CHECK(is_vector,env);	\
  read_byte_arg(i,pc);		\
  read_byte_arg(j,pc);		\
  ENV_REF(env,env,i,j);		\
  SHOVE_VAL(sp,env);		\
  VCHECK(PEEK_VAL(sp));		\
}

/* arg1: depth arg2: dist */
#define BC_SET_ENV_CODE		\
{				\
  int i,j,counter;		\
  LispObject env;		\
  LispObject val;		\
    /**/			\
  val=TOP_VAL(sp);		\
  env=PEEK_VAL(sp);		\
  BC_CHECK(is_vector,env);	\
				\
  read_byte_arg(i,pc);		\
  read_byte_arg(j,pc);		\
  SET_ENV_REF(env,i,j,val);	\
}

/* Arg1: Depth */
#define BC_POP_ENV_CODE		\
{				\
  int i,counter;		\
  LispObject env=PEEK_VAL(sp);	\
				\
  read_byte_arg(i,pc);		\
  ENV_NTH(env,i);		\
  SHOVE_VAL(sp,env);		\
  VCHECK(PEEK_VAL(sp));		\
}


#define BC_MAKE_ENV_CODE	\
{				\
  int i;			\
				\
  read_byte_arg(i,pc);		\
  MAKE_ENV(sp,i);		\
  VCHECK(PEEK_VAL(sp));		\
  GC_RESTORE_GLOBALS;		\
}

/* Object reference */
/* arg: n */
#define BC_VREF_CODE		\
{				\
  LispObject tmp=TOP_VAL(sp);	\
/**/				\
BC_BUG(				\
  if (intval(tmp) > PEEK_VAL(sp)->VECTOR.length) 			\
    CallError(sp+2,"duff vector-ref",PEEK_VAL(sp),NONCONTINUABLE);	\
       )				\
  SHOVE_VAL(sp,vref(PEEK_VAL(sp),	\
		    intval(tmp)));	\
  VCHECK(PEEK_VAL(sp));	\
}
#if 0
  if (intval(tmp) > PEEK_VAL(sp)->VECTOR.length)
    CallError(sp+2,"duff vector-ref",PEEK_VAL(sp),NONCONTINUABLE);
#endif
/* arg: n */
#define BC_SET_VREF_CODE		\
{					\
  LispObject val=TOP_VAL(sp);		\
  LispObject loc;			\
  loc=TOP_VAL(sp);  			\
/**/					\
  vref(PEEK_VAL(sp),intval(loc))=val;	\
  SHOVE_VAL(sp,val);			\
}


#define BC_SLOT_REF_CODE	       \
{					\
  LispObject obj=PEEK_VAL(sp);		\
  int i;				\
/**/					\
  read_byte_arg(i,pc);			\
  SHOVE_VAL(sp,slotref(obj,i));	\
  VCHECK(PEEK_VAL(sp));		\
}

#define BC_SLOT_REF_0_CODE	\
{				\
  LispObject obj;		\
  obj=PEEK_VAL(sp);		\
  SHOVE_VAL(sp,slotref(obj,0));	\
}

#define BC_SLOT_REF_1_CODE	\
{				\
  LispObject obj;		\
/**/				\
  obj=PEEK_VAL(sp);		\
  SHOVE_VAL(sp,slotref(obj,1));	\
}

#define BC_SET_SLOT_CODE 		\
{					\
  LispObject val;			\
  LispObject obj;			\
  int i;				\
/**/					\
  val=TOP_VAL(sp);			\
  obj=PEEK_VAL(sp);			\
/**/					\
  read_byte_arg(i,pc);			\
  slotref(obj,i)=val;			\
  SHOVE_VAL(sp,val);			\
}

/* set-slot 1 is called lots. set-slot 0 isn't. */
#define BC_SET_SLOT_1_CODE		\
{					\
  LispObject val;			\
  LispObject obj;			\
/**/					\
  val=TOP_VAL(sp);			\
  obj=PEEK_VAL(sp);			\
/**/					\
  slotref(obj,1)=val;			\
  SHOVE_VAL(sp,val);			\
}

#define BC_SET_TYPE_CODE       		\
{			       		\
  LispObject type;			\
  type=TOP_VAL(sp);			\
/**/  					\
  lval_typeof(PEEK_VAL(sp))=intval(type);	\
}

#define BC_BRANCH_CODE		\
{				\
  int i;			\
  bytecode *opc=pc;		\
  				\
  read_int_arg(i,pc);		\
  pc=ADJUST_PC(opc,i);		\
}

#define BC_BRANCH_NIL_CODE	\
{				\
  int i;			\
				\
  if (TOP_VAL(sp)==BCnil)	\
    {				\
      bytecode *opc=pc;		\
      read_int_arg(i,pc);	\
      pc=ADJUST_PC(opc,i);	\
    }				\
  else				\
    skip_int_arg(pc);		\
}

#define BC_APPLY_ARGS_CODE	\
{			\
  LispObject args,fn;	\
  nargs=0;		\
    /**/		\
  args=TOP_VAL(sp);	\
  fn=TOP_VAL(sp);	\
  SHOVE_VAL(sp,fn);	\
  while (args!=nil)	\
    {			\
      PUSH_VAL(sp,CAR(args));		\
      args=CDR(args);	\
      nargs++;		\
    }			\
      /**/		\
  PUSH_VAL(sp,fn);	\
  goto apply_label;	\
}

/* The tricky ones.... */
/* stack is: fn <lab> a0 a1....an fn */
/* return is: val */

#define BC_APPLY_ANY_CODE		\
{					\
  int abs_args,real_args;		\
  LispObject fn,tmp;			\
  LispObject *arg_start;		\
  read_sign_arg(nargs,pc);		\
apply_label:				\
  abs_args=nargs<0? -nargs: nargs;	\
  fn=TOP_VAL(sp);			\
  switch(typeof(fn))			\
    {					\
    case TYPE_GENERIC:		        \
      {					\
	LispObject ptr,*walker,fast,slow;\
	LispObject meths;		\
	int count,depth;		\
					\
	arg_start=(sp-nargs)+1;		\
	BC_BUG( ((int) *(arg_start-3) &1) ? 0 : CallError(stacktop,"Impossible return", BCnil,NONCONTINUABLE)); \
	fast=(generic_fast_method_cache(fn)); 	\
	slow=generic_slow_method_cache(fn);		\
	depth=intval(generic_discrimination_depth(fn)); \
	/* is there a cache ? */		\
	if (fast!=BCnil)				\
	  {					\
	    /** Method lookup **/		\
	    walker=arg_start;			\
	    ptr=CAR(fast);			\
	    while (ptr!=BCnil && CAR(ptr)==classof(*(walker)))	\
	      {					\
		ptr=CDR(ptr);				\
		walker++; 				\
	      }						\
							\
	    if (ptr==BCnil)				\
	      {						\
		meths=CDR(fast);			\
		goto call_method;			\
	      }						\
	    /* then the slow cache */			\
	    ptr=slow;					\
	    walker=arg_start;				\
	    count=0;					\
							\
	    while(ptr!=BCnil && count<depth)		\
	      {						\
		if (CAR(CAR(ptr))==classof(*(walker)))	\
		  {		/* move down 1 */	\
		    ptr=CDR(CAR(ptr));			\
		    walker++;				\
		    count++;				\
		  }					\
		else					\
		  ptr=CDR(ptr);				\
	      }						\
							\
	    if (count==depth)				\
	      {						\
		generic_fast_method_cache(fn)=ptr;	\
		meths=CDR(ptr);				\
		goto call_method;			\
	      }						\
	    /* not in slow cache */			\
	  }						\
	/* no cache */					\
	  {						\
	  LispObject res,args;				\
	  PUSH_VAL(sp,fn);				\
	  args=allocate_n_conses(sp+1,nargs);		\
	  ptr=args;					\
	  walker=arg_start;				\
	  count=0;					\
	  while (count<nargs)				\
	    {						\
	      CAR(ptr)= *walker;			\
	      ptr=CDR(ptr); ++walker; ++count;		\
	    }						\
	  fn=PEEK_VAL(sp);				\
	    /* Call the methods...*/			\
	  SET_STACK(sp,arg_start);			\
          *sp=fn; *(sp+1)=args;	sp++;		\
	  GC_RESTORE_GLOBALS;			\
          fn=GLOBAL_REF(Generic_Lookup_Fn);		\
          APPLY_BVF(fn);				\
	  break;			\
	  }						\
      call_method:					\
	/* method calling code */			\
	  CALL_METHOD_LIST(arg_start,meths,nargs);	\
      }							\
      break;						\
    case TYPE_B_FUNCTION:    				\
    case TYPE_B_MACRO:					\
      {							\
	int real_args=					\
	  intval(bytefunction_nargs(fn));	\
	if (nargs>=0 && real_args<0)			\
	  {						\
	    int j=nargs+1;				\
	    int k= -real_args;				\
	    LispObject *cons_sp;			\
	    *(++sp)=BCnil;				\
            cons_sp=sp+2;				\
	    /*loop til we have lost enough*/		\
	    while (k!=j)				\
	      {						\
		LispObject tmp;			\
		*(sp+1)=fn;			\
		sp--; 		\
                *cons_sp=*sp;					\
		*(cons_sp+1)=*(sp+1);				\
		tmp=Fn_cons(cons_sp);				\
		*sp=tmp; 					\
		cons_sp--;					\
		fn=*(sp+2);					\
		j--;						\
	      }						\
	    GC_RESTORE_GLOBALS;				\
	  }						\
	APPLY_BVF(fn);				\
	}						\
       break;						\
      							\
    default:						\
      {							\
	LispObject res;					\
	arg_start=sp-abs_args;			\
	BC_BUG( ((int) *(arg_start-2)) &1 ? 0 : CallError(stacktop,"Impossible return", BCnil,NONCONTINUABLE)); \
	res=module_apply_args(arg_start+1,nargs,fn);	\
	GC_RESTORE_GLOBALS;				\
	SET_STACK(sp,arg_start-1);			\
	pc=SET_PC(sp);			  		\
	PUSH_VAL(sp,res);				\
      }							\
      break;						\
    }							\
}

#define BC_APPLY_BVF_CODE	\
{			        \
  LispObject fn;	        \
  int nargs;		        \
  read_byte_arg(nargs,pc);	\
/**/				\
  fn=TOP_VAL(sp);		\
  APPLY_BVF(fn);		\
}

#define BC_APPLY_CFN_CODE	\
{				       	\
  LispObject fn,ret;			\
  int args;				\
    /**/				\
  fn=TOP_VAL(sp);			\
  read_sign_arg(args,pc);		\
  BC_BUG(fprintf(stderr,"Apply %s %d\n", \
		 stringof(fn->C_FUNCTION.name->SYMBOL.pname), args)); \
    /**/				\
  if (args!=fn->C_FUNCTION.argtype)	\
    exit(0);				\
  args=(args > 0 ? args : -args);	\
  ret=(fn->C_FUNCTION.func)(sp-args+1);	\
  GC_RESTORE_GLOBALS;			\
    /**/				\
  POP_VALS(sp,args+1);			\
  pc=SET_PC(sp);			\
  PUSH_VAL(sp,ret);			\
}

#define BC_APPLY_CFN2_CODE		\
{	 				\
  LispObject tmp;			\
  int args,absargs;			\
	 				\
  read_sign_arg(args,pc);		\
  absargs=(args > 0 ? args : -args);	\
  tmp=NTH_REF(sp,absargs);		\
	 				\
  BC_BUG(fprintf(stderr,"Apply %s %d\n",\
		 stringof(tmp->C_FUNCTION.name->SYMBOL.pname), args)); \
  					\
  if (args!=tmp->C_FUNCTION.argtype)	\
    exit(0);				\
  					\
  tmp=(tmp->C_FUNCTION.func)(sp-absargs+1);	\
  GC_RESTORE_GLOBALS;			\
  POP_VALS(sp,absargs+1);		\
  PUSH_VAL(sp,tmp);			\
}

#define BC_APPLY_METHODS_CODE	\
{					\
  LispObject ml;			\
  int args;				\
  LispObject *base;			\
  					\
  read_byte_arg(args,pc);		\
  base=sp-args;				\
  					\
  ml=TOP_VAL(sp);			\
					\
  CALL_METHOD_LIST(base,ml,args);	\
}

#define BC_APPLY_METHOD_LIST_CODE 	\
{					\
  LispObject tmp,meths,*base;		\
  int n=0;				\
					\
  meths=TOP_VAL(sp);			\
  tmp=TOP_VAL(sp);			\
					\
  base=sp+1;				\
  while(tmp!=BCnil)			\
    {					\
      PUSH_VAL(sp,CAR(tmp));		\
      tmp=CDR(tmp);			\
      n++;				\
    }					\
  					\
  CALL_METHOD_LIST(base,meths,n);	\
}

#define BC_PUSH_LABEL_CODE	\
{ /* istream should hold an offset */	\
  bytecode *new_pc;		        \
  LispObject xx;			\
  int i;				\
  bytecode *opc=pc;			\
/**/					\
  read_int_arg(i,pc);			\
  new_pc=ADJUST_PC(opc,i);		\
  BC_BUG( if (GLOBAL_REF(BC_Debug)==BCtrue) fprintf(stderr,"Push lab: %x",new_pc));	\
  STASH_PC(sp,new_pc);		\
}

/* stack is: fn <addr> retval		*/
#define BC_RETURN_CODE	/* and back */	\
{					\
  LispObject tmp=TOP_VAL(sp);		\
/**/					\
  HANDLE_SIGNALS();			\
  VCHECK(tmp);				\
  POP_VALS(sp,1);				     \
  pc=SET_PC(sp);	\
  PUSH_VAL(sp,tmp);			\
}

/** External environment */
#define BC_CONTEXT_CODE	\
{			\
  PUSH_VAL(sp,this_context);		\
}			\

#define BC_EXIT_CODE 				\
{						\
  BC_BUG( if (GLOBAL_REF(BC_Debug)==BCtrue) fprintf(stderr,"{exiting: %x}",sp));	\
  return (TOP_VAL(sp));				\
}

/* allocation */

#define BC_CONS_CODE	\
{			\
  LispObject tmp;	\
/**/			\
  tmp=Fn_cons(sp-1);	\
  POP_VALS(sp,1);	\
  SHOVE_VAL(sp,tmp);		\
  GC_RESTORE_GLOBALS;	\
}

#define BC_NULLP_CODE	\
{			\
  if (PEEK_VAL(sp)==BCnil)\
    SHOVE_VAL(sp,BCtrue);	\
  else				\
    SHOVE_VAL(sp,BCnil);	\
}

#define BC_EQP_CODE	\
{			\
  LispObject tmp;	\
/**/			\
  tmp=TOP_VAL(sp);	\
/**/			\
  if (PEEK_VAL(sp)==tmp) \
    SHOVE_VAL(sp,BCtrue); \
  else			 \
    SHOVE_VAL(sp,BCnil); \
}

#define BC_CONSP_CODE	\
{			\
  LispObject tmp;	\
  tmp=PEEK_VAL(sp);	\
/**/			\
  if (is_cons(tmp))	\
    SHOVE_VAL(sp,BCtrue);	\
  else				\
    SHOVE_VAL(sp,BCnil);	\
}


#define BC_ALLOC_CLOSURE_CODE 		\
{ /* expect <label> <env> on stack, nargs in stream */			  \
  LispObject env,ctxt;							  \
  LispObject tmp,tmp2;							  \
  bytecode *start;							  \
  int nargs;								  \
  int offset;							  \
  /* ought to be a long */						  \
  read_sign_arg(nargs,pc);						  \
  									  \
  tmp=allocate_instance(sp+1,						  \
			ByteFunction);		  \
  lval_typeof(tmp)=TYPE_B_FUNCTION;					  \
  bytefunction_env(tmp)=TOP_VAL(sp);					  \
  /* Grab context+offset from stack...*/				  \
  UNSTASH_PC(sp,ctxt,offset);						\
  PUSH_VAL(sp,tmp);						\
  bytefunction_globals(tmp)=ctxt;			\
  tmp2=allocate_integer(sp+1,offset);			\
  tmp=PEEK_VAL(sp);					\
  bytefunction_offset(tmp)=tmp2;			\
  tmp2=allocate_integer(sp+1,nargs);			\
  tmp=PEEK_VAL(sp);					\
  bytefunction_nargs(tmp)=tmp2;				\
  GC_RESTORE_GLOBALS;					\
}

#define BC_ALLOC_EXT_CLOSURE_CODE 		\
{ /* expect <label> <env> <info> on stack, nargs in stream */			  \
  LispObject env,ctxt;							  \
  LispObject tmp,tmp2;							  \
  bytecode *start;							  \
  int nargs;								  \
  int offset;							  \
  /* ought to be a long */						  \
  read_sign_arg(nargs,pc);						  \
  									  \
  tmp=allocate_instance(sp+1,						  \
			ExtByteFunction);		  \
  lval_typeof(tmp)=TYPE_B_FUNCTION;					  \
  extbytefunction_info(tmp)=TOP_VAL(sp);				\
  bytefunction_env(tmp)=TOP_VAL(sp);					  \
  /* Grab context+offset from stack...*/				  \
  UNSTASH_PC(sp,ctxt,offset);						\
  PUSH_VAL(sp,tmp);						\
  bytefunction_globals(tmp)=ctxt;			\
  tmp2=allocate_integer(sp+1,offset);			\
  tmp=PEEK_VAL(sp);					\
  bytefunction_offset(tmp)=tmp2;			\
  tmp2=allocate_integer(sp+1,nargs);			\
  tmp=PEEK_VAL(sp);					\
  bytefunction_nargs(tmp)=tmp2;				\
  GC_RESTORE_GLOBALS;					\
}

/* Common functions --- assq, memq, scanq */
#define BC_ASSQ_CODE	\
{			        \
  LispObject ob,lst,val;	\
				\
  lst=TOP_VAL(sp);		\
  ob=PEEK_VAL(sp);		\
  val=BCnil;			\
				\
  while (lst!=BCnil)		\
    {				\
      if (CAR(CAR(lst))==ob)	\
	{			\
	  val=CAR(lst);		\
	  break;		\
	}			\
      lst=CDR(lst);		\
    }				\
  				\
  SHOVE_VAL(sp,val);		\
}

#define BC_MEMQ_CODE		\
{				\
  LispObject ob,lst,val;	\
  				\
  lst=TOP_VAL(sp);		\
  ob=PEEK_VAL(sp);		\
  val=BCnil;			\
				\
  while (lst!=BCnil)		\
    {				\
      if (CAR(lst)==ob)		\
	{			\
	  val=lst;		\
	  break;		\
	}			\
      lst=CDR(lst);		\
    }				\
  SHOVE_VAL(sp,val);		\
}

/* no check for unbalanced lists, etc */
#define BC_SCANQ_CODE 			\
{					\
  LispObject ob,lst,fn;			\
  extern LispObject unbound;		\
  lst=TOP_VAL(sp);			\
  ob=PEEK_VAL(sp);			\
					\
  while (lst!=BCnil && CAR(lst)!=ob)	\
    {					\
      	lst=CDR(CDR(lst));		\
    }					\
  if (lst==BCnil)			\
    SHOVE_VAL(sp,unbound);		\
  else					\
    SHOVE_VAL(sp,CAR(CDR(lst)));	\
}

#define APPLY_BVF(fn)	\
  pc=BF2PC(fn);			\
  PUSH_VAL(sp,bytefunction_env(fn));

#ifdef WITH_SPECIAL_METHOD
#define ON_SPECIAL_METHOD(x) x
#else
#define ON_SPECIAL_METHOD(x)
#endif

/* Inserted by other macros */
/* bungs return onto stack */
#define CALL_METHOD_LIST(base,ml,nargs)	\
{					\
  LispObject mf,res;				\
  short type; 					\
  mf=method_function(CAR(ml));			\
  type=typeof(mf);				\
  if (type==TYPE_B_FUNCTION)			\
    {						\
      SET_NTH_REF(base,1,ml);			\
      APPLY_BVF(mf);				\
      break;					\
    }						\
  if (type==TYPE_C_FUNCTION)			\
    res=(mf->C_FUNCTION.func)(base);		\
  else						\
    {						\
      if (type==TYPE_I_FUNCTION)		\
	res = call_method(base,nargs,ml);	       \
      else						\
	CallError(base,"Illegal type",ml,NONCONTINUABLE); \
    }						\
  GC_RESTORE_GLOBALS;				\
  SET_STACK(sp,base-2);				\
  pc=SET_PC(sp);  			\
  PUSH_VAL(sp,res);			\
  break;				\
}

#define HANDLE_SIGNALS()						\
{								\
  if (SYSTEM_GLOBAL_VALUE(system_interrupt_flag))			\
    {				/* fix up return address */	\
      int flags=SYSTEM_GLOBAL_VALUE(system_interrupt_flag);		\
      LispObject i;						\
      i=allocate_integer(sp+1,SYSTEM_GLOBAL_VALUE(system_interrupt_flag));			\
      SYSTEM_GLOBAL_VALUE(system_interrupt_flag)=0;			\
      STASH_PC(sp,pc-1);/* back here*/						\
      PUSH_VAL(sp,GLOBAL_REF(Signal_Thread_Fn));		/* fn called */			\
      PUSH_VAL(sp,GLOBAL_REF(Interpreter_Thread));			\
      PUSH_VAL(sp,i);			\
      APPLY_BVF(GLOBAL_REF(Signal_Thread_Fn));		\
      break;							\
    }								\
}
#ifdef nope /* Mon Nov  2 16:10:20 1992 */
/**/
/**/    ON_SPECIAL_METHOD(							\
/**/	case TYPE_SPECIAL_METHOD:					\
/**/	  {								\
/**/	    LispObject res;						\
/**/	    BC_METHOD_SWITCH(sp,intval(special_method_id(mf)));		\
/**/	    mf=PEEK_VAL(sp);						\
/**/	    SET_STACK(sp,base-1);					\
/**/	    POP_VALS(sp,1);					\
/**/	    pc=SET_PC(sp);  			\
/**/	    PUSH_VAL(sp,mf);						\
/**/	  }								\
/**/	  break;							\
/**/		)
#endif /* nope Mon Nov  2 16:10:20 1992 */

#define BC_METHOD_SWITCH(stack,id)	\
{					\
  LispObject arg1,arg2,res;		\
  switch (id)				\
    {					\
    case METHOD_INT_ADD:		\
      {					\
	int i;				\
	arg1=TOP_VAL(stack);		\
	arg2=PEEK_VAL(stack);		\
					\
	i=intval(arg1)+intval(arg2);	\
	res=allocate_integer(stack,i);	\
	SHOVE_VAL(stack,res);		\
      }					\
      break;				\
					\
    case METHOD_INT_DIFF:		\
      {					\
	int i;				\
	arg2=TOP_VAL(stack);		\
	arg1=PEEK_VAL(stack);		\
					\
	i=intval(arg1)-intval(arg2);	\
	res=allocate_integer(stack,i);	\
	SHOVE_VAL(stack,res);		\
      }					\
      break;				\
    case METHOD_INT_MULT:		\
      {					\
	int i;				\
	arg2=TOP_VAL(stack);		\
	arg1=PEEK_VAL(stack);		\
					\
	i=intval(arg1)*intval(arg2);	\
	res=allocate_integer(stack,i);	\
	SHOVE_VAL(stack,res);		\
      }					\
      break;				\
					\
    case METHOD_INT_DIV:		\
      {					\
	int i;				\
	arg2=TOP_VAL(stack);		\
	arg1=PEEK_VAL(stack);		\
					\
	i=intval(arg1)/intval(arg2);	\
	res=allocate_integer(stack,i);	\
	SHOVE_VAL(stack,res);		\
      }					\
      break;				\
      					\
    case METHOD_INT_EQUAL:		\
      {					\
	int i;				\
	arg2=TOP_VAL(stack);		\
	arg1=PEEK_VAL(stack);		\
					\
	if (intval(arg1)==intval(arg2))	\
	  SHOVE_VAL(stack,lisptrue);	\
	else				\
	  SHOVE_VAL(stack,BCnil);		\
      }					\
      break;				\
					\
    case METHOD_SYMBOL_EQUAL:		\
      {					\
	int i;				\
	arg2=TOP_VAL(stack);		\
	arg1=PEEK_VAL(stack);		\
					\
	if ((arg1)==(arg2))		\
	  SHOVE_VAL(stack,lisptrue);	\
	else				\
	  SHOVE_VAL(stack,BCnil);		\
      }					\
      break;				\
      					\
    case METHOD_STREAM_STRING_WRITE:	\
      break;				\
					\
    case METHOD_STREAM_READ:		\
      break;				\
					\
    case METHOD_SLOT_REF_0:		\
      {					\
	arg1=PEEK_VAL(stack);		\
	SHOVE_VAL(stack,slotref(arg1,0)); \
      }					  \
      break;				  \
					  \
    case METHOD_SLOT_REF_1:		  \
      {					  \
	arg1=PEEK_VAL(stack);		  \
	SHOVE_VAL(stack,slotref(arg1,1)); \
      }					  \
      break;				  \
					  \
    case METHOD_SLOT_REF_2:		  \
      {					  \
	arg1=PEEK_VAL(stack);		  \
	SHOVE_VAL(stack,slotref(arg1,2)); \
      }					  \
      break;				  \
					  \
    case METHOD_SLOT_REF_3:		  \
      {					  \
	arg1=PEEK_VAL(stack);		  \
	SHOVE_VAL(stack,slotref(arg1,3)); \
      }					  \
      break;				  \
					  \
    case METHOD_SLOT_SET_0:		  \
      {					  \
	arg2=TOP_VAL(stack);		  \
	arg1=PEEK_VAL(stack);		  \
					  \
	slotref(arg1,0)=arg2;		  \
	SHOVE_VAL(stack,arg2);		  \
      }					  \
      break;				  \
					  \
    case METHOD_SLOT_SET_1:		  \
      {					  \
	arg2=TOP_VAL(stack);		  \
	arg1=PEEK_VAL(stack);		  \
					  \
	slotref(arg1,1)=arg2;		  \
	SHOVE_VAL(stack,arg2);		  \
      }					  \
      break;				  \
					  \
    case METHOD_SLOT_SET_2:		  \
      {					  \
	arg2=TOP_VAL(stack);		  \
	arg1=PEEK_VAL(stack);		  \
					  \
	slotref(arg1,2)=arg2;		  \
	SHOVE_VAL(stack,arg2);		  \
      }					  \
      break;				  \
					  \
    case METHOD_SLOT_SET_3:		  \
      {					  \
	arg2=TOP_VAL(stack);		  \
	arg1=PEEK_VAL(stack);		  \
					  \
	slotref(arg1,3)=arg2;		  \
	SHOVE_VAL(stack,arg2);		  \
      }					  \
      break;				  \
      					  \
    }					  \
}



