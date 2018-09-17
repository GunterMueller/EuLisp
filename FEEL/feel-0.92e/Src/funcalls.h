/* 
 * macros for calling eulispII functions
 */

#ifndef FUNCALLS_H
#define FUNCALLS_H

#define ARG_STACK_PUSH(x) STACK(x)
#define ARG_STACK_POP(x) STACK(x)

#define CALL_FN(function,args) \
  function(stacktop,stacktop+number_of_args)

#define ARG_0(stack) (*stack)
#define ARG_1(stack) (*(stack+1))
#define ARG_2(stack) (*(stack+2))
#define ARG_3(stack) (*(stack+3))

#ifdef __STDC__
#define EUDECL(fun)  \
LispObject fun(LispObject *)

#define EUFUN_0(name) LispObject name(LispObject *stackbase) \
                      { LispObject *stacktop = stackbase; \
			/*toplabel:*/

#define EUFUN_1(name,arg)  \
LispObject name (LispObject *stackbase) \
  { \
      LispObject arg; \
      LispObject *stacktop = stackbase+1; \
   /*toplabel:*/ \
      arg = ARG_0(stackbase); \

#define EUFUN_2(name,a1,a2) \
LispObject name (LispObject *stackbase) \
  { \
    LispObject a1; \
    LispObject a2; \
    LispObject *stacktop = stackbase+2; \
 /*toplabel:*/ \
    a1 = ARG_0(stackbase); \
    a2 = ARG_1(stackbase);

#define EUFUN_3(name, a1, a2, a3) \
  LispObject name (LispObject *stackbase) \
    { \
      LispObject a1; \
      LispObject a2; \
      LispObject a3; \
      LispObject *stacktop = stackbase+3; \
   /*toplabel:*/ \
      a1 = ARG_0(stackbase); \
      a2 = ARG_1(stackbase); \
      a3 = ARG_2(stackbase);

#define EUFUN_4(name, a1, a2, a3, a4) \
  LispObject name (LispObject *stackbase) \
    { \
      LispObject a1; \
      LispObject a2; \
      LispObject a3; \
      LispObject a4; \
      LispObject *stacktop = stackbase+4; \
   /*toplabel:*/ \
      a1 = ARG_0(stackbase); \
      a2 = ARG_1(stackbase); \
      a3 = ARG_2(stackbase); \
      a4 = ARG_3(stackbase);

#else
#define EUDECL(fun)  \
LispObject fun()

#define EUFUN_0(name) \
LispObject name(stackbase) \
LispObject *stackbase; \
  { LispObject *stacktop = stackbase; \
      /*toplabel:*/

#define EUFUN_1(name,arg)  \
LispObject name (stackbase) \
LispObject *stackbase; \
  { \
      LispObject arg; \
      LispObject *stacktop = stackbase+1; \
   /*toplabel:*/ \
      arg = ARG_0(stackbase); \

#define EUFUN_2(name,a1,a2) \
LispObject name (stackbase) \
LispObject *stackbase;	    \
  { \
    LispObject a1; \
    LispObject a2; \
    LispObject *stacktop = stackbase+2; \
 /*toplabel:*/ \
    a1 = ARG_0(stackbase); \
    a2 = ARG_1(stackbase);

#define EUFUN_3(name, a1, a2, a3) \
  LispObject name (stackbase) \
    LispObject *stackbase;   \
    { \
      LispObject a1; \
      LispObject a2; \
      LispObject a3; \
      LispObject *stacktop = stackbase+3; \
   /*toplabel:*/ \
      a1 = ARG_0(stackbase); \
      a2 = ARG_1(stackbase); \
      a3 = ARG_2(stackbase);

#define EUFUN_4(name, a1, a2, a3, a4) \
 LispObject name (stackbase)	\
   LispObject *stackbase; \
    { \
      LispObject a1; \
      LispObject a2; \
      LispObject a3; \
      LispObject a4; \
      LispObject *stacktop = stackbase+4; \
   /*toplabel:*/ \
      a1 = ARG_0(stackbase); \
      a2 = ARG_1(stackbase); \
      a3 = ARG_2(stackbase); \
      a4 = ARG_3(stackbase);
      
#endif	
	/* tacky, but needed -- x is the number of args before the nested call 
	   hopefully, the optimiser will sort this little lot out */
#define NEST(x,call) \
	( stacktop += x, *stacktop = call , stacktop += -x, *(stacktop+x))

#define STACK_TMP(x) (*stacktop = (x) , stacktop++) /* change cos of seq. points in
						       NARY_PUSH */
#define UNSTACK_TMP(x) (x) = *--stacktop

#define EUFUN_CLOSE  }

#define EUCALL_0(name) \
   name (stacktop)

#define EUCALL_1(name, arg) \
     ( \
      ARG_0(stacktop) = arg, \
      name (stacktop) \
     )
      
#define EUCALL_2(name,arg1,arg2) \
	 ( \
	  ARG_0(stacktop) = arg1, \
	  ARG_1(stacktop) = arg2, \
	  name (stacktop) \
	 )			        
				        
#define EUCALL_3(name,arg1,arg2,arg3) \
	 ( \
	  ARG_0(stacktop) = arg1, \
	  ARG_1(stacktop) = arg2, \
	  ARG_2(stacktop) = arg3, \
	  name (stacktop) \
	 )			  
				  
#define EUCALL_4(name,arg1,arg2,arg3,arg4) \
	 ( \
	  ARG_0(stacktop) = arg1, \
	  ARG_1(stacktop) = arg2, \
	  ARG_2(stacktop) = arg3, \
	  ARG_3(stacktop) = arg4, \
	  name (stacktop) \
	 )

#define EUCALLSET_0(val,name) val = name (stacktop)

#define EUCALLSET_1(val, name, arg) \
     { \
      ARG_0(stacktop) = arg; \
      val = name (stacktop); \
     }
      
#define EUCALLSET_2(val,name,arg1,arg2) \
	 { \
	  ARG_0(stacktop) = arg1; \
	  ARG_1(stacktop) = arg2; \
	  val = name (stacktop); \
	 }			        
				        
#define EUCALLSET_3(val,name,arg1,arg2,arg3) \
	 { \
	  ARG_0(stacktop) = arg1; \
	  ARG_1(stacktop) = arg2; \
	  ARG_2(stacktop) = arg3; \
	  val = name (stacktop); \
	 }			  
				  
#define EUCALLSET_4(val,name,arg1,arg2,arg3,arg4) \
	 { \
	  ARG_0(stacktop) = arg1; \
	  ARG_1(stacktop) = arg2; \
	  ARG_2(stacktop) = arg3; \
	  ARG_3(stacktop) = arg4; \
	  val = name (stacktop); \
	  }

#define EULET(name,value) 

/* call a function with the same arguments.. assumes they are never changed */
/* only a little dodgy */

#define RECALL(fun) fun (stackbase)

/* we don't use this often */
#define EUTAIL_3(a1,a2,a3) \
    {  *stackbase=a1;	   \
       *(stackbase+1) = a2;\
       *(stackbase+2) = a3;\
       stacktop = stackbase + 3 ;\
       goto toplabel;	   \
     }			   

#define BEGIN_NARY_EUCALL() \
    do \
      { LispObject *argbase = stacktop; \
	int argcount=0;

#define NARY_PUSH_ARG(val)\
	*(argbase+(argcount++))=val;
	
#define NARY_EUCALL(fun)  fun (argbase)
#define NARY_EUCALL_1(fun,arg) fun(argbase,arg)      
#define END_NARY_EUCALL() \
	} while(0)

#define RETURN_EUCALL(call) do { stacktop=stackbase; return (call); } while (0)

#define DISCARD_OBJ() --stacktop

/* not really sure if these macros work -- not used anywhere */
/* should allow us to call a lisp function from a C function */

#define FUNCALL(mod,fun) \
  EUCALL_2(Fn_apply, \
	   EUCALL_2(Fn_binding_location, \
		    get_module(stacktop, get_symbol(stacktop, mod)), \
		    get_symbol(stacktop, fun)), \
	   ARG_0(stacktop))

#define FUNCALL_0(mod,fun) \
  ( \
   ARG_0(stacktop) = nil, \
   FUNCALL(mod,fun) \
  )

#define FUNCALL_1(mod,fun,arg) \
  ( \
   ARG_0(stacktop) = EUCALL_2(Fn_cons,arg,nil), \
   FUNCALL(mod,fun) \
  )

#define FUNCALL_2(mod,fun,arg1,arg2) \
  ( \
   ARG_0(stacktop) = EUCALL_2(Fn_cons,arg1,nil), \
   ARG_0(stacktop) = EUCALL_2(Fn_cons,arg2,ARG_0(stacktop)), \
   FUNCALL(mod,fun) \
  )

#define FUNCALL_3(mod,fun,arg1,arg2,arg3) \
  ( \
   ARG_0(stacktop) = EUCALL_2(Fn_cons,arg1,nil), \
   ARG_0(stacktop) = EUCALL_2(Fn_cons,arg2,ARG_0(stacktop)), \
   ARG_0(stacktop) = EUCALL_2(Fn_cons,arg3,ARG_0(stacktop)), \
   FUNCALL(mod,fun) \
  )

#define FUNCALL_4(mod,fun,arg1,arg2,arg3,arg4) \
  ( \
   ARG_0(stacktop) = EUCALL_2(Fn_cons,arg1,nil), \
   ARG_0(stacktop) = EUCALL_2(Fn_cons,arg2,ARG_0(stacktop)), \
   ARG_0(stacktop) = EUCALL_2(Fn_cons,arg3,ARG_0(stacktop)), \
   ARG_0(stacktop) = EUCALL_2(Fn_cons,arg4,ARG_0(stacktop)), \
   FUNCALL(mod,fun) \
  )

#endif


