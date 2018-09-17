/* A few typedefs to get things started */
#define MACHINE_BSD 1

#include "runtime.h"

#ifdef __STDC__

extern LispObject Slowcall(LispObject*);
extern LispObject elvira_slowcall_object;

typedef LispObject (LispFN)(LispObject*);
/* really now gratuitous documentation... */
typedef LispObject (LispFN0)(LispObject*);
typedef LispObject (LispFN1)(LispObject*);
typedef LispObject (LispFN2)(LispObject*);
typedef LispObject (LispFN3)(LispObject*);
typedef LispObject (LispFN4)(LispObject*);
typedef LispObject (LispFN5)(LispObject*);

/* Stack Organisation

------------------------------------------------------
| ARG1 ... ARGn | DISPLAY | LOCAL1 ... LOCALn | 
------------------------------------------------------
^               ^                             ^
argptr          localptr                      callbase

*/

/*  argument access */
#define arg(n) argptr[(n)-1]
#define a1 argptr[0]
#define a2 argptr[1]
#define a3 argptr[2]
#define a4 argptr[3]
#define a5 argptr[4]
#define a6 argptr[5]
#define a7 argptr[6]
#define a8 argptr[7]
#define a9 argptr[8]
#define a10 argptr[9]

/* alias iregs to aregs */
#define i1 a1
#define i2 a2
#define i3 a3
#define i4 a4
#define i5 a5
#define i6 a6
#define i7 a7
#define i8 a8
#define i9 a9
#define i10 a10

/* access to display save slot */
#define SAVE_DISPLAY localptr[0] = dp;
#define RESTORE_DISPLAY dlp = dp = localptr[0];

/* access to compiler temporaries */
#define local(n) localptr[n+1]

#define l0 localptr[1]
#define l1 localptr[2]
#define l2 localptr[3]
#define l3 localptr[4]
#define l4 localptr[5]
#define l5 localptr[6]
#define l6 localptr[7]
#define l7 localptr[8]
#define l8 localptr[9]
#define l9 localptr[10]
#define l10 localptr[11]
#define l11 localptr[12]
#define l12 localptr[13]
#define l13 localptr[14]
#define l14 localptr[15]
#define l15 localptr[16]
#define l16 localptr[17]
#define l17 localptr[18]
#define l18 localptr[19]
#define l19 localptr[20]
#define l20 localptr[21]

#define callarg(n) (*(callbase+(n-1)))

/* creation, deletion and access to run-time temporaries */
#define MAKE_TEMP(value) \
  PUSH(value); \
  callbase++;

#define UNMAKE_TEMP callbase--

#define TEMP(n) callbase[-n]

/* basic stack manipulation */

#define PUSH(x) (topptr[0] = x, ++topptr)
#define POP(x) (topptr-- , x = topptr[0])
/* tacky hack */
#define POPVAL() (topptr--, topptr[0])
#define o1 POPVAL()
/* --- Function linkage --- */

#define CALL(mod,offset,nargs) \
{ \
  SAVE_DISPLAY; \
  dlp = dp = nil; \
  topptr=callbase; \
  PUSH( \
     ((*((LispFN/**/nargs *)Module_/**/mod.functions[offset])) == NULL ? \
      elvira_slowcall_object = Module_/**/mod.values[offset], \
      (LispFN/**/nargs *)Slowcall : \
      (*((LispFN/**/nargs *)Module_/**/mod.functions[offset])))(callbase)); \
  RESTORE_DISPLAY; \
}

#define FastLCALL(fn,nargs) \
{ \
  SAVE_DISPLAY; \
  dlp = dp = nil; \
  topptr=callbase;\
  PUSH(fn(callbase)); \
  RESTORE_DISPLAY; \
}

#define PSEUDORET(result) \
  (*callbase = (result),topptr=callbase+1)
/* --- dynamic extent continuations --- */

#define BEGIN_W_CC \
  while (1) \
    { \
    callbase[0] = (allocate_continue(callbase)); \
    callbase[1] = callbase[0]; \
printf("cbase: %x dp1: %x\n", callbase, dp); \
    callbase++; topptr=callbase+1;\
    transfer_display_to_heap(callbase); \
    if (set_continue(callbase,callbase[-1])) { \
       topptr=callbase;\
       callbase+= -1; \
       RESTORE_DISPLAY;\
printf("cbase: dp2: %x\n", callbase, dp);\
       if (typeof(callbase[0])!=TYPE_CONTINUE) \
	 exit(0); \
       callbase[0] = (callbase[0])->CONTINUE.value; \
       break; \
    };

		
/* THIS IS BROKEN FOR THE PRESENT... */

#define END_W_CC \
  topptr = callbase; callbase+= -1;  \
  unset_continue(callbase[0]); \
  callbase[0] = callbase[1]; \
printf("dp3: %x\n", dp); \
  break; \
}

#define SLIDE(n)  /* Move 'n' args down the stack to argbase */ \
	      {					\
		int i;				\
		for (i=0; i< n ; i++)		\
		  arg(i+1)=callarg(i+1);		\
		    \
	      }					

/* --- with-handler --- */

#define BEGIN_W_H \
  callarg(2)=(HANDLER_STACK()); \
  HANDLER_STACK() = Fn_cons(callbase); \
  topptr = callbase+1;

#define END_W_H \
  HANDLER_STACK() = CDR(HANDLER_STACK())

/* --- unwind-protect --- */

#define BEGIN_U_P \
{ \
	    LispObject unwind_cont,cleanup;\
\
            cleanup = *callbase; \
            unwind_cont = allocate_continue(callbase); \
	    *(callbase+1) = unwind_cont; \
	    callbase +=2; \
	      \
            if (set_continue(callbase,unwind_cont)) { \
	      *callbase=cleanup; *(callbase+1)=nil; \
              module_mv_apply_1(callbase); \
              call_continuation(callbase+1, \
				unwind_cont->CONTINUE.target, \
				unwind_cont->CONTINUE.value); \
	    } \
            else { \
              unwind_cont->CONTINUE.unwind = TRUE; 
            
#define END_U_P \
	    { LispObject tmp; \
	      tmp = *callbase; \
	      callbase += -2; \
	      cleanup = *callbase; \
	      unwind_cont =  *(callbase+1); \
	      *callbase = tmp;  \
              unset_continue(unwind_cont); \
	      /* return value is TOS */ \
	      *(callbase+1)= cleanup; \
	      *(callbase+2)=nil; \
              module_mv_apply_1(callbase+1); \
	      /* finally, return the returned value */ \
	    } \
          } \
	  }

extern LispObject Integer,Pair,String,Real;

#define STATICINT(n,x) static struct integer_structure S/**/n = { TYPE_INT,-1,0,x}
#define STATICCONS(n,x1,x2) static struct cons_structure S/**/n = {TYPE_CONS,-1,&Cons,&x1,&x2}
#define STATICSTRING(n,s) static struct string_structure S/**/n = {TYPE_STRING,-1,&String,s}
#define STATICFLOAT(n,x) static struct float_structure S/**/n = { TYPE_FLOAT,-1,&Real,x}
#define STATICSYM(n) static LispObject S/**/n
#define STATICVEC(n,vals) static struct cons_structure S/**/n **HELP**
#define STATICCHAR(n,code) static struct character_structure S/**/n = { TYPE_CHAR,-1,&Character,0,(char) code }

/* extern LispFN3 Fn_cons;*/
#define CONS_WITH_ARGS()  \
  (topptr=callbase,PUSH(Fn_cons(callbase)))

#define UNKNOWN(a)
#define STATIC(a) statics[a]
#define LOCAL(mod,a) Module_/**/mod/**/.values[a]
#define NONLOCAL(mod,a) Module_/**/mod/**/.values[a]

#define ENTRY

extern LispObject dp,dlp;
extern void init_stack_frame(LispObject,int);

/* New definitions... */

#define VREF(v,i) vref((v),(i))
#define LVREF_WITH_ARGS() (vref(*callbase,intval(*(callbase+1))))

#define CAR_WITH_ARGS() (CAR(*callbase))
#define CDR_WITH_ARGS() (CDR(*callbase))

#define LAST_FRAME(v) (VREF(v,1))
#define DISPLAYREF(d,i) (VREF(d,2+i))
#define FRAME_TYPE(v) (VREF(v,0))

#define INLINE_ALLOC() dlp = dp

#define ALLOC(n) \
{ \
  LispObject frame = allocate_vector(stacktop,localptr,n+2); \
  init_stack_frame(frame,n); \
  LAST_FRAME(frame) = dp; \
  dlp = dp = frame; \
  SAVE_DISPLAY; \
}

#define DEALLOC \
  dlp = dp = LAST_FRAME(dp);

extern LispObject allocate_e_function(LispObject *,LispObject,LispObject (*)(),int);
extern LispObject allocate_e_macro(LispObject *,LispObject,LispObject (*)(),int);
extern void transfer_display_to_heap(LispObject *callbase);

#define FUNCTION(mod,name,args) \
  (allocate_e_function(stacktop,callbase, \
		      (LispObject) &Module_/**/mod, \
		      (LispObject (*)()) name, \
		      args))

#define MACRO(mod,name,args) \
  (allocate_e_macro(stacktop,callbase, \
		    (LispObject) &Module_/**/mod, \
		    name, \
		    args))

extern LispObject module_mv_apply_1(LispObject *);

#define APPLY() ((*callbase)=(module_mv_apply_1(callbase)), topptr=callbase+1)

/* 
 * Dynamics...
 */

extern LispObject *dynamic_ref(LispObject);
extern LispObject dynamic_setq(LispObject,LispObject);
 
#define make_dynamic(argptr,str,val) (get_symbol(argptr,str)->SYMBOL.gvalue = val)

#define DYNAMIC(name) (*dynamic_ref(statics[name]))
#define DYNAMIC_SETQ(name,value) (dynamic_setq(name,value))

#define BIND(name,val) \
          { \
	    struct envobject newenv; \
	    LispObject yow = (LispObject) &newenv; \
	    typeof(yow) = TYPE_ENV; \
	    gcof(yow) = -1; \
	    classof(yow) = Object; \
	    newenv.variable = statics[name]; \
	    newenv.value = val; \
	    newenv.next = DYNAMIC_ENV(); \
	    newenv.mutable = TRUE; \
	    *callbase=yow; \
	    callbase++; topptr = callbase+1; \
	    DYNAMIC_ENV() = (Env) &newenv; \

#define UNBIND() \
	    DYNAMIC_ENV() = DYNAMIC_ENV()->next; \
	    callbase += -1; \
          }


#else /* !!__stdc__ */

extern LispObject Slowcall(LispObject *);
extern LispObject elvira_slowcall_object;

typedef LispObject (LispFN0)(LispObject *);
typedef LispObject (LispFN1)(LispObject *);
typedef LispObject (LispFN2)(LispObject *);
typedef LispObject (LispFN3)(LispObject *);
typedef LispObject (LispFN4)(LispObject *);
typedef LispObject (LispFN5)(LispObject *);
typedef LispObject (LispFN6)(LispObject *);
typedef LispObject (LispFN7)(LispObject *);
typedef LispObject (LispFN8)(LispObject *);
typedef LispObject (LispFN)(LispObject *);

#define CALL(mod,offset,nargs,arglist)  \
         { \
            LispObject tmp = dp; \
            STACK(tmp); \
            dlp = dp = nil; \
            o1 = \
           ((*((LispFN##nargs *)Module_##mod.functions[offset])) == NULL ? \
	   elvira_slowcall_object = Module_##mod.values[offset], \
	   (LispFN##nargs *)Slowcall : \
	   (*((LispFN##nargs *)Module_##mod.functions[offset]))) arglist; \
           UNSTACK(1); \
           dlp = dp = tmp; \
         }

#define LCALL(offset,nargs,arglist)   \
         { \
            LispObject tmp = dp; \
            STACK(tmp); \
            dlp = dp = nil; \
            o1 = \
              (functions[offset] == NULL ? \
	       elvira_slowcall_object = local_values[offset], \
	       (LispFN##nargs *)Slowcall : \
	       (*((LispFN##nargs *)functions[offset]))) arglist; \
            UNSTACK(1); \
            dlp = dp = tmp; \
	  }

#define FastLCALL(fn,nargs,arglist) \
         { \
            LispObject tmp = dp; \
            STACK_TMP(tmp); \
            dlp = dp = nil; \
            o1 = (fn) arglist; \
            UNSTACK_TMP(tmp); \
            dlp = dp = tmp; \
	 }


/* Not used these days */
#define DCALL(dispref,nargs) (*((LispFN##nargs *)(dispref)))
#define RCALL(reg,nargs) (*((LispFN##nargs *)reg))

/*
#define CWCC(name) {  LispObject temp_cont = allocate_continue(),tdp;\
                        tdp = dp; \
			STACK(tdp); \
			if (!set_continue(temp_cont)) {\
			  o1 = name(temp_cont);\
			  UNSTACK(1); \
			  dlp = dp = tdp; \
                          unset_continue(temp_cont);\
			  }\
			else {\
			  dlp = dp = tdp; \
			  UNSTACK(1); \
			  o1 = temp_cont->CONTINUE.value;\
			  }\
		   }
*/

#define BEGIN_W_CC() \
         while (1) { \
           LispObject temp_cont = allocate_continue(); \
           STACK(temp_cont); \
           transfer_display_to_heap(callbase); \
	   if (set_continue(temp_cont)) { \
             o1 = temp_cont->CONTINUE.value; \
             UNSTACK(1); \
             break; \
	   } \
           i1 = temp_cont;

#define END_W_CC() \
           unset_continue(temp_cont); \
           UNSTACK(1); \
	   break; \
	 }

#define BEGIN_W_H(reg) HANDLER_STACK() = Fn_cons(reg,HANDLER_STACK())
#define END_W_H()      HANDLER_STACK() = CDR(HANDLER_STACK())

#define BEGIN_U_P(reg) \
         { \
	    LispObject unwind_cont,cleanup;\
\
            cleanup = *callbase; \
            unwind_cont = allocate_continue(callbase); \
	    *callbase = unwind_cont;\
	    *(callbase+1) = cleanup; \
	    callbase+=2;\
\
            if (set_continue(callbase,unwind_cont)) { \
              module_mv_apply_1(cleanup,nil); \
              call_continuation(callbase,unwind_cont->CONTINUE.target, \
				unwind_cont->CONTINUE.value); \
	    } \
            else { \
              unwind_cont->CONTINUE.unwind = TRUE; 
            
#define END_U_P \
              unset_continue(callbase,unwind_cont); \
              { \
                LispObject tmp = o1; \
	        LispObject stacktop = callbase;\
                STACK_TMP(tmp); \
                module_mv_apply_1(stacktop); \
                UNSTACK_TMP(tmp); \
                o1 = tmp; \
	      } \
	    } \
	 callbase += -2; \
          }

extern LispObject Integer,Pair,String,Real;

#define STATICINT(n,x) static struct integer_structure S##n = { TYPE_INT,-1,0,x}
#define STATICCONS(n,x1,x2) static struct cons_structure S##n = {TYPE_CONS,-1,&Cons,&x1,&x2}
#define STATICSTRING(n,s) static struct string_structure S##n = {TYPE_STRING,-1,&String,s}
#define STATICFLOAT(n,x) static struct float_structure S##n = { TYPE_FLOAT,-1,&Real,x}
#define STATICSYM(n) static LispObject S##n
#define STATICVEC(n,vals) static struct cons_structure S##n **HELP**
#define STATICCHAR(n,code) static struct character_structure S##n = { TYPE_CHAR,-1,&Character,0,(char) code }

/* extern LispFN3 Fn_cons;*/

#define UNKNOWN(a)
#define STATIC(a) statics[a]
#define LOCAL(mod,a) Module_##mod##.values[a]
#define NONLOCAL(mod,a) Module_##mod##.values[a]
/*
#define ENTRY LispObject dlp=dp
*/
#define ENTRY

/*
#define ALLOC(n) dp=dlp=frame(n,dlp)
#define DEALLOC dp=dlp=dlp->dlp
*/

extern LispObject dp,dlp;
extern void init_stack_frame(LispObject,int);

#define GCprotect(name) name=nil,STACK(name);
#define GCpop(n) UNSTACK(n)

/* New definitions... */

#define VREF(v,i) vref(v,i)
#define LVREF(v,i) vref(v,intval(i))

#define FRAME_TYPE(v) (VREF(v,0))
#define LAST_FRAME(v) (VREF(v,1))
#define DISPLAYREF(d,i) (VREF(d,2+i))
#define ARG(d,i) DISPLAYREF(d,i)

#define FRAME(n) ALLOC(n)
#define ALLOC(n) \
         { \
	   char space[sizeof(struct vector_structure)+((n-1)+2)*sizeof(LispObject)]; \
           LispObject frame = (LispObject) space; \
	   init_stack_frame(frame,n); \
           LAST_FRAME(frame) = dp; \
           dlp = dp = frame; \
           STACK(frame);

#define DEALLOC \
          UNSTACK(1); \
          dlp = dp = LAST_FRAME(dp); }

extern LispObject allocate_e_function(LispObject *,LispObject,LispObject (*)(),int);
extern LispObject allocate_e_macro(LispObject *,LispObject,LispObject (*)(),int);
extern void transfer_display_to_heap(LispObject *);

#define FUNCTION(mod,name,args) \
          allocate_e_function((LispObject) &Module_##mod,(LispObject (*)()) name,args)

#define MACRO(mod,name,args) \
          allocate_e_macro((LispObject) &Module_##mod,name,args)

#define LINK_REG(reg) apply1(reg,i1)

/*
          switch (typeof(reg)) { \
	   case TYPE_E_FUNCTION: \
	    { \
	      LispObject tmp = dp; \
	      STACK(tmp); \
	      dp = (LispObject) (reg->C_FUNCTION.env); \
	      o1 = (reg->C_FUNCTION.func) args; \
	      dp = tmp; \
	      UNSTACK(1); \
	      break; \
	    } \
           default: \
	    CallError("LINK_REG: unknown operator thingy",reg,NONCONTINUABLE); \
	  }

*/

extern LispObject module_mv_apply_1(LispObject,LispObject);

#define LINK_OBJ(reg) o1 = module_mv_apply_1(reg,i1)

#define APPLY(r1,r2) o1 = module_mv_apply_1(r1,r2)

/* 
 * Dynamics...
 */

extern LispObject *dynamic_ref(LispObject);
extern LispObject dynamic_setq(LispObject,LispObject);

#define make_dynamic(str,val) (get_symbol(str)->SYMBOL.gvalue = val)

#define DYNAMIC(name) (*dynamic_ref(statics[name]))
#define DYNAMIC_SETQ(name,value) (dynamic_setq(name,value))

#define BIND(name,val) \
          { \
	    struct envobject newenv; \
	    LispObject yow = (LispObject) &newenv; \
	    newenv.type = TYPE_ENV; \
	    newenv.gc = -1; \
	    newenv.class = Object; \
	    newenv.variable = statics[name]; \
	    newenv.value = val; \
	    newenv.next = DYNAMIC_ENV(); \
	    newenv.mutable = TRUE; \
	    STACK(yow); \
	    DYNAMIC_ENV() = (Env) &newenv; 

#define UNBIND() \
	    DYNAMIC_ENV() = DYNAMIC_ENV()->next; \
            UNSTACK(1); \
          }
#endif /* __STDC__*/
