
extern void process_import_spec(LispObject*,LispObject,LispObject);


EUDECL(module_eval);
EUDECL(process_top_level_form);
EUDECL(old_module_eval);
EUDECL(module_mv_apply_1);
LispObject module_apply_args(LispObject *,int,LispObject);
extern LispObject module_set(LispObject*);
extern LispObject module_set_new(LispObject*,LispObject,LispObject,LispObject);
extern LispObject module_set_new_constant(LispObject*,
					  LispObject,LispObject,LispObject);

extern LispObject put_module(LispObject*,LispObject,LispObject);
extern LispObject get_module(LispObject*,LispObject);
extern int module_loaded_p(LispObject*,LispObject);
extern int module_binding_exists_p(LispObject*,LispObject,LispObject);

extern LispObject register_module_import(LispObject*);
extern LispObject process_exports(LispObject*);

extern LispObject global_module_table;
LispObject symbol_ref(LispObject *,LispObject,LispObject,LispObject);
extern SYSTEM_GLOBAL(LispObject,current_interactive_module);


/******* Really just for calls.c (macroexpansion) and modops *****/
extern LispObject Cb_no_function_fn;
extern LispObject Fn_backtrace_by_arg(LispObject *);

#define BINDING_HOME(x) (CAR(x))
#define BINDING_MUTABLE(x) (!(typeof(x)&1))
#define BINDING_EXPORTED(x) (typeof(x)&2)
#define BINDING_VALUE(x) (CDR(x))

/* All except into+mutable must be safe */

#define GET_BINDING(mod, sym)	EUCALL_2(Fn_table_ref,mod->MODULE.bindings,sym)
#define ADD_BINDING(mod, sym, value, mutable) \
{			        		\
  LispObject xx;				\
  STACK_TMP(sym);				\
  xx=EUCALL_2(Fn_cons,(LispObject)mod,value);		\
  lval_typeof(xx)=(mutable==nil ? TYPE_FIXMBIND : TYPE_MBIND);	\
  UNSTACK_TMP(sym);				\
  EUCALL_3(Fn_table_ref_setter,			\
           BINDING_HOME(xx)->MODULE.bindings,sym,xx);	    \
}

#define IMPORT_BINDING(mod,name,bind)		\
  EUCALL_3(Fn_table_ref_setter,mod->MODULE.bindings,name,bind);

#define SET_BINDING_MUTABLE(bind,flag) \
if (flag!=nil) 				\
     lval_typeof(bind)&=~1;		\
else					\
  lval_typeof(bind)|=1			

#define SET_BINDING_EXPORT(bind) (lval_typeof(bind)|=2)


#ifdef MACHINE_SYSTEMV
# define SYM_CACHE_SET(sym,mod,value)   \
  ((vref((sym)->SYMBOL.lmodule, system_scheduler_number)=mod),  \
   (vref((sym)->SYMBOL.lvalue, system_scheduler_number)=value))

# define SYM_CACHE_MODULE(sym) vref((sym)->SYMBOL.lmodule,system_scheduler_number)
				      
# define SYM_CACHE_VALUE(sym) vref((sym)->SYMBOL.lvalue,system_scheduler_number)
# define SYM_CACHE_INIT(sym)	
#else
# define SYM_CACHE_SET(sym,mod,value)			\
  (((sym)->SYMBOL.lmodule)=(mod), ((sym)->SYMBOL.lvalue)=(value))
     
# define SYM_CACHE_MODULE(sym) 	((sym)->SYMBOL.lmodule)
# define SYM_CACHE_VALUE(sym)	((sym)->SYMBOL.lvalue)
# define SYM_CACHE_INIT(sym)
#endif
