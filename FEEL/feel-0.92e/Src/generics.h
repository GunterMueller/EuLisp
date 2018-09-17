/*

 * Generic operators...
 
 */

extern LispObject make_generic(char *,int);
extern LispObject make_method(LispObject (*)(),LispObject,int);
extern LispObject make_default_generic(char *,LispObject (*)(),int);
extern LispObject Fn_next_method_p(LispObject,LispObject);
extern LispObject Fn_call_next_method(Env,LispObject);
extern LispObject Fn_current_method(LispObject);

extern EUDECL( call_generic);
extern void my_add_method(LispObject*,LispObject,LispObject);
extern LispObject flat_list_copy(LispObject*);

/*

 * Call utitlities.

 */

extern LispObject generic_apply(LispObject*, LispObject,LispObject);
extern LispObject generic_apply_1(LispObject*, LispObject,LispObject);
extern LispObject generic_apply_2(LispObject*, LispObject,
				               LispObject, LispObject);
extern LispObject generic_apply_3(LispObject*, LispObject,
				  LispObject,LispObject,LispObject);
extern LispObject generic_apply_4(LispObject*,  LispObject,
			  LispObject, LispObject, LispObject, LispObject);

#define is_generic(x) 	(EUCALL_2(Fn_subclassp,classof(x),Generic) != nil)

#define generic_name(x)		slotref(x,0)
#define generic_home(x) 	slotref(x,1)
#define generic_argtype(x)	slotref(x,2)
#define generic_method_class(x)	slotref(x,3)
#define generic_discriminator(x)	slotref(x,4)
#define generic_cache_table(x)	slotref(x,5)
#define generic_method_table(x) slotref(x,6)

#define N_SLOTS_IN_GENERIC_CLASS 7

#define method_qualifier(x) 	slotref(x,0)
#define method_signature(x)	slotref(x,1)
#define method_host(x)		slotref(x,2)
#define method_function(x) 	slotref(x,3)
#define method_fixed(x) 	slotref(x,4)

#define is_method(x) 	(EUCALL_2(Fn_subclassp,classof(x),Method) != nil)

#define N_SLOTS_IN_METHOD_CLASS 5
