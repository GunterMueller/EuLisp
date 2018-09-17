/* External interface */
#ifndef NGENERICS_H
#define NGENERICS_H
extern LispObject generic_apply(LispObject*,LispObject);
extern LispObject generic_apply_1(LispObject*, LispObject,LispObject);
extern LispObject generic_apply_2(LispObject*, LispObject,
				               LispObject, LispObject);
extern LispObject generic_apply_3(LispObject*, LispObject,
				  LispObject,LispObject,LispObject);
extern LispObject generic_apply_4(LispObject*,  LispObject,
			  LispObject, LispObject, LispObject, LispObject);

#define is_generic(x) 	 	(typeof(x)==TYPE_GENERIC)

/* macros for generics */

#define N_SLOTS_IN_GENERIC 10

#define generic_name(x)			(slotref(x,0))
#define generic_home(x)			(slotref(x,1))
#define generic_argtype(x)    		(slotref(x,2))
#define generic_fast_method_cache(x) 	(slotref(x,3))
#define generic_slow_method_cache(x) 	(slotref(x,4))
#define generic_method_table(x)		(slotref(x,5))
#define generic_method_class(x) 	(slotref(x,6))
#define generic_discriminator(x) 	(slotref(x,7))
#define generic_discrimination_depth(x)	(slotref(x,8))
#define generic_setter(x)		(slotref(x,9))		
#define N_SLOTS_IN_METHOD 5

#define method_qualifier(x) 	slotref(x,0)
#define method_signature(x)	slotref(x,1)
#define method_host(x)		slotref(x,2)
#define method_function(x) 	slotref(x,3)
#define method_fixed(x) 	slotref(x,4)

#define is_method(x) 	(typeof(x)==TYPE_METHOD)

#define is_special_method(x) (typeof(x)==TYPE_SPECIAL_METHOD)

#define N_SLOTS_IN_SPECIAL_METHOD 1
#define special_method_id(x) 	slotref(x,0)

#endif


