extern LispObject Fn_tablep(LispObject*);
extern LispObject Fn_table_parameters(LispObject*);
extern LispObject Fn_table_ref(LispObject*);
extern LispObject Fn_table_ref_setter(LispObject*);
/* NB make_table in in global.h */
extern LispObject make_table(LispObject*);
extern int hash(char *);

#define TREF(tab,key)            EUCALL_2(Fn_table_ref, tab,key)
#define TREF_UPDATE(tab,key,val) EUCALL_3(Fn_table_ref_setter,tab,key,val)
#define TABLE_PARAMS(tab)        EUCALL_1(Fn_table_parameters,tab)

/* 
 * Definition of new table structure
 */

#define table_values(x)		(slotref(x,0))
#define table_population(x) 	(slotref(x,1))
#define table_threshold(x) 	(slotref(x,2))
#define table_comparator(x)	(slotref(x,4))
#define table_hash_fn(x)	(slotref(x,5))
#define table_fill(x)		(slotref(x,6))

#define N_SLOTS_IN_TABLE 7

/* Internal values */
#define MIN_TABLE_SIZE 	16

#define KEYOF(x)	CAR(x)
#define VALOF(x)	CDR(x)
