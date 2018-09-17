/* 
  * Definition of Bytevector functions
  *
*/

/* External interface */
#ifdef BCI

LispObject apply_nary_bytefunction(LispObject */*stacktop*/, int args, LispObject fn);
void add_boot_module(LispObject);

/* Internal reperesentation */
#define N_SLOTS_IN_BYTEFUNCTION  5

/* represented as: module #, offset */
/* better would be codevector, offset, static vector ? */

#define bytefunction_env(bvf)		slotref(bvf,0)
#define bytefunction_offset(bvf)	slotref(bvf,1)
#define bytefunction_nargs(bvf)		slotref(bvf,2)
#define bytefunction_globals(bvf)	slotref(bvf,3)
#define bytefunction_code(bvf)		vref(bytefunction_globals(bvf),0)
#define bytefunction_setter(bvf)	slotref(bvf,4)

#define extbytefunction_info(ebvf)		slotref(ebvf,5)

#endif
