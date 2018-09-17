/*
  * obread_p.h
  *  local defines for object reader
  *
  */

#ifndef obread_p_h
#define obread_p_h

#define MIN_ID		0
#define FIRST_USER_ID  	16
#define LAST_USER_ID 	255
#define MAX_ID 		LAST_USER_ID

#define UC(x) ((unsigned char) (x))

/* Internal representation */
typedef int Type_Id;

#define GET_READER(rd,id)    	(vref(vref(rd,0),(id)))
#define GET_WRITER(rd,obj)	(lookup_by_cpl(stacktop,vref(rd,1),obj))
#define SET_WRITER(rd,class,value)   \
               (EUCALL_3(Fn_table_ref_setter,vref(rd,1),class,value))
#define WRITER_ID(wrt) 		(UC(intval(CAR(wrt))))
#define WRITER_FN(wrt)		(CDR(wrt))

#define MAKE_WRITER(id,fn)	(EUCALL_2(Fn_cons,(id),(fn)))

/* Type ids */

#define READ_INT UC(1)
#define READ_CONS UC(2)
#define READ_NULL UC(3)
#define READ_VECTOR UC(4)
#define READ_SYMBOL UC(5)
#define READ_STRING UC(6)
#define READ_FLOAT UC(7)

#endif
