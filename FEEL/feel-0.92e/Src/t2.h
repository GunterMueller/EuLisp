/* 
  * Private include file for
  * tables
  */

/* Internal values */
#define MIN_TABLE_SIZE 	16
#define TABLE_FILL_FACTOR 4
#define KEYOF(x)	CAR(x)
#define VALOF(x)	CDR(x)
#define ELTP(x) 	is_cons(x)

#define T_THRESH(x)	(is_cons(table_threshold(x)) ? CAR(table_threshold(x)) : table_threshold(x))
#define T_NELTS(x)	(is_cons(table_threshold(x)) 					\
			 ? intval(table_population(x))-intval(CDR(table_threshold(x)))	\
			 : intval(table_population(x)))
