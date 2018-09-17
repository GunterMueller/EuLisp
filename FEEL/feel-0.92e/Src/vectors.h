/* ******************************************************************** */
/*  vectors.h        Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Vector module prototypes    	                                        */
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, November 1989
 */

extern LispObject Fn_vectorp( LispObject* );
extern LispObject Fn_make_vector( LispObject* );
extern LispObject Fn_vector_length( LispObject* );
extern LispObject Fn_vector_ref( LispObject* );
extern LispObject Fn_vector_ref_updator(LispObject*);
extern LispObject Fn_vector( LispObject* );

void initialise_vectors(LispObject*);

#define vecrefupdator(v,i,obj) (*(&(v->VECTOR.base)+i)=obj) /* For hack use */
#define vecref(v,i) (*(&(v->VECTOR.base)+i))
