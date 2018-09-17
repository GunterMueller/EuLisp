/* ******************************************************************** */
/*  class.h          Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Class fun prototypes                                                 */
/* ******************************************************************** */

#define CLASS_KEY(class) class /* The bit of a class used for key tables */

/* Class object accessors... */

extern LispObject Fn_classp(LispObject*);
extern LispObject Fn_class_of(LispObject*);
extern LispObject Fn_subclassp(LispObject*);
extern LispObject search_keylist(LispObject*,LispObject,LispObject);

extern LispObject Gf_make_instance(LispObject *);
