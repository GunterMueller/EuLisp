/*
  * obread.h
  * interface for obread
  */

/* class of the reader */

extern LispObject object_reader;

/* functions */

extern void write_obj(LispObject *,LispObject, unsigned char **,
		      LispObject);
extern LispObject read_obj(LispObject *,unsigned char **, LispObject);

#define EUBUG(x) 
