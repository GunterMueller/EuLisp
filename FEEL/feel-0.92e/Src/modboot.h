#ifndef MODBOOT_H
#define MODBOOT_H
extern void open_module(LispObject *,MODULE *,LispObject *,char *,int);
extern void close_module(void);
extern LispObject make_module_function(LispObject *,char *,
				       LispObject (*)(LispObject*),int);
extern LispObject make_unexported_module_function(LispObject *,
						  char *, LispObject (*)(LispObject *),
						  int);
extern LispObject make_unexported_module_special(
		       LispObject *,char *,LispObject (*)(LispObject *));
extern LispObject make_module_special(LispObject *,
				      char *,LispObject (*)(LispObject *));
extern LispObject make_module_entry(LispObject *,char *,LispObject);
extern LispObject make_unexported_module_entry(LispObject *,char *,LispObject);
extern LispObject make_module_entry_using_symbol(LispObject *,LispObject,LispObject);

extern LispObject make_module_generic(LispObject *,char *,int);
extern LispObject make_wrapped_module_generic(
		     LispObject *, char *,int,LispObject (*)(LispObject *));
extern LispObject make_module_method_1(LispObject *,
		      LispObject,LispObject (*)(LispObject *),LispObject);
extern LispObject make_module_method_2(LispObject *,
				       LispObject,LispObject (*)(LispObject *),
				       LispObject,LispObject);

extern LispObject make_unexported_function(LispObject *, char *,
					   LispObject (*)(LispObject *),int);
#ifdef Obsolete
extern LispObject make_method_1(LispObject,
				LispObject (*)(LispObject *),LispObject);
extern LispObject make_method_2(LispObject,LispObject (*)(LispObject *),
				LispObject,LispObject);
#endif

extern 
  LispObject 
    make_anonymous_module_env_function_2(LispObject *,LispObject,
					 LispObject (*)(LispObject *),
					 int,LispObject,LispObject,
					 LispObject,LispObject);

extern 
  LispObject 
    make_anonymous_module_env_function_1(LispObject *,LispObject,
					 LispObject (*)(LispObject *),
					 int,LispObject,LispObject);

/* Hack!! */

extern LispObject make_nary(char *,LispObject (*)(LispObject *),int);

extern LispObject make_module_macro(LispObject *,char *,LispObject (*)(LispObject *),int);
#endif
