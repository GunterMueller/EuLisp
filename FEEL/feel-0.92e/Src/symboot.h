/* ******************************************************************** */
/*  symbols.h        Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/*  General symbol hacking and global oblist                            */
/* ******************************************************************** */

extern LispObject global_oblist;
extern LispObject get_symbol(LispObject*,char *);
extern LispObject get_symbol_by_copying(LispObject*,char *);

extern int reserved_symbol_p(LispObject);

extern LispObject sym_nil;

extern LispObject sym_define;
extern LispObject sym_function,sym_macro,sym_constant;

extern LispObject sym_defclass,sym_defcondition,sym_defconstant,sym_defgeneric,
                  sym_deflocal,sym_defmacro,sym_defmethod,sym_defstruct,
                  sym_defun;

extern LispObject sym_defmodule,sym_load_module,sym_start_module,
                  sym_enter_module,sym_loaded_modules;

extern LispObject sym_root;

extern LispObject sym_lambda,sym_macro_lambda,
                  sym_setq,sym_if,sym_progn,sym_quote;

extern LispObject sym_import,sym_expose,
                  sym_expose_except,sym_rename,sym_export;

extern LispObject sym_root;

extern LispObject sym_letcc,sym_unwind_protect;
extern LispObject sym_with_handler;

extern LispObject sym_defvar,sym_dynamic_setq,
                  sym_dynamic_set,sym_dynamic,sym_dynamic_let;

extern void initialise_symbols(LispObject*);

extern LispObject sym_methods;

extern LispObject sym_rest;

extern LispObject sym_cons;

extern LispObject sym_initarg,sym_initargs,sym_initform,sym_reader,sym_writer,
                  sym_accessor,sym_class,sym_mutable;

extern LispObject sym_constructor,sym_metaclass,sym_metaclass_initargs;

extern LispObject sym_position;

extern LispObject sym_message,sym_error_value;

extern LispObject sym_anonymous_class;

extern LispObject sym_name,sym_superclass,sym_slot_descriptions;

extern LispObject sym_exit;

extern LispObject sym_evalcm;

extern LispObject sym_tagbody;
