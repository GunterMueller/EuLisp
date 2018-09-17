/* ******************************************************************** */
/*  bootstrap.h      Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* C-bootstrapping prototypes and defines                               */
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, December 1989
 */

#ifndef BOOTSTRAP_H
#define BOOTSTRAP_H

/* Cbstracted class accessors (not used averywhere...) */

#define CLASS_NAME(class)          (class->CLASS.name)
#define CLASS_SUPER(class)         (class->CLASS.superclass)
#define CLASS_SUB(class)           (class->CLASS.subclasses)
#define CLASS_DESCS(class)         (class->CLASS.slot_table)

extern LispObject symbol_name;
extern LispObject symbol_superclass;
extern LispObject symbol_slot_descriptions;

extern void gen_class(LispObject *,LispObject *);
void set_class_size(LispObject *, LispObject, LispObject, int);

extern void initialize_boot_classes(LispObject *);

extern LispObject Null;

#define N_SLOTS_IN_CLASS N_SLOTS_IN_STRUCT(struct class_structure)
#define N_SLOTS_IN_THREAD N_SLOTS_IN_STRUCT(struct thread_structure)

#endif
