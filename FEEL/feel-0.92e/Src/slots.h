/* ******************************************************************** */
/*  slots.h          Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Slot/slot description manipulation	                                */
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, December 1989
 */

#ifndef SLOTS_H
#define SLOTS_H

/* slot structure */
/* These are never used */
#define slot_desc_name(slot) slotref(slot,0)
#define slot_desc_position(slot) slotref(slot,1)
#define slot_desc_initargs(slot) slotref(slot,2)
#define slot_desc_initform(slot) slotref(slot,3)
#define slot_desc_mutable(slot) slotref(slot,4)
#define N_SLOTS_IN_SD_CLASS 6

#define init_slot_initarg(slot) (CAR(slot))
#define init_slot_initfunction(slot) (CDR(slot))

#endif
