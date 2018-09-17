/* ******************************************************************** */
/*  slots.c          Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Slot stuff			                                        */
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, Noveber 1989
 *   Abstracted the slot access operations from the class ops into this file
 */

#include <stdio.h>
#include "funcalls.h"
#include "defs.h"
#include "structs.h"
#include "global.h"
#include "error.h"

#include "bootstrap.h"
#include "table.h" 
#include "ngenerics.h"

#include "class.h"

#include "modboot.h"
#include "slots.h"

/* The slot form access primitives */

EUFUN_1( Fn_slot_description_name, desc)
{
  if (EUCALL_2(Fn_subclassp,classof(desc),Slot_Description) == nil)
    CallError(stacktop,"slot-description-name: not slot description",
	      ARG_1(stackbase),NONCONTINUABLE);

  return(slot_desc_name(ARG_0(stackbase)));
}
EUFUN_CLOSE

EUFUN_1( Fn_slot_description_position, desc)
{
  if (EUCALL_2(Fn_subclassp,classof(desc),Slot_Description) == nil)
    CallError(stacktop,"slot-description-position: not slot description",
	      ARG_0(stackbase),NONCONTINUABLE);

  return(slot_desc_position(ARG_0(stackbase)));
}
EUFUN_CLOSE

EUFUN_2( Fn_slot_description_position_setter, desc, val)
{
  if (EUCALL_2(Fn_subclassp,classof(desc),Slot_Description) == nil)
    CallError(stacktop,
	      "(setter slot-description-position): not slot description",
	      ARG_0(stackbase),NONCONTINUABLE);

  return(slot_desc_position(ARG_0(stackbase)) = ARG_1(stackbase));
}
EUFUN_CLOSE

EUFUN_1( Fn_slot_description_initargs, desc)
{
  if (EUCALL_2(Fn_subclassp,classof(desc),Slot_Description) == nil)
    CallError(stacktop,"slot-description-initargs: not slot description",
	      ARG_0(stackbase),NONCONTINUABLE);

  return(slot_desc_initargs(ARG_0(stackbase)));
}
EUFUN_CLOSE

EUFUN_1( Fn_slot_description_initform, desc)
{
  if (EUCALL_2(Fn_subclassp,classof(desc),Slot_Description) == nil)
    CallError(stacktop,"slot-description-initform: not slot description",
	      ARG_0(stackbase),NONCONTINUABLE);

  return(slot_desc_initform(ARG_0(stackbase)));
}
EUFUN_CLOSE

EUFUN_2( Fn_find_slot_description, class, slot_name)
{
  if (typeof(class) != TYPE_CLASS) 
    CallError(stacktop,
	      "non class in find_slot_description",class,NONCONTINUABLE);
  if (!is_symbol(slot_name))
    CallError(stacktop,
	      "non symbol for slot name in find_slot_description",slot_name,
	      NONCONTINUABLE);

  if (class->CLASS.slot_table == nil) 
    return(nil);
  else
    return(EUCALL_2(Fn_table_ref,class->CLASS.slot_table,slot_name));
}
EUFUN_CLOSE

/* Initialise this 'module' */
#define SET_ASSOC(a,b) \
  { LispObject tmp,tmp2; \
    STACK_TMP(a); \
    tmp2=b; \
    UNSTACK_TMP(tmp); \
    set_anon_associate(stacktop,tmp,tmp2); \
  }
void initialise_slots(LispObject *stacktop)
{
  (void) make_module_function(stacktop,"slot-description-name",
			      Fn_slot_description_name,1);
  SET_ASSOC(make_module_function(stacktop,"slot-description-position",
				 Fn_slot_description_position,1),
       make_unexported_module_function(stacktop,"s-d-p-s",
				       Fn_slot_description_position_setter,2));
  (void) make_module_function(stacktop,"slot-description-initargs",
			      Fn_slot_description_initargs,1);
  (void) make_module_function(stacktop,"slot-description-initform",
			      Fn_slot_description_initform,1);
}
