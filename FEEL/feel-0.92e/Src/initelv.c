/* ******************************************************************** */
/* init_elvira.c     Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Interpreter elvira.			                                */
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, August 1990
 */

#include "defs.h"
#include "structs.h"

/* No Elvira as yet... */

/* LispObject dlp; */

void initialise_elvira_modules(LispObject *stacktop) 
{
  extern LispObject dp;
  add_root(&dp);
}

