/* ******************************************************************** */
/*  defs.h           Copyright (C) Codemist and University of Bath 1989 */
/*                                                                      */
/* Basic definitions global constants and config options                */
/* ******************************************************************** */

/*
 * Change Log:
 *   Version 1, April 1989
 */

#ifndef DEFS_H
#define DEFS_H

#ifndef TRUE
#define TRUE	(1)
#define FALSE 	(0)
#endif

#define CONTINUABLE	(2)
#define NONCONTINUABLE	(3)

#define STUB(name) printf("*** stub %s called\n", name)

#define IGNORE(name)	(name = name)

/* reverse a few flags */
#ifndef LOWTAGINTS
#define NOLOWTAGINTS
#endif

#define WITH_SYS_TIMES
#ifndef DEBUG
#define NODEBUG
#endif

#ifdef MACHINE_SYSTEMV
#define MAX_PROCESSORS (10)
#endif

#ifdef MACHINE_BSD
#define MAX_PROCESSORS (1)
#endif

#ifdef MACHINE_ANY
#define MAX_PROCESSORS (1)
#endif

#ifndef MAX_PROCESSORS
 "Specify one of BSD, ANY, SYSTEMV"
#endif

#define RESET_GLOBAL_STACK() /** GC_STACK_POINTER() = stacktop**/

#ifdef ALIGN8
#define BYTE_ALIGNMENT 8
#else 
#define BYTE_ALIGNMENT 4
#endif

#if defined(NORCROFT) || defined(UNSIGNED_CHARS)
#define CHAR_IS_UNSIGNED
#define SIGNED_CHAR signed char
#else
#define CHAR_IS_SIGNED
#endif

/* Configuration section */

/* Variables that we  don't want to own up about 
   STACK_START_MISALIGNED: Clipper requires stack-pointers to be
                           multiples of 4, and not multiples of 8
   FD_SET_PATCH_NEEDED: 4.2 BSD machine
   FEEL_RC_FILE: 	name of initialisation file 
   CHECK_KEYBOARD:		signals are broken, so put ^C catcher in interpret loop
 */

/* Standard values... */
#define HAS_POPEN
#define FEEL_RC_FILE "/.feelrc"
#undef HAS_BCOPY /* All systems have memcpy --- right */

/* Machine dependant changes... */
#ifdef __clipper
#define STACK_START_MISALIGNED
#define FD_SET_PATCH_NEEDED
#endif

#ifdef msdos
#undef HAS_POPEN
#define FEEL_RC_FILE "./feel.rc"
#define DIR_SEP "\\" /* Actually not necessary... */
#define CHECK_KEYBOARD 5000
#endif

#if 1
#define HAS_READFILENO 
#endif

#ifdef NORCROFT
#undef HAS_POPEN
#endif

#ifndef DIR_SEP
#define DIR_SEP "/"
#endif

#endif /* DEFS_H */
/* End of defs.h */
