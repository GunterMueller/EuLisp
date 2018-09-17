/* */
#ifndef lint
static char *RcsId="$Id: debug.c,v 1.1 1994/02/08 14:14:18 djb Exp $";
#endif

/* Functions for Debugging
 * This file is part of YY-server of YYonX (1.2 Distribution)
 * $Id: debug.c,v 1.1 1994/02/08 14:14:18 djb Exp $
 */

/****************************************************************************
;;;
;;;  Copyright (C) 1989,1990,1991 Aoyama Gakuin University
;;;
;;;		All Rights Reserved
;;;
;;; This software is developed for the YY project of Aoyama Gakuin University.
;;; Permission to use, copy, modify, and distribute this software
;;; and its documentation for any purpose and without fee is hereby granted,
;;; provided that the above copyright notices appear in all copies and that
;;; both that copyright notice and this permission notice appear in 
;;; supporting documentation, and that the name of Aoyama Gakuin
;;; not be used in advertising or publicity pertaining to distribution of
;;; the software without specific, written prior permission.
;;;
;;; This software is made available AS IS, and Aoyama Gakuin makes no
;;; warranty about the software, its performance or its conformity to
;;; any specification. 
;;;
;;; To make a contact: Send E-mail to ida@csrl.aoyama.ac.jp for overall
;;; issues. To ask specific questions, send to the individual authors at
;;; csrl.aoyama.ac.jp. To request a mailing list, send E-mail to 
;;; yyonx-request@csrl.aoyama.ac.jp.
;;;
;;; Authors:
;;;   Version 1.0 90/09/26 by Keisuke 'Keiko' Tanaka
;;;				(keisuke@csrl.aoyama.ac.jp)
;;;   Version 2.0 90/08/27 by Keisuke 'Keiko' Tanaka
;;;			Page Mode Territory is supported
;;;   Version 2.3 90/11/05 by Keisuke 'Keiko' Tanaka
;;;			Copyright Notice is rewritten
;;;
****************************************************************************/

/****************************************************************************
  $Revision: 1.1 $ Written by Keisuke 'Keiko' Tanaka
  $Date: 1994/02/08 14:14:18 $
****************************************************************************/

#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include "yydefs.h"

#define MAXDEBUGSTACKLEVEL	30

struct debug_stack {
    int dbLevel;
    char *dbLabel;
    char *dbFunc;
} ;
static struct debug_stack DebugStack[MAXDEBUGSTACKLEVEL];
static struct debug_stack *CurDebugStack;
static int DebugStackLevel;
static FILE *DebugFp;
static void (*DebugPrintFunc)();
static void (*DebugCloseFunc)();

static void debug_def_print(fp, fmt, a1,a2,a3,a4,a5,a6,a7,a8,a9)
    FILE *fp;
    char *fmt;
{
    fprintf(fp, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
    /* fflush(fp); */
}

static void debug_def_close(fp)
    register FILE *fp;
{
    fflush(fp);
    fclose(fp);
}

void debug_init(conf, outfile, pfunc, cfunc)
    char *conf;		/* Configuration File */
    char *outfile;	/* Output File */
    void (*pfunc)();
    void (*cfunc)();
{
    FILE *fp;
    long clock;
    DebugFp = stderr;
    DebugStackLevel = 0;
    if (outfile != (char *)NULL) {
	if ((DebugFp = fopen(outfile, "w")) == (FILE *)NULL)
	    DebugFp = stderr;
    }
    DebugPrintFunc = (pfunc? pfunc: debug_def_print);
    DebugCloseFunc = (cfunc? pfunc: debug_def_close);

    /* Read Configuration File */
    if ((fp = fopen(conf, "r")) != (FILE *)NULL) {
	char buf[1024];
	while (fgets(buf, sizeof(buf), fp))
	    parse_debug_line(buf);
	fclose(fp);
    }
    time(&clock);
    DebugPrintFunc(DebugFp, "Debug Start -- %s", ctime(&clock));
}

void debug_term()
{
    DebugCloseFunc(DebugFp);
}

bool debug_setfunc(label, func)
    char *label;
    char *func;
{
    if (DebugStackLevel >= MAXDEBUGSTACKLEVEL) {
	(*DebugPrintFunc)(DebugFp,
			  "***** Function '%s' *****\n", func);
	(*DebugPrintFunc)(DebugFp, "***** STACK OVERFLOW *****\n");
	return FALSE;
    }
    if (DebugStackLevel++ > 0)
	CurDebugStack++;
    else
	CurDebugStack = DebugStack;
    CurDebugStack->dbLabel = label;
    CurDebugStack->dbFunc = func;
    if ((CurDebugStack->dbLevel = debug_search_table(label, func)) > 0)
	(*DebugPrintFunc)(DebugFp, "***** Function '%s' (%d) *****\n",
			  func, CurDebugStack->dbLevel);
    return TRUE;
}

void debug_endfunc(func)
    char *func;
{
    register int depth = DebugStackLevel;
    register struct debug_stack *stk = CurDebugStack;
    if (depth <= 0)
	return;
    for ( ; depth > 0; depth--, stk--)
	if (strSAME(stk->dbFunc, func)) {
	    CurDebugStack = stk;
	    DebugStackLevel = depth;
	    break;
	}
    CurDebugStack--;
    DebugStackLevel--;
}

#define DEBUGON(l)	(DebugStackLevel>0 && ((l)<=CurDebugStack->dbLevel))

void debug_print(level, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
    int level;
    char *fmt;
{
    if (DEBUGON(level))
	(*DebugPrintFunc)(DebugFp,fmt,a1,a2,a3,a4,a5,a6,a7,a8,a9);
}

bool debug_on(level)
    int level;
{
    return (DEBUGON(level));
}

struct debug_control_table {
    char *ctrlLabel;
    char *ctrlFunc;
    int ctrlNumber;
    struct debug_control_table *ctrlNext;
} ;
typedef struct debug_control_table db_ctrl_tbl;

db_ctrl_tbl *DebugControlTable = (db_ctrl_tbl *)NULL;

parse_debug_line(str)
    char *str;
{
    db_ctrl_tbl *new;
    char *s;
    if (!isalpha(*str))
	return;
    new = (db_ctrl_tbl *)memALLOC(sizeof(db_ctrl_tbl));
    s = strDUP(str);
    /* First Argument - Label */
    new->ctrlLabel = s;
    for ( ; !isspace(*s) ; s++)
	if (*s == EOS) return;
    *s++ = EOS;
    while (isspace(*s)) s++;
    /* Second Argument - Function */
    new->ctrlFunc = s;
    for ( ; !isspace(*s) ; s++)
	if (*s == EOS) return;
    *s++ = EOS;
    while (isspace(*s)) s++;
    /* 3rd Argument - Level */
    new->ctrlNumber = atoi(s);
    new->ctrlNext = DebugControlTable;
    /* DebugPrintFunc(DebugFp, "DEBUG LABEL:%s, FUNC%s, LEVEL:%d\n",
       new->ctrlLabel, new->ctrlFunc, new->ctrlNumber); */
    DebugControlTable = new;
}

int debug_search_table(label, func)
    char *label;
    char *func;
{
    db_ctrl_tbl *tb, *pending;
    
    for (tb = DebugControlTable, pending = (db_ctrl_tbl *)NULL;
	 tb != (db_ctrl_tbl *)NULL; tb = tb->ctrlNext) {
	if (strSAME(tb->ctrlLabel, "*"))
	    continue;
	if (strSAME(tb->ctrlLabel, label)) {
	    if (strSAME(tb->ctrlFunc, func))
		return tb->ctrlNumber;
	    if (strSAME(tb->ctrlFunc, "*"))
		pending = tb;
	}
    }
    return(pending? pending->ctrlNumber: 0);
}
