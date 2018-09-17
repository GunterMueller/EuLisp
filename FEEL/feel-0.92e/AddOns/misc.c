#ifndef lint
static char *RcsId="$Header: /a_mount/brad/denton_export/denton/You/NewYou/AddOns/RCS/misc.c,v 1.1 1994/02/08 14:14:07 djb Exp $" ;
#endif

/* Miscellaneous Functions for YY-server
 * This file is part of YY-server of YYonX (1.2 Distribution)
 * $Header: /a_mount/brad/denton_export/denton/You/NewYou/AddOns/RCS/misc.c,v 1.1 1994/02/08 14:14:07 djb Exp $
 */

/****************************************************************************
;;;
;;;  Copyright (C) 1989,1990 Aoyama Gakuin University
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
;;;   Version 1.0 90/08/10 by Keisuke 'Keiko' Tanaka
;;;				(keisuke@csrl.aoyama.ac.jp)
;;;   Version 2.0 90/08/27 by Keisuke 'Keiko' Tanaka
;;;			Page Mode Territory was supported
;;;   Version 2.2 90/11/05 by Keisuke 'Keiko' Tanaka
;;;			Copyright Notice was rewritten
;;;
****************************************************************************/

/****************************************************************************
  $Revision: 1.1 $ Written by Keisuke 'Keiko' Tanaka
  $Date: 1994/02/08 14:14:07 $
****************************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include "yydefs.h"

void warning();
extern char *malloc();

/* void (*memALLOCC_ERRFUNC)() = warning; */

char *memALLOC(size)
    unsigned int size;
{
    register char *s = malloc(size);
    if (s == (char *)NULL)
	fprintf(stderr, "No enough memory..\n");
    return s;
}

char *strDUP(s)
    register char *s;
{
    register char *p = memALLOC(strlen(s)+1);
    if (p != (char *)NULL)
	strcpy(p, s);
    return p;
}

int getargs(s, av, max)
    register char *s;
    char *av[];
    register int max;
{
    register int num = 0;
    if (!isalnum(*s))
	return 0;
    while (*s != EOS && max-- > 0) {
	*av++ = s; num++;
	while (*s != EOS && !isspace(*s))
	    s++;
	if (*s != EOS)
	    *s++ = EOS;
    }
    return num;
}

FILE *do_file_open(def_dir_path, dir_path, fname, type)
    char *def_dir_path;	/* Default Directory Path */
    char *dir_path;	/* Directory Path */
    char *fname;
    char *type;
{
}
    

/*
 *
 *
 */
