/* Common Definition for YY-server
 * This file is part of YY-server of YYonX (1.3 Distribution)
 * $Id: yydefs.h,v 1.1 1994/02/08 14:12:24 djb Exp $
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
;;;   Version 1.0 90/02/26 by Keisuke 'Keiko' Tanaka
;;;				(keisuke@csrl.aoyama.ac.jp)
;;;   Version 2.0 90/08/27 by Keisuke 'Keiko' Tanaka
;;;			Page Mode Territory is supported
;;;   Version 2.1 90/11/05 by Keisuke 'Keiko' Tanaka
;;;			Copyright Notice is rewritten
;;;
****************************************************************************/

/****************************************************************************
  $Revision: 1.1 $ Written by Keisuke 'Keiko' Tanaka
  $Date: 1994/02/08 14:12:24 $
****************************************************************************/

typedef int	bool;
typedef u_char	byte;

/* #define private		static */

#define TRUE		1
#define FALSE		0

#define EOS		'\0'
#define EOL		'\n'
#define SINGLEQUOTE	'\''
#define DOUBLEQUOTE	'\"'
#define BACKSLASH	'\\'
#define ERR		(-1)

extern int YyErrNo;
extern int SysErrNo;

extern char *getenv();
extern char *malloc();
#ifndef EULISP
#define xalloc  malloc
#define xfree   free
#endif
#ifdef EULISP
#define xalloc feel_malloc
#define xfree feel_free
#define free feel_free
#define malloc feel_malloc
#endif

#define strSAME(s1,s2)	(strcmp((s1),(s2))==0)
#define MAX(a1,a2)	((a1)>(a2)?(a1):(a2))
#define MIN(a1,a2)	((a1)<(a2)?(a1):(a2))
#define XOR(a1,a2)	((a1)^(a2))

#include <errno.h>
#include <sys/time.h>
#if 0
#include <sysexits.h>
#endif
#include "yydefault.h"
#include <ctype.h>

#define KEYINPUTBUFLEN	80

#define YYPI2		23040	/* 64*360 */
#define YYPI3_2		17280	/* 64*270 */
#define YYPI		11520	/* 64*180 */
#define YYPI_2		5760	/* 64*90 */

extern char *Version;

extern void cleanup();
extern void warning();
extern char *mamALLOC();
extern char *strDUP();
