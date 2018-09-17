/* Define Default Values for YYonX
 * This file is part of YY-server of YYonX (1.3 Distribution)
 * $Id: yydefault.h,v 1.1 1994/02/08 14:12:24 djb Exp $
 *
 * Modified for Eulisp...
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

#define YYFONTINFOFILE		"./yyfontinfo"
#define YYINPUTEDITFILE		"./yyinputedit"
#define YYSERVER_LABEL		"YY Window Tool Kit - Aoyama Gakuin Univ./CSK Corp."
#define YYWINDOWNAME		"YY Window Tool Kit"
#define YYICONNAME		"YY Window"
#define YYRESOURCENAME		"yywindow"
#define YYCLASSNAME		"YYWindow"

/* Gross system nasties */
#ifdef LITTLE_ENDIAN 
#define THREE_BYTE_OFFSET 0
#else
#define THREE_BYTE_OFFSET 1
#endif    

#define YYWINDEFAULTWIDTH	600
#define YYWINDEFAULTHEIGHT	600
#define YYWINMINWIDTH		600
#define YYWINMINHEIGHT		600
#define YYWINMAXWIDTH		2000
#define YYWINMAXHEIGHT		1000
#define YYWINDEFAULTBORDERWIDTH	2
#ifdef KEISUKEENV
#define YYWINDEBUGBORDERWIDTH	1
#else
#define YYWINDEBUGBORDERWIDTH	0
#endif

#define DEFAULTTIMEOUT		100
#define TIMEOUTFORXFLUSH	100
#define TIMEOUTFORKEYIN		100

#define YYMAXONEPACKETSIZE	1024

#define YYPROTO_UNIX_PORT		"/tmp/.YY."
#define YYPROTO_INET_PORT		6750

#define YYPROTO_MAGIC		14876

#define YYPROTO_VERSION			0
#define YYPROTO_RELEASE			1
#define YYPROTO_SYNC_COUNTER		15
#define YYPROTO_MOUSE_STAY_TIME		500

#ifdef KEISUKEENV
#define DEFAULTCHANNEL			2
#else
#define DEFAULTCHANNEL			0
#endif

#define YYDEFAULTCURSORFONT		XC_cross
