/* Header File for Font Handling
 * This file is part of YY-server of YYonX (1.3 Distribution)
 * $Id: yyfont.h,v 1.1 1994/02/08 14:12:24 djb Exp $
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
;;;   Version 1.0 90/08/26 by Keisuke 'Keiko' Tanaka
;;;				(keisuke@csrl.aoyama.ac.jp)
;;;   Version 2.0 90/08/27 by Keisuke 'Keiko' Tanaka
;;;			Page Mode Territory is supported
;;;   Version 2.2 90/11/05 by Keisuke 'Keiko' Tanaka
;;;			Copyright Notice is rewritten
;;;
****************************************************************************/

/****************************************************************************
  $Revision: 1.1 $ Written by Keisuke 'Keiko' Tanaka
  $Date: 1994/02/08 14:12:24 $
****************************************************************************/

struct _yy_char_info {
    int ciAscent;
    int ciDescent;
    int ciHeight;
    int ciWidth;
    int ciRBearing;
    int ciLBearing;
} ;
typedef struct _yy_char_info	YYCHAR;

struct _yy_font_x_table {
    struct _yy_font_x_info {
	Font fxFontID;
	char *fxFontName;
	YYCHAR fxMaxBound;
	YYCHAR fxMinBound;
	XFontStruct *fxFontStruct;
    } fxFontInfo;
    struct _yy_font_x_table *fxNext;
} ;
typedef struct _yy_font_x_table	yy_font_x_table;
typedef struct _yy_font_x_info	YYXFONT;

struct _yy_font_table {
    struct _yy_font_info {
	bool fiLoaded;
	int fiFID;
	char *fiFontName;
	YYCHAR fiMaxBound;
	YYCHAR fiMinBound;
	YYXFONT *fiSingle;
	YYXFONT *fiDouble;
    } yfFontInfo;
    struct _yy_font_table *yfNext;
} ;

#define MAXB1ASCENT(yfont)	((yfont)->fiSingle->fxMaxBound.ciAscent)
#define MAXB2ASCENT(yfont)	((yfont)->fiDouble->fxMaxBound.ciAscent)
#define MAXB1DESCENT(yfont)	((yfont)->fiSingle->fxMaxBound.ciDescent)
#define MAXB2DESCENT(yfont)	((yfont)->fiDouble->fxMaxBound.ciDescent)

struct _text_box_entry {
    int x, y;	/* Left Top */
    int w, h;	/* Width and Height */
} ;
typedef struct _text_box_entry	TEXTBOX;

extern YYFONT *call_font();
extern TEXTBOX *YYTextSize();
extern int YYTextWidth();
extern void do_draw_text_on_page();
