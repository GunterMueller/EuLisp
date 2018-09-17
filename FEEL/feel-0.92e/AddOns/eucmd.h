/********
 Project: YY-X	
 Eulisp/YY header
 *******/


/****************************************************************************
;;;
;;;  Copyright (C) 1989 Aoyama Gakuin University
;;;
;;;		All Rights Reserved
;;;
;;; Permission to use, copy, modify, and distribute this software
;;; and its documentation for any purpose and without fee is hereby granted,
;;; provided that the above copyright notice appear in all copies and that
;;; both that copyright notice and this permission notice appear in 
;;; supporting documentation, and that the names of Aoyama Gakuin 
;;; not be used in advertising or publicity pertaining to distribution of
;;; the software without specific, written prior permission.
;;;
;;; Aoyama Gakuin provide this software AS IS without express or implied
;;; warranty.
;;; This software is made available AS IS, and Aoyama Gakuin make no
;;; warranty about the software, its performance or its conformity to
;;; any specification.
;;;
;;; Any person obtaining a copy of this software is requested to send
;;; their name and post office or electronic mail address to:
;;;    YY Coordinator
;;;    CSRL, Information Science Research Center
;;;    Aoyama Gakuin University
;;;    4-4-25 Shibuya, Shibuya-ku Tokyo, JAPAN 150
;;;    (yy-coordinator@csrl.aoyama.ac.jp)
;;;
****************************************************************************/

/****************************************************************************
#
# $Revision: 1.1 $ Written by Keisuke 'Keiko' Tanaka
#			$Date: 1993/01/18 12:57:24 $
# Log:
#  Version 1.0 is written by Keisuke 'Keiko' Tanaka
#				(keisuke@csrl.aoyama.ac.jp)
# 
****************************************************************************/

#define ARG_INT 1
#define ARG_STR 2
#define ARG_LIST 3
#define ARG_PAIR_LIST 4
#define ARG_FONTDATA 5

typedef char *command_function;

typedef struct _cmd_arg {
	int argType;
	int defVal;
	char *defPtr;
	char *argLabel;
} cmd_arg ;

struct _reply_entry {
  int reType;
  char *reDoc;
} ;
typedef struct _reply_entry reply_entry;

struct _command_entry {
  char *cmdLabel;
  cmd_arg *cmdArgs;
  char *cmdComment;
  reply_entry *ceReplyData;
} ;


typedef struct _command_entry command_entry;



struct _command_control_entry {
	command_entry *ceTable;
	int ceTableSize;
	int ceDoneInit;
	int ceDoneSave;
	int ceExitFlag;
	int ceNoEchoFlag;
	int ceErrCode;
	int ceMaxCmdLeng;
	char *ceHelpFmt;
} ;

typedef struct _command_control_entry CMD;

#define CMDERRCODE(ctrl)	((ctrl)->ceErrCode)
#define CMDSETEXIT(ctrl)	((ctrl)->ceExitFlag = 1)
#define CMDCHKEXIT(ctrl)	((ctrl)->ceExitFlag)

#define CMDTBLSIZE(ct)	(sizeof(ct)/sizeof(command_entry))

#define CMDMAXONELINE	1024
#define CMDMAXARGS	1024

/* Command types */
#define CMD_COMMAND 1
#define CMD_INSTRUCTION 2
#define CMD_NOTIFICATION 3
