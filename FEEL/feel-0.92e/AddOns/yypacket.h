/* Header File foe YY-Packet
 * This file is part of YY-server of YYonX (1.3 Distribution)
 * $Id: yypacket.h,v 1.1 1994/02/08 14:12:24 djb Exp $
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
;;;   Version 2.3 90/11/05 by Keisuke 'Keiko' Tanaka
;;;			Copyright Notice is rewritten
;;;   Version 2.10 91/01/30 by Keisuke 'Keiko' Tanaka
;;;			Add definitions for frame-animation and image handling
;;;
****************************************************************************/

/****************************************************************************
  $Revision: 1.1 $ Written by Keisuke 'Keiko' Tanaka
  $Date: 1994/02/08 14:12:24 $
****************************************************************************/

/***************************************************************************
 * Packet Header:
 *  +----------------+----------------+----------------+----------------+
 *  | COMMAND        |  P  A  C  K  E  T  L  E  N  G  T  H              |
 *  +----------------+----------------+----------------+----------------+
 *  |                |                |                | TYPE           |
 *  +----------------+----------------+----------------+----------------+
 *
 ***************************************************************************/

typedef u_long				x_color;
typedef struct _yy_comm_channel		yy_comm_channel;
typedef struct _yy_packet_header	yy_packet_header;
typedef struct _yy_packet_block		yy_packet_block;
typedef struct _yy_packet		yy_packet;
typedef struct _yy_packet_queue_ent_	YYPKTQUEENT;
typedef struct _yy_packet_system_queue	yy_packet_system_queue;

struct _yy_packet_header {
    byte hdCommand;
    byte hdPacketLength[3];
    byte hdDummy[3];
    byte hdPacketType:6, hdBlockType:2;
} ;
#define YYPACKET_BLOCKTYPE		3
#define YYPACKET_BLOCKTYPE_ONLYONE	0
#define YYPACKET_BLOCKTYPE_FIRSTBLOCK	1
#define YYPACKET_BLOCKTYPE_CONTINUE	2
#define YYPACKET_BLOCKTYPE_LASTBLOCK	3

struct _yy_packet_block {
    /* Data Area */
    byte *pbBuf;	/* data in this block */
    int pbBlockSize;	/* buffer size */
    int pbLength;	/* length of meaningful data */
    /* Read/Write Control */
    int pbUnreadLength;	/* not read.. */
    int pbFreeSize;	/* size of free area this block */
    /* List */
    struct _yy_packet_block *pbNextBlock;
    struct _yy_packet_block *pbPrevBlock;
} ;

struct _yy_packet {
    /* These Entries are brought from Packet */
    int pktPriority;
    int pktLength;
    int pktType;
    int pktCommand;
    struct _yy_packet_block pktFirstBlock;
    /* Read/Write Control */
    bool pktReadFlag;	/* TRUE: read, FLASE: write */
    int pktUnreadLength;	/* Unread data */
    struct _yy_packet_block *pktRWBlock;
    byte *pktRWPtr;		/* */
} ;

struct _yy_packet_queue_ent_ {
    struct _yy_packet		 *pqPacketBody;
    struct _yy_packet_queue_ent_ *pqNextPacket;
    struct _yy_packet_queue_ent_ *pqPrevPacket;
} ;

struct _yy_packet_system_queue {
    struct _yy_packet_queue_ent_ *sysqSend;
    struct _yy_packet_queue_ent_ *sysqRecv;
    struct _yy_packet_queue_ent_ *sysqRead;
    struct _yy_packet_queue_ent_ *sysqWrite;
    struct _yy_packet_queue_ent_ *sysqEvent;
} ;

struct _yy_channel_status {
    int csDomain;		/* UNIX Domain or INET Domain */
    bool csTimeDump;	/* Time dumped per Packet */
    struct _yy_time_table *csTblTop;
    struct _yy_time_table *csTblEnd;
    int csRecvPkt;
    int csSendPkt;
    int csEventPkt;
} ;
struct _yy_connection {
    int FD;		/* Socket */
    int SPackets;
    int SBlocks;
    int SBytes;
    int RPackets;
    int RBlocks;
    int RBytes;
} ;
typedef struct _yy_connection	yy_connection;

typedef struct _yy_font_table	yy_font_table;

struct _yy_comm_channel {
    struct _yy_channel_for_x_server {
	int xFd;			/* X Connection */
	bool xNeedFlush;		/* Should we send XFlush Request? */
	char *xPrivate;			/* X Window Control */
    } ccXChannel;
    struct _yy_channel_for_yy_client {
	int yyDebugMode;
	int yyServNo;			/* Channel # */
	int yyUnixReqFd;		/* for Multi Connection */
	int yyInetReqFd;		/* for Multi Connection */
	int yyEvtReqFd;			/* for Multi Connection */
	struct _yy_connection yyCmdCH;	/* Command Stream */
	struct _yy_connection yyEvtCH;	/* Event Stream */
	int yySyncCount;
	int yySyncPacketNum;
	int yyMaxPacketSize;
	bool yyHaveKeyIN;
	struct _yy_channel_status yyStatBuf;
    } ccYYChannel;
    yy_font_table *ccFontTable;
    yy_packet_system_queue ccSystemQueue;
    struct _yy_timeout_table {
	int toWaitTime;
	int toXFlushCount;
	int toKeyinCount;
	int toDClickCount;
    } ccYYTimeOutTable;
#ifdef DEBUG
    struct {
	int dbWriteBlocked;
	struct timeval dbTimer;
    } ccDeBugTable;
#endif
} ;
typedef	struct _yy_timeout_table yy_timeout_table;

#define YYDB_NONE		0
#define YYDB_SYNC		01
#define DebugMode(ch)		((ch)->ccYYChannel.yyDebugMode)
#define ccYYDebugMode		ccYYChannel.yyDebugMode
#define DebugSyncMode(ch)	(DebugMode(ch) & YYDB_SYNC)

#define ccXFd			ccXChannel.xFd
#define ccXNeedFlush		ccXChannel.xNeedFlush
#define ccXPrivate		ccXChannel.xPrivate

#define ccYYServNo		ccYYChannel.yyServNo

#define ccYYFd			ccYYChannel.yyCmdCH.FD
#define ccYYCmdCH		ccYYChannel.yyCmdCH
#define ccYYFd2			ccYYChannel.yyEvtCH.FD
#define ccYYEvtCH		ccYYChannel.yyEvtCH

#define ccYYUnixReqFd		ccYYChannel.yyUnixReqFd
#define ccYYInetReqFd		ccYYChannel.yyInetReqFd
#define ccYYSyncCount		ccYYChannel.yySyncCount
#define ccYYSyncPacketNum	ccYYChannel.yySyncPacketNum
#define ccYYHaveKeyIN		ccYYChannel.yyHaveKeyIN
#define ccYYStat		ccYYChannel.yyStatBuf

#define ccYYWaitTime		ccYYTimeOutTable.toWaitTime
#define ccYYXFlushCount		ccYYTimeOutTable.toXFlushCount
#define ccYYKeyinCount		ccYYTimeOutTable.toKeyinCount
#define ccYYDClickCount		ccYYTimeOutTable.toDClickCount

#define ccDBTimer		ccDeBugTable.dbTimer

#define QUE(c)			(&(c)->ccSystemQueue)

extern void reset_packet_read_ptr();
extern yy_packet *alloc_new_yy_packet();
extern yy_packet *create_error_packet();
extern void append_packet_entry_integer();
extern void append_packet_entry_bytes();
extern void append_packet_entry_onebyte();
extern void append_packet_entry_string_with_length();
extern void append_packet_entry_color();
#define append_packet_entry_string(p, s)	\
	append_packet_entry_string_with_length((p), strlen((s)), (s))
extern int read_packet_entry_integer();
extern int read_packet_entry_bytes();
extern int read_packet_entry_onebyte();
extern int read_packet_entry_string();
#ifdef COLORNAME
extern char *read_packet_entry_color();
#else
extern x_color read_packet_entry_color();
#endif
extern void dump_yy_packet();

#define YYPACKETTYPE_NONE	00
#define YYPACKETTYPE_COMMAND	01
#define YYPACKETTYPE_SYNC	02
#define YYPACKETTYPE_ACK	04
#define YYPACKETTYPE_NACK	05
#define YYPACKETTYPE_EVENT	010
#define YYPACKETTYPE_ERROR	040

#define YYCOMMAND_INIT				0

#define YYCOMMAND_CREATE_TERRITORY		1
#define YYCOMMAND_DISPLAY_TERRITORY		2
#define YYCOMMAND_MOVE_TERRITORY		3
#define YYCOMMAND_RESIZE_TERRITORY		4
#define YYCOMMAND_DESTROY_TERRITORY		5
#define YYCOMMAND_REPARENT_TERRITORY		6
#define YYCOMMAND_RAISE_TERRITORY		7
#define YYCOMMAND_LOWER_TERRITORY		8

#define YYCOMMAND_LOAD_FONT			10

#define YYCOMMAND_DRAW_POINT			20
#define YYCOMMAND_DRAW_LINE			21
#define YYCOMMAND_DRAW_CIRCLE			22
#define YYCOMMAND_DRAW_LINES			23
#define YYCOMMAND_DRAW_POLYGON			24
#define YYCOMMAND_DRAW_ARC			25
#define YYCOMMAND_DRAW_RECTANGLE		26

#define YYCOMMAND_FILL_POLYGON			27
#define YYCOMMAND_FILL_RECTANGLE		28
#define YYCOMMAND_FILL_CIRCLE			29
#define YYCOMMAND_FILL_ARC			30

#define YYCOMMAND_DRAW_TEXT			31

#define YYCOMMAND_CLEAR_TERRITORY		32
#define YYCOMMAND_DRAW_BACKGROUND		33
#define YYCOMMAND_CREATE_BITMAP			34
#define YYCOMMAND_OPERATE_BITBLT		35
#define YYCOMMAND_SAVE_PICTURE			36
#define YYCOMMAND_LOAD_PICTURE			37
#define YYCOMMAND_TRANSLATE_PICTURE		38
#define YYCOMMAND_ROTATE_PICTURE		39

#define YYCOMMAND_LOAD_FILE			40

#define YYCOMMAND_DRAW_OVAL			41
#define YYCOMMAND_FILL_OVAL			42
#define YYCOMMAND_DRAW_VTEXT			43
#define YYCOMMAND_DRAW_RTEXT			44

#define YYCOMMAND_QUERY_COLOR			45

#define YYCOMMAND_GET_COLOR_NUMRGB		46
#define YYCOMMAND_GET_COLOR_RGB			49
#define YYCOMMAND_GET_COLOR_NUMBER		50
#define YYCOMMAND_CHANGE_COLOR_RGB		51
#define YYCOMMAND_FREE_COLOR			52

#define YYCOMMAND_DEFINE_ANIMATION_FRAME	54
#define YYCOMMAND_START_ANIMATION		55
#define YYCOMMAND_STOP_ANIMATION		56
#define YYCOMMAND_DESTROY_ANIMATION_FRAME	57
#define YYCOMMAND_PUT_ANIMATION_FRAME		58

#define YYCOMMAND_GET_IMAGE			60
#define YYCOMMAND_PUT_IMAGE			61

#define YYCOMMAND_KEY_EVENT			70
#define YYCOMMAND_SELECT_TERRITORY		71
#define YYCOMMAND_SET_EVENT_MASK		72
#define YYCOMMAND_MOUSE_EVENT			73
#define YYCOMMAND_MASK_EVENT			74

#define YYCOMMAND_SET_PAGE_ATTRIBUTE		80
#define YYCOMMAND_MOVE_PAGE_TERRITORY		81
#define YYCOMMAND_START_INPUT			82
#define YYCOMMAND_ABORT_INPUT			83
#define YYCOMMAND_DRAW_TEXT_ON_PAGE		84
#define YYCOMMAND_GET_POSITION_ON_PAGE		85
#define YYCOMMAND_CHANGE_PAGE_SIZE		86

#define YYCOMMAND_CREATE_CURSOR_TERRITORY	90
#define YYCOMMAND_MOVE_CURSOR_HOTSPOT		91
#define YYCOMMAND_CHANGE_CURSOR_BITMAP		92
#define YYCOMMAND_MOVE_CURSOR			93
#define YYCOMMAND_DESTROY_CURSOR		94
#define YYCOMMAND_GET_CURSUR_POSITION		95
#define YYCOMMAND_DISPLAY_CURSOR		52

#define YYCOMMAND_DEBUG_DO_INPUT		250
#define YYCOMMAND_DEBUG_RESET_TIMER		251
#define YYCOMMAND_DEBUG_SET_TIMER		252
#define YYCOMMAND_DEBUG_SHOW_TABLE		253
#define YYCOMMAND_DEBUG_LIST_COLOR		254
#define YYCOMMAND_DEBUG_LIST_TERRITORY		255


#define YYNOERROR			0
#define YYERROR_NOTYYPACKET		1
#define YYERROR_NODISP			2
#define YYERROR_NOTERRITORY		3
#define YYERROR_NOCOLOR			4
#define YYERROR_DUPROOT			5
#define YYERROR_NOROOT			6
#define YYERROR_TYPEMISMATCH		7
#define YYERROR_SYSERR			8
#define YYERROR_NOFONT			9

#define ALLOC_ACKPACKET(p)	\
	(alloc_new_yy_packet((p)->pktCommand, YYPACKETTYPE_ACK, 0, NULL))
#define ALLOC_NACKPACKET(p)	\
	(alloc_new_yy_packet((p)->pktCommand, YYPACKETTYPE_NACK, 0, NULL))
#define ALLOC_ERRPACKET(p)	\
	(alloc_new_yy_packet((p)->pktCommand, YYPACKETTYPE_ERROR, 0, NULL))
#define ALLOC_EVENTPACKET(cmd)	\
	(alloc_new_yy_packet(cmd, YYPACKETTYPE_EVENT, 0, NULL))
#define SETYYERRORCODE(p, code)	(append_packet_entry_integer((p), (code)))


#define YYMASK_ALL		0777777777

#define YYMASK_BUTTON_PRESS	07
#define YYMASK_RIGHT_PRESS	01
#define YYMASK_MIDDLE_PRESS	02
#define YYMASK_LEFT_PRESS	04

#define YYMASK_BUTTON_RELEASE	070
#define YYMASK_RIGHT_RELEASE	010
#define YYMASK_MIDDLE_RELEASE	020
#define YYMASK_LEFT_RELEASE	040

#define YYMASK_MOVE		0100
#define YYMASK_ENTER		0200
#define YYMASK_IN		YYMASK_ENTER
#define YYMASK_LEAVE		0400
#define YYMASK_OUT		YYMASK_LEAVE

#define YYMASK_STAY		01000

#define YYMASK_DOUBLE_CLICK	016000
#define YYMASK_RIGHT_DOUBLE	02000
#define YYMASK_MIDDLE_DOUBLE	04000
#define YYMASK_LEFT_DOUBLE	010000

#define YYMASK_INTR		0200000
#define YYMASK_META		0100000000
#define YYMASK_CTRL		0200000000
#define YYMASK_SHIFT		0400000000


#define YYIMAGEFORM_COLOR	0x8000
#define YYIMAGEFORM_GRAY	0x4000
#define YYIMAGEFORM_BW		0x2000
#define YYIMAGEFORM_NCW		1
#define YYIMAGEFORM_YY		2

/*
 * Local variables:
 * eval: (set-kanji-fileio-code 'EUC)
 * end:
 */
