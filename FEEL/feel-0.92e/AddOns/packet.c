#ifndef lint
static char *RcsId
    = "$Id: packet.c,v 1.1 1994/02/08 14:13:55 djb Exp $";
#endif

/* Packets for YY-protocol
 * This file is part of YY-server of YYonX (1.3 Distribution)
 * $Id: packet.c,v 1.1 1994/02/08 14:13:55 djb Exp $
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
  $Date: 1994/02/08 14:13:55 $
****************************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include "yydefs.h"
#include "yypacket.h"


#ifdef EULISP
#include "feel_malloc.h"
#endif

/*
 * Packet consists ..
 */

/*
 *
 *    1 2 3 4 5 6 7 8  1 2 3 4 5 6 7 8  1 2 3 4 5 6 7 8  1 2 3 4 5 6 7 8
 *   +----------------+----------------+----------------+----------------+
 * 0 | Command        |      Packet Length                               |
 *   +----------------+----------------+----------------+----------------+
 * 1 | -------------- | -------------- | -------------- | Packet Type    |
 *   +----------------+----------------+----------------+----------------+
 * 2 |    Data ...                                                       |
 *
 */

/*
 * Packet Type:
 *  1 2 3 4 5 6 7 8
 *  X X X X X X      - Data Class
 *  0 0 0 0 0 0      - ignore data class
 *  0 0 0 0 0 1      - Command
 *  0 0 0 0 1 0      - Syncronous Packet
 *  0 0 0 1 0 0      - Reply for Command (ACK)
 *  0 0 0 1 1 1      - Reply for Command (NACK)
 *  0 0 1 0 0 0      - Event on Server
 *  1 0 0 0 0 0      - Error on Server
 *              X X  - Type
 *              0 0  - Only this one packet
 *              0 1  - this is a first packet of sequence
 *              1 0  - this is a middle packet of sequence
 *              1 1  - this is a last packet of sequence
 */

extern int YYPacketBlockSize;

#define YYPACKET_LENGTH(pkt)	((pkt)->pktLength)

#define YYPACKET_READ_PTR(pkt)	((pkt)->pktRWPtr)
#define YYPACKET_WRITE_PTR(pkt)	((pkt)->pktRWPtr)
#define YYPACKET_CUR_BLOCK(pkt)	((pkt)->pktRWBlock)
#define YYPACKET_NEXT_BLOCK(pkt)	((pkt)->pktRWBlock->pbNextBlock)

#define YYPACKET_LENGTH_INBLOCK(pkt) ((pkt)->pktRWBlock->pbLength)
#define YYPACKET_BLOCKSIZE(pkt)	((pkt)->pktRWBlock->pbBlockSize)
#define YYPACKET_FILLEDSIZE_INBLOCK(pkt)	\
	((pkt)->pktRWBlock->pbBlockSize - (pkt)->pktRWBlock->pbFreeSize)
#define YYPACKET_FREESIZE_INBLOCK(pkt) ((pkt)->pktRWBlock->pbFreeSize)
#define YYPACKET_UNREADLENGTH_INBLOCK(pkt)	\
	((pkt)->pktRWBlock->pbUnreadLength)
#define YYPACKET_READLENGTH_INBLOCK(pkt)	\
	((pkt)->pktRWBlock->pbLength - (pkt)->pktRWBlock->pbUnreadLength)

static void alloc_new_packet_block();
static int goto_next_packet_block();


/***************************************************************************
 * Packet Queue Control
 ***************************************************************************/

static YYPKTQUEENT *FreeQueueEntList
    = (YYPKTQUEENT *)NULL;

/*
 * YYPKTQUEENT *alloc_packet_queue_ent()
 *
 * $B?7$7$$%-%e!<%(%s%H%j$r3d$jEv$F$k(B
 */
static YYPKTQUEENT *alloc_packet_queue_ent()
{
    register YYPKTQUEENT *qent;
    debug_setfunc("packet", "alloc_packet_queue_ent");
    if (FreeQueueEntList != (YYPKTQUEENT *)NULL) {
	debug_print(5, "Get Entry from Free-List\n");
	qent = FreeQueueEntList;
	FreeQueueEntList = FreeQueueEntList->pqNextPacket;
    } else {
	debug_print(5, "Allocate New Area\n");
	qent = (YYPKTQUEENT *)memALLOC(sizeof(YYPKTQUEENT));
    }
    bzero((char *)qent, sizeof(YYPKTQUEENT));
    debug_endfunc("alloc_packet_queue_ent");
    return qent;
}

/*
 * void free_packet_queue_ent(qent)
 *
 * $B%-%e!<%(%s%H%j(B qent $B$r(B Free-List $B$K2C$(!$:FMxMQ$G$-$k$h$&$K$9$k(B
 */
static void free_packet_queue_ent(qent)
    register YYPKTQUEENT *qent;
{
    qent->pqNextPacket = FreeQueueEntList;
    FreeQueueEntList = qent;
}

/*
 * YYPKTQUEENT *append_queent_from_packet_queue(top, qent)
 *
 * $B%-%e!<%(%s%H%j(B qent $B$r(B top $B$r@hF,$H$9$k%-%e!<%(%s%H%j%j%9%H$N(B
 * $B:G8eHx$KDI2C$9$k(B
 * top $B$,(B NULL $B$G$"$l$P!$(Bqent $B$,%j%9%H$N@hF,$K$J$k(B
 *
 * Return Value:
 *  $B%-%e!<%j%9%H$N@hF,%"%I%l%9(B
 */
static YYPKTQUEENT *append_queent_from_packet_queue(top, qent)
    YYPKTQUEENT *top;
    register YYPKTQUEENT *qent;
{
    register YYPKTQUEENT *last = top;
    debug_setfunc("packet", "append_queent_from_packet_queue");
    qent->pqNextPacket = (YYPKTQUEENT *)NULL;
    if (last == (YYPKTQUEENT *)NULL) {
	top = qent;
    } else {
	while (last->pqNextPacket != (YYPKTQUEENT *)NULL)
	    last = last->pqNextPacket;
	last->pqNextPacket = qent;
    }
    qent->pqPrevPacket = last;
    debug_endfunc("append_queent_from_packet_queue");
    return top;
}

/*
 * YYPKTQUEENT *pick_queent_from_packet_queue(top, qent)
 *
 * $B%-%e!<%(%s%H%j(B qent $B$r(B top $B$r@hF,$H$9$k%-%e!<%(%s%H%j%j%9%H$N(B
 * $BCf$+$i<h$j=P$9(B
 *
 * Return Value:
 *  $B%-%e!<%j%9%H$N@hF,%"%I%l%9(B
 */
static YYPKTQUEENT *pick_queent_from_packet_queue(top, qent)
    YYPKTQUEENT *top;
    YYPKTQUEENT *qent;
{
    if (qent->pqPrevPacket != (YYPKTQUEENT *)NULL)
	qent->pqPrevPacket->pqNextPacket = qent->pqNextPacket;
    else
	top = qent->pqNextPacket;
    if (qent->pqNextPacket != (YYPKTQUEENT *)NULL)
	qent->pqNextPacket->pqPrevPacket = qent->pqPrevPacket;
    qent->pqNextPacket = qent->pqPrevPacket = (YYPKTQUEENT *)NULL;
    return top;
}

/***************************************************************************
 * Packet Data Area Control
 *
 * $B%Q%1%C%H%G!<%?%(%j%"$O%Q%1%C%H>e$N<B:]$N%G!<%?$r<}G<$9$kNN0h$N(B
 * $B$3$H$G$"$k(B
 ***************************************************************************/

struct _yy_packet_data_area_ent_ {
    byte *Ptr;
    struct _yy_packet_data_area_ent_ *Next;
} ;
typedef struct _yy_packet_data_area_ent_ YYPKTDAREAENT;

/* FreePktDataAreaList $B$OL$;HMQ$N%Q%1%C%H%G!<%?%(%j%"$N%j%9%H$G$"$k(B
 * UnusedPktDAreaEntList $B$O6u$-%Q%1%C%H%G!<%?%(%j%"$rJ];}$9$k$?$a$N(B
 * $B%G!<%?%(%j%"$N6u$-%j%9%H$G$"$k(B
 */
static YYPKTDAREAENT *FreePktDataAreaList
    = (YYPKTDAREAENT *)NULL;
static YYPKTDAREAENT *UnusedPktDAreaEntList
    = (YYPKTDAREAENT *)NULL;

/*
 * byte *alloc_packet_data_area()
 *
 * $B?7$7$$%Q%1%C%H%G!<%?NN0h$r3d$jEv$F$k(B
 */
byte *alloc_packet_data_area()
{
    register byte *ptr;
    debug_setfunc("packet", "alloc_packet_data_area");
    if (FreePktDataAreaList != (YYPKTDAREAENT *)NULL) {
	register YYPKTDAREAENT *dent;
	debug_print(9, "Get %dbytes area from free-list\n", YYPacketBlockSize);
	ptr = FreePktDataAreaList->Ptr;
	/* $B%(%s%H%j$r6u%(%s%H%j$N%j%9%H$X(B */
	dent = FreePktDataAreaList;
	FreePktDataAreaList = FreePktDataAreaList->Next;
	dent->Next = UnusedPktDAreaEntList;
	UnusedPktDAreaEntList = dent;
    } else {
	debug_print(9, "Allocate %dbytes area\n", YYPacketBlockSize);
	ptr = (byte *)memALLOC(YYPacketBlockSize);
    }
    debug_print(9, "ADDRESS for PacketDataArea:0x%x\n", ptr);
    debug_endfunc("alloc_packet_data_area");
    return ptr;
}

/*
 * void free_packet_data_area(ptr)
 *
 * $B%Q%1%C%H%G!<%?NN0h(B ptr $B$rL$;HMQ%G!<%?NN0h$N%j%9%H$K2C$($k(B
 */
void free_packet_data_area(ptr)
    register byte *ptr;
{
    register YYPKTDAREAENT *dent;
    debug_setfunc("packet", "free_packet_data_area");
    debug_print(9, "ADDRESS for PacketDataArea:0x%x\n", ptr);
    if (UnusedPktDAreaEntList != (YYPKTDAREAENT *)NULL) {
	dent = UnusedPktDAreaEntList;
	UnusedPktDAreaEntList = UnusedPktDAreaEntList->Next;
    } else {
	debug_print(9, "Allocate %dbytes area\n", sizeof(YYPKTDAREAENT));
	dent = (YYPKTDAREAENT *)memALLOC(sizeof(YYPKTDAREAENT));
    }
    dent->Ptr = ptr;
    dent->Next = FreePktDataAreaList;
    /* right ??? */
    FreePktDataAreaList = dent;
    debug_endfunc("free_packet_data_area");
    /*NoReturnValue*/
}





/***************************************************************************
 *
 ***************************************************************************/



/*
 * int fix_yy_packet(pkt)
 *
 * $B%Q%1%C%H%5%$%:!$3F%V%m%C%/$NM-8zD9$r(B fix
 * $B%X%C%@>pJs$r<}G<(B
 */

fix_yy_packet(pkt)
    register yy_packet *pkt;
{
    register yy_packet_block *pb = &pkt->pktFirstBlock;
    register yy_packet_header *hp;
    register bool num = 0;

    debug_setfunc("packet", "fix_yy_packet");
    debug_print(1, "Command: %d\n", pkt->pktCommand);
    pkt->pktLength = 0;
    for (pb = &pkt->pktFirstBlock;
	 pb != (yy_packet_block *)NULL; pb = pb->pbNextBlock, num++) {
	int leng;
	/* Fix Length of Block */
	leng = pb->pbLength = pb->pbBlockSize - pb->pbFreeSize;
	pkt->pktLength += leng;
	/* Fix Header */
	hp = (yy_packet_header *)pb->pbBuf;
	hp->hdCommand = pkt->pktCommand;
	leng >>= 2;
	bcopy(((byte *)&leng)+1, hp->hdPacketLength, 3);
	hp->hdPacketType = pkt->pktType;
	hp->hdBlockType = (num > 0? YYPACKET_BLOCKTYPE_CONTINUE:
			   YYPACKET_BLOCKTYPE_FIRSTBLOCK);
    }
    hp->hdBlockType = (num > 1? YYPACKET_BLOCKTYPE_LASTBLOCK:
		       YYPACKET_BLOCKTYPE_ONLYONE);
    YYPACKET_CUR_BLOCK(pkt) = &pkt->pktFirstBlock;
    debug_endfunc("fix_yy_packet");
}

/*
 * yy_packet *alloc_new_yy_packet(command, type, priority, block, leng)
 *
 * $B?7$7$$%Q%1%C%HNN0h$r3NJ]$9$k(B
 * command, type, priority $B$O$=$NDL$j$N$b$N$,@_Dj$5$l$k(B
 * block $B$,(B NULL $B$G$J$1$l$P!$$=$l$,:G=i$N%V%m%C%/$NFbMF$H$7$F:NMQ$5$l$k(B
 * block $B$,(B NULL $B$J$i$P(B YYPacketBlockSize $B$NNN0h$,3NJ]$5$l$k(B
 */
yy_packet *alloc_new_yy_packet(command, type, priority, block, leng)
    int command;
    int type;
    int priority;
    byte *block; int leng;
{
    register yy_packet *pkt = (yy_packet *)memALLOC(sizeof(yy_packet));
    debug_setfunc("packet", "alloc_new_yy_packet");

    debug_print(1, "Command: %d(%x)\n", command, command);
    if (pkt == (yy_packet *)NULL)
	return (yy_packet *)NULL;
    pkt->pktCommand = (command & 0377);
    pkt->pktType = type;
    pkt->pktPriority = priority;
    pkt->pktLength = 0;
    pkt->pktReadFlag = FALSE;
    pkt->pktRWBlock = &pkt->pktFirstBlock;
    if (block != (byte *)NULL) {
	pkt->pktFirstBlock.pbBuf = block;
	pkt->pktFirstBlock.pbBlockSize = leng;
	pkt->pktFirstBlock.pbFreeSize = sizeof(yy_packet_header);
	pkt->pktFirstBlock.pbLength = leng;
    } else {
	pkt->pktFirstBlock.pbBuf = alloc_packet_data_area();
	pkt->pktFirstBlock.pbBlockSize = YYPacketBlockSize;
	pkt->pktFirstBlock.pbFreeSize = YYPacketBlockSize;
	pkt->pktFirstBlock.pbLength = 0;
    }
    pkt->pktFirstBlock.pbUnreadLength = 0;
    pkt->pktFirstBlock.pbNextBlock = (yy_packet_block *)NULL;
    pkt->pktFirstBlock.pbPrevBlock = (yy_packet_block *)NULL;
    /* Skip Header Area */
    pkt->pktRWPtr = pkt->pktFirstBlock.pbBuf + sizeof(yy_packet_header);
    pkt->pktFirstBlock.pbFreeSize -= sizeof(yy_packet_header);
    debug_endfunc("alloc_new_yy_packet");
    return pkt;
}



void remove_yy_packet(pkt)
    yy_packet *pkt;
{
    register yy_packet_block *pb = &pkt->pktFirstBlock;
    register yy_packet_block *next;
    debug_setfunc("packet", "remove_yy_packet");
    free_packet_data_area(pb->pbBuf);
    for (pb = pb->pbNextBlock; pb != (yy_packet_block *)NULL; pb = next) {
	next = pb->pbNextBlock;
	free_packet_data_area(pb->pbBuf);
	free((char *)pb);
    }
    (void)xfree((char *)pkt);
    debug_endfunc("remove_yy_packet");
}

/*
 * append_packet_entry_integer(pkt, value)
 *
 * pkt $B$G<($5$l$k%Q%1%C%H$K@0?tCM(B value $B$rDI2C$9$k(B
 * $BDI2C0LCV$r<($9%]%$%s%?$OJQ99$5$l$k(B
 * $BDI2C0LCV$,(B4$B$N4p=`0LCV$G$J$$>l9g$O$9$9$a$k(B
 */
void append_packet_entry_integer(pkt, value)
    yy_packet *pkt;
    int value;
{
#ifdef FOURBYTESINT
    register int l;
    debug_setfunc("packet", "append_packet_entry_interger");
    debug_print(1, "Block Length = %d/%d\n",
		YYPACKET_FILLEDSIZE_INBLOCK(pkt), YYPACKET_BLOCKSIZE(pkt));
    if ((l = (YYPACKET_FILLEDSIZE_INBLOCK(pkt)&03)) > 0) {
	l = 4 - l;
	YYPACKET_FREESIZE_INBLOCK(pkt) -= l;
	while (l-- > 0)
	    *YYPACKET_WRITE_PTR(pkt)++ = (byte)EOS;
    }
    if (YYPACKET_FREESIZE_INBLOCK(pkt) <= 0)
	alloc_new_packet_block(pkt, (byte *)NULL);
    (void) bcopy((byte *)&value, YYPACKET_WRITE_PTR(pkt), 4);
    YYPACKET_WRITE_PTR(pkt) += 4;
    YYPACKET_FREESIZE_INBLOCK(pkt) -= 4;
    debug_endfunc("append_packet_entry_interger");
#else
    NOT YET;
#endif
}

/*
 * append_packet_entry_color(pkt, value)
 *
 * pkt $B$G<($5$l$k%Q%1%C%H$K(B X $B$N%+%i!<CM(B value $B$rDI2C$9$k(B
 * $BDI2C0LCV$r<($9%]%$%s%?$OJQ99$5$l$k(B
 * $BDI2C0LCV$,(B4$B$N4p=`0LCV$G$J$$>l9g$O$9$9$a$k(B
 *
 * $B8=CJ3,$G$O(B integer $B$H$J$C$F$$$k!%(B
 *
 */
void append_packet_entry_color(pkt, color)
    yy_packet *pkt;
    x_color color;
{
#ifdef FOURBYTESINT
    register int l;
    debug_setfunc("packet", "append_packet_entry_color");
    append_packet_entry_integer(pkt, color);
    debug_endfunc("append_packet_entry_color");
#else
    NOT YET;
#endif
}

/*
 * append_packet_entry_bytes(pkt, value, count)
 *
 * pkt $B$G<($5$l$k%Q%1%C%H$K@0?tCM(B value $B$N2<0L(B count $B%P%$%H$rDI2C$9$k(B
 * $BDI2C0LCV$r<($9%]%$%s%?$OJQ99$5$l$k(B
 * $BDI2C0LCV$NJd@5$O9T$J$o$J$$(B
 * count $B$N@5Ev@-$O%A%'%C%/$7$J$$(B
 */
void append_packet_entry_bytes(pkt, value, count)
    yy_packet *pkt;
    int value;
    int count;
{
#ifdef FOURBYTESINT
    byte *vp = (byte *)&value;
    vp += (4-count);
    while (count-- > 0) {
	if (YYPACKET_FREESIZE_INBLOCK(pkt) <= 0)
	    alloc_new_packet_block(pkt, (byte *)NULL);
	*YYPACKET_WRITE_PTR(pkt)++ = *vp++;
	YYPACKET_FREESIZE_INBLOCK(pkt)--;
    }
#else
    NOT YET;
#endif
}

/*
 * append_packet_entry_onebyte(pkt, value)
 *
 * pkt $B$G<($5$l$k%Q%1%C%H$K@0?tCM(B value $B$rDI2C$9$k(B
 * $BDI2C0LCV$r<($9%]%$%s%?$OJQ99$5$l$k(B
 * $BDI2C0LCV$NJd@5$O9T$J$o$J$$(B
 */
void append_packet_entry_onebyte(pkt, value)
    yy_packet *pkt;
    byte value;
{
#ifdef FOURBYTESINT
    if (YYPACKET_FREESIZE_INBLOCK(pkt) <= 0)
	alloc_new_packet_block(pkt, (byte *)NULL);
    *YYPACKET_WRITE_PTR(pkt)++ = value;
    YYPACKET_FREESIZE_INBLOCK(pkt)--;
#else
    NOT YET;
#endif
}

/*
 * append_packet_entry_string(pkt, size, string)
 *
 * pkt $B$G<($5$l$k%Q%1%C%H$KJ8;zNs(B str $B$rDI2C$9$k(B
 * str $BCf$NM-8zJ8;zNs?t$O(B size $B$,J];}$7$F$$$k(B
 * $BDI2C0LCV$r<($9%]%$%s%?$OJQ99$5$l$k(B
 * $BDI2C0LCV$,(B4$B$N4p=`0LCV$G$J$$>l9g$O$9$9$a$k(B
 *	(append_packet_entry_integer $B$K$h$k(B)
 */
void append_packet_entry_string_with_length(pkt, size, str)
    yy_packet *pkt;
    int size;
    byte *str;
{
#ifdef FOURBYTESINT
    append_packet_entry_integer(pkt, size);
    while (size > 3) {
	size -= 4;
	if (YYPACKET_FREESIZE_INBLOCK(pkt) <= 0)
	    alloc_new_packet_block(pkt, (byte *)NULL);
	bcopy(str, YYPACKET_WRITE_PTR(pkt), 4);
	str += 4;
	YYPACKET_WRITE_PTR(pkt) += 4;
	YYPACKET_FREESIZE_INBLOCK(pkt) -= 4;
    }
    if (size > 0) {
	int rest = 4 - size;
	if (YYPACKET_FREESIZE_INBLOCK(pkt) <= 0)
	    alloc_new_packet_block(pkt, (byte *)NULL);
	while (size-- > 0)
	    *YYPACKET_WRITE_PTR(pkt)++ = *str++;
	while (rest-- > 0)
	    *YYPACKET_WRITE_PTR(pkt)++ = (byte)EOS;
	YYPACKET_FREESIZE_INBLOCK(pkt) -= 4;
    }
#else
    NOT YET;
#endif
}

/*
 * void alloc_new_packet_block(pkt, block, leng)
 *
 * pkt $B$N8=:_$N%5%$%:$r(B fix $B$7$F(B
 * pkt $B$K?7$?$J%V%m%C%/$rDI2C$9$k(B
 * block $B$,(B NULL $B$G$J$1$l$P%V%m%C%/FbMF$H$7$F;H$&(B
 * block $B$,(B NULL $B$J$i?7$?$KNN0h$r3NJ](B
 */
static void alloc_new_packet_block(pkt, block, leng)
    yy_packet *pkt;
    byte *block;
    int leng;
{
    register yy_packet_block *last;
    register yy_packet_block *new;
    debug_setfunc("packet", "alloc_new_packet_block");
    new = (yy_packet_block *)memALLOC(sizeof(yy_packet_block));
    for (last = &pkt->pktFirstBlock;
	 last->pbNextBlock != (yy_packet_block *)NULL;
	 last = last->pbNextBlock)
	;
    debug_print(5, "Command on Last Block %d(0x%x) -- %x\n",
		last->pbBuf, last->pbBuf, *((int *)last->pbBuf));
    last->pbNextBlock = new;
    new->pbPrevBlock = last;
    new->pbNextBlock = (yy_packet_block *)NULL;
    if (block != (byte *)NULL) {
	new->pbLength = leng;
	new->pbBuf = block;
	new->pbBlockSize = leng;
	new->pbFreeSize = 0;
    } else {
	new->pbLength = 0;
	new->pbBuf = alloc_packet_data_area();
	new->pbBlockSize = YYPacketBlockSize;
	new->pbFreeSize = new->pbBlockSize - sizeof(yy_packet_header);
    }
    /* fix ptr - Skip Header */
    YYPACKET_CUR_BLOCK(pkt) = new;
    YYPACKET_WRITE_PTR(pkt) = new->pbBuf + sizeof(yy_packet_header);
    /* Skip Header */
    debug_endfunc("alloc_new_packet_block");
}

/*********************************************************************
 * Read Packet Entry..
 *
 * read_packet_entry_ $B%7%j!<%:$O$9$Y$FFI$_$@$70LCV$N0\F0$r9T$J$&(B
 *********************************************************************/

/*
 * reset_packet_read_ptr(pkt)
 */
void reset_packet_read_ptr(pkt)
    yy_packet *pkt;
{
    yy_packet_block *pb;
    yy_packet_header *hp;
    int leng;
    int num;
    debug_setfunc("packet", "reset_packet_read_ptr");
    pb = &pkt->pktFirstBlock;
    pkt->pktRWBlock = pb;
    pkt->pktRWPtr = pb->pbBuf + sizeof(yy_packet_header);
    pb->pbUnreadLength = pb->pbLength - sizeof(yy_packet_header);
    /* Get Header Info. */
    hp = (yy_packet_header *)(pb->pbBuf);
    /*pkt->pktCommand = (int)(hp->hdCommand);*/
    debug_print(1, "Command:%d(0x%x)\n", pkt->pktCommand, pkt->pktCommand);
    pkt->pktType = hp->hdPacketType;
    pkt->pktLength = 0;
    for (num = 0; pb != (yy_packet_block *)NULL; pb = pb->pbNextBlock, num++) {
	pkt->pktLength += pb->pbLength;
	debug_print(1, "PKT#%d <Length=%d>\n", num, pb->pbLength);
    }
    debug_print(1, "Packet Length = %d\n", pkt->pktLength);
    debug_endfunc("reset_packet_read_ptr");
}

/*
 * int read_packet_entry_integer(pkt)
 *
 * pkt $B$+$i@0?tCM$r0l$DFI$_9~$`(B
 * $BFI$_9~$`0LCV$,(B4$B$N4p=`0LCV$G$J$$>l9g$O$9$9$a$k(B
 */

int read_packet_entry_integer(pkt)
    yy_packet *pkt;
{
#ifdef FOURBYTESINT
    int val, l;
    if ((l = (YYPACKET_READLENGTH_INBLOCK(pkt)&3)) > 0) {
	l = 4 - l;
	YYPACKET_READ_PTR(pkt) += l;
	YYPACKET_UNREADLENGTH_INBLOCK(pkt) -= l;
    }
    if (YYPACKET_UNREADLENGTH_INBLOCK(pkt) <= 0)
	if (goto_next_packet_block(pkt) < 0)
	    return -1;
    (void) bcopy(YYPACKET_READ_PTR(pkt), (byte *)&val, 4);
    YYPACKET_READ_PTR(pkt) += 4;
    YYPACKET_UNREADLENGTH_INBLOCK(pkt) -= 4;
    return val;
#else
    NOT YET;
#endif
}

/*
 * u_long read_packet_entry_color(pkt)
 *
 * pkt $B$+$i(B X $B$N%+%i!<CM$r0l$DFI$_9~$`(B
 * $BFI$_9~$`0LCV$,(B4$B$N4p=`0LCV$G$J$$>l9g$O$9$9$a$k(B
 *
 */

#ifdef COLORNAME
char *read_packet_entry_color(pkt)
#else
x_color read_packet_entry_color(pkt)
#endif
	yy_packet *pkt;
{
#ifdef FOURBYTESINT
#ifdef COLORNAME
	int l;
	static char name[256];
	l = read_packet_entry_integer(pkt);
	read_packet_entry_string(pkt, l, name);
	return name;
#else /* !COLORNAME */
	x_color color;
	color = read_packet_entry_integer(pkt);
	return color;
#endif
#else
	NOT YET;
#endif
}

/*
 * int read_packet_entry_bytes(pkt, count)
 *
 * pkt $B$G<($5$l$k%Q%1%C%H$+$i(B count $B%P%$%H$r<h$j=P$9(B
 * $BFI$_$@$73+;O0LCV$NJd@5$O9T$J$o$J$$(B
 * count $B$N@5Ev@-$O%A%'%C%/$7$J$$(B
 */

int read_packet_entry_bytes(pkt, count)
	yy_packet *pkt;
	int count;
{
#ifdef FOURBYTESINT
	int val = 0;
	register byte *vp = (byte *)&val;
	vp += (4-count);
	while (count-- > 0) {
		if (YYPACKET_UNREADLENGTH_INBLOCK(pkt) <= 0)
			if (goto_next_packet_block(pkt) < 0)
				return -1;
		*vp++ = *YYPACKET_READ_PTR(pkt)++;
		YYPACKET_UNREADLENGTH_INBLOCK(pkt)--;
	}
	return val;
#else
	NOT YET;
#endif
}

/*
 * int read_packet_entry_onebyte(pkt)
 *
 * pkt $B$G<($5$l$k%Q%1%C%H$+$i(B 1$B%P%$%H$r<h$j=P$9(B
 * $BFI$_$@$73+;O0LCV$NJd@5$O9T$J$o$J$$(B
 */

int read_packet_entry_onebyte(pkt)
	yy_packet *pkt;
{
#ifdef FOURBYTESINT
	register int val;
	if (YYPACKET_UNREADLENGTH_INBLOCK(pkt) <= 0)
		if (goto_next_packet_block(pkt) < 0)
			return -1;
	val = *YYPACKET_READ_PTR(pkt)++;
	YYPACKET_UNREADLENGTH_INBLOCK(pkt)--;
	return val;
#else
	NOT YET;
#endif
}

/*
 * int read_packet_entry_string(pkt, size, buf)
 *
 * pkt $B$G<($5$l$k%Q%1%C%H$+$i(B size $B%P%$%H$NJ8;zNs$r<h$j=P$9(B
 * buf $B$O(B size+1 $B0J>e$NBg$-$5$r;}$D$H2>Dj$9$k(B
 * $BFI$_$@$73+;O0LCV$NJd@5$O9T$J$&(B
 */

int read_packet_entry_string(pkt, size, buf)
	yy_packet *pkt;
	register int size;
	register byte *buf;
{
#ifdef FOURBYTESINT
	int val, l, rest;
	if ((l = (YYPACKET_READLENGTH_INBLOCK(pkt)&3)) > 0) {
		l = 4 - l;
		YYPACKET_READ_PTR(pkt) += l;
		YYPACKET_UNREADLENGTH_INBLOCK(pkt) -= l;
	}
	for (rest = (size & 3), size -= 4; size >= 0; size -= 4) {
		if (YYPACKET_UNREADLENGTH_INBLOCK(pkt) <= 0)
			if (goto_next_packet_block(pkt) < 0)
				return -1;
		(void) bcopy(YYPACKET_READ_PTR(pkt), buf, 4);
		YYPACKET_READ_PTR(pkt) += 4;
		buf += 4;
		YYPACKET_UNREADLENGTH_INBLOCK(pkt) -= 4;
	}
	if (YYPACKET_UNREADLENGTH_INBLOCK(pkt) <= 0)
		if (goto_next_packet_block(pkt) < 0)
			return -1;
	(void) bcopy(YYPACKET_READ_PTR(pkt), buf, rest);
	YYPACKET_READ_PTR(pkt) += rest;
	buf += rest;
	YYPACKET_UNREADLENGTH_INBLOCK(pkt) -= rest;
	*buf = (byte)EOS;
	return 0;
#else
	NOT YET;
#endif
}

/*
 * int goto_next_packet_block(pkt)
 *
 * pkt $B$K$*$$$F<!$N%V%m%C%/$rFI$_=P$9=`Hw$r$9$k(B
 */

static int goto_next_packet_block(pkt)
    yy_packet *pkt;
{
    register yy_packet_block *new = YYPACKET_NEXT_BLOCK(pkt);
    debug_setfunc("packet", "goto_next_packet_block");
    if (new == (yy_packet_block *)NULL)
	return -1;
    YYPACKET_CUR_BLOCK(pkt) = new;
    /* Skip Header */
    YYPACKET_READ_PTR(pkt) = new->pbBuf + sizeof(yy_packet_header);
    new->pbUnreadLength = new->pbLength - sizeof(yy_packet_header);
    debug_endfunc("goto_next_packet_block");
    return 0;
}

/************************************************************************
 * Queue
 ***********************************************************************/

/*
 * yy_packet *get_packet_from_readq(queue)
 *
 * ReadQueue $B$+$i%Q%1%C%H$r0l$D<h$j=P$9(B
 * $B<h$j=P$7$?%Q%1%C%H$O(B queue $B$+$i$O$O$:$l!$FI$_=P$7MQ$K@_Dj$5$l$k(B
 */

yy_packet *get_packet_from_readq(queue)
    yy_packet_system_queue *queue;
{
    YYPKTQUEENT *oldq;
    yy_packet *pkt;
    debug_setfunc("packet", "get_packet_from_readq");
    if ((oldq = queue->sysqRead) == (YYPKTQUEENT *)NULL)
	return (yy_packet *)NULL;
    pkt = oldq->pqPacketBody;
    if ((queue->sysqRead = oldq->pqNextPacket) != (YYPKTQUEENT *)NULL)
	queue->sysqRead->pqPrevPacket = (YYPKTQUEENT *)NULL;
    reset_packet_read_ptr(pkt);
    if(debug_on(8))
	dump_yy_packet(pkt);
    free_packet_queue_ent(oldq);
    debug_endfunc("get_packet_from_readq");
    return pkt;
}

/*
 * void put_block_on_recvq(queue, block)
 *
 * block $B$r(B recv_queue $B$KEPO?$9$k(B
 * $B$=$NEPO?$K$h$j%Q%1%C%H$,$=$m$C$?$i<+F0E*$K(B
 * recv_queue $B$+$i(B read_queue $B$K$^$o$9(B
 */

void put_block_on_recvq(queue, block, leng)
    yy_packet_system_queue *queue;
    byte *block;
    int leng;
{
    YYPKTQUEENT *qent, *last_qent;
    int command;
    yy_packet_header *hp = (yy_packet_header *)block;
    debug_setfunc("packet", "put_block_on_recvq");
    if (block != (byte *)NULL) {
	command = (int)(hp->hdCommand);
    } else
	command = -1;
    debug_print(5, "Command:%d(0x%x), Block Length = %d\n",
		command, command, leng);

    /* Search Packet Entry and Register this block */
    for (qent = queue->sysqRecv, last_qent = (YYPKTQUEENT *)NULL;
	 qent != (YYPKTQUEENT *)NULL; qent = (last_qent = qent)->pqNextPacket)
	if (qent->pqPacketBody->pktCommand == command)
	    break;
    if (qent != (YYPKTQUEENT *)NULL) {
	alloc_new_packet_block(qent->pqPacketBody, block, leng);
    } else {
	qent = alloc_packet_queue_ent();
	qent->pqPacketBody
	    = alloc_new_yy_packet(command, 0, 0, block, leng);
	qent->pqNextPacket = (YYPKTQUEENT *)NULL;
	qent->pqPrevPacket = last_qent;
	if (last_qent != (YYPKTQUEENT *)NULL)
	    last_qent->pqNextPacket = qent;
	else
	    queue->sysqRecv = qent;
    }

    /* If this packet has been completed, move it into Read Queue */
    if (hp->hdBlockType == YYPACKET_BLOCKTYPE_ONLYONE
	|| hp->hdBlockType == YYPACKET_BLOCKTYPE_LASTBLOCK) {
	debug_print(8, " Move Packet from recvq to readq\n");
	/* remove this packet from RecvQueue */
	if (qent->pqNextPacket != (YYPKTQUEENT *)NULL)
	    qent->pqNextPacket->pqPrevPacket = qent->pqPrevPacket;
	if (qent->pqPrevPacket != (YYPKTQUEENT *)NULL)
	    qent->pqPrevPacket->pqNextPacket = qent->pqNextPacket;
	else
	    queue->sysqRecv = qent->pqNextPacket;
	/* Register it on Readq */
	if (queue->sysqRead == (YYPKTQUEENT *)NULL) {
	    queue->sysqRead = qent;
	    qent->pqPrevPacket = (YYPKTQUEENT *)NULL;
	    qent->pqNextPacket = (YYPKTQUEENT *)NULL;
	} else {
	    register YYPKTQUEENT *rq = queue->sysqRead;
	    while (rq->pqNextPacket != (YYPKTQUEENT *)NULL)
		rq = rq->pqNextPacket;
	    debug_print(8, " Put Packet on tail of readq\n");
	    rq->pqNextPacket = qent;
	    qent->pqPrevPacket = rq;
	    qent->pqNextPacket = (YYPKTQUEENT *)NULL;
	}
    }
    debug_endfunc("put_block_on_recvq");
}

int get_block_from_sendq(queue, bpp, lengp)
    yy_packet_system_queue *queue;
    byte **bpp;
    int *lengp;
{
	yy_packet *pkt;
	debug_setfunc("packet", "get_block_from_sendq");
	if (queue->sysqSend == (YYPKTQUEENT *)NULL) {
		debug_print(5, "SendQ has no packet\n");
		debug_endfunc("get_block_from_sendq");
		return 0;
	}
	pkt = queue->sysqSend->pqPacketBody;
	debug_print(3, "Take One Block from Packet\n");
	debug_print(5, "Command Number#%d, Packet Type 0%o\n",
		    pkt->pktCommand, pkt->pktType);
	if (YYPACKET_CUR_BLOCK(pkt) == (yy_packet_block *)NULL) {
		/* free area for this packet */
		YYPKTQUEENT *old = queue->sysqSend;
		YYPKTQUEENT *new = queue->sysqSend->pqNextPacket;
		debug_print(3, "Aha! This Packet has no more block..\n");
		remove_yy_packet(pkt);
		free_packet_queue_ent(old);
		if ((queue->sysqSend = new) == (YYPKTQUEENT *)NULL) {
			debug_print(5, "SendQ has no packet\n");
			debug_endfunc("get_block_from_sendq");
			return 0;
		}
		new->pqPrevPacket = (YYPKTQUEENT *)NULL;
		pkt = new->pqPacketBody;
		debug_print(3, "Take One Block from Packet\n");
		debug_print(5, "Command Number#%d, Packet Type 0%o\n",
			    pkt->pktCommand, pkt->pktType);
	}
	*lengp = YYPACKET_LENGTH_INBLOCK(pkt);
	*bpp = YYPACKET_CUR_BLOCK(pkt)->pbBuf;
	YYPACKET_CUR_BLOCK(pkt) = YYPACKET_NEXT_BLOCK(pkt);
	debug_endfunc("get_block_from_sendq");
	return *lengp;
}

int get_block_from_eventq(queue, bpp, lengp)
	yy_packet_system_queue *queue;
	byte **bpp;
	int *lengp;
{
	yy_packet *pkt;
	debug_setfunc("packet", "get_block_from_eventq");
	if (queue->sysqEvent == (YYPKTQUEENT *)NULL) {
		debug_print(5, "EventQ has no packet\n");
		debug_endfunc("get_block_from_eventq");
		return 0;
	}
	pkt = queue->sysqEvent->pqPacketBody;
	debug_print(3, "Take One Block from Packet\n");
	debug_print(5, "Command Number#%d, Packet Type 0%o\n",
		    pkt->pktCommand, pkt->pktType);
	if (YYPACKET_CUR_BLOCK(pkt) == (yy_packet_block *)NULL) {
		/* free area for this packet */
		YYPKTQUEENT *old = queue->sysqEvent;
		YYPKTQUEENT *new = queue->sysqEvent->pqNextPacket;
		debug_print(3, "Aha! This Packet has no more block..\n");
		remove_yy_packet(pkt);
		free_packet_queue_ent(old);
		if ((queue->sysqEvent = new) == (YYPKTQUEENT *)NULL) {
			debug_print(5, "EventQ has no packet\n");
			debug_endfunc("get_block_from_eventq");
			return 0;
		}
		new->pqPrevPacket = (YYPKTQUEENT *)NULL;
		pkt = new->pqPacketBody;
		debug_print(3, "Take One Block from Packet\n");
		debug_print(5, "Command Number#%d, Packet Type 0%o\n",
			    pkt->pktCommand, pkt->pktType);
	}
	*lengp = YYPACKET_LENGTH_INBLOCK(pkt);
	*bpp = YYPACKET_CUR_BLOCK(pkt)->pbBuf;
	YYPACKET_CUR_BLOCK(pkt) = YYPACKET_NEXT_BLOCK(pkt);
	debug_endfunc("get_block_from_eventq");
	return *lengp;
}

void put_packet_on_sendq(queue, pkt)
    yy_packet_system_queue *queue;
    yy_packet *pkt;
{
    register YYPKTQUEENT *cur;
    register YYPKTQUEENT **qp;

    debug_setfunc("packet", "put_packet_on_sendq");
    fix_yy_packet(pkt);
    if (debug_on(8))
	dump_yy_packet(pkt);
    qp = ((pkt->pktType == YYPACKETTYPE_EVENT)?
	  &queue->sysqEvent: &queue->sysqSend);
    if ((cur = *qp) == (YYPKTQUEENT *)NULL) {
	cur = *qp = alloc_packet_queue_ent();
	cur->pqPacketBody = pkt;
	cur->pqNextPacket = (YYPKTQUEENT *)NULL;
	cur->pqPrevPacket = (YYPKTQUEENT *)NULL;
    } else {
	register YYPKTQUEENT *new = alloc_packet_queue_ent();
	while (cur->pqNextPacket != (YYPKTQUEENT *)NULL)
	    cur = cur->pqNextPacket;
	cur->pqNextPacket = new;
	new->pqPacketBody = pkt;
	new->pqPrevPacket = cur;
	new->pqNextPacket = (YYPKTQUEENT *)NULL;
    }
    debug_endfunc("put_packet_on_sendq");
}

void dump_yy_packet(pkt)
    yy_packet *pkt;
{
    int num = 1;
    yy_packet_block *pb;
    debug_setfunc("packet", "dump_yy_packet");
    debug_print(1, "PacketLength: %d\n", YYPACKET_LENGTH(pkt));
    pb = &pkt->pktFirstBlock;
    for ( ; pb != (yy_packet_block *)NULL; pb = pb->pbNextBlock, num++) {
	byte *vp;
	int line, size;
	size = pb->pbBlockSize - pb->pbFreeSize;
	debug_print(2, "Block #%d - Block Size: %d, Length: %d\n",
		    num, pb->pbBlockSize, size);
	debug_print(3, "     0  1  2  3  4  5  6  7\n");
	vp = pb->pbBuf;
	line = 0;
	while (size > 0) {
	    byte *p;
	    int col, len;
	    debug_print(3, "%3d", line++);
	    for (p = vp, col = 8, len = size;
		 len > 0 && col > 0; len--, col--, p++)
		debug_print(3, " %02x", (int)*p);
	    while (col-- > 0)
		debug_print(3, "   ");
	    debug_print(3, "   ");
	    for (p = vp, col = 8, len = size;
		 len > 0 && col > 0; len--, col--, p++)
		debug_print(3, "%c",
			    (*p > ' ' && *p < 0177? *p: ' '));
	    vp = p;
	    size = len;
	    debug_print(3, "\n");
	}
    }
    debug_endfunc("dump_yy_packet");
}

#ifdef notdef

typedef struct _yy_packet_block_ YYPACKETBLOCK;
struct _yy_packet_block_ {
    int pbSize;		/* (Max) Size of Data Area */
    int pbLeng;		/* Length of Active Data */
    byte *pbBuf;	/* Data Area */
    struct _yy_packet_block_ *pbNextBlock;
    struct _yy_packet_block_ *pbPrevBlock;
} ;

#define YYPKTOP_NONE		0	/* $B2?$b$7$J$$(B ($B%(%i!<(B) */
#define YYPKTOP_READ		1	/* $B%Q%1%C%H9=B$$+$i%G!<%?$NFI$_$@$7(B */
#define YYPKTOP_WRITE		2	/* $B%Q%1%C%H9=B$$X$N%G!<%?$N=q$-9~$_(B */
#define YYPKTOP_RECV		3	/* $B%Q%1%C%H%G!<%?$N<u?.(B */
#define YYPKTOP_SEND		4	/* $B%Q%1%C%H%G!<%?$NAw?.(B */
#define YYPKTOP_ALLOC		5	/* $BNN0h$N3NJ](B */
#define YYPKTOP_FREE		6	/* $BNN0h$N3+J|(B */

struct _yy_packet_ {
    /* Packet Control Information */
    int pktOPType;			/* Operation Type */
    int pktPriority;			/* Priority */
    /* Packet Header Information */
    int pktYYOpe;			/* YY-Operation Number */
    int pktType;			/* Type of Packet */
    int pktLeng;			/* Total Size of this packet */
    YYPACKETBLOCK pktFirstBlock;	/* First Block of this packet */
    /* Packet Read/Write Control */
    YYPACKETBLOCK *pktCurBlock;		/* Current Block */
    union {
	byte *rwRead;			/* Read PTR */
	byte *rwWrite;			/* Write PTR */
    } pktRWPtrEnt;
    int pktActiveInBlock;		/* Length of Written Data in Block */
    int pktFreeInBlock;			/* Length of Room in Block */
    int pktReadInBlock;			/* Read Data in Block */
    int pktUnreadInBlock;		/* Unread Data in Block */
} ;
#define pktReadPtr	pktRWPtrEnt.rwRead
#define pktWritePtr	pktRWPtrEnt.rwWrite


/*
 * void goto_next_read_packet(pkt)
 *
 * YY $B$K$*$1$k%Q%1%C%H(B pkt $B$K$*$$$F!$8=:_$N%V%m%C%/$N(B
 * $B<!$N%V%m%C%/$rFI$_=P$9$h$&$K@_Dj$9$k(B
 */
static void goto_next_read_packet(pkt)
    register YYPACKET *pkt;
{
    register YYPACKETBLOCK *next;
#ifdef DEBUG_PRINT
    debug_setfunc("packet", "goto_next_read_packet");
#endif /*DEBUG_PRINT*/
    /* fix length of unread data in Packet */
    pkt->pktUnreadLength -= YYPACKET_CURBLOCK(pkt)->pbUnreadLength;
    YYPACKET_CURBLOCK(pkt)->pbUnreadLength = 0;
    next = YYPACKET_CURBLOCK(pkt)->pbNextBlock;
    if (next != (YYPACKETBLOCK *)NULL) {
	next->pbReadPtr = next->pbBuf + sizeof(YYPACKETHEADER);
	next->pbUnreadLength = next->pbLength - sizeof(YYPACKETHEADER);
	YYPACKET_CURBLOCK(pkt) = next;
    } else {
	pkt->pktUnreadLength = 0;
    }
#ifdef DEBUG_PRINT
    debug_endfunc("goto_next_read_packet");
#endif /*DEBUG_PRINT*/
    /*NoReturnValue*/
}

/*
 * void skip_packet_read_ptr(pkt, bytes)
 *
 * $B%Q%1%C%H(B pkt $B$NFI$_$@$70LCV$r(B bytes $B$@$1$9$9$a$k(B
 * bytes $B?J$a$k4V$K%V%m%C%/$NNN0h$,=*$o$C$F$7$^$C$?$i(B
 * $B<!$N%V%m%C%/$K?J$`(B
 */
static void skip_packet_read_ptr(pkt, bytes)
    register YYPACKET *pkt;
    register int bytes;
{
#ifdef DEBUG_PRINT
    debug_setfunc("packet", "skip_packet_read_ptr");
#endif /*DEBUG_PRINT*/
    if (YYPACKET_CURBLOCK(pkt)->pbUnreadLength < bytes) {
	bytes -= YYPACKET_CURBLOCK(pkt)->pbUnreadLength;
	goto_next_read_packet(pkt);
    }
    if (YYPACKET_CURBLOCK(pkt)->pbUnreadLength >= bytes) {
	YYPACKET_READ_PTR(pkt) += bytes;
	pkt->pktUnreadLength -= bytes;
	YYPACKET_CURBLOCK(pkt)->pbnreadLength -= bytes;
    } else {
	pkt->pktUnreadLength = 0;
	YYPACKET_CURBLOCK(pkt)->pbnreadLength = 0;
    }
#ifdef DEBUG_PRINT
    debug_endfunc("skip_packet_read_ptr");
#endif /*DEBUG_PRINT*/
    /*NoReturnValue*/
}

/*
 * void fix_packet_read_ptr(pkt, fix_boundary, room)
 *
 * $B%Q%1%C%H(B pkt $B$NFI$_$@$70LCV$r9g$o$;$k(B
 * fix_boundary $B$,(B TRUE $B$G$"$l$P!$(B4$B%P%$%H$N6-3&$K(B
 * $B9g$o$;$k$h$&$K$9$k(B
 * $B8=:_$N%V%m%C%/Cf$K(B room $B%P%$%H$NL$FI$NNN0h$,;D$C$F$$$J$1$l$P(B
 * $B<!$N%V%m%C%/$rFI$_=P$9$h$&$K@_Dj$9$k(B
 *   ($B$3$N%V%m%C%/$N;D$jItJ,$O<N$F$i$l$k(B)
 */
static void fix_packet_read_ptr(pkt, fix_boundary, room)
    register YYPACKET *pkt;
    register bool fix_boundary;
    register int room;
{
    register int bound;
#ifdef DEBUG_PRINT
    debug_setfunc("packet", "fix_packet_read_ptr");
#endif /*DEBUG_PRINT*/
    if (fix_boundary &&
	(bound = ((YYPACKET_CURBLOCK(pkt)->pbUnreadLength)&3)) > 0) {
#ifdef DEBUG_PRINT
	debug_print(9, "Fix Boundary\n");
#endif /*DEBUG_PRINT*/
	(void)skip_packet_read_ptr(pkt, (4 - bound));
    }
    if (YYPACKET_UNREADLENGTH_INBLOCK(pkt) < room)
	goto_next_read_packet(pkt);
#ifdef DEBUG_PRINT
    debug_endfunc("fix_packet_read_ptr");
#endif /*DEBUG_PRINT*/
}

/***************************************************************************
 *****  WritePtr
 ***************************************************************************/

/*
 * void goto_next_write_packet(pkt)
 *
 * YY $B$K$*$1$k%Q%1%C%H(B pkt $B$K$*$$$F!$8=:_$N%V%m%C%/$N(B
 * $B<!$N%V%m%C%/$K=q$-9~$_$,$G$-$k$h$&$K@_Dj$9$k(B
 */
static void goto_next_write_packet(pkt)
    register YYPACKET *pkt;
{
    register YYPACKETBLOCK *next;
#ifdef DEBUG_PRINT
    debug_setfunc("packet", "goto_next_write_packet");
#endif /*DEBUG_PRINT*/
    next = YYPACKET_CURBLOCK(pkt)->pbNextBlock;
    if (next == (YYPACKETBLOCK *)NULL) {
#ifdef DEBUG_PRINT
	debug_print(9, "Allocate New Packet Block\n");
#endif /*DEBUG_PRINT*/
	next = alloc_packet_block(pkt, (bytes *)NULL);
    }
    next->pbWritePtr = next->pbBuf + sizeof(YYPACKETHEADER);
    next->pbFreeLength = next->pbSize - sizeof(YYPACKETHEADER);
    YYPACKET_CURBLOCK(pkt) = next;
#ifdef DEBUG_PRINT
    debug_endfunc("goto_next_write_packet");
#endif /*DEBUG_PRINT*/
    /*NoReturnValue*/
}

/*
 * void skip_packet_write_ptr(pkt, bytes)
 *
 * $B%Q%1%C%H(B pkt $B$N=q$-9~$_0LCV$r(B bytes $B$@$1$9$9$a$k(B
 * bytes $B?J$a$k4V$K%V%m%C%/$NNN0h$,=*$o$C$F$7$^$C$?$i(B
 * $B<!$N%V%m%C%/$K?J$`(B
 */
static void skip_packet_write_ptr(pkt, bytes)
    register YYPACKET *pkt;
    register int bytes;
{
#ifdef DEBUG_PRINT
    debug_setfunc("packet", "skip_packet_write_ptr");
#endif /*DEBUG_PRINT*/
    if (YYPACKET_CURBLOCK(pkt)->pbFreeLength < bytes) {
	bytes -= YYPACKET_CURBLOCK(pkt)->pbFreeLength;
	goto_next_read_packet(pkt);
    }
    if (YYPACKET_CURBLOCK(pkt)->pbFreeLength >= bytes) {
	YYPACKET_WRITE_PTR(pkt) += bytes;
	YYPACKET_CURBLOCK(pkt)->pbFreeLength -= bytes;
	pkt->pktLength += bytes;
    } else {
	YYPACKET_CURBLOCK(pkt)->pbFreeLength = 0;
    }
#ifdef DEBUG_PRINT
    debug_endfunc("skip_packet_write_ptr");
#endif /*DEBUG_PRINT*/
    /*NoReturnValue*/
}


/*
 * void fix_packet_write_ptr(pkt, fix_boundary, room)
 *
 * $B%Q%1%C%H(B pkt $B$N=q$-9~$_0LCV$r9g$o$;$k(B
 * fix_boundary $B$,(B TRUE $B$G$"$l$P!$(B4$B%P%$%H$N6-3&$K(B
 * $B9g$o$;$k$h$&$K$9$k(B
 * $B8=:_$N%V%m%C%/Cf$K(B room $B%P%$%H$N=q$-9~$_NN0h$,;D$C$F$$$J$1$l$P(B
 * $B<!$N%V%m%C%/$r=q$-9~$`$h$&$K@_Dj$9$k(B
 *   ($B$3$N%V%m%C%/$N;D$jItJ,$OITDj>uBV$H$J$k(B)
 */
static void fix_packet_write_ptr(pkt, fix_boundary, room)
    register YYPACKET *pkt;
    register bool fix_boundary;
    register int room;
{
    register int bound;
#ifdef DEBUG_PRINT
    debug_setfunc("packet", "fix_packet_write_ptr");
#endif /*DEBUG_PRINT*/
    if (fix_boundary &&
	(bound = ((YYPACKET_CURBLOCK(pkt)->pbLength)&3)) > 0) {
#ifdef DEBUG_PRINT
	debug_print(9, "Fix Boundary\n");
#endif /*DEBUG_PRINT*/
	(void)skip_packet_read_ptr(pkt, (4 - bound));
    }
    if (YYPACKET_UNREADLENGTH_INBLOCK(pkt) < room)
	goto_next_read_packet(pkt);
#ifdef DEBUG_PRINT
    debug_endfunc("fix_packet_write_ptr");
#endif /*DEBUG_PRINT*/
}

/***************************************************************************
 *****
 ***************************************************************************/

bool pkt_entry_integer(pkt, dp)
    register YYPACKET *pkt;
    int *dp;
{
    register bool status = TRUE;
#ifdef DEBUG_PRINT
    debug_setfunc("packet", "pkt_entry_integer");
#endif /*DEBUG_PRINT*/
    switch (pkt->pktOperation) {
    case YYPKT_READ:
	/* Get One Interger Value from Packet */
	fix_packet_read_ptr(pkt, YYPKT_FIX_BOUNDARY, 4);
	if (YYPACKET_UNREAD_LENGTH(pkt) > 3) {
#ifdef FOURBYTESINT
	    (void)bcopy(YYPACKET_READ_PTR(pkt), dp, 4);
#else /*!FOURBYTESINT*/
	    if (sizeof(int) < 4) {
		(void)bcopy(YYPACKET_READ_PTR(pkt), dp, 4);
	    } else {
		*dp = 0;
		(void)bcopy(YYPACKET_READ_PTR(pkt), dp+(sizeof(int)-4), 4);
	    }
#endif /*!FOURBYTESINT*/
	    skip_packet_read_ptr(pkt, 4);
	} else {
	    *dp = 0; status = FALSE;
	}
	break;
    case YYPKT_WRITE:
	/* Put One Interger Value on Packet */
	fix_packet_write_ptr(pkt, YYPKT_FIX_BOUNDARY, 4);
#ifdef FOURBYTESINT
	(void)bcopy(dp, YYPACKET_WRITE_PTR(pkt), 4);
#else /*!FOURBYTESINT*/
	if (sizeof(int) < 4) {
	    (void)bzero(YYPACKET_WRITE_PTR(pkt), 4);
	    (void)bcopy(dp, YYPACKET_WRITE_PTR(pkt)+(4-sizeof(int)),
			sizeof(int));
	} else {
	    (void)bcopy(dp+(sizeof(int)-4), YYPACKET_WRITE_PTR(pkt), 4);
	}
#endif /*!FOURBYTESINT*/
	skip_packet_write_ptr(pkt, 4);
	break;
    case YYPKT_ALLOC:
    case YYPKT_FREE:
	/*Do Nothing*/
	break;
    }
#ifdef DEBUG_PRINT
    debug_endfunc("pkt_entry_integer");
#endif /*DEBUG_PRINT*/
    return TRUE;
}

typedef struct {
    int Leng;
    char *Ptr;
} pktstring;

bool pkt_entry_string(pkt, dp)
    register YYPACKET *pkt;
    pkt_string *dp;
{
    register bool status = TRUE;
#ifdef DEBUG_PRINT
    debug_setfunc("packet", "pkt_entry_string");
#endif /*DEBUG_PRINT*/
    switch (pkt->pktOperation) {
    case YYPKT_READ:
	/* Get String from Packet */
	pkt_entry_integer(pkt, dp->leng);
	if (dp->leng > 0) {
	    dp->ptr = (char *)memALLOC(dp->leng);
	} else {
	    int dummy;
	    pkt_entry_integer(pkt, &dummy);
	}
	break;
    case YYPKT_WRITE:
	/* Put One Interger Value on Packet */
	fix_packet_write_ptr(pkt, YYPKT_FIX_BOUNDARY, 4);
#ifdef FOURBYTESINT
	(void)bcopy(dp, YYPACKET_WRITE_PTR(pkt), 4);
#else /*!FOURBYTESINT*/
	if (sizeof(int) < 4) {
	    (void)bzero(YYPACKET_WRITE_PTR(pkt), 4);
	    (void)bcopy(dp, YYPACKET_WRITE_PTR(pkt)+(4-sizeof(int)),
			sizeof(int));
	} else {
	    (void)bcopy(dp+(sizeof(int)-4), YYPACKET_WRITE_PTR(pkt), 4);
	}
#endif /*!FOURBYTESINT*/
	skip_packet_write_ptr(pkt, 4);
	break;
    case YYPKT_ALLOC:
    case YYPKT_FREE:
	/*Do Nothing*/
	break;
    }
#ifdef DEBUG_PRINT
    debug_endfunc("pkt_entry_integer");
#endif /*DEBUG_PRINT*/
    return TRUE;
}

#endif

/* $RCSFile:$
 */
