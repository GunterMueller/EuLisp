ARCH = ard
#
# Feel Add-on Makefile
#
# It relies on the file ${ARCH}-elvira.o existing 
# in the directory ../Objs, plus whatever libraries you may need.

FEELDIR=..
CC= FEELDIR=`pwd`/.. ../bin/ecc.ard -g -I..
#CC = FEELDIR=`pwd`/.. ../bin/ecc.2 -g
SOURCEDIR = ../Src

#Name your machine...

MACHINE = BSD

#what to call the thing
FEELNAME = dbmfeel.${ARCH}

#copied from Feel makefile
FLAGS =  -DWITH_SYSTEMV_SOCKETS  -I${SOURCEDIR}

new socket io
#NEWSOCKSOURCES = new_sockio.c
#NEWSOCKOBJECTS = new_sockio.o
#NEWSOCKLIBS = 

#dbm
DBMSOURCES = eudbm.c 
DBMOBJECTS = eudbm.o 
DBMHDRS = eudbm.h
DBMLIBS = -L. -lgdbm

# YYonX
#YYSOURCES = euclient.c packet.c misc.c debug.c
#YYOBJECTS = euclient.o packet.o misc.o debug.o
#YYHDRS = eucmd.h         yydefault.h     yyfont.h  yydefs.h        yypacket.h 

# modify this if you have loads of architectures
# [If someone knows how to do mapcar in sh/make, let me know.
#  FILES=(mapcar wrap-sources LIBRARIES)]

## Modify these if you add libraries

ALLSOURCES = ${NEWSOCKSOURCES} ${DBMSOURCES} \
		${MALLOCSOURCES} ${YYSOURCES}

ALLOBJECTS = ${NEWSOCKOBJECTS} ${DBMOBJECTS} \
		${MALLOCOBJECTS} ${YYOBJECTS}

ALLHDRS = ${NEWSOCKHDRS} ${DBMHDRS} \
		${MALLOCHDRS} ${YYHDRS}

ALLLIBRARIES = ${NEWSOCKLIBS} ${DBMLIBS} \
		${MALLOCLIBS} ${YYLIBS}

# This should not need to be changed
ELVIRAOBJECT=../Objs/${ARCH}-elvira.o

MORE_FLAGS =	 -DMACHINE_${MACHINE} \
		 -DMODULE_PATH=\"${MODULE_PATH}\" \
		 -DLOAD_PATH_NAME=\"${LOAD_PATH_NAME}\" \
		 -DSEMAPHORES_${SEMAPHORES} \
		 -DMAKE_DATE="\"${DATE}\"" \
		 -DFOURBYTESINT

${FEELNAME}: ../Objs/${ARCH}-elvira.o ${ALLOBJECTS}
	${CC} -o ${FEELNAME} ${FLAGS} ${ALLOBJECTS} ${ELVIRAOBJECT} \
		${ALLLIBRARIES}

depend: ${ALLSOURCES} ${ALLHDRS} 
	makedepend -s "# DO NOT DELETE THIS LINE" -- ${FLAGS} \
		${MORE_FLAGS} -- ${ALLSOURCES}

.c.o:
	${CC} -c $*.c ${FLAGS} ${MORE_FLAGS}

# DO NOT DELETE THIS LINE

euclient.o: /usr/include/poll.h /usr/include/sys/poll.h /usr/include/stdio.h
euclient.o: /usr/include/varargs.h /usr/include/fcntl.h
euclient.o: /usr/include/sys/fcntl.h /usr/include/signal.h
euclient.o: /usr/include/sys/signal.h /usr/include/sys/types.h
euclient.o: /usr/include/sys/un.h /usr/include/strings.h ${SOURCEDIR}/defs.h

euclient.o: ${SOURCEDIR}/funcalls.h ${SOURCEDIR}/structs.h \
		/usr/include/setjmp.h
euclient.o: ${SOURCEDIR}/system_t.h ${SOURCEDIR}/system_p.h \
		${SOURCEDIR}/global.h
euclient.o: ${SOURCEDIR}/state.h ${SOURCEDIR}/error.h \
		${SOURCEDIR}/bootstrap.h yydefs.h

euclient.o: /usr/include/errno.h /usr/include/sys/errno.h
euclient.o: /usr/include/sys/time.h /usr/include/sys/time.h yydefault.h
euclient.o: /usr/include/ctype.h yypacket.h eucmd.h

packet.o: /usr/include/stdio.h /usr/include/varargs.h
packet.o: /usr/include/sys/types.h yydefs.h /usr/include/errno.h
packet.o: /usr/include/sys/errno.h /usr/include/sys/time.h
packet.o: /usr/include/sys/time.h yydefault.h /usr/include/ctype.h yypacket.h

misc.o: /usr/include/stdio.h /usr/include/varargs.h /usr/include/sys/types.h
misc.o: yydefs.h /usr/include/errno.h /usr/include/sys/errno.h
misc.o: /usr/include/sys/time.h /usr/include/sys/time.h yydefault.h
misc.o: /usr/include/ctype.h

debug.o: /usr/include/stdio.h /usr/include/varargs.h /usr/include/strings.h
debug.o: /usr/include/sys/types.h yydefs.h /usr/include/errno.h
debug.o: /usr/include/sys/errno.h /usr/include/sys/time.h
debug.o: /usr/include/sys/time.h yydefault.h /usr/include/ctype.h
