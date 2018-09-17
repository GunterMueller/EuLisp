# ###################################################################### #
# The FEEL makefile...                                                   #
# ###################################################################### #

#
# $Id: Makefile,v 1.11 1994/09/30 11:37:06 djb Exp $
#

##########################################################################
# Customisation section...                                               #
##########################################################################

# Edit the following for your favourite machine.

# Where the Modules, lib and Image directories will live
#BASE = ${HOME}/Feel
BASE = ${HOME}/SOFTWARE/LISP/EuLisp/FEEL/feel-0.92e

# Feel knows about the systems below:
# Stardent Iris Sun4 RS6000 Gould Sun3 Clipper Vax Alliant 386

# Here is some info in case you have something different:
#     WITH_SYSTEMV_SOCKETS - System V IP communication
#     WITH_BSD_SOCKETS     - BSD IP communication (4.3, or see defs.h)
#     WITH_SYSTEMV_SIGNALS - System V signalling
#     WITH_BSD_SIGNALS     - BSD signalling
#     DONT_HAVE_STDLIB_H   - half-hearted ANSI without stdlib.h
#     ALIGN8 - if the machine is fussy about 8 byte alignments, e.g., doubles
#     TRAP_ALL		   - vague attempt to catch segvs, buses, etc. 
#			     implied by MACHINE=SYSTEMV
#
# Architecture and machine type...
# MACHINE is ANY,BSD or SYSTEMV. 
# ANY is any 'reasonable' architecture - things that run UNIX, and many others
# BSD is supported on any machine with a *-swch.s file in this 
# directory. See the note in the manual for what it does, and how to port it.
# SYSTEMV is for sun3/4, Stardents and Alliant concentrix machines 
# It ought to work for any UNIX machine that has shared memory primitives 
# and that BSD config. works for; 
#
# NB|| BSD works on MSDOS boxes(!)
#
# Systems not listed but have worked in the 'ANY' config:
# DEC MIPS (try hacking the ard-swch code for BSD version)
# 

# Further flag information
# -DBCI 		Bytecode interpreter
# -DLOWTAGINTS		Low tag integers (breaks AddOns)
# -DPVM3		pvm3 interface (moved from AddOns)
# -DNO_MALLOC		uses sbrk/brk rather than malloc/realloc
#			(only use when no external libs linked)

# common options

# how to optimise your code
STDOFLAGS = 

# everybody needs these
STDFLAGS =  -DWITH_FUDGE 
CPP = cc -E

# now take your pick:

# Stardent
# SYSFLAGS = -DWITH_SYSTEMV_SOCKETS -DDONT_HAVE_STDLIB_H -DBCI 
# OFLAGS =-O -inline
# MACHINE = BSD # Can be SYSTEMV
# ARCH = ard#a43
# PVM_ARCH = TITN
# CC = cc -43
# LIBS = -lm

# Norcroft-stardent
# SYSFLAGS = -DBCI -DNEWTELOS -DNORCROFT -DDEBUG
# OFLAGS =  #-O -inline
# MACHINE = ANY #BSD # Can be SYSTEMV
# ARCH = ard#a43
# PVM_ARCH = TITN
# CC = /net/brad/denton_export/users/jpff/Norcroft/ncc.mips2 \
#	-I/net/brad/denton_export/users/jpff/ncc.include
# LIBS = 

# Iris
# SYSFLAGS = -DWITH_SYSTEMV_SOCKETS -DBCI -D__STDC__  -DUNSIGNED_CHARS
# OFLAGS =
# MACHINE = BSD # SYSTEMV
# ARCH = irx
# PVM_ARCH = SGI
# CC = cc
# LIBS = -lm -lsun 

# Sun4
# SYSFLAGS = -DWITH_SYSTEMV_SOCKETS -DWITH_BSD_SIGNALS -DBCI -DALIGN8 
# OFLAGS = -O2
# MACHINE = BSD # SYSTEMV
# ARCH = sun4
# PVM_ARCH = SUN4
# CC = gcc 
# LIBS = -lm 

# RS6000
# SYSFLAGS = -DWITH_SYSTEMV_SOCKETS -DWITH_SYSTEMV_SIGNALS \
#		-DTRAP_ALL -D_ALL_SOURCE
# MACHINE = ANY
# ARCH = any
# PVM_ARCH = RS6K
# CC = xlc
# LIBS = -lm

# Gould
# SYSFLAGS = -DWITH_BSD_SIGNALS \
#		-DDONT_HAVE_STDLIB_H  -DALIGN8
# MACHINE = ANY
# ARCH = any
# PVM_ARCH = 
# CC = gcc
# LIBS = -lm

# Sun3
# SYSFLAGS =  -DWITH_SYSTEMV_SOCKETS -DDONT_HAVE_STDLIB_H -DWITH_BSD_SIGNALS
# MACHINE = BSD
# ARCH = sun3
# PVM_ARCH = SUN3
# CC = gcc
# LIBS = -lm

# Clipper
# SYSFLAGS = -DWITH_BSD_SIGNALS -DALIGN8 
# MACHINE = BSD
# ARCH = clip
# PVM_ARCH = 
# CC = ncc
# LIBS = /lib/crt0.o

# Vax
# SYSFLAGS = -mg -O -DDONT_HAVE_STDLIB_H -DBCI
# MACHINE = ANY
# ARCH = any
# PVM_ARCH = 
# CC = gcc
# LIBS = -lmg

# Alliant
# SYSFLAGS = -DWITH_BSD_SIGNALS -DWITH_BSD_SOCKETS -DALIGN8
# MACHINE = SYSTEMV
# ARCH = i860
# PVM_ARCH = I860
# CC = fxc
# LIBS = -lm

# 386 [Should work for linux]
# ...
# SYSFLAGS = -DWITH_SYSTEMV_SOCKETS -DDONT_HAVE_STDLIB_H -DBCI -DUNSIGNED_CHARS
# OFLAGS =  -O 
# MACHINE = BSD
# ARCH = 386
# PVM_ARCH = # BSD386 or LINUX (doesn't exist for MSDOS!)
# CC = gcc
# LIBS = -lm

# i860
# Not an alliant? 
# Try MACHINE=BSD, then change the shared memory primitives around
#

# CYGWIN [Should work for linux]
# ...
SYSFLAGS = -DWITH_SYSTEMV_SOCKETS -DBCI -DUNSIGNED_CHARS
OFLAGS =  -O 
MACHINE = ANY #BSD
ARCH = 386
# PVM_ARCH = # BSD386 or LINUX (doesn't exist for MSDOS!)
CC = gcc
LIBS = -lm

# ################################################################### #
# PVM3 linkins						      	      #
# ################################################################### #

# tailor these lines for your PVM3 installation, otherwise comment them out

#PVMLIBS = -lpvm3 -lgpvm3 -L${HOME}/pvm3/lib/${PVM_ARCH}

#PVMFLAGS = -DPVM3 -I${HOME}/pvm3/include 

# ################################################################### #
# Sundry constants						      #
# ################################################################### #
LEX = flex
YACC = bison -y -t

# Local module path

MODULE_PATH = ${BASE}/Modules/

# Semaphoring...
# Hardware semaphores are OK, but
SEMAPHORES = SOFTWARE

# Do the business...

# ###################################################################### #
# Working section...                                                     #
# ###################################################################### #

LOAD_PATH_NAME = FEEL_LOAD_PATH
BOOT_PATH_NAME = FEEL_BOOT_PATH

MACHINEABBREV = ${ARCH}

# invent flags
FLAGS = ${STDFLAGS} ${STDOFLAGS} ${SYSFLAGS} ${OFLAGS} ${PVMFLAGS}
BCIOPTFLAGS = -O -inline

# Reader specific sources...

READERSOURCES = reader.c feelflex.lex feelflex.skl
READERIMMOBJS = lex.yy.c 
READEROBJECTS = lex.yy.o reader.o

# Default Elvira sources...

ELVIRASOURCES = initelv.c
ELVIRAOBJECTS = initelv.o

# PVM sources

PVMSOURCES = eupvm3.c
PVMOBJECTS = eupvm3.o

# Main sources...

SOURCES = copyalloc.c copy.c arith.c basic.c chars.c error.c \
	  main.c modules.c set.c streams.c \
	  t2.c vectors.c bootstrap.c lists.c  \
	  class.c ngenerics.c \
          symboot.c modboot.c toplevel.c root.c specials.c \
	  calls.c ccc.c macros.c system.c semaphores.c state.c \
	  sockets.c sio.c modops.c lamport.c bvecs2.c \
	  obread.c 

OBJECTS = copyalloc.o copy.o arith.o basic.o chars.o error.o \
	  main.o modules.o set.o streams.o \
	  t2.o vectors.o bootstrap.o lists.o  \
	  class.o ngenerics.o  \
          symboot.o modboot.o toplevel.o root.o specials.o \
	  calls.o ccc.o macros.o system.o semaphores.o state.o \
	  sockets.o sio.o modops.o lamport.o bvecs2.o \
	  obread.o 

HDRS =  bootstrap.h	generics.h	modules.h	specials.h \
	toplevel.h 	calls.h		global.h	ngenerics.h \
	state.h       	vectors.h	ccc.h		lamport.h \
	structs.h     	version.h 	class.h       	lex_global.h \
	root.h        	symboot.h     	y.tab.h		copy.h \
	runtime.h	syssockets.h	funcalls.h	weak.h \
	defs.h		lists.h       	semaphores.h  	system_p.h \
	yyfudge.c 	allocate.h    	error.h       	macros.h \
	sio.h         	system_t.h	bvf.h		modops.h \
	garbage.h  	format.h      	modboot.h     	slots.h \
	table.h		threads.h 	iset.h 		sockets.h \
	interpret.h   	bytecodes.h   	t2.h		reader.h \
	obread.h	obread_p.h	

COSOURCES = ${MACHINEABBREV}-swch.s threads.c 

COOBJECTS = ${MACHINEABBREV}-swch.o threads.o

DISTRIBCO = sun3-swch.s sun4-swch.s ard-swch.s clip-swch.s any-swch.s \
	    i860-swch.s 386-swch.s irx-swch.s Makefile Makefile.386

ISOURCES = interpret.c
IHDRS = bytecodes.h interpret.h
IOBJECTS = interp.o

you:	${OBJECTS} ${COOBJECTS} ${READEROBJECTS} ${ELVIRAOBJECTS} \
	${IOBJECTS} ${PVMOBJECTS}
	rm -f you
	${CC} ${LINKFLAGS} -o you ${OBJECTS} ${COOBJECTS} \
			${READEROBJECTS} ${IOBJECTS} ${ELVIRAOBJECTS} \
			${PVMOBJECTS} ${PVMLIBS} \
			${LIBS} 
	echo 

test:	${OBJECTS} ${COOBJECTS} ${READEROBJECTS} ${ELVIRAOBJECTS} \
	${IOBJECTS} ${PVMOBJECTS}
	-touch test
	rm test
	${CC} -yinitialise_readstr -o test ${OBJECTS} ${COOBJECTS} \
			${READEROBJECTS} ${IOBJECTS} ${ELVIRAOBJECTS} \
			${PVMOBJECTS} ${PVMLIBS} \
			${LIBS} 
	echo 

proyou:	${OBJECTS} ${COOBJECTS} ${READEROBJECTS} ${ELVIRAOBJECTS} ${PVMOBJECTS}
	${CC} -p -o proyou ${OBJECTS} ${COOBJECTS} \
		     	   ${READEROBJECTS} ${ELVIRAOBJECTS} \
			   ${PVMOBJECTS} ${PVMLIBS} \
			   ${LIBS} 
	echo 

elvira:  ${OBJECTS} ${COOBJECTS} ${READEROBJECTS} ${IOBJECTS} ${PVMOBJECTS}
	ld -r -o feel_elvira.o ${OBJECTS} ${COOBJECTS} ${READEROBJECTS} \
				${IOBJECTS} ${PVMOBJECTS} ${PVMLIBS} \
				${LIBS}
	mv feel_elvira.o ../Objs/${ARCH}-elvira.o
	echo 

feel_elvira.o:	elvira

reader:		
	${LEX} comp.lex
	${YACC} test.yac

interp.o: interpret.c bytecodes.h interpret.h
	-${CC} -c interpret.c  ${FLAGS} \
		 -DMACHINE_${MACHINE} \
		 -DMODULE_PATH=\"${MODULE_PATH}\" \
		 -DLOAD_PATH_NAME=\"${LOAD_PATH_NAME}\" \
		 -DBOOT_PATH_NAME=\"${BOOT_PATH_NAME}\" \
		 -DSEMAPHORES_${SEMAPHORES} \
                 -DMAKE_DATE="\"`date`\""
	mv interpret.o interp.o

main.o: main.c version.h
	sed -e "s/MAKE_DATE.*/MAKE_DATE \"`date`\"/" < version.h > xxvers
	mv xxvers version.h
	${CC} -c main.c ${FLAGS} \
                 -DMACHINE_${MACHINE} \
                 -DMODULE_PATH=\"${MODULE_PATH}\" \
                 -DLOAD_PATH_NAME=\"${LOAD_PATH_NAME}\" \
		 -DBOOT_PATH_NAME=\"${BOOT_PATH_NAME}\" \
                 -DSEMAPHORES_${SEMAPHORES}

# Sicko bytecode hacking
# preprocess explicitly, then compile the result, after stripping out
# cpp control codes. Ugh. Makes debugging easier though.
#interp.o: interpret.c bytecodes.h interpret.h
#	-${CC} -c interpret.c  ${FLAGS} \
#		 -DMACHINE_${MACHINE} \
#		 -DMODULE_PATH=\"${MODULE_PATH}\" \
#		 -DBOOT_PATH_NAME=\"${BOOT_PATH_NAME}\" \
#		 -DLOAD_PATH_NAME=\"${LOAD_PATH_NAME}\" \
#		 -DSEMAPHORES_${SEMAPHORES} \
#                 -DMAKE_DATE="\"`date`\"" \
#            | grep -v "^#"  > interp.c
#	${CC} ${FLAGS} ${BCIOPTFLAGS} -c interp.c

#reader hacking
lex.yy.c: ${READERSOURCES} 
	${LEX} -I -Sfeelflex.skl feelflex.lex

.c.o:
	${CC} -c $*.c ${FLAGS} \
		 -DMACHINE_${MACHINE} \
		 -DMODULE_PATH=\"${MODULE_PATH}\" \
		 -DLOAD_PATH_NAME=\"${LOAD_PATH_NAME}\" \
		 -DBOOT_PATH_NAME=\"${BOOT_PATH_NAME}\" \
		 -DSEMAPHORES_${SEMAPHORES} 


install:	install.${MACHINE} 

install.BSD: you 
	@if [ ! -d ${BASE}/lib ]; then mkdir ${BASE}/lib; fi
	cp you ${BASE}/lib/you.`arch`
	@if [ "`uname`" = "IRIX" ]; then mv ${BASE}/lib/you. ${BASE}/lib/you.sgi; fi

install.ANY: you
	@if [ ! -d ${BASE}/lib ]; then mkdir ${BASE}/lib; fi
	cp you ${BASE}/lib/you.`arch`

install.SYSTEMV: you
	@if [ ! -d ${BASE}/lib ]; then mkdir ${BASE}/lib; fi
	cp you ${BASE}/lib/you.`arch`.sysv
	@if [ "`uname`" = "IRIX" ]; then mv ${BASE}/lib/you..sysv ${BASE}/lib/you.sgi.sysv; fi

# Some systems have X, others have (g)cc -M. Makedepend in pref.

depend: ${SOURCES} ${HDRS} ${READERSOURCES} ${COSOURCES} ${PVMSOURCES}
	makedepend -s "# DO NOT DELETE THIS LINE" -- ${FLAGS} -- ${SOURCES} ${HDRS} ${READERSOURCES} ${COSOURCES} ${PVMSOURCES} \
	|| (sed -n -e '1,/^# DO NOT DELETE/d' < Makefile > tmp ; \
	    for i in  ${SOURCES} ${HDRS} ${READERSOURCES} ${COSOURCES} ${PVMSOURCES} ; do ${CPP} ${FLAGS} -M $$i; done >> tmp ;\
	    mv tmp Makefile)

clean:
	rm -f ${OBJECTS} ${COOBJECTS} ${READEROBJECTS} ${PVMOBJECTS} you


${MACHINE}-coroutine.o:	${MACHINE}-coroutine.s
		${CC} -c ${MACHINE}-coroutine.s

#if there is odd stuff below this line, remove it and do make depend.
# if make depend fails don't worry. It only gets the header dependencies 
# right.
# DO NOT DELETE THIS LINE

