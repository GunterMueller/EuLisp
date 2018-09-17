# Cheapo makefile for generic boot files
.SUFFIXES:
.SUFFIXES: .em .sc 

#Configuration
SOCKETS=t
STRIPMODULES=t

#Where to look
FEEL_PATH=${HOME}/Feel

#Shell hackery
FEEL_ENV = \
	FEEL_PATH=${FEEL_PATH} \
	FEEL_START_MODULE=eulisp0	

#For optimised compiling (can't set flags in compiled version)
FEEL=${FEEL_PATH}/bin/feel -procs 1 -heap 7

#For linking
FEELC=${FEEL_PATH}/bin/feel -boot compiler -procs 1 -heap 7 

#For cross-transfers
#HACKPATH=add-load-path
#RELOAD=!>>
#for building self
HACKPATH=xx-add-load-path
RELOAD=!>
MODMAP=you.mods

# IMPORTANT: This must be kept in order of loading!

BOOTOBJS = boot.sc init.sc macros0.sc extras0.sc defs.sc gens.sc numbers.sc \
null.sc cons.sc vector.sc list.sc telos1.sc copy.sc stream.sc \
error0.sc thread.sc wait.sc lock.sc table.sc character.sc string.sc \
collect.sc condition.sc eulisp0.sc standard.sc standard0.sc list-fns.sc \
loops.sc reader.sc

BOOTSRCS = boot.em init.em macros0.em extras0.em defs.em gens.em numbers.em \
null.em cons.em vector.em list.em telos1.em copy.em stream.em \
error0.em thread.em wait.em lock.em table.em character.em string.em \
collect.em condition.em eulisp0.em standard.em standard0.em list-fns.em \
loops.em reader.em

TESTOBJS1 =
TESTSRCS1 =
TESTSRCS = ${BOOTSRCS} ${TESTSRCS1}
TESTOBJS = ${BOOTOBJS} ${TESTOBJS1}

IMAGE=image.ebc image.est

image: image.ebc
init: init.em
test: test.ebc

init.em: gen-init.em class-defs.em class-macs.em
	echo '(!> class-macs)(set-sockets ${SOCKETS})(!> gen-init)(write-c-code)(exit)' | ${FEEL_ENV} ${FEEL} 


image.ebc: ${BOOTOBJS}
	(echo '(!> combine)(combine-mods-with-file (quote image) "${MODMAP}" (quote (' ; \
	(echo ${BOOTOBJS} | sed -e 's/\.sc/ /g') ; echo ')))(exit)') | ${FEEL_ENV}  ${FEELC}
	test -f image.ebc

test.ebc: ${TESTOBJS}
	(echo '(!> combine)(combine-mods-with-file (quote test) "${MODMAP}" (quote (' ; \
	(echo ${TESTOBJS} | sed -e 's/\.sc/ /g') ; echo ')))(backtrace)(exit)') | ${FEEL_ENV} ${FEELC}
	test -f test.ebc

.em.sc: 
	rm -f $*.sc
	echo '(!> compile)(!> compile)(!> mod-hack)(${HACKPATH} "Xfer")(${RELOAD} macros0)\
	      (${RELOAD} defs) (!> loops)(!> compile) (if t (progn ((setter optimize-code) t)\
	  ((setter strip-module) ${STRIPMODULES})) nil)(comp2sc (quote $*))(backtrace)(exit)' | ${FEEL_ENV} ${FEEL}
	test -f $*.sc

install: ${IMAGES}
	cp image.ebc image.est ${FEEL_PATH}/Images
		
# I deliberately do not delete documentation files

clean:
	rm -f *.sc core *.i *.fm

# I don't put all the dependencies in -- just things that build fast

standard0.sc: standard.sc 
standard.sc: macros0.sc extras0.sc telos1.sc defs.sc error0.sc init.sc
