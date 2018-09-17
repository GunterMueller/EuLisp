#
# Makefile for compiler
# 
.SUFFIXES:
.SUFFIXES: .em .sc 

#Where to look
FEEL_PATH=${HOME}/Feel

#Shell hackery
FEEL_ENV = \
	FEEL_PATH=${FEEL_PATH} \
	FEEL_START_MODULE=eulisp0	

#For optimised compiling (can't set flags in compiled version)
FEEL=${FEEL_PATH}/bin/feel -heap 7

#For linking
FEELC=${FEEL_PATH}/bin/feel -boot compiler -heap 7 

MODDESC=you.mods
STRIPMODULES=t

# Order is important --- do not change!  

SRCS =  scan-args.em abs-syntx.em comp-utl.em low-seman.em i-macros.em \
	byte-stream.em peep-macs.em comp-defn.em instruct.em props.em \
	mod-cache.em pass.em compstate.em peep-macs.em peephole.em stop.em \
	syntx-env.em assem.em peep-drv.em syntx-utl.em generate.em mod-info.em \
	rshow.em pass-0.em syntax.em output.em fn-docs.em gen-docs.em \
	gen-code.em link2.em iface.em combine.em out-fast.em in-fast.em \
	triang.em strtest.em low-asm.em compile.em

OBJS =  scan-args.sc abs-syntx.sc comp-utl.sc low-seman.sc i-macros.sc \
	byte-stream.sc peep-macs.sc comp-defn.sc instruct.sc props.sc \
	mod-cache.sc pass.sc compstate.sc peep-macs.sc peephole.sc stop.sc \
	syntx-env.sc assem.sc peep-drv.sc syntx-utl.sc generate.sc mod-info.sc \
	rshow.sc pass-0.sc syntax.sc output.sc fn-docs.sc gen-docs.sc \
	gen-code.sc link2.sc iface.sc combine.sc out-fast.sc in-fast.sc \
	triang.sc strtest.sc low-asm.sc compile.sc


LOADOBJS = byte-stream.sc comp-utl.sc in-fast.sc
LOADSRCS = byte-stream.em comp-utl.em in-fast.em

TESTSRCS =  test.em triang.em strtest.em 
TESTOBJS =  test.sc triang.sc strtest.sc
DISTFILES = Makefile instruct.emc iset.h

# copied from CBoot/Makefile

BOOTOBJS = boot.sc init.sc macros0.sc extras0.sc defs.sc gens.sc numbers.sc \
null.sc cons.sc vector.sc list.sc telos1.sc copy.sc stream.sc \
error0.sc thread.sc lock.sc table.sc character.sc string.sc collect.sc \
condition.sc eulisp0.sc standard.sc standard0.sc list-fns.sc \
loops.sc reader.sc

IMAGES = loader.ebc compiler.ebc
STATICIMAGES = loader.est compiler.est

all: compiler loader 

loader: loader.ebc 
compiler: compiler.ebc
test: test.ebc
telos: telos.ebc

compiler.ebc: ${OBJS} 
	(echo '(!> combine)(combine-mods-with-file (quote compiler) "${MODDESC}" (quote (' ; \
	(echo ${BOOTOBJS} ${OBJS} | sed -e 's/\.sc/ /g') ; echo ')))') | ${FEEL_ENV}  ${FEELC}
	test -f compiler.ebc

loader.ebc: ${LOADOBJS}
	(echo '(!> combine)(combine-mods-with-file (quote loader) "${MODDESC}" (quote (' ; \
	(echo ${BOOTOBJS} ${LOADOBJS} | sed -e 's/\.sc/ /g') ; echo ')))!exit') | ${FEEL_ENV}  ${FEELC}
	test -f loader.ebc

test.ebc: ${OBJS}
	(echo "(!> combine)(combine-mods-with-file (quote test) \"${MODDESC}\" (quote (" ; (echo ${BOOTOBJS} ${OBJS} | sed -e 's/\.sc/ /g') ; echo ")))(exit)") | ${FEEL_ENV} ${FEELC}
	test -f test.ebc


instruct.em: instruct.emc iset.h
	/lib/cpp < instruct.emc | grep -v "^#" > instruct.em
	
.em.sc:
	-rm -f $*.sc
	echo '(!> tmp-macs)(!> peep-macs)(!> compile)((setter strip-module) ${STRIPMODULES}) ((setter optimize-code) t) (comp2sc (quote $*)) (!> bci)(byte-counts)(!> compile)(exit)' | ${FEEL_ENV} ${FEEL}
	test -f $*.sc


tar: ${SRCS}
	tar chf - ${SRCS} ${DISTFILES} | compress > comp.tar.Z

install: ${IMAGES}
	cp ${IMAGES} ${STATICIMAGES} ${FEEL_PATH}/Images
	
abs-syntx.sc: list-fns.sc scan-args.sc
do.sc:
list-fns.sc:
pass.sc: abs-syntx.sc list-fns.sc
syntx-env.sc: mod-cache.sc comp-utl.sc abs-syntx.sc list-fns.sc
assem.sc: instruct.sc list-fns.sc comp-defn.sc byte-stream.sc
foo.sc: peephole.sc list-fns.sc byte-stream.sc
peep-drv.sc: peephole.sc instruct.sc list-fns.sc byte-stream.sc
syntx-utl.sc: syntx-env.sc stop.sc props.sc pass.sc list-fns.sc
low-asm.sc: list-fns.sc
peep2.sc: peephole.sc instruct.sc list-fns.sc peep-macs.sc byte-stream.sc
test-call.sc: 
boot-utils.sc: boot.sc  
gen-code.sc: syntx-env.sc stop.sc props.sc rshow.sc pass.sc generate.sc syntx-utl.sc list-fns.sc
low-load.sc: list-fns.sc
peephole.sc: instruct.sc list-fns.sc comp-defn.sc peep-macs.sc byte-stream.sc
boot.sc:
generate.sc: compstate.sc instruct.sc list-fns.sc byte-stream.sc
low-seman.sc: abs-syntx.sc list-fns.sc
test.sc: list-fns.sc
bootcall.sc: list-fns.sc
i-macros.sc: list-fns.sc scan-args.sc
proggy.sc: list-fns.sc
iface.sc: syntx-env.sc props.sc comp-utl.sc syntx-utl.sc list-fns.sc
mexp.sc:
props.sc: low-seman.sc list-fns.sc
triang.sc:
combine.sc: link2.sc instruct.sc list-fns.sc comp-defn.sc assem.sc byte-stream.sc
initmeth.sc: boot-utils.sc newinit.sc
mod-cache.sc: comp-utl.sc list-fns.sc
resolve.sc: instruct.sc list-fns.sc
use.sc: props.sc pass.sc abs-syntx.sc list-fns.sc
comp-defn.sc: comp-utl.sc list-fns.sc
inlines.sc:
mod-info.sc: syntx-env.sc props.sc pass.sc list-fns.sc scan-args.sc
rshow.sc:
wibble.sc: list-fns.sc
peep-macs.sc: list-fns.sc
instruct.sc: i-macros.sc list-fns.sc scan-args.sc
scan-args.sc:
wow.sc: list-fns.sc
compstate.sc: props.sc abs-syntx.sc list-fns.sc scan-args.sc byte-stream.sc
interpret.sc: list-fns.sc
nlet.sc: list-fns.sc
xxx.sc: list-fns.sc
comp-utl.sc: list-fns.sc
junk.sc: list-fns.sc
non-std0.sc:
stop.sc: list-fns.sc
compile.sc: stop.sc iface.sc rshow.sc comp-utl.sc gen-code.sc output.sc list-fns.sc syntax.sc pass-0.sc
byte-stream.sc: list-fns.sc
daftgen.sc: list-fns.sc
link.sc: stop.sc list-fns.sc comp-defn.sc byte-stream.sc
output.sc: syntx-env.sc props.sc compstate.sc mod-info.sc instruct.sc list-fns.sc peep-drv.sc comp-defn.sc assem.sc byte-stream.sc
strtest.sc:
debug.sc: list-fns.sc
link2.sc: list-fns.sc comp-defn.sc byte-stream.sc
pass-0.sc: stop.sc props.sc pass.sc mod-info.sc abs-syntx.sc syntx-utl.sc list-fns.sc scan-args.sc
syntax.sc: syntx-env.sc list-fns.sc

clean:
	rm -f *.c *.sc *.i *.xm xx
