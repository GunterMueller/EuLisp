# Cheapo makefile for generic boot files
# Produces init.em and you.mods only!

#Don't want to pick up _any_ default paths...
.SUFFIXES:
.SUFFIXES: .em .sc 

FEEL=${HOME}/Feel/bin/feel
SOCKETS=t

init: init.em 
map: you.mods

all: map init

init.em: gen-init.em class-defs.em class-macs.em
	-cp init.em init.em.bak
	echo '(!> class-macs)(set-sockets ${SOCKETS})(!> gen-init)(write-i-code)(exit)' | ${FEEL}

you.mods: ../mips/you
	-${FEEL} -procs 1 -map -heap 2 < /dev/null
	test -r you.mods

clean: 
	rm -f init.em you.mods


