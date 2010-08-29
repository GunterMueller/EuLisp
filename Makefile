### Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
###-----------------------------------------------------------------------------
### ---                   EuLisp System 'youtoo'
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### Top-level Makefile
###-----------------------------------------------------------------------------

ARCH := $(shell uname -m)
include Lib.$(ARCH)/Makefile

###-----------------------------------------------------------------------------
### Miscellaneous commands
###-----------------------------------------------------------------------------

.PHONY: default
default: euxlisp youtoo

.PHONY: all
all: default doc

.PHONY: euxlisp
euxlisp:
	@echo "BUILDING EuXLisp ..."
	@$(MAKE) -C EuXLisp
	@echo "DONE"

.PHONY: youtoo
youtoo:
	@echo "BUILDING Youtoo ..."
	@$(MAKE) -C Youtoo
	@echo "DONE"

.PHONY: test
test:
	@echo "TESTING installation ..."
	@$(MAKE) -C Youtoo test
	@echo "DONE"

README.org: index.org
	@sed 's%file:%http://henry.github.com/EuLisp/%' $< > $@

.PHONY: README
README: index.html TODO.html README.org
	@echo "UPDATING all README.html files ..."
	@$(MAKE) -C Youtoo $@
	@$(MAKE) -C Eu2C $@
	@$(MAKE) -C Modules $@
	@echo "DONE"

.PHONY: doc
doc: README
	@$(MAKE) -C Doc

.PHONY: clean
clean:
	@echo "CLEANING ..."
	@$(MAKE) -C Youtoo $@
	@$(MAKE) -C Modules $@
	@$(MAKE) -C Examples $@
	@$(MAKE) -C Doc $@
	@echo "DONE"

.PHONY: gitclean
gitclean:
	@echo "GIT-CLEANING ..."
	@$(MAKE) -C Youtoo $@
	@$(MAKE) -C Modules $@
	@$(MAKE) -C Examples $@
	@$(MAKE) -C Doc $@
	@echo "DONE"

.PHONY: distclean
distclean: clean
	@echo "DIST-CLEANING ..."
	@$(MAKE) -C Youtoo $@
	@$(MAKE) -C Modules $@
	@$(MAKE) -C Examples $@
	@$(MAKE) -C Doc $@
	@rm -rf */platforms Bin.* Lib.*
	@rm -f .eulrc.*
	@echo "DONE"

###-----------------------------------------------------------------------------
