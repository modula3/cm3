# Copyright (C) 1992, Digital Equipment Corporation 
# All rights reserved.
# See the file COPYRIGHT for a full description.
#
# Last modified on Wed Jun  8 19:48:25 PDT 1994 by heydon

# "make all" runs all tests, first updating any out-of-date result files
# "make results" builds all the result files anew
# "make update" updates any result files that are out of date

PROG = ParserTest

RES-FILES = good.exprs.txt.out good.cmds.txt.out good.mod.txt.out\
            bad.exprs.txt.out bad.cmds.txt.out

all: $(RES-FILES)
	@run-tests $(RES-FILES)

results:
	rm -f $(RES-FILES)
	$(MAKE) update

update: $(RES-FILES)

.SUFFIXES: .txt.in .txt.out

.txt.in.txt.out:
	../`/udir/heydon/libi/csh/m3arch`/$(PROG) `option $?` $? > $@
