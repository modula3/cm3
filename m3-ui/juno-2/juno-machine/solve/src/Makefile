# Copyright (C) 1992, Digital Equipment Corporation 
# All rights reserved.
# See the file COPYRIGHT for a full description.
#
# Last modified on Mon Apr 29 15:38:35 PDT 1996 by heydon

# "make all" runs all tests, first updating any out-of-date result files
# "make results" builds all the result files anew
# "make update" updates any result files that are out of date

PROG = SolveTest

RES-FILES = test01.sx.out test02.sx.out test03.sx.out

all: $(RES-FILES)
	@run-tests $(RES-FILES)

results:
	rm -f $(RES-FILES)
	$(MAKE) update

update: $(RES-FILES)

.SUFFIXES: .sx.in .sx.out

.sx.in.sx.out:
	../`/udir/heydon/libi/csh/m3arch`/$(PROG) < $? > $@
