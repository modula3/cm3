# Copyright (C) 1992, Digital Equipment Corporation 
# All rights reserved.
# See the file COPYRIGHT for a full description.
#
# Last modified on Fri Sep 23 10:43:26 PDT 1994 by heydon

# "make all" runs all tests, first updating any out-of-date result files
# "make results" builds all the result files anew
# "make update" updates any result files that are out of date

PROG = FromTTY

RES-FILES = heapgrow.txt.out fromelts.txt.out delete.txt.out \
            dmin.txt.out change.txt.out

all: $(RES-FILES)
	@run-tests $(RES-FILES)

results:
	rm -f $(RES-FILES)
	$(MAKE) update

update: $(RES-FILES)

.SUFFIXES: .txt.in .txt.out

.txt.in.txt.out:
	../`/udir/heydon/libi/csh/m3arch`/$(PROG) $? > $@
