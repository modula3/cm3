# Copyright (C) 1992, Digital Equipment Corporation 
# All rights reserved.
# See the file COPYRIGHT for a full description.
#
# Last modified on Fri Jun  9 10:56:38 PDT 1995 by heydon

# "make all" runs all tests, first updating any out-of-date result files
# "make results" builds all the result files anew
# "make update" updates any result files that are out of date

PROG = RuntimeTest

RES-FILES = bc-01.txt.out bc-02.txt.out bc-03.txt.out bc-04.txt.out\
            bc-05.txt.out bc-06.txt.out bc-07.txt.out bc-08.txt.out\
            bc-09.txt.out bc-10.txt.out bc-11.txt.out bc-12.txt.out\
            bc-13.txt.out bc-14.txt.out bc-15.txt.out bc-16.txt.out\
            bc-17.txt.out bc-18.txt.out bc-19.txt.out\
            equal.txt.out is-int.txt.out is-int2.txt.out

all: $(RES-FILES)
	@run-tests $(RES-FILES)

results:
	rm -f $(RES-FILES)
	$(MAKE) update

update: $(RES-FILES)

.SUFFIXES: .txt.in .txt.out

.txt.in.txt.out:
	../`/udir/heydon/libi/csh/m3arch`/$(PROG) < $? > $@
