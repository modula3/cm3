# Copyright (C) 1992, Digital Equipment Corporation 
# All rights reserved.
# See the file COPYRIGHT for a full description.
#
# Last modified on Thu Apr 21 17:18:16 PDT 1994 by heydon

# "make all" runs all tests, first updating any out-of-date result files
# "make results" builds all the result files anew
# "make update" updates any result files that are out of date

PROG = ScopeTest

RES-FILE = test.txt.out

all: $(RES-FILE)
	@run-tests $(RES-FILE)

results:
	rm -f $(RES-FILE)
	$(MAKE) update

update: $(RES-FILE)

$(RES-FILE):
	../`/udir/heydon/libi/csh/m3arch`/$(PROG) > $@
