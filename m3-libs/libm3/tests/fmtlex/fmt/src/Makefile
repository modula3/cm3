# Copyright (C) 1992, Digital Equipment Corporation 
# All rights reserved.
# See the file COPYRIGHT for a full description.
#
# Last modified on Thu Mar 24 15:53:07 PST 1994 by heydon
#      modified on Fri Feb 25 12:51:06 PST 1994 by kalsow

# For each '.in' file, we produce a '.out' file. A '.out' file is out-of-date
# if it was modified less recently than its corresponding '.in' file.
#
# "make update" updates any '.out' files that are out of date
#    Use this if any of the '.in' files have changed. In this case, the
#    resulting '.out' file should be checked by hand to ensure that the test
#    program is producing the correct output.
#
# "make results" builds all the '.out' files anew
#    Use this if the test program or the Lex implementation has changed.
#
# "make [all]" runs all tests, first updating any out-of-date result files
#    This runs the test program on each '.in' file, storing the result in a
#    temporary file. The temporary file is then compared to the corresponding
#    (known good) '.out' file, and any discrepancies are reported.

PROG = FmtTest

RES-FILES = \
  integer.txt.out zero.txt.out infinity.txt.out nan.txt.out\
  sci.txt.out fix.txt.out auto.txt.out round.txt.out alpha.txt.out

all: $(RES-FILES)
	@run-tests $(RES-FILES)

results:
	rm -f $(RES-FILES)
	$(MAKE) update

update: $(RES-FILES)

.SUFFIXES: .txt.in .txt.out

.txt.in.txt.out:
	../`/udir/heydon/libi/csh/m3arch`/$(PROG) $? > $@
