# Copyright (c) 2000 California Institute of Technology
# All rights reserved. See the file COPYRIGHT for a full description.
# $Id: Makefile,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $

SHIPS= ktoklib kyacclib klexlib ktok klex kyacc kext parserlib parserlib/test
REVSHIPS= `echo $(SHIPS) | xargs -n1 echo | tail -r`
M3BUILD= m3build -O
M3SHIP= echo
EXPORT= ../m3browserhack/export.sh

all: .parsertop ships buildhtml browserhack
clean: cleanships cleanthis cleanhtml

buildhtml:
	@echo "(parserlib) ===> html" ;\
	(cd html/src; ./build) ; echo

cleanhtml:
	(cd html; rm -f *.html)

.parsertop:
	echo _PARSER_TOP=\"`pwd`\" > .parsertop

cleanthis:
	rm -f .parsertop

ships: 
	@for s in $(SHIPS); do\
		echo "(parserlib) ===> $$s";\
		(cd $$s; $(M3BUILD); $(M3SHIP));  \
	done

cleanships:
	@for s in $(REVSHIPS); do\
		echo "(parserlib) (cleaning) ===> $$s";\
		(cd $$s; $(M3BUILD) clean || true);  \
	done

browserhack:
	@echo "(parserlib) ===> export" ;\
	$(EXPORT) k* ;\
	$(EXPORT) parserlib; \
	echo
