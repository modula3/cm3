# Copyright (C) 1992, Digital Equipment Corporation
# All rights reserved.
# See the file COPYRIGHT for a full description.
#
# Last modified on Fri Jul  7 09:36:48 PDT 1995 by kalsow

YACC = bison -y

all: lex.yy.c  y.tab.c

lex.yy.c: ../Parse.lex ; lex ../Parse.lex

y.tab.c: ../Parse.yacc
	@echo "expect 3 shift/reduce conflicts"
	$(YACC) ../Parse.yacc

clean: ; rm -f lex.yy.c y.tab.c

