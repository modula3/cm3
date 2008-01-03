# Copyright (C) 1992, Digital Equipment Corporation
# All rights reserved.
# See the file COPYRIGHT for a full description.
#
# Last modified on Wed Aug 10 15:43:22 PDT 1994 by kalsow

YACC = bison -y

all: lex.yy.c  y.tab.c

lex.yy.c: ../parse.lex ; flex ../parse.lex

y.tab.c: ../parse.yacc
	@echo "expect 2 shift/reduce, 1 reduce/reduce conflicts"
	$(YACC) ../parse.yacc

clean: ; rm -f lex.yy.c y.tab.c

