# Copyright (C) 1992, Digital Equipment Corporation
# All rights reserved.
# See the file COPYRIGHT for a full description.

# Last modified on Mon Jun 29 15:08:51 PDT 1992 by heydon

# makefile for /proj/m3/pkg/juno-compiler/src/Grammar

# On Bigtop, these lines produce the Juno.out file using Bill Kalsow's
# LLGen(1) Modula-2+ program.
#
Juno.out: Juno.bnf
	LLGen +first +uses < Juno.bnf > Juno.out
	rm -f Juno.def Juno.mod Juno.tables
