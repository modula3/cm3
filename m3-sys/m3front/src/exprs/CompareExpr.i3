(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CompareExpr.i3                                        *)
(* Last Modified On Sat Apr 28 00:19:24 1990 By muller         *)
(*      Modified On Wed Jul 26 08:31:50 1989 By kalsow         *)

INTERFACE CompareExpr;

IMPORT Expr;

TYPE Op = {GT,GE,LT,LE};

PROCEDURE New (a, b: Expr.T;  op: Op): Expr.T;

END CompareExpr.
