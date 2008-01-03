(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: EqualExpr.i3                                          *)
(* Last Modified On Thu Aug  3 16:25:34 1989 By kalsow         *)

INTERFACE EqualExpr;

IMPORT Expr, CG;

PROCEDURE New (a, b: Expr.T;  op: [ CG.Cmp.EQ .. CG.Cmp.NE ]): Expr.T;

END EqualExpr.
