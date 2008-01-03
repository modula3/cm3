(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RangeExpr.i3                                          *)
(* Last Modified On Fri Jan  5 17:53:38 1990 By kalsow         *)

INTERFACE RangeExpr;

IMPORT Expr;

PROCEDURE New (a, b: Expr.T): Expr.T;

PROCEDURE Split (e: Expr.T;  VAR min, max: Expr.T): BOOLEAN;

END RangeExpr.
