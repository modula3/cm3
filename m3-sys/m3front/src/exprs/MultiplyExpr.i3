(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MultiplyExpr.i3                                       *)
(* Last Modified On Wed Jul 26 10:31:37 1989 By kalsow         *)

INTERFACE MultiplyExpr;

IMPORT Expr;

PROCEDURE New (a, b: Expr.T): Expr.T;

END MultiplyExpr.
