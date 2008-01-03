(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: NegateExpr.i3                                         *)
(* Last Modified On Wed Jul 26 13:19:03 1989 By kalsow         *)

INTERFACE NegateExpr;

IMPORT Expr;

PROCEDURE New (a: Expr.T): Expr.T;

END NegateExpr.
