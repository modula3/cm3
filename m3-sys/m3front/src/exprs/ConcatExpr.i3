(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ConcatExpr.i3                                         *)
(* Last Modified On Mon Jul 31 14:32:17 1989 By kalsow         *)

INTERFACE ConcatExpr;

IMPORT Expr;

PROCEDURE New (a, b: Expr.T): Expr.T;

END ConcatExpr.
