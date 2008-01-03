(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: SubtractExpr.i3                                       *)
(* Last Modified On Thu Nov 29 23:24:38 1990 By muller         *)
(*      Modified On Mon Jul 31 13:28:37 1989 By kalsow         *)

INTERFACE SubtractExpr;

IMPORT Expr;

PROCEDURE New (a, b: Expr.T; extended := FALSE): Expr.T;

END SubtractExpr.
