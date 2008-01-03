(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: DerefExpr.i3                                          *)
(* Last Modified On Thu Jun 28 03:43:00 1990 By muller         *)
(*      Modified On Wed Jul 26 08:50:54 1989 By kalsow         *)

INTERFACE DerefExpr;

IMPORT Expr;

PROCEDURE New (a: Expr.T): Expr.T;

PROCEDURE SetOffset (e: Expr.T; n: INTEGER);

END DerefExpr.
