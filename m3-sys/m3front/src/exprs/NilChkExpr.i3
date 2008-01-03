(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: NilChkExpr.i3                                         *)
(* Last Modified On Wed Apr  4 07:39:28 1990 By muller         *)
(*      Modified On Wed Jul 26 13:22:06 1989 By kalsow         *)

INTERFACE NilChkExpr;

IMPORT Expr;

PROCEDURE New (a: Expr.T): Expr.T;

PROCEDURE SetOffset (a: Expr.T; offset: INTEGER);

END NilChkExpr.
