(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: OrExpr.i3                                             *)
(* Last Modified On Thu Jul 27 13:16:32 1989 By kalsow         *)

INTERFACE OrExpr;

IMPORT Expr;

PROCEDURE New (a, b: Expr.T): Expr.T;

END OrExpr.
