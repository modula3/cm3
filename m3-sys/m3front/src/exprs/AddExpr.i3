(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AddExpr.i3                                            *)
(* Last Modified On Thu Nov  2 17:47:40 1989 By muller         *)
(*      Modified On Mon Jul 24 14:30:20 1989 By kalsow         *)

INTERFACE AddExpr;

IMPORT Expr;

PROCEDURE New (a, b: Expr.T): Expr.T;

END AddExpr.
