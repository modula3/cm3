(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ModExpr.i3                                            *)
(* Last Modified On Wed Jul 26 10:26:44 1989 By kalsow         *)

INTERFACE ModExpr;

IMPORT Expr;

PROCEDURE New (a, b: Expr.T): Expr.T;

END ModExpr.
