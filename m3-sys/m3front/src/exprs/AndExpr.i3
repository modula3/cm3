(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AndExpr.i3                                            *)
(* Last Modified On Tue Jul 25 16:15:26 1989 By kalsow         *)

INTERFACE AndExpr;

IMPORT Expr;

PROCEDURE New (a, b: Expr.T): Expr.T;

END AndExpr.
