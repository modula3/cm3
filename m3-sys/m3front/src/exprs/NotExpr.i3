(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: NotExpr.i3                                            *)
(* Last Modified On Wed Jul 26 13:25:11 1989 By kalsow         *)

INTERFACE NotExpr;

IMPORT Expr;

PROCEDURE New (a: Expr.T): Expr.T;

END NotExpr.
