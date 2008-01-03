(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: SubscriptExpr.i3                                      *)
(* Last Modified On Mon Jul 31 13:22:38 1989 By kalsow         *)

INTERFACE SubscriptExpr;

IMPORT Expr;

PROCEDURE New (a, b: Expr.T): Expr.T;

END SubscriptExpr.
