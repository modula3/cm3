(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: DivideExpr.i3                                         *)
(* Last Modified On Wed Jul 26 09:00:09 1989 By kalsow         *)

INTERFACE DivideExpr;

IMPORT Expr;

PROCEDURE New (a, b: Expr.T): Expr.T;

END DivideExpr.
