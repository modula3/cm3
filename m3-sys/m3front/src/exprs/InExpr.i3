(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: InExpr.i3                                             *)
(* Last Modified On Wed Jul 26 10:02:46 1989 By kalsow         *)

INTERFACE InExpr;

IMPORT Expr;

PROCEDURE New (a, b: Expr.T): Expr.T;

END InExpr.
