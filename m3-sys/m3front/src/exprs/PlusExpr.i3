(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: PlusExpr.i3                                           *)
(* Last Modified On Thu Jul 27 13:18:46 1989 By kalsow         *)

INTERFACE PlusExpr;

IMPORT Expr;

PROCEDURE New (a: Expr.T): Expr.T;

END PlusExpr.
