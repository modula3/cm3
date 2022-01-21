(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: LoopholeExpr.i3                                           *)
(* Last Modified On Thu Apr 30 13:24:03 PDT 1992 By kalsow     *)

INTERFACE LoopholeExpr;

IMPORT Expr, Type;

PROCEDURE New (a: Expr.T;  t: Type.T): Expr.T;

END LoopholeExpr.
