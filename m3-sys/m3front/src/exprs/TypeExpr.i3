(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TypeExpr.i3                                           *)
(* Last Modified On Mon Jul 31 13:40:51 1989 By kalsow         *)

INTERFACE TypeExpr;

IMPORT Expr, Type;

PROCEDURE New (t: Type.T): Expr.T;

PROCEDURE Split (e: Expr.T;  VAR t: Type.T): BOOLEAN;

END TypeExpr.
