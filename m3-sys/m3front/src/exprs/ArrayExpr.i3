(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ArrayExpr.i3                                          *)
(* Last Modified On Thu Aug 27 09:10:06 PDT 1992 By kalsow     *)
(*      Modified On Fri Dec 21 01:19:11 1990 By muller         *)

INTERFACE ArrayExpr;

IMPORT Type, Expr, Target;

PROCEDURE New (type: Type.T;  args: Expr.List;  dots: BOOLEAN): Expr.T;

PROCEDURE Is (e: Expr.T): BOOLEAN;

PROCEDURE GetBounds (array: Expr.T; VAR min, max: Target.Int): BOOLEAN;

PROCEDURE Subscript (array, index: Expr.T;  VAR e: Expr.T): BOOLEAN;

END ArrayExpr.
