(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: IntegerExpr.i3                                        *)
(* Last Modified On Tue Aug 25 14:21:07 PDT 1992 By kalsow     *)

INTERFACE IntegerExpr;

IMPORT Expr, Target;

PROCEDURE New  (READONLY value: Target.Int): Expr.T;

PROCEDURE Split (e: Expr.T;  VAR value: Target.Int): BOOLEAN;

PROCEDURE Compare  (a, b: Expr.T;  VAR sign: INTEGER): BOOLEAN;
PROCEDURE Add      (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Subtract (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Multiply (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Div      (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Mod      (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Negate   (a: Expr.T;     VAR c: Expr.T): BOOLEAN;

END IntegerExpr.
