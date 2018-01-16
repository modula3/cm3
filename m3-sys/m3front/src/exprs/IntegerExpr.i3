(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: IntegerExpr.i3                                        *)
(* Last Modified On Tue Aug 25 14:21:07 PDT 1992 By kalsow     *)

INTERFACE IntegerExpr;
(* INTEGER and LONGINT *Constants* only. *)

IMPORT Type, Expr, Target;

PROCEDURE New  (type: Type.T;  READONLY value: Target.Int): Expr.T;
(* PRE: Type.IsSubtype (type, Int.T) OR Type.IsSubtype (type, LInt.T) *)

PROCEDURE Split (e: Expr.T;  VAR v: Target.Int;  VAR t: Type.T): BOOLEAN;

PROCEDURE Compare  (a, b: Expr.T;  VAR sign: INTEGER): BOOLEAN;
PROCEDURE Add      (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Subtract (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Multiply (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Div      (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Mod      (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Negate   (a: Expr.T;     VAR c: Expr.T): BOOLEAN;
PROCEDURE Abs      (a: Expr.T;     VAR c: Expr.T): BOOLEAN;
PROCEDURE ToInt    (a: Expr.T;     VAR i: INTEGER): BOOLEAN;

END IntegerExpr.
