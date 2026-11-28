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

TYPE Op = {Add, Subtract, Multiply, Negate};

PROCEDURE Overflows (op: Op;  a, b: Expr.T): BOOLEAN;
(* TRUE if a and b (b is ignored for Negate) are integer constants and op
   on them overflows their type, so that the constant fold (Add, Subtract,
   Multiply, Negate) wraps around. *)

PROCEDURE CheckOverflow (op: Op;  a, b: Expr.T);
(* If Overflows (op, a, b), report it: an error with Host.doOverflowChk,
   otherwise a warning. *)

PROCEDURE IsMinusOne (e: Expr.T): BOOLEAN;
(* TRUE if e is the integer constant -1. *)

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
