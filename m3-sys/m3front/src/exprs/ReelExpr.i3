(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ReelExpr.i3                                           *)
(* Last Modified On Thu Aug 20 11:13:27 PDT 1992 By kalsow     *)

INTERFACE ReelExpr;

IMPORT Expr, Type, Target;

TYPE Precision = Target.Precision;

PROCEDURE New  (READONLY value: Target.Float;  pre: Precision): Expr.T;

(* returns TRUE iff the operation was successful *)
PROCEDURE Compare  (a, b: Expr.T;  VAR sign: INTEGER): BOOLEAN;
PROCEDURE Add      (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Subtract (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Multiply (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Divide   (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Mod      (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Min      (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Max      (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Negate   (a: Expr.T;     VAR c: Expr.T): BOOLEAN;
PROCEDURE Abs      (a: Expr.T;     VAR c: Expr.T): BOOLEAN;
PROCEDURE Floor    (a: Expr.T;     VAR c: Expr.T): BOOLEAN;
PROCEDURE Ceiling  (a: Expr.T;     VAR c: Expr.T): BOOLEAN;
PROCEDURE Trunc    (a: Expr.T;     VAR c: Expr.T): BOOLEAN;
PROCEDURE Round    (a: Expr.T;     VAR c: Expr.T): BOOLEAN;
PROCEDURE Float    (a: Expr.T;  t: Type.T;  VAR c: Expr.T): BOOLEAN;

END ReelExpr.
