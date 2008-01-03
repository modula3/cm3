(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AddressExpr.i3                                        *)
(* Last Modified On Mon Aug 24 09:55:55 PDT 1992 By kalsow     *)

INTERFACE AddressExpr;

IMPORT Expr, Target;

PROCEDURE New (READONLY value: Target.Int): Expr.T;

PROCEDURE Split (e: Expr.T;  VAR value: Target.Int): BOOLEAN;

PROCEDURE Compare  (a, b: Expr.T;  VAR sign: INTEGER): BOOLEAN;
PROCEDURE Add      (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;
PROCEDURE Subtract (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN;

END AddressExpr.
