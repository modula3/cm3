(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: EnumExpr.i3                                           *)
(* Last Modified On Thu Aug 20 10:32:34 PDT 1992 By kalsow     *)
(*      Modified On Wed Aug 23 15:45:01 1989 By muller         *)

INTERFACE EnumExpr;

IMPORT Expr, Type, Target;

PROCEDURE New (type: Type.T;  READONLY value: Target.Int): Expr.T;

PROCEDURE Split (e: Expr.T;  VAR value: Target.Int;  VAR t: Type.T): BOOLEAN;

PROCEDURE SplitPair (a, b: Expr.T;  VAR x, y: Target.Int): BOOLEAN;

PROCEDURE Compare (a, b: Expr.T;  VAR sign: INTEGER): BOOLEAN;

END EnumExpr.
