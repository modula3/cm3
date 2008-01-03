(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: NamedExpr.i3                                          *)
(* Last Modified On Fri Jun 24 08:47:36 PDT 1994 By kalsow     *)

INTERFACE NamedExpr;

IMPORT M3ID, Expr, Value;

PROCEDURE New (name: M3ID.T;  value: Value.T): Expr.T;

PROCEDURE FromValue (value: Value.T): Expr.T;

PROCEDURE Split (e: Expr.T;  VAR name: M3ID.T;  VAR v: Value.T): BOOLEAN;

PROCEDURE SplitName (e: Expr.T;  VAR name: M3ID.T): BOOLEAN;

END NamedExpr.
