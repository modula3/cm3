(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: VarExpr.i3                                            *)
(* Last Modified On Fri Jun 24 08:49:19 PDT 1994 By kalsow     *)

INTERFACE VarExpr;

IMPORT M3ID, Type, Expr, Variable;

PROCEDURE New (t: Type.T;  name: M3ID.T): Expr.T;

PROCEDURE Obj (e: Expr.T): Variable.T;

END VarExpr.
