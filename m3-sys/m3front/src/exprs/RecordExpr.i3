(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RecordExpr.i3                                         *)
(* Last Modified On Fri Jun 24 08:48:36 PDT 1994 By kalsow         *)

INTERFACE RecordExpr;

IMPORT M3ID, Type, Expr;

PROCEDURE New (type: Type.T;  args: Expr.List): Expr.T;

PROCEDURE Is (e: Expr.T): BOOLEAN;

PROCEDURE Qualify (record: Expr.T;  id: M3ID.T;  VAR e: Expr.T): BOOLEAN;

END RecordExpr.
