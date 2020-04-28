(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RecordExpr.i3                                         *)
(* Last Modified On Fri Jun 24 08:48:36 PDT 1994 By kalsow         *)

INTERFACE RecordExpr;
(* A record constructor. *) 

IMPORT M3ID, Type, Expr, CG;

PROCEDURE New (type: Type.T;  args: Expr.List): Expr.T;

PROCEDURE Is (e: Expr.T): BOOLEAN;
(* Purely syntactic. Will not look through a ConsExpr. *)

PROCEDURE Qualify (record: Expr.T;  id: M3ID.T;  VAR e: Expr.T): BOOLEAN;

PROCEDURE CheckRT
  (expr: Expr.T; VAR(*OUT*) Code: CG.RuntimeError; VAR(*OUT*) Msg: TEXT);

END RecordExpr.
