(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: VarExpr.i3                                            *)
(* Last Modified On Fri Jun 24 08:49:19 PDT 1994 By kalsow     *)

INTERFACE VarExpr;

IMPORT M3ID, Type, Expr, Variable, MSIR;

PROCEDURE New (t: Type.T;  name: M3ID.T): Expr.T;

PROCEDURE Obj (e: Expr.T): Variable.T;

(* If e is a bit-field WITH alias, write rhs through it (InsertBitField),
   return TRUE; else FALSE.  Used by AssignStmt for a no-lvalue var LHS. *)
PROCEDURE BitFieldStoreMSIR (e: Expr.T;  rhs: MSIR.Value): BOOLEAN;

END VarExpr.
