(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: UserProc.i3                                           *)
(* Last Modified On Fri Jul 20 11:35:15 1990 By muller         *)
(*      Modified On Wed Jun 28 12:54:58 1989 By kalsow         *)

INTERFACE UserProc;

IMPORT CallExpr, Expr, Value;

VAR (*READONLY*) Methods: CallExpr.MethodList;

PROCEDURE Initialize ();
PROCEDURE IsProcedureLiteral (e: Expr.T;  VAR proc: Value.T): BOOLEAN;

END UserProc.
