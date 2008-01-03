(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ProcExpr.i3                                           *)
(* Last Modified On Thu Jul 27 13:25:40 1989 By kalsow         *)

INTERFACE ProcExpr;

IMPORT Expr, Value;

PROCEDURE New (proc: Value.T): Expr.T;

PROCEDURE Split (e: Expr.T;  VAR proc: Value.T): BOOLEAN;

PROCEDURE Compare (a, b: Expr.T;  VAR sign: INTEGER): BOOLEAN;

END ProcExpr.
