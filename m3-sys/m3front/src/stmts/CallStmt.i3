(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CallStmt.i3                                           *)
(* Last Modified On Mon Jul 24 11:02:27 1989 By kalsow         *)

INTERFACE CallStmt;

IMPORT Expr, Stmt;

PROCEDURE New (e: Expr.T): Stmt.T;

END CallStmt.
