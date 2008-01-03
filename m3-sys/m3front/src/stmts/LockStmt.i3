(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: LockStmt.i3                                           *)
(* Last Modified On Fri Jun 24 09:21:38 PDT 1994 By kalsow     *)

INTERFACE LockStmt;

IMPORT Stmt;

PROCEDURE Parse (): Stmt.T;

END LockStmt.
