(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RepeatStmt.i3                                         *)
(* Last Modified On Fri Jun 24 09:32:47 PDT 1994 By kalsow     *)

INTERFACE RepeatStmt;

IMPORT Stmt;

PROCEDURE Parse (): Stmt.T;

END RepeatStmt.
