(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WhileStmt.i3                                          *)
(* Last Modified On Fri Jun 24 09:22:43 PDT 1994 By kalsow     *)

INTERFACE WhileStmt;

IMPORT Stmt;

PROCEDURE Parse (): Stmt.T;

END WhileStmt.
