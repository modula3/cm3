(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ExitStmt.i3                                           *)
(* Last Modified On Fri Jun 24 09:21:11 PDT 1994 by kalsow     *)
(*      Modified On Sat Jun  3 15:29:59 PDT 1989 by muller     *)

INTERFACE ExitStmt;

IMPORT Stmt;

PROCEDURE Parse (): Stmt.T;

END ExitStmt.
