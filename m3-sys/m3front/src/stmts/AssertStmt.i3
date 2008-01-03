(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AssertStmt.i3                                         *)
(* Last Modified On Fri Jun 24 08:55:56 PDT 1994 By kalsow     *)
(*      Modified On Fri Oct 13 18:09:17 1989 By muller         *)

INTERFACE AssertStmt;

IMPORT Stmt;

PROCEDURE Parse (): Stmt.T;

END AssertStmt.
