(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: BlockStmt.i3                                          *)
(* Last Modified On Fri Jun 24 08:56:15 PDT 1994 By kalsow     *)

INTERFACE BlockStmt;

IMPORT M3, Stmt, Tracer;

PROCEDURE Parse (needScope := TRUE): Stmt.T;

PROCEDURE ExtractFails (t: Stmt.T): M3.ExSet;

PROCEDURE BodyOffset (t: Stmt.T): INTEGER;

PROCEDURE ParseTrace (): Tracer.T;
PROCEDURE CheckTrace (t: Tracer.T;  VAR cs: Stmt.CheckState);

END BlockStmt.
