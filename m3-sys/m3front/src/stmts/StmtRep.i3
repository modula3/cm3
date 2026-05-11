(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: StmtRep.i3                                            *)
(* Last Modified On Mon Sep 21 09:19:08 PDT 1992 By kalsow     *)

INTERFACE StmtRep;

IMPORT M3, Stmt, CaptureAnalysis;

REVEAL
  M3.Stmt = M3.Node BRANDED "Stmt.T" OBJECT
    next   : M3.Stmt;
  METHODS
    check       (VAR cs: M3.CheckState);
    compile     (): Stmt.Outcomes;
    outcomes    (): Stmt.Outcomes;
    compileMSIR ()                     := MSIRDefault;
    (* Emit MSIR for this statement. On encountering an unsupported
       construct, calls MSIRBuilder.Abandon — the enclosing proc
       will be dropped at EndProc. *)
    scan        (ca: CaptureAnalysis.T) := ScanDefault;
    (* Walk this statement, recording up-level variable captures in ca.
       Concrete implementations recurse into sub-statements and
       sub-expressions.  The default is a no-op (correct for leaf stmts
       with no sub-nodes). *)
  END;

PROCEDURE Init (stmt: M3.Stmt);
(* initializes the common fields of a Stmt.T *)

PROCEDURE MSIRDefault (s: M3.Stmt);
PROCEDURE ScanDefault  (s: M3.Stmt;  ca: CaptureAnalysis.T);

END StmtRep.
