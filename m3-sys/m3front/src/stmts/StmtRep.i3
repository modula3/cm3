(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: StmtRep.i3                                            *)
(* Last Modified On Mon Sep 21 09:19:08 PDT 1992 By kalsow     *)

INTERFACE StmtRep;

IMPORT M3, Stmt;

REVEAL
  M3.Stmt = M3.Node BRANDED "Stmt.T" OBJECT
    next   : M3.Stmt;
  METHODS
    check       (VAR cs: M3.CheckState);
    compile     (): Stmt.Outcomes;
    outcomes    (): Stmt.Outcomes;
  END;

PROCEDURE Init (stmt: M3.Stmt);
(* initializes the common fields of a Stmt.T *)

END StmtRep.
