(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TryStmt.i3                                            *)
(* Last Modified On Fri Jun 24 09:32:46 PDT 1994 By kalsow     *)

INTERFACE TryStmt;

IMPORT Stmt, CG;

PROCEDURE Parse (): Stmt.T;

PROCEDURE InHandler (): BOOLEAN;
(* Returns "TRUE" if we're currently typechecking the body of a handler *)

PROCEDURE LoadInfoPtr ();
(* Load the address of the RT0.RaiseActivation record passed
   to the handler that's currently being compiled. *)

PROCEDURE PushHandler (info: CG.Var;  offset: INTEGER;   direct: BOOLEAN);
PROCEDURE PopHandler ();
(* Records the entry & exit from handler code *)

END TryStmt.
