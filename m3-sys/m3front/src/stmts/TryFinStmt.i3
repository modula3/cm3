(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TryFinStmt.i3                                         *)
(* Last Modified On Fri Jun 24 09:32:47 PDT 1994 By kalsow     *)
(*      Modified On Mon Nov 27 16:24:34 1989 By muller         *)

INTERFACE TryFinStmt;

IMPORT Stmt, MSIR;

PROCEDURE Parse (body: Stmt.T; ): Stmt.T;

PROCEDURE EmitFinallyMSIR (body, finally: Stmt.T;  obj: MSIR.Value;
                           midx: INTEGER);
(* MSIR for TRY body FINALLY ... END as EH scopes (D26).  The FINALLY code
   is 'finally', or if that is NIL a call of method slot midx of obj (LOCK's
   release). *)

END TryFinStmt.
