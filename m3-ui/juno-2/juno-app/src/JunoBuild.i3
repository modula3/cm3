(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun 16 13:34:26 PDT 1994 by heydon                   *)
(*      modified on Fri Aug  7 21:51:50 PDT 1992 by myers                    *)
<* PRAGMA LL *>

INTERFACE JunoBuild;

(* Higher-level procedures for compiling editor commands. *)

IMPORT JunoScope, JunoAST, JunoCompileErr;

PROCEDURE Cmd(cmd: JunoAST.Cmd; scp: JunoScope.T): CARDINAL
    RAISES {JunoCompileErr.Error};
(* Compiles "cmd" under scope "scp", and installs the resulting bytestream in
   a fixed slot of the run-time global code table "JunoRT.code_tbl" (possibly
   overwriting the last bytestream installed there). Returns the index of this
   fixed slot in the code table. *)

PROCEDURE CurrCmd(
    cmd: JunoAST.Cmd;
    scp: JunoScope.T;
    checkTotal := FALSE):
    CARDINAL RAISES {JunoCompileErr.Error};
(* Calls "Cmd" above on the command "IF <cmd> FI; HALT". If "checkTotal", then
   first calls "JunoChckBNF.TotalCommand" of the resulting command to
   guarantee it has the proper form. *)

END JunoBuild.
