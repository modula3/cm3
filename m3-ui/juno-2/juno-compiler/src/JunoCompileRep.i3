(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct 31 09:38:22 PST 1994 by heydon                   *)
(*      modified on Sat Oct 17 18:01:02 PDT 1992 by gnelson                  *)
(*      modified on Fri Aug  7 21:54:01 PDT 1992 by myers                    *)

INTERFACE JunoCompileRep;

(* This interface reveals to clients like JunoAssemble that the result
   produced by JunoCompile.Cmd is actually a restricted form of JunoAST.T. *)

IMPORT JunoAST, StackTbl, JunoScope;
FROM JunoCompileErr IMPORT Error;

TYPE
  Result = BRANDED "JunoCompileRep.Result" OBJECT
    cmd: JunoAST.Cmd
  END;

PROCEDURE Cmd(
    cmd: JunoAST.Cmd;
    scp: JunoScope.T;
    stack_tbl: StackTbl.T;
    annotate := TRUE;
    pure := FALSE):
  Result RAISES {Error};
(* Compile the command "cmd" according to the "TotalCmd" non-terminal of the
   Juno grammar in the scope "scp", producing a bytecode stream to be
   executed. The scope "scp" must be a "Unit" scope. By default, this
   procedure first annotates "cmd"; use "annotate := FALSE" if "cmd" has
   already been annotated.

   Raises "Error" in the event of a compilation error, or if "pure" is set and
   "cmd" contains a reference to a global variable or procedure. *)

END JunoCompileRep.
