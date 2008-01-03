(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Sep  9 09:51:49 PDT 1994 by heydon                   *)
(*      modified on Sat Aug 22 23:32:17 PDT 1992 by myers                    *)
<* PRAGMA LL *>

MODULE JunoBuild;

IMPORT JunoAST, JunoScope, JunoCompile, JunoCompileErr, JunoChkBNF;
IMPORT Atom;

VAR (* CONST *) ccmd_proc := Atom.FromText("Current Command");
(* The name of this procedure is printed in run-time error messages. Since it
   contains a space character, there is no possibility that the user could
   declare a procedure with the same name. *)

PROCEDURE Cmd(cmd: JunoAST.Cmd; scp: JunoScope.T): CARDINAL
    RAISES {JunoCompileErr.Error} =
  VAR
    proc_head := NEW(JunoAST.ProcHeader, name := ccmd_proc,
      outs := JunoAST.EmptyIdList, inouts := JunoAST.EmptyIdList,
      ins := JunoAST.EmptyIdList, bp := JunoAST.End);
    proc_decl := NEW(JunoAST.ProcDecl, bp := JunoAST.End,
      header := proc_head, body := cmd);
    proc := JunoScope.NewProc(proc_decl, mod := NIL);
  BEGIN
    EVAL JunoCompile.ProcDecl(ccmd_proc, proc, scp);
    RETURN proc.index
  END Cmd;

VAR
  if := NEW(JunoAST.If, bp := JunoAST.End);
  seq := NEW(JunoAST.Seq, bp := JunoAST.End, c1 := if, c2 := JunoAST.HaltVal);

PROCEDURE CurrCmd(
    cmd: JunoAST.Cmd;
    scp: JunoScope.T;
    checkTotal := FALSE):
    CARDINAL RAISES {JunoCompileErr.Error} =
  BEGIN
    if.body := cmd;
    IF checkTotal THEN JunoChkBNF.TotalCmd(if) END;
    RETURN Cmd(seq, scp)
  END CurrCmd;

BEGIN
END JunoBuild.
