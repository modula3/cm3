(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 20 21:06:24 PDT 1995 by heydon                   *)

MODULE ExternalProc;

IMPORT View, JunoAST, JunoScope, JunoRT;

VAR
  root: View.Root;
  modName: JunoAST.Id;
  scope: JunoScope.T;

PROCEDURE SetupBind(mod: JunoAST.Id; scp: JunoScope.T; rt: View.Root) =
  BEGIN
    root := rt;
    modName := mod;
    scope := scp
  END SetupBind;

PROCEDURE Bind(name: JunoAST.Id; cl: Closure; in, out := 0) =
  <* FATAL JunoScope.NameClash *>
  VAR 
    slot := JunoRT.GetExtCodeIndex(
      JunoRT.ProcAttr{modName, name, JunoRT.Sig{out,0,in}}); 
  BEGIN
    cl.rt := root;
    JunoRT.ext_code_tbl[slot] := cl;
    JunoScope.Bind(scope, name, NEW(JunoScope.Proc, index := slot,
      in_cnt := in, inout_cnt := 0, out_cnt := out, body := NIL,
      external := TRUE))
  END Bind;

BEGIN
END ExternalProc.
