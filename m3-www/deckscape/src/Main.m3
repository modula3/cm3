(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Mon Aug 19 22:13:52 PDT 1996 by mhb       *)

MODULE Main EXPORTS Main;

IMPORT ParseParams, Stdio, Storage, Trestle, VBT, WorkspaceVBT;

<* FATAL ANY *>

VAR v: VBT.T;
BEGIN 
  WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
    IF pp.keywordPresent("-clean") THEN
      v := WorkspaceVBT.New()
    ELSE
      v := Storage.Restore()
    END
  END;
  Trestle.Install(v);
  Trestle.AwaitDelete(v);
  Storage.Save(v);
END Main.
