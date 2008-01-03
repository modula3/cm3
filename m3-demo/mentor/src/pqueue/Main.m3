(* Copyright 1992 Digital Equipment Corporation.            *)
(* Distributed only by permission.                          *)
(* Last modified on Tue May  3 09:49:05 PDT 1994 by najork  *)
(*      modified on Thu Sep 24 12:47:49 PDT 1992 by mhb     *)
(*      modified on Fri Jul 24 02:13:34 1992 by owicki      *)

MODULE Main EXPORTS Main;

IMPORT PQueueBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact(
      "Priority Queue", 
      Rsrc.BuildPath ("$PQUEUEPATH", PQueueBundle.Get()));
END Main.
