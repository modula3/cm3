(* Copyright 1992 Digital Equipment Corporation.            *)
(* Distributed only by permission.                          *)
(* Last modified on Sun May  1 17:25:26 PDT 1994 by najork  *)
(*      modified on Thu Sep 24 12:37:48 PDT 1992 by mhb     *)
(*      modified on Thu Jul 23 00:02:50 1992 by karsenty    *)

MODULE Main EXPORTS Main;

IMPORT MaxflowBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact(
    "Maximum Flow",
    Rsrc.BuildPath("$MAXFLOW", MaxflowBundle.Get()));
END Main.


