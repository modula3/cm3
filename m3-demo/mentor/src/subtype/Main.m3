(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Jul 28 18:07:19 1993 by luca *)
(*      modified on Tue Jul 20 13:13:36 PDT 1993 by hania *)

MODULE Main EXPORTS Main;

IMPORT SubtypeBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact(
    "Subtyping",
    Rsrc.BuildPath("$SUBTYPEPATH", SubtypeBundle.Get()));
END Main.
