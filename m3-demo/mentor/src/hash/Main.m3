(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Sep 24 12:23:55 PDT 1992 by mhb   *)
(*      modified on Fri Jul 31 17:04:00 PDT 1992 by swart *)

MODULE Main EXPORTS Main;

IMPORT HashBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact(
    "Hashing", Rsrc.BuildPath("$HASHPATH", HashBundle.Get()));
END Main.


