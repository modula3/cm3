(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Sep 24 11:16:39 PDT 1992 by mhb *)

MODULE Main EXPORTS Main;

IMPORT BinpackBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact(
    "Binpacking",
    Rsrc.BuildPath("$BINPACKPATH", BinpackBundle.Get()));
END Main.
