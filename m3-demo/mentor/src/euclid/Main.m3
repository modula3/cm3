(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Sep 24 12:14:33 PDT 1992 by mhb *)

MODULE Main EXPORTS Main;

IMPORT EuclidBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact(
    "Euclid P47",
    Rsrc.BuildPath("$EUCLIDPATH", EuclidBundle.Get()));
END Main.


