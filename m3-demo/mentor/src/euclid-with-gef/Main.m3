(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Mon Feb 22 10:23:18 PST 1993 by steveg *)
(*      modified on Thu Sep 24 12:14:33 PDT 1992 by mhb *)

MODULE Main EXPORTS Main;

IMPORT EuclidBundle, gefBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact(
    "Euclid P47",
    Rsrc.BuildPath("$EUCLIDPATH", gefBundle.Get(), EuclidBundle.Get()));
END Main.


