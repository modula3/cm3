(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Mon Oct 12 22:47:02 PDT 1992 by ramshaw *)
(*      modified on Tue Jul 21 05:31:59 1992 by mhb *)

MODULE Main EXPORTS Main;

IMPORT HullBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact("Convex Hulls",
                      Rsrc.BuildPath(
                        "$HULLPATH", HullBundle.Get()));
END Main.


