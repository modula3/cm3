(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Jul 14 16:51:52 PDT 1993 by harrison *)
(*      modified on Sun Jul 11 11:14:21 PDT 1993 by mhb *)

MODULE Main;

IMPORT BresenhamBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact(
    "Bresenham",
    Rsrc.BuildPath("$BRESENHAMPATH", BresenhamBundle.Get()));
END Main.
