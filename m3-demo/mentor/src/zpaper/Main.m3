(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Thu Sep 24 13:16:29 PDT 1992 by mhb            *)

MODULE Main EXPORTS Main;

IMPORT Rsrc, ZeusPanel, ZPaperBundle;

BEGIN
  ZeusPanel.Interact(
    "Zeus Wallpaper",
    Rsrc.BuildPath("$ZPAPERPATH", ZPaperBundle.Get()));
END Main.


