(* Copyright 1992 Digital Equipment Corporation.            *)
(* Distributed only by permission.                          *)
(* Last modified on Thu Apr 28 16:09:44 PDT 1994 by najork  *)
(*      modified on Thu Sep 24 12:43:33 PDT 1992 by mhb     *)
(*      modified on Fri Jul 24 21:55:56 1992 by kalsow      *)

MODULE Main;

IMPORT ParseBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact(
    "Parsing", Rsrc.BuildPath("$PARSEPATH", ParseBundle.Get()));
END Main.
