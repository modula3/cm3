(* Copyright 1993 by Digital Equipment Corp. *)

MODULE Main;

IMPORT ShortestPathBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact(
    "ShortestPath",
    Rsrc.BuildPath("$SHORTESTPATH", ShortestPathBundle.Get()))
END Main.
