(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Sep 24 12:55:39 PDT 1992 by mhb                      *)
(*      modified on Wed Aug  5 11:31:47 PDT 1992 by heydon                   *)

MODULE Main;

IMPORT Rsrc, SearchTreeBundle, ZeusPanel;

BEGIN
  ZeusPanel.SetSessTitle("SearchTree", "Search Tree");
  ZeusPanel.Interact(
    "Search Tree Animations",
    Rsrc.BuildPath("$SEARCHTREEPATH", SearchTreeBundle.Get()));
END Main.

