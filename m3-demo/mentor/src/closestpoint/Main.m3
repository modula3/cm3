(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE Main EXPORTS Main;

IMPORT CPBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact(
    "Closest Pair",
    Rsrc.BuildPath("$CLOSESTPAIRPATH", CPBundle.Get()));
END Main.
