(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

MODULE Main EXPORTS Main;

IMPORT WheelerBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact(
    "Wheeler",
    Rsrc.BuildPath("$WHEELERPATH", WheelerBundle.Get()));
END Main.
