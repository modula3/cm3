(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jul 25 14:07:43 PDT 1994 by heydon                   *)

MODULE Main;

IMPORT UnionFindBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact("UnionFind",
    Rsrc.BuildPath("$UnionFindPATH", UnionFindBundle.Get()))
END Main.
