(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Wed Sep 23 15:45:08 PDT 1992 by mhb        *)
(*      modified on Thu Aug 20 15:44:35 PDT 1992 by johnh      *)

MODULE Main EXPORTS Main;

IMPORT Rsrc, SortBundle, ZeusPanel;

BEGIN
  ZeusPanel.Interact(
    "Sorting", Rsrc.BuildPath("$SORTINGPATH", SortBundle.Get()))
END Main.
