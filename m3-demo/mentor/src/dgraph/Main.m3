(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Fri Sep 25 11:14:05 PDT 1992 by mjordan*)
(*      modified on Tue Jul 21 05:31:59 1992 by mhb *)

MODULE Main EXPORTS Main;

IMPORT ZeusPanel, DGraphBundle, Rsrc;

BEGIN
  ZeusPanel.Interact("MENTOR: Directed Graphs",
                     Rsrc.BuildPath("$DGraphPATH", DGraphBundle.Get()));
END Main.


