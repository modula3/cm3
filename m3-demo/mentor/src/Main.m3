(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Tue May  3 17:36:41 PDT 1994 by najork     *)
(*      modified on Thu Sep 24 16:12:53 PDT 1992 by mhb        *)
(*      modified on Thu Sep  3 18:04:23 PDT 1992 by johnh      *)
(*      modified on Thu Jul 23 00:51:45 1992 by steveg         *)

MODULE Main;

IMPORT MentorBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.Interact(
    "MENTOR: SRC Algorithm Animations",
    Rsrc.BuildPath ("$MENTORPATH", MentorBundle.Get()));
END Main.
