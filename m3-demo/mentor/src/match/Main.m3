(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Sun May  1 16:28:45 PDT 1994 by najork     *)
(*      modified on Thu Jul 16 22:58:24 1992 by mhb            *)

MODULE Main;

IMPORT MatchBundle, Rsrc, ZeusPanel;

BEGIN
  ZeusPanel.SetTitle("MENTOR: Match");
  ZeusPanel.Interact ("Matching", 
                      Rsrc.BuildPath ("$MATCHPATH", MatchBundle.Get()));
END Main.
