(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Sep 24 13:05:29 PDT 1992 by mhb     *)
(*      modified on Tue Jul 21 13:03:58 PDT 1992 by guarino *)

MODULE Main EXPORTS Main;

IMPORT Rsrc, StringSearchBundle, ZeusPanel;

BEGIN
  ZeusPanel.Interact("String Searching",
                     Rsrc.BuildPath("$STRINGSEARCHPATH",
                                    StringSearchBundle.Get()));
END Main.


