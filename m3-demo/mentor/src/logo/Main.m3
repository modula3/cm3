(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Tue Sep 29 14:06:56 PDT 1992 by steveg     *)
(*      modified on Wed Sep 23 15:45:08 PDT 1992 by mhb        *)
(*      modified on Thu Aug 20 15:44:35 PDT 1992 by johnh      *)

MODULE Main EXPORTS Main;

IMPORT Rsrc, LogoBundle, ZeusPanel;

BEGIN
  ZeusPanel.Interact("Logo", Rsrc.BuildPath("LOGOPATH", LogoBundle.Get()))
END Main.
