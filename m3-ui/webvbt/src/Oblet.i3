(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jan 18 11:22:20 PST 1996 by najork                   *)
(*      modified on Thu Jan 18 11:00:51 PST 1996 by mhb                      *)

INTERFACE Oblet;

IMPORT HTMLVBTG, HTML, VBT;

PROCEDURE DisplayOblet (v: HTMLVBTG.T; vsplit: VBT.T; s: HTMLVBTG.State; 
    oblet: HTML.Oblet);

END Oblet.
