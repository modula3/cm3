(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Main.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE Main;
IMPORT PaneMan;
IMPORT TestVBTStarter;
BEGIN
  PaneMan.Run(TestVBTStarter.S);
END Main.
