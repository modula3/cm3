(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: IOPaneMan.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE IOPaneMan;
IMPORT PaneManVBT;
IMPORT TermIO;
TYPE
  T <: Public;
  Public = PaneManVBT.T OBJECT
  METHODS
    getIO(): TermIO.T;
  END;
END IOPaneMan.
