(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

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
