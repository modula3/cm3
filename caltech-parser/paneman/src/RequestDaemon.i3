(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE RequestDaemon;
IMPORT PaneManVBT;
IMPORT PaneManOp;
TYPE
  T <: Public;
  Public = PaneManOp.T OBJECT METHODS
    init(pm: PaneManVBT.T): T;
  END;
END RequestDaemon.
