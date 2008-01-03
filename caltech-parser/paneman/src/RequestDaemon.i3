(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: RequestDaemon.i3,v 1.2 2001-09-19 14:22:13 wagner Exp $ *)

INTERFACE RequestDaemon;
IMPORT PaneManVBT;
IMPORT PaneManOp;
TYPE
  T <: Public;
  Public = PaneManOp.T OBJECT METHODS
    init(pm: PaneManVBT.T): T;
  END;
END RequestDaemon.
