(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PaneVBT.i3,v 1.2 2001-09-19 14:22:13 wagner Exp $ *)

INTERFACE PaneVBT;
(* a PaneVBT is the main VBT displayed in a VBTPane.
   most document panes will be VBTPanes. *)
IMPORT VBT;
IMPORT Rd, Wr;
IMPORT PaneManOp;
TYPE
  T = VBT.Leaf OBJECT
  METHODS
    init(rd: Rd.T; pm: PaneManOp.T): T;
    write(wr: Wr.T);
    clone(): T;
  END;

END PaneVBT.
