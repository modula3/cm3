(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: VBTPane.mg,v 1.2 2001-09-19 14:22:14 wagner Exp $ *)

GENERIC MODULE VBTPane(SubPaneVBT);
IMPORT Pane;
IMPORT PaneVBT;
IMPORT PaneManVBT;
IMPORT Rd, Wr;
IMPORT VBT;

REVEAL
  T = Pane.T BRANDED OBJECT
  OVERRIDES
    read := Read;
    write := Write;
    clone := Clone;
  END;

PROCEDURE Read(self: T; rd: Rd.T := NIL): VBT.T =
  VAR
    pm := NARROW(self.paneMan, PaneManVBT.T);
  BEGIN
    RETURN NEW(SubPaneVBT.T).init(rd, pm.request);
  END Read;

PROCEDURE Write(self: T; wr: Wr.T) =
  VAR
    paneVBT := NARROW(self.pane, PaneVBT.T);
  BEGIN
    paneVBT.write(wr);
  END Write;

PROCEDURE Clone(self: T): VBT.T =
  VAR
    paneVBT := NARROW(self.pane, PaneVBT.T);
  BEGIN
    RETURN paneVBT.clone();
  END Clone;

BEGIN
END VBTPane.
