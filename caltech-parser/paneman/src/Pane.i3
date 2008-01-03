(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Pane.i3,v 1.2 2001-09-19 14:22:13 wagner Exp $ *)

INTERFACE Pane;
IMPORT HVSplit;
IMPORT VBT;
IMPORT Rd, Wr;
CONST
  Brand = "Pane";
TYPE
  T <: Public;
  Public = HVSplit.T OBJECT
    path: TEXT;
    pane: VBT.T := NIL;        (* main document-specific VBT *)
    paneMan: VBT.T := NIL;
  METHODS
    read(rd: Rd.T := NIL): VBT.T; (* make new VBT to be assigned to pane *)
    clone(): VBT.T;               (* cloned VBT to be assigned to pane *)
    write(wr: Wr.T);
    discard();

    hilite(state: BOOLEAN);
    (* default: call self.pane.misc with
       TakeSelection/Lost, keyboard focus *)
  END;
  (* preferably the pane should not acquire the keyboard focus
     directly from Trestle. Otherwise Pane.T must behave like TextPane.T *)
PROCEDURE Equal(a, b: T): BOOLEAN;
END Pane.

