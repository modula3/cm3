(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: MiniPane.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE MiniPane;
IMPORT VBT;
IMPORT PaneFrame;
IMPORT TextVBT;
IMPORT TextPaneSquat;
IMPORT Debug;
IMPORT Split;
IMPORT HVSplit;
IMPORT Axis;
CONST
  DebugLevel = 90;
REVEAL
  T = TextPaneSquat.T BRANDED OBJECT
  OVERRIDES
    installPane := InstallPane;
  END;
PROCEDURE InstallPane(frame: T; pane: VBT.T) =
  BEGIN
    Debug.S("MiniPane.InstallPane", DebugLevel);
    frame := HVSplit.T.init(frame, Axis.T.Ver, adjustable := FALSE);
    frame.status := NEW(TextVBT.T).init("...");
    frame.pane := pane;
    Split.AddChild(frame, pane);
  END InstallPane;
BEGIN
END MiniPane.
