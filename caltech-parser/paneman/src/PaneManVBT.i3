(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE PaneManVBT;
IMPORT HVSplit;
IMPORT VBT;
IMPORT Axis;
IMPORT StarterScan, StarterList;
IMPORT PaneManOp;
IMPORT StarterScanList;
TYPE
  T <: Public;
  Public = HVSplit.T OBJECT
    time: VBT.TimeStamp := 0;     (* Time of last user command *)
    stealFocusWarning: BOOLEAN := FALSE;
    (* Set if the PaneManVBT is about to mysteriously lose the
       keyboard focus even though the head pane shall not be unhilited.
       only used in TextPane.m3 because TextPorts are thieves *)
    mu: MUTEX;
    request: PaneManOp.T;
  METHODS
    init(s: StarterList.T; startingStarters: StarterScanList.T := NIL): T;

    (* caller of these methods must lock mu. *)
    focusTo(pane: VBT.T);
    rotFocus(reverse: BOOLEAN := FALSE);
    splitPane(hv: Axis.T);
    unSplit();
    unSplit1();
    setPane(from: StarterScan.T): BOOLEAN;
    listPanes();
    inputPath(forWrite: BOOLEAN);
    cookedKey(READONLY key: VBT.KeyRec);
    getSelectedPane(): VBT.T;

    (* i/o. caller must lock mu. *)
    print(message: TEXT);
    input(prompt, default: TEXT; result: InputCallback);
    (* on callback, mu is locked. *)

    setPaneUpdater(pu: PaneUpdater);
    (* apply to all visible panes, and to any
       panes that become visible later.
       call with "mu" locked. "mu" is locked on "apply" call.*)
  END;
  InputCallback = PaneManOp.InputCallback;
  PaneUpdater = OBJECT METHODS
    apply(pane: VBT.T);
  END;

END PaneManVBT.
