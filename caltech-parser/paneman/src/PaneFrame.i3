(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PaneFrame.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE PaneFrame;
IMPORT Pane;
IMPORT PaneList;
IMPORT Starter;
IMPORT VBT;
IMPORT Rd;
REVEAL
  Pane.T <: Public;
TYPE
  T = Pane.T;
  Public = Pane.Public OBJECT
    status: VBT.T;
    kind, title: TEXT;
    canHilite: BOOLEAN := TRUE;
    hilited: BOOLEAN := FALSE; (* TRUE if pane is accepting keystrokes *)
    visible: BOOLEAN := FALSE; (* primary panes may not be visible *)
    primary: BOOLEAN := TRUE;  (* TRUE if not a clone *)
    clones: PaneList.T := NIL; (* paneFrames mirroring this one *)
    parent: T := NIL;          (* paneFrame from which self is cloned *)
    starter: Starter.T;        (* used in frameClone *)
  METHODS
    
    (* "frame" methods called by the pane manager.
       these methods can assume paneMan.mu is locked. *)
    
    init(path, title, kind: TEXT; paneMan: VBT.T;
         starter: Starter.T; insteadRd: Rd.T := NIL): T;
    (* get new pane using read method, and call installPane *)
    
    frameClone(): T;                 (* clone pane and add to parent *)
    frameDiscard();                  (* default: Unclone + PaneDiscard *)
    frameHilite(state: BOOLEAN);     (* default: StatusHilite + PaneHilite *)
    frameVisible(state: BOOLEAN);    (* default is to set visible flag *)
    frameWrite(newPath: TEXT := NIL);(* possibly change path, write method *)


(* methods used in PaneFrame implementation *)

    installPane(pane: VBT.T);
    (* install pane and make status bar *)

    frameUnclone();
    (* If frame is a clone, then remove from parent's clones list
       If frame is primary, reattach clones to the first clone *)

    statusUpdate();    (* update status bar *)

    focusWarn();
  END;

END PaneFrame. 
