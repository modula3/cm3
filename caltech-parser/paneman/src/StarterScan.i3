(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: StarterScan.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE StarterScan;
IMPORT VBT;
IMPORT Starter, StarterList;
IMPORT PaneFrame;
IMPORT Rd;

CONST
  Brand = "StarterScan";

TYPE
  T <: REFANY;

(* making a T *)

PROCEDURE FromKey(key: CHAR; title: TEXT := NIL): T;

PROCEDURE FromPath(path: TEXT; title: TEXT := NIL): T;

PROCEDURE FromStarter(s: Starter.T; title: TEXT := NIL): T;

PROCEDURE FromRd(rd: Rd.T; s: Starter.T; path, title: TEXT := NIL): T;

PROCEDURE Default(): T;


(* For the PaneManVBT: using a T to get a path or PaneFrame *)

PROCEDURE GetPath(from: T): TEXT;

PROCEDURE NewPaneFrame(s: StarterList.T; paneMan: VBT.T;
                       from: T): PaneFrame.T;

PROCEDURE Equal(a, b: T): BOOLEAN;
END StarterScan.
