(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: TextSubsPaneUpdater.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE TextSubsPaneUpdater;
IMPORT TextSubs;
IMPORT PaneManVBT;

TYPE
  T <: Public;
  Public = PaneManVBT.PaneUpdater OBJECT
  METHODS
    init(): T;

    setSubs(subs: TextSubs.T);
    (* if not called from same thread as "apply",
       must call "setSubs" and "apply" with "paneMan.mu" locked. *)

  END;

END TextSubsPaneUpdater.
