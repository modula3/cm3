(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: TextSubsViewer.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE TextSubsViewer;
IMPORT TextSubs;
IMPORT StarterScanList;
IMPORT TermIO;

TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(startingStarters: StarterScanList.T := NIL): T;
    setSubs(subs: TextSubs.T);
    getIO(): TermIO.T;
    install(closingKillsProcess := TRUE);
    awaitDelete();
  END;

END TextSubsViewer. 
