(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE TextSubsViewer;
IMPORT TextPaneStarter;
IMPORT PSReaderStarter;
IMPORT IOPaneMan;
IMPORT TextSubsPaneUpdater;
IMPORT StarterScanList;
IMPORT TextSubs;
IMPORT StarterList;
IMPORT Trestle;
IMPORT TrestleComm;
IMPORT TrestleExtras;
IMPORT TermIO;

<* FATAL TrestleComm.Failure *>

REVEAL
  T = Public BRANDED "PSViewer" OBJECT
    paneMan: IOPaneMan.T;
    paneUpdater: TextSubsPaneUpdater.T;
  OVERRIDES
    init := Init;
    setSubs := SetSubs;
    install := Install;
    awaitDelete := AwaitDelete;
    getIO := GetIO;
  END;

PROCEDURE Init(self: T; startingStarters: StarterScanList.T := NIL): T =
  VAR
    s := StarterList.List2(TextPaneStarter.S, PSReaderStarter.S);
  BEGIN
    self.paneUpdater := NEW(TextSubsPaneUpdater.T).init();
    self.paneMan := NEW(IOPaneMan.T).init(s, startingStarters);
    RETURN self;
  END Init;

PROCEDURE SetSubs(self: T; subs: TextSubs.T) =
  BEGIN
    LOCK self.paneMan.mu DO
      self.paneUpdater.setSubs(subs);
      self.paneMan.setPaneUpdater(self.paneUpdater);
    END;
  END SetSubs;

PROCEDURE Install(self: T; closingKillsProcess := TRUE) =
  BEGIN
    Trestle.Install(self.paneMan);
    IF closingKillsProcess THEN
      TrestleExtras.LazyAwaitDelete(self.paneMan);
    END;
  END Install;

PROCEDURE AwaitDelete(self: T) =
  BEGIN
    Trestle.AwaitDelete(self.paneMan);
  END AwaitDelete;

PROCEDURE GetIO(self: T): TermIO.T =
  BEGIN
    RETURN self.paneMan.getIO();
  END GetIO;

BEGIN
END TextSubsViewer. 
