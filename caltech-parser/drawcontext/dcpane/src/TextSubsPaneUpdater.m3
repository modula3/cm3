(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: TextSubsPaneUpdater.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE TextSubsPaneUpdater;
IMPORT TextSubs;
IMPORT VBT;
IMPORT VBTClass;
IMPORT Pane;
IMPORT PSReaderPaneVBT;

REVEAL
  T = Public BRANDED "TextSubsPaneUpdater" OBJECT
    subs: TextSubs.T := NIL;
  OVERRIDES
    init := Init;
    setSubs := SetSubs;
    apply := Apply;
  END;

PROCEDURE Init(self: T): T =
  BEGIN
    RETURN self;
  END Init;

PROCEDURE SetSubs(self: T; subs: TextSubs.T) =
  BEGIN
    self.subs := subs;
  END SetSubs;

PROCEDURE Apply(self: T; v: VBT.T) =
  VAR
    pane := NARROW(v, Pane.T).pane;
  BEGIN
    TYPECASE pane OF
    | PSReaderPaneVBT.T(ps) =>
      ps.setSubs(self.subs);
    ELSE
    END;
  END Apply;

BEGIN
END TextSubsPaneUpdater.
