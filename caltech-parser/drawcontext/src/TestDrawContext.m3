(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: TestDrawContext.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE TestDrawContext;
IMPORT DrawContext;
IMPORT DrawContextClass;
IMPORT Region;
IMPORT Rect;
REVEAL
  T = Public BRANDED "TestDrawContext" OBJECT
    vis := FALSE;
  OVERRIDES
    init := Init;
    reset := Reset;
    visible := Visible;
    gBox := GBox;
  END;

PROCEDURE Init(self: T; dc: DrawContext.T): T =
  BEGIN
    self.setClip(dc.clip);
    self.setTransform(dc.transform);
    self.setTextBounder(dc.textBounder);
    self.vis := FALSE;
    RETURN self;
  END Init;

PROCEDURE Reset(self: T) =
  BEGIN
    self.vis := FALSE;
  END Reset;

PROCEDURE Visible(self: T): BOOLEAN =
  BEGIN
    RETURN self.vis;
  END Visible;

PROCEDURE GBox(self: T; r: Rect.T) =
  BEGIN
    self.vis := self.vis OR Region.OverlapRect(r,self.clip);
  END GBox; 

BEGIN
END TestDrawContext.
