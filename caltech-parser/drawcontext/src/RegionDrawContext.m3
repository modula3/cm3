(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: RegionDrawContext.m3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

MODULE RegionDrawContext;
IMPORT Region;
IMPORT Rect;
IMPORT DrawContextClass;
IMPORT TextBounder;
REVEAL
  T = Public BRANDED OBJECT
    region := Region.Empty;
  OVERRIDES
    init := Init;
    toRegion := GetRegion;
    gBox := GBox;
  END;

PROCEDURE Init(self: T; textBounder: TextBounder.T): T =
  BEGIN
    self.region := Region.Empty;
    self.textBounder := textBounder;
    RETURN self;
  END Init;

PROCEDURE GetRegion(self: T): Region.T =
  BEGIN
    RETURN self.region;
  END GetRegion;

PROCEDURE GBox(self: T; r: Rect.T) =
  BEGIN
    self.region := Region.JoinRect(r, self.region);
  END GBox; 

BEGIN
END RegionDrawContext.
